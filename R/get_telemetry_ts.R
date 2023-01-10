#' Return Telemetry station timeseries data
#' @description Make a request to the /telemetrystations/telemetrytimeseries endpoint to retrieve raw, hourly, or daily telemetry station timeseries data by station abbreviations, within a given date range (start and end dates).
#' @param abbrev character indicating station abbreviation
#' @param parameter character indicating which parameter should be retrieved. Default is "DISCHRG" (discharge), all parameters are not avaliable at all telemetry stations.
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating data type to return, either "raw", "hour", or "day". Default is "day"
#' @param include_third_party logical, Whether to retrieve data from other third party sources if necessary. Default is TRUE
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe with telemetry station timeseries data
#' @export
#' @examples
#' # Retrieve daily discharge for CLAFTCCO telemetry station
#' ts <- get_telemetry_ts(
#'            abbrev              = "CLAFTCCO",
#'            parameter           = "DISCHRG",
#'            start_date          = "2015-01-01",
#'            end_date            = "2022-01-01",
#'            timescale           = "day",
#'            include_third_party = TRUE
#'            )
#' ts
#'
#' # Plot daily discharge data
#' plot(ts$value~ts$datetime, type = "l")
get_telemetry_ts <- function(
    abbrev              = NULL,
    parameter           = "DISCHRG",
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    timescale           = "day",
    include_third_party = TRUE,
    api_key             = NULL
) {

  # check if valid abbreviation was given
  if(is.null(abbrev)) {

    stop(paste0("Please enter a valid Telemetry station abbreviation"))

  }

  # Base API URL for Daily Diversion Records
  base <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeseries", timescale, "/?")

  # reformat dates to MM-DD-YYYY and format for API query
  start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
  end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Downloading data from CDSS API...\nTelemetry station abbreviation: ", abbrev,
                 "\nParameter: ", parameter,
                 "\nTimescale: ", timescale)
          )

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Set correct name of date field for querying raw data
    if(timescale == "raw") {

      # raw date field name
      date_field <- "measDateTime"

    } else {

      # hour and day date field name
      date_field <- "measDate"

    }

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
      "&abbrev=", abbrev,
      "&endDate=", end,
      "&startDate=", start,
      "&includeThirdParty=", tolower(as.character(include_third_party)),
      "&parameter=", parameter,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # check whether to use API key or not
    if(!is.null(api_key)) {

      # Construct query URL w/ API key
      url <- paste0(url, "&apiKey=", api_key)

    }

    # GET request to CDSS API
    tryCatch(
      {
        # query CDSS API
        cdss_data <-
          url %>%
          httr::GET() %>%
          httr::content(as = "text") %>%
          jsonlite::fromJSON() %>%
          dplyr::bind_rows() %>%
          .$ResultList

      },
      error = function(e) {

        message(paste0("Error in data retrieval at telemetry station: ", abbrev))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\nTelemetry station: ", abbrev,
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nParameter: ", parameter))
        message(paste0('\nHere is the URL address that was queried:\n'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(paste0('-----------------------------------------'))
        message(e)
        stop()

      }
    )

    # Standardize names
    if(timescale == "raw") {

      # rename columns for raw data return
      cdss_data <-
        cdss_data %>%
        dplyr::rename(
          "date"  = "measDateTime",
          "value" = "measValue",
          "unit"  = "measUnit"
          )

    } else {

      # rename columns for hour and day data return
      cdss_data <-
        cdss_data %>%
        dplyr::rename(
          "date"  = "measDate",
          "value" = "measValue",
          "unit"  = "measUnit"
          )

    }

    # Tidy data
    cdss_data <-
      cdss_data %>%
      dplyr::mutate(
        datetime   = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
        timescale  = timescale
      ) %>%
      janitor::clean_names()

    # bind data from this page
    data_df <- dplyr::bind_rows(data_df, cdss_data)

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  # return final binded dataframe
  return(data_df)

}


