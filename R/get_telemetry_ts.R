utils::globalVariables(c("."))
#' Return Telemetry station time series data
#' @description Make a request to the /telemetrystations/telemetrytimeseries endpoint to retrieve raw, hourly, or daily telemetry station time series data by station abbreviations, within a given date range (start and end dates).
#' @param abbrev character indicating station abbreviation
#' @param parameter character indicating which parameter should be retrieved. Default is "DISCHRG" (discharge), all parameters are not avaliable at all telemetry stations.
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating data type to return, either "raw", "hour", or "day". Default is "day"
#' @param include_third_party logical, Whether to retrieve data from other third party sources if necessary. Default is TRUE
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe with telemetry station time series data
#' @export
#' @examples
#' \dontrun{
#' # Retrieve daily discharge for CLAFTCCO telemetry station
#' telem_ts <- get_telemetry_ts(
#'            abbrev              = "CLAFTCCO",
#'            parameter           = "DISCHRG",
#'            start_date          = "2015-01-01",
#'            end_date            = "2022-01-01",
#'            timescale           = "day",
#'            include_third_party = TRUE
#'            )
#'
#' # Plot daily discharge data
#' plot(telem_ts$meas_value~telem_ts$datetime, type = "l")
#' }
get_telemetry_ts <- function(
    abbrev              = NULL,
    parameter           = "DISCHRG",
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    timescale           = "day",
    include_third_party = TRUE,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "parameter", "start_date", "end_date", "timescale"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # list of valid timescales
  timescale_lst <- c("raw", "hour", "day")

  # check if type is NULL, default timescale to "day"
  if(is.null(timescale)) {

    # set timescale to "day"
    timescale <- "day"

  }

  # convert timescale to lowercase
  timescale <- tolower(timescale)

  # check if type is correctly inputted
  if(!timescale %in% timescale_lst) {

    stop(paste0("Invalid `timescale` argument: ", timescale,
                "\nPlease enter one of the following valid timescales:\n",
                paste0("'", c(timescale_lst), "'", collapse = ", ")
                )
      )
    }

  # base URL
  base <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeseries", timescale, "/?")

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   ignore  = c("include_third_party"),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  abbrev        <- null_convert(abbrev)
  parameter     <- null_convert(parameter)

  # reformat and extract valid start date
  start <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%m-%d-%Y",
    sep    = "%2F"
  )

  # reformat and extract valid end date
  end <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%m-%d-%Y",
    sep    = "%2F"
  )

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving telemetry station time series data (",timescale, " - ", parameter, ")"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      # "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
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
    tryCatch({

      # query CDSS API
      cdss_data <- parse_gets(url = url)

      },
      error = function(e) {

        # error message handler
        message(
          query_error(
            arg_lst = input_args,
            ignore  = c("url", "e"),
            url     = url,
            e_msg   = e
            )
          )

        stop()

        })

    # Extract Result List
    cdss_data <- cdss_data$ResultList

    # Standardize names
    if(timescale == "raw") {

      # set clean names
      names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

      # set datetime column
      cdss_data$datetime   <- as.POSIXct(cdss_data$meas_date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
      cdss_data$timescale  <- timescale


    } else {

      # set clean names
      names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

      # set datetime column
      cdss_data$datetime   <- as.POSIXct(cdss_data$meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
      cdss_data$timescale  <- timescale


    }

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)
    # data_df <- dplyr::bind_rows(data_df, cdss_data)

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
