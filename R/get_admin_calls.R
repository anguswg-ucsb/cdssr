utils::globalVariables(c("."))

#' Return active/historic administrative calls data
#' @description Make a request to the api/v2/administrativecalls endpoints to locate active or historical administrative calls by division, location WDID, or call number within a specified date range.
#' @param division character or numeric, indicating the water division to query
#' @param location_wdid character or numeric, call location structure WDID
#' @param call_number numeric, unique call identifier
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param active logical, whether to get active or historical administrative calls. Default iS TRUE and will retrieve active administrative calls.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of administrative calls data
#' @export
#' @examples
#' # Retrieve ACTIVE administrative calls
#' active_calls <- get_admin_calls(
#'                division     = 1,
#'                active       = TRUE
#'            )
#' active_calls
#'
#' # Retrieve HISTORICAL administrative calls
#' historic_calls <- get_admin_calls(
#'                division     = 1,
#'                start_date   = "2000-01-01",
#'                end_date     = "2005-01-01",
#'                active       = FALSE
#'                )
#' historic_calls
get_admin_calls <- function(
  division            = NULL,
  location_wdid       = NULL,
  call_number         = NULL,
  start_date          = "1900-01-01",
  end_date            = Sys.Date(),
  active              = TRUE,
  api_key             = NULL
  ) {

    # check function arguments for missing/invalid inputs
    arg_lst <- check_args(
      arg_lst = as.list(environment()),
      ignore  = c("api_key", "start_date", "end_date", "active"),
      f       = "all"
    )

    # if invalid/missing arguments found, stop function
    if(!is.null(arg_lst)) {

      stop(arg_lst)

    }

    # whether to retrieve active or historical admin calls.
    if(active == TRUE) {

      # Base API URL for Admin calls (ACTIVE)
      base <- "https://dwr.state.co.us/Rest/GET/api/v2/administrativecalls/active/?"

    } else {

      # Base API URL for Admin calls (HISTORICAL)
      base <- "https://dwr.state.co.us/Rest/GET/api/v2/administrativecalls/historical/?"

    }

    # convert arguments to characters if necessary
    division      <- null_convert(division)
    location_wdid <- null_convert(location_wdid)
    call_number   <- null_convert(call_number)

    # stopifnot(is.character(division) | is.character(location_wdid) | is.character(call_number))

    # format multiple location WDID query string
    location_wdid <- collapse_vect(
      x   = location_wdid,
      sep = "%2C+"
    )

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
    message(paste0("Retrieving Administrative calls (", ifelse(active, "ACTIVE", "HISTORICAL"), ")", " data"))

    # while more pages are available, send get requests to CDSS API
    while (more_pages) {

      # Construct query URL w/o API key
      url <- paste0(
        base,
        "format=json&dateFormat=spaceSepToSeconds",
        "&min-dateTimeSet=", start,
        "&max-dateTimeSet=", end,
        "&division=", division,
        "&callNumber=", call_number,
        "&pageSize=", page_size,
        "&pageIndex=", page_index
      )

      # add location WDIDs if given
      if(!is.null(location_wdid)) {

        url <- paste0(
          url,
          "&locationWdid=", location_wdid
        )

      }

      # api_key <- NULL
      # check whether to use API key or not
      if(!is.null(api_key)) {

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

          message(paste0("Error in data retrieval of administrative calls"))
          message(paste0("Perhaps the URL address is incorrect OR there is no data available."))
          message(paste0("Query:\nDivision: ", division,
                         "\nStart date: ", start_date,
                         "\nEnd date: ", end_date,
                         "\nLocation WDID(s): ", location_wdid))
          message(paste0('\nHere is the URL address that was queried:\n'))
          message(paste0(url))
          message(paste0('And, here is the original error message:'))
          message(paste0('-----------------------------------------'))
          message(e)
          stop()

        }
      )

      # set clean names
      names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

      # add extra columns
      cdss_data$active        <- active
      cdss_data$datetime      <- as.POSIXct(cdss_data$date_time_set, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
      cdss_data$priority_date <- as.POSIXct(cdss_data$priority_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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
