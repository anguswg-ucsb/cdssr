#' Request active/historic administrative calls data
#' @param active logical, whether to get active or historical administrative calls. Default iS TRUE and will retrieve active administrative calls.
#' @param min_division numeric, minimum division number to get administrative calls for. Default is 1.
#' @param max_division numeric, maximum division number to get administrative calls for. Default is 7.
#' @param location_wdid numeric, call location structure WDID
#' @param call_number numeric, unique call identifier
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate relocate
#' @importFrom janitor clean_names
#' @return dataframe of administrative calls
#' @export
#' @examples
#' # Retrieve ACTIVE administrative calls
#' active_calls <- get_admin_calls(
#'                active        = TRUE,
#'                min_division  = 1,
#                 max_division  = 7
#'            )
#' active_calls
#'
#' # Retrieve HISTORICAL administrative calls
#' historic_calls <- get_admin_calls(
#'                active         = FALSE,
#'                min_division = 1,
#'                max_division = 7,
#'                start_date   = "2000-01-01",
#'                end_date     = "2005-01-01"
#'                )
#' historic_calls
get_admin_calls <- function(
  active              = TRUE,
  min_division        = NULL,
  max_division        = NULL,
  location_wdid       = NULL,
  call_number         = NULL,
  start_date          = "1900-01-01",
  end_date            = Sys.Date(),
  api_key             = NULL
  ) {

  # whether to retrieve active or historical admin calls.
  if(active == TRUE) {

    # Base API URL for Admin calls (ACTIVE)
    base <- "https://dwr.state.co.us/Rest/GET/api/v2/administrativecalls/active/?"

  } else {

    # Base API URL for Admin calls (HISTORICAL)
    base <- "https://dwr.state.co.us/Rest/GET/api/v2/administrativecalls/historical/?"

  }

  # format location wdids if given
  if(!is.null(location_wdid)) {

    location_wdid <- paste0(location_wdid, collapse = "%2C+")

  }

  # if no min division given
  if(is.null(min_division)) {

    min_division <- 1

  }

  # if no max division given
  if(is.null(max_division)) {

    max_division <- 7

  }

  # if min division is greater than maximum division
  if(min_division > max_division) {

    division <- c(min_division, max_division)

    max_division <- division[which.max(division)]
    min_division <- division[which.min(division)]

  }

  # if min division outside range of divisions, set to the min
  if(min_division > 7 | min_division < 1) {

    min_division <- 1

  }

  # if max division outside range of divisions, set to the max
  if(max_division > 7 | max_division < 1) {

    max_division <- 7

  }

  # # if single division given
  # if(length(divisions) > 1) {
  #
  #   min_division <- min(divisions)
  #   max_division <- max(divisions)
  #
  # } else if(length(divisions == 1)) {
  #
  #   min_division <- min(divisions)
  #   max_division <- max(divisions)
  #
  # } else {
  #
  #   min_division <- 1
  #   max_division <- 7
  #
  # }
  #
  # # if diversion greater than 7 given
  # if(max_division > 7 ) {
  #
  #   max_division <- 7
  #
  # }
  #
  # # if diversion less than 1 given
  # if(min_division < 1 ) {
  #
  #   min_division <- 1
  #
  # }

    # reformat dates to MM-DD-YYYY and format for API query
    start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
    end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

    # maximum records per page
    page_size  <- 50000

    # initialize empty dataframe to store data from multiple pages
    data_df = data.frame()

    # initialize empty list to store data from multiple pages
    # data_lst   <-  list()

    # initialize first page index
    page_index <- 1

    # Loop through pages until there are no more pages to get
    more_pages <- TRUE

    # while more pages are avaliable, send get requests to CDSS API
    while (more_pages) {

      # Construct query URL w/o API key
      url <- paste0(
        base,
        "format=json&dateFormat=spaceSepToSeconds",
        "&min-dateTimeSet=", start,
        "&max-dateTimeSet=", end,
        "&min-division=", min_division,
        "&max-division=", max_division,
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

      message(paste0("Downloading data from CDSS API..."))
      message(paste0("Administrative calls"))

      if(active == TRUE) {
        message(paste0("ACTIVE"))
      } else {
        message(paste0("HISTORICAL"))
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
          message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
          message(paste0("Query:\nMinimum division: ", min_division,
                         "\nMaximum division: ", max_division,
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

      # Tidy data
      cdss_data <-
        cdss_data %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          active         = active,
          datetime       = as.POSIXct(date_time_set, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
          priority_date  = as.POSIXct(priority_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
          page_index     = page_index,
          source         = 'CDSS'
        )

      # bind data from this page
      data_df <- dplyr::bind_rows(data_df, cdss_data)

      # add data from this page to list
      # data_lst[[page_index]] <- cdss_data

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
