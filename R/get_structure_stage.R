#' Request Structure stage/volume Records
#'
#' @param wdid character indicating WDID code of structure
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate relocate
#' @importFrom janitor clean_names
#' @return dataframe with stage/volume data for CDSS structure of interest
get_structure_stage <- function(
    wdid            = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # check if valid WDID was entered
  if(is.null(wdid)) {
    stop(paste0("Please enter a valid WDID"))
  }

  # Base API URL for Daily Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/stagevolume/?"

  # reformat dates to MM-DD-YYYY and format for API query
  start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
  end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Downloading data from CDSS API...\nStage/Volume records\nWDID: ", wdid))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-dataMeasDate=", start,
      "&max-dataMeasDate=", end,
      "&wdid=", wdid,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # check whether to use API key or not
    if(!is.null(api_key)) {

      # construct query URL w/ API key
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

        # message(paste0("Error in data retrieval at WDID: ", wdid, "\nPerhaps the URL address is incorrect OR there are no data available.\n"))
        message(paste0("Error in data retrieval at WDID: ", wdid))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\nWDID: ", wdid,
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date))
        message(paste0('Here is the URL address that was queried:'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(e)
        stop()

      }
    )

    # Tidy data
    cdss_data <-
      cdss_data %>%
        janitor::clean_names() %>%
        dplyr::rename(
          "date"     = "data_meas_date"
          ) %>%
      dplyr::mutate(
        datetime   = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
      ) %>%
      dplyr::relocate(wdid, date, datetime, stage, volume)

    # bind data from this page
    data_df <- dplyr::bind_rows(data_df, cdss_data)

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  return(data_df)

}



