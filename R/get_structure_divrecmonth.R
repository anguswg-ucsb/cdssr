#' Return Structure Monthly Diversion/Release Records
#' @description Make a request to the api/v2/structures/divrec/divrecmonth/ endpoint to retrieve monthly structure  diversion/release data for a specified WDID within a specified date range.
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Defaults to "diversion"
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @return dataframe with monthly structure data for the CDSS structure of interest
get_structure_divrecmonth<- function(
    wdid            = NULL,
    wc_identifier   = "diversion",
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # check if parameter is valid
  if(!wc_identifier %in% c("diversion", "release")) {

    stop(paste0("Invalid `wc_identifier` argument \nMust be one of: \n'diversion' or 'release'"))
  }

  # check if valid WDID was entered
  if(is.null(wdid)) {

    stop(paste0("Please enter a valid WDID"))

  }

  # Base API URL for Monthly Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?"

  # format multiple WDID query
  if(!is.null(wdid)) {

    # if abbreviations are in a list, unlist to a character vector
    if(is.list(wdid) == TRUE) {

      wdid <- unlist(wdid)

    }

    wdid <- paste0(unlist(strsplit(wdid, " ")), collapse = "%2C+")

  }

  # reformat MM-DD-YYYY dates to MM-YYYY format for API query
  start <- gsub("-", "%2F",   format(as.Date(start_date, '%Y-%m-%d'), "%m-%Y"))
  end   <- gsub("-", "%2F",   format(as.Date(end_date, '%Y-%m-%d'), "%m-%Y"))

  # format wcidentifer query
  wc_identifier <- paste0(gsub(":", "%3A",   unlist(strsplit(wc_identifier, " "))), collapse = "+")

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving monthly ", wc_identifier, " data from CDSS API..."))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&fields=wdid%2CwaterClassNum%2CwcIdentifier%2CmeasInterval%2CdataMeasDate%2CdataValue%2CmeasUnits%2CobsCode%2CapprovalStatus",
      "&min-dataMeasDate=", start,
      "&max-dataMeasDate=", end,
      "&wcIdentifier=*", wc_identifier,
      "*&wdid=", wdid,
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
                       "\nEnd date: ", end_date,
                       "\nWC Identifier: ", wc_identifier))
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
      # dplyr::rename(
      #   "interval" = "measInterval",
      #   "date"     = "dataMeasDate",
      #   "value"    = "dataValue",
      #   "unit"     = "measUnits"
      # ) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        datetime = as.Date(paste0(data_meas_date, "-01"))
        )

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


