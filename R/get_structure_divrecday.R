#' Request Structure Daily Diversion/Release Records
#'
#' @param wdid character or numeric indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Defaults to "diversion"
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate relocate
#' @importFrom janitor clean_names
#' @return dataframe with diversion or flow data for CDSS structure of interest
#' @export
#' @examples
#' # Retrieve daily diversion records for given period
#' daily_div <- get_structure_divrecday(
#'                wdid             = "2302917",
#'                wc_identifier    = "diversion",
#'                start_date       = "1988-01-01",
#'                end_date         = "1992-01-01"
#'            )
#' daily_div
#'
#' # plot daily diversions
#' plot(daily_div$value~daily_div$datetime, type = "s")
get_structure_divrecday <- function(
    wdid            = NULL,
    wc_identifier   = "diversion",
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

       # check if valid WDID was entered
       if(is.null(wdid)) {

         stop(paste0("Please enter a valid WDID"))

       }

       # Base API URL for Daily Diversion Records
       base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecday/?"

       # reformat dates to MM-DD-YYYY and format for API query
       start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
       end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

      # maximum records per page
      page_size  <- 50000

      # initialize empty dataframe to store data from multiple pages
      data_df = data.frame()

      # # initialize empty list to store data from multiple pages
      # data_lst   <-  list()

      # initialize first page index
      page_index <- 1

      # Loop through pages until there are no more pages to get
      more_pages <- TRUE

      # while more pages are avaliable, send get requests to CDSS API
      while (more_pages) {

        # check whether to use API key or not
        if(!is.null(api_key)) {

          # construct query URL w/ API key
          url <- paste0(
            base,
            "format=json&dateFormat=spaceSepToSeconds",
            "&fields=wdid%2CwaterClassNum%2CwcIdentifier%2CmeasInterval%2CdataMeasDate%2CdataValue%2CmeasUnits%2CobsCode%2CapprovalStatus",
            "&wcIdentifier=*", wc_identifier,
            "*&min-dataMeasDate=", start,
            "&max-dataMeasDate=", end,
            "&wdid=", wdid,
            "&pageSize=", page_size,
            "&pageIndex=", page_index,
            "&apiKey=", api_key
          )

        } else {

          # Construct query URL w/o API key
          url <- paste0(
            base,
            "format=json&dateFormat=spaceSepToSeconds",
            "&fields=wdid%2CwaterClassNum%2CwcIdentifier%2CmeasInterval%2CdataMeasDate%2CdataValue%2CmeasUnits%2CobsCode%2CapprovalStatus",
            "&wcIdentifier=*", wc_identifier,
            "*&min-dataMeasDate=", start,
            "&max-dataMeasDate=", end,
            "&wdid=", wdid,
            "&pageSize=", page_size,
            "&pageIndex=", page_index
          )

        }


        message(paste0("Downloading data from CDSS API..."))
        message(paste0("WDID: ", wdid))

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
          dplyr::rename(
            "interval" = "measInterval",
            "date"     = "dataMeasDate",
            "value"    = "dataValue",
            "unit"     = "measUnits"
          ) %>%
          dplyr::mutate(
            datetime   = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
            page_index = page_index,
            source     = 'CDSS'
          ) %>%
          janitor::clean_names() %>%
          dplyr::relocate(wdid, water_class_num, wc_identifier, interval, date, datetime, value, unit)

        # bind data from this page
        data_df <- dplyr::bind_rows(data_df, cdss_data)

        # # add data from this page to list
        # data_lst[[page_index]] <- cdss_data

        # Check if more pages to get to continue/stop while loop
        if (nrow(cdss_data) < page_size) {

          more_pages <- FALSE

        } else {

          page_index <- page_index + 1

        }

      }

      # # Bind rows of list of dataframes
      # data_df <- dplyr::bind_rows(data_lst)

      return(data_df)

    }








