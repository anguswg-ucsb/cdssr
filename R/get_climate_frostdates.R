  #' Return climate stations frost dates
  #' @description Make a request to the /climatedata/climatestationfrostdates endpoint to retrieve climate stations frost dates data by station number within a given date range (start and end dates)
  #' @param station_number character, climate data station number
  #' @param start_date character date to request data start point YYYY-MM-DD
  #' @param end_date character date to request data end point YYYY-MM-DD
  #' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
  #' @importFrom httr GET content
  #' @importFrom jsonlite fromJSON
  #' @importFrom dplyr bind_rows mutate `%>%`
  #' @importFrom janitor clean_names
  #' @return dataframe of climate data frost dates data
  get_climate_frostdates <- function(
      station_number      = NULL,
      start_date          = "1900-01-01",
      end_date            = Sys.Date(),
      api_key             = NULL
  ) {

    # check if parameter is valid
    if(is.null(station_number)) {
      stop(paste0("Invalid `station_number` argument"))
    }

    # base API URL
    base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationfrostdates/?"

    # extract start/end years from YYYY-MM-DD for API query
    start_year <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
    end_year   <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")

    # maximum records per page
    page_size  <- 50000

    # initialize empty dataframe to store data from multiple pages
    data_df    <-  data.frame()

    # initialize first page index
    page_index <- 1

    # Loop through pages until there are no more pages to get
    more_pages <- TRUE

    # print message
    message(paste0("Retrieving climate station frost date data from CDSS API..."))

    # while more pages are available, send get requests to CDSS API
    while (more_pages) {

      # Construct query URL w/o API key
      url <- paste0(
        base,
        "format=json&dateFormat=spaceSepToSeconds",
        "&min-calYear=", start_year,
        "&max-calYear=", end_year,
        "&stationNum=", station_number,
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
          message(paste0("Error in climate station frost date query"))
          message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
          message(paste0("Query:\n----------------------------------",
                         "\nStation number: ", station_number,
                         "\nStart date: ", start_date,
                         "\nEnd date: ", end_date
          ))
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
