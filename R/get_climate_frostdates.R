utils::globalVariables(c("."))
#' Return climate stations frost dates
  #' @description Make a request to the /climatedata/climatestationfrostdates endpoint to retrieve climate stations frost dates data by station number within a given date range (start and end dates)
  #' @param station_number character, climate data station number
  #' @param start_date character date to request data start point YYYY-MM-DD
  #' @param end_date character date to request data end point YYYY-MM-DD
  #' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
  #' @importFrom httr GET content
  #' @importFrom jsonlite fromJSON
  #' @return dataframe of climate data frost dates data
  #' @export
  get_climate_frostdates <- function(
      station_number      = NULL,
      start_date          = "1900-01-01",
      end_date            = Sys.Date(),
      api_key             = NULL
  ) {

    # list of function inputs
    input_args <- as.list(environment())

    # check function arguments for missing/invalid inputs
    arg_lst <- check_args(
      arg_lst = input_args,
      ignore  = c("api_key", "start_date", "end_date"),
      f       = "any"
    )

    # if invalid/missing arguments found, stop function
    if(!is.null(arg_lst)) {

      stop(arg_lst)

    }

    # base API URL
    base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationfrostdates/?"

    # # convert arguments to strings if necessary
    # str_args(
    #   arg_lst = as.list(environment()),
    #   envir   = environment()
    # )

    # convert arguments to characters if necessary
    station_number       <- null_convert(station_number)

    # reformat and extract valid start date
    start <- parse_date(
      date   = start_date,
      start  = TRUE,
      format = "%Y"
      )

    # reformat and extract valid end date
    end <- parse_date(
      date   = end_date,
      start  = FALSE,
      format = "%Y"
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
    message(paste0("Retrieving climate station frost date data"))

    # while more pages are available, send get requests to CDSS API
    while (more_pages) {

      # Construct query URL w/o API key
      url <- paste0(
        base,
        "format=json&dateFormat=spaceSepToSeconds",
        "&min-calYear=", start,
        "&max-calYear=", end,
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

      # set clean names
      names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

      # bind data from this page
      data_df <- rbind(data_df, cdss_data)

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
