utils::globalVariables(c("."))
#' Return parcel use time series
#' @description Make a request to the /structures/parcelusets endpoint to retrieve parcel use time series data by WDID, within a given date range (start and end dates)
#' @param wdid character or numeric, indicating WDID code of structure
#' @param start_date character date to request starting year to retrieve data (YYYY-MM-DD). Default start date is "1900-01-01", therefore requests data starting from 1900.
#' @param end_date character date to  request ending year to retrieve data (YYYY-MM-DD). Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of parcel use time series data
#' @export
get_parceluse_ts <- function(
    wdid          = NULL,
    start_date    = "1900-01-01",
    end_date      = Sys.Date(),
    api_key       = NULL
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
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/parcelusets/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wdid  <- null_convert(wdid)

  # reformat and extract valid start date
  start <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%Y",
    sep    = ""
  )

  # reformat and extract valid end date
  end <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%Y",
    sep    = ""
  )

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving parcel use time series"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-calYear=", start,
      "&max-calYear=", end,
      "&wdid=", wdid,
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

    # set datetime column
    # cdss_data$datetime <-  as.POSIXct(cdss_data$meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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
