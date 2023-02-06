utils::globalVariables(c("."))
#' Return daily climate data
#' @description Make a request to the /climatedata/climatestationtsday endpoint to retrieve climate stations daily time series data by station number, or Site IDs within a given date range (start and end dates)
#' @param station_number character, climate data station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of climate data station daily time series data
get_climate_ts_day <- function(
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
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
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # check if parameter is valid
  if(!param %in% c("Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip",
                     "Snow","SnowDepth", "SnowSWE", "Solar","VP", "Wind")) {

    stop(paste0("Invalid `param` argument \nMust be one of: \nEvap, FrostDate, MaxTemp, MeanTemp, MinTemp, Precip, Snow, SnowDepth, SnowSWE, Solar, VP, Wind"))
  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationtsday/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # format multiple site_id query string
  site_id <- collapse_vect(
    x   = site_id,
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
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving daily climate time series data (", param, ")"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-measDate=", start,
      "&max-measDate=", end,
      "&stationNum=", station_number,
      "&siteId=", site_id,
      "&measType=", param,
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
    cdss_data$datetime <-  as.POSIXct(cdss_data$meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return monthly climate data
#' @description Make a request to the /climatedata/climatestationtsmonth endpoint to retrieve climate stations monthly time series data by station number, or Site IDs within a given date range (start and end dates)
#' @param station_number character, climate data station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of climate data station monthly timeseries data
get_climate_ts_month <- function(
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
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
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # check if parameter is valid
  if(!param %in% c("Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip",
                   "Snow","SnowDepth", "SnowSWE", "Solar","VP", "Wind")) {

    stop(paste0("Invalid `param` argument \nMust be one of: \nEvap, FrostDate, MaxTemp, MeanTemp, MinTemp, Precip, Snow, SnowDepth, SnowSWE, Solar, VP, Wind"))
  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationtsmonth/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # format multiple site_id query string
  site_id <- collapse_vect(
    x   = site_id,
    sep = "%2C+"
  )

  # reformat and extract valid start date
  start_year <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%Y",
    sep    = "%2F"
  )

  # reformat and extract valid end date
  end_year <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%Y",
    sep    = "%2F"
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
  message(paste0("Retrieving monthly climate time series data (", param, ")"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-calYear=", start_year,
      "&max-calYear=", end_year,
      "&stationNum=", station_number,
      "&siteId=", site_id,
      "&measType=", param,
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
    cdss_data$datetime <-  as.POSIXct(
                                paste0(cdss_data$cal_year, "-",
                                       ifelse(cdss_data$cal_month_num > 9, cdss_data$cal_month_num, paste0("0", cdss_data$cal_month_num)),
                                       "-01"),
                                format="%Y-%m-%d", tz = "UTC"
                                )

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

#' Return climate station time series data
#' @description Make a request to the /climatedata/climatestationts endpoints to retrieve daily or monthly (climatestationtsday or climatestationtsmonth)climate station time series data by station number or Site IDs within a given date range (start and end dates)
#' @param station_number character, surface water station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating the time series time step. Either "day", "month", "year". Default is to return daily time series.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of climate station timeseries data
#' @export
#' @examples
#' \dontrun{
#' # Retrieve daily maximum temperatures
#' daily_maxtemp <- get_climate_ts(
#'   site_id     = "USC00055984",
#'   param       = "MaxTemp",
#'   start_date  = "2017-01-01",
#'   end_date    = "2020-01-01",
#'   timescale   = "day"
#'   )
#'
#' # plot daily maximum temp at climate station
#' plot(daily_maxtemp$value~daily_maxtemp$datetime, type = "l")
#'
#' # Retrieve monthly precipitation
#' monthly_precip <- get_climate_ts(
#'   site_id     = "USC00055984",
#'   param       = "Precip",
#'   start_date  = "2000-01-01",
#'   end_date    = "2022-01-01",
#'   timescale   = "month"
#'    )
#'
#'  # plot daily max temp at climate station
#'  plot(monthly_precip$avg_value~monthly_precip$datetime, type = "l")
#'  }
get_climate_ts <- function(
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    timescale           = NULL,
    api_key             = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key", "start_date", "end_date", "timescale"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # list of valid timescales
  day_lst       <- c("day", "days", "daily", "d")
  month_lst     <- c("month", "months", "monthly", "mon", "mons", "m")
  timescale_lst <- c(day_lst, month_lst)

  # check if type is NULL, default timescale to "day"
  if(is.null(timescale)) {

    # set timescale to "day"
    timescale <- "day"

  }

  # convert timescale to lowercase
  timescale <- tolower(timescale)

  # check if type is correctly provided
  if(!timescale %in% timescale_lst) {

    stop(paste0("Invalid `timescale` argument: '", timescale, "'",
                "\nPlease enter one of the following valid timescales:",
                "\n", paste0("'", c(day_lst), "'", collapse = ", "),
                "\n", paste0("'", c(month_lst), "'", collapse = ", ")
                )
    )
  }

  # check which timescale to request data for

  # request daily climate timeseries data
  if(timescale %in% day_lst) {

    climate_ts <-
      get_climate_ts_day(
        station_number = station_number,
        site_id        = site_id,
        param          = param,
        start_date     = start_date,
        end_date       = end_date,
        api_key        = api_key
      )

  }

  # request monthly climate timeseries data
  if(timescale %in% month_lst) {

    climate_ts <-
      get_climate_ts_month(
        station_number = station_number,
        site_id        = site_id,
        param          = param,
        start_date     = start_date,
        end_date       = end_date,
        api_key        = api_key
      )

  }

  # return timeseries dataframe
  return(climate_ts)


}

