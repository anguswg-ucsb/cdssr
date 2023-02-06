utils::globalVariables(c("."))
#' Return daily surface water time series data
#' @description Make a request to the /surfacewater/surfacewatertsday endpoint to retrieve surface water stations daily time series data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of surface water station daily time series data
get_sw_ts_day <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
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

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsday/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # format multiple abbrev query string
  abbrev <- collapse_vect(
    x   = abbrev,
    sep = "%2C+"
  )

  # format multiple usgs_id query string
  usgs_id <- collapse_vect(
    x   = usgs_id,
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
  message(paste0("Retrieving daily surface water time series data"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&abbrev=", abbrev,
      "&min-measDate=", start,
      "&max-measDate=", end,
      "&stationNum=", station_number,
      "&usgsSiteId=", usgs_id,
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
    cdss_data$datetime      <- as.POSIXct(cdss_data$meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return monthly surface water time series data
#' @description Make a request to the /surfacewater/surfacewatertsmonth endpoint to retrieve surface water stations monthly time series data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of surface water station monthly time series data
get_sw_ts_month <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
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

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsmonth/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # format multiple abbrev query string
  abbrev <- collapse_vect(
    x   = abbrev,
    sep = "%2C+"
  )

  # format multiple usgs_id query string
  usgs_id <- collapse_vect(
    x   = usgs_id,
    sep = "%2C+"
  )

  # reformat and extract valid start date
  start_year <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%Y"
  )

  # reformat and extract valid end date
  end_year <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%Y"
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
  message(paste0("Retrieving monthly surface water time series data"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&abbrev=", abbrev,
      "&min-calYear=", start_year,
      "&max-calYear=", end_year,
      "&stationNum=", station_number,
      "&usgsSiteId=", usgs_id,
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
                                         ifelse(cdss_data$cal_mon_num > 9, cdss_data$cal_mon_num, paste0("0", cdss_data$cal_mon_num)),
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

#' Return water year surface water time series data
#' @description Make a request to the /surfacewater/surfacewatertswateryear endpoint to retrieve surface water stations water year time series data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of annual surface water station time series data
get_sw_ts_wyear <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
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

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertswateryear/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # format multiple abbrev query string
  abbrev <- collapse_vect(
    x   = abbrev,
    sep = "%2C+"
  )

  # format multiple usgs_id query string
  usgs_id <- collapse_vect(
    x   = usgs_id,
    sep = "%2C+"
  )

  # reformat and extract valid start date
  start_year <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%Y"
  )

  # reformat and extract valid end date
  end_year <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%Y"
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
  message(paste0("Retrieving yearly surface water water year time series data"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&abbrev=", abbrev,
      "&min-waterYear=", start_year,
      "&max-waterYear=", end_year,
      "&stationNum=", station_number,
      "&usgsSiteId=", usgs_id,
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

#' Return surface water time series data
#' @description Make a request to the /surfacewater/surfacewaterts/ endpoints (surfacewatertsday, surfacewatertsmonth, surfacewatertswateryear) to retrieve surface water station time series data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character,	station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character, USGS Site ID
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating the time series time step. Either "day", "month", "year". Default is to return daily time series.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of surface water station time series data
#' @export
#' @examples
#' \dontrun{
#' # Retrieve surface water daily time series
#' sw_ts_day <-
#'  get_sw_ts(
#'   abbrev     = "CLAFTCCO",
#'   start_date = "2000-01-01",
#'   end_date   = "2022-01-01",
#'   timescale  = "day"
#'   )
#'
#' # plot daily flow
#' plot(sw_ts_day$value~sw_ts_day$datetime, type = "s")
#'
#' # Retrieve surface water monthly time series
#' sw_ts_month <-
#'  get_sw_ts(
#'   abbrev     = "CLAFTCCO",
#'   start_date = "2000-01-01",
#'   end_date   = "2022-01-01",
#'   timescale  = "month"
#'   )
#' # plot average monthly flow
#' plot(sw_ts_month$avg_qcfs~sw_ts_month$datetime, type = "s")
#'
#' # Retrieve surface water water year time series
#' sw_ts_year <-
#'  get_sw_ts(
#'   abbrev     = "CLAFTCCO",
#'   start_date = "2000-01-01",
#'   end_date   = "2022-01-01",
#'   timescale  = "wateryear"
#'   )
#'
#' # plot average water year flow
#' plot(sw_ts_year$avg_qcfs~sw_ts_year$water_year, type = "s")
#' }
get_sw_ts <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    timescale           = "day",
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

  # list of valid timescales
  day_lst       <- c("day", "days", "daily", "d")
  month_lst     <- c("month", "months", "monthly", "mon", "mons", "m")
  year_lst      <- c('wyear', 'water_year', 'wyears', 'water_years', 'wateryear',
                     'wateryears', 'wy', 'year', 'years', 'yearly', 'annual', 'annually', 'yr', 'y')
  timescale_lst <- c(day_lst, month_lst, year_lst)

  # check if type is NULL, default timescale to "day"
  if(is.null(timescale)) {

    # set timescale to "day"
    timescale <- "day"

  }

  # convert timescale to lowercase
  timescale <- tolower(timescale)

  # check if type is correctly inputed
  if(!timescale %in% timescale_lst) {

    stop(paste0("Invalid `timescale` argument: '", timescale, "'",
                "\nPlease enter one of the following valid timescales:",
                "\n", paste0("'", c(day_lst), "'", collapse = ", "),
                "\n", paste0("'", c(month_lst), "'", collapse = ", "),
                "\n", paste0("'", c(year_lst), "'", collapse = ", ")
                )
         )
  }

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  station_number  <- null_convert(station_number)

  # check which timescale to request data for

  # request surface water daily time series data
  if(timescale %in% day_lst) {

    sw_ts <-
      get_sw_ts_day(
          abbrev         = abbrev,
          station_number = station_number,
          usgs_id        = usgs_id,
          start_date     = start_date,
          end_date       = end_date,
          api_key        = api_key
          )

  }

  # request surface water monthly time series data
  if(timescale %in% month_lst) {

    sw_ts <-
      get_sw_ts_month(
          abbrev         = abbrev,
          station_number = station_number,
          usgs_id        = usgs_id,
          start_date     = start_date,
          end_date       = end_date,
          api_key        = api_key
          )

    }

  # request surface water wateryear time series data
  if(timescale %in% year_lst) {

    sw_ts <-
      get_sw_ts_wyear(
          abbrev         = abbrev,
          station_number = station_number,
          usgs_id        = usgs_id,
          start_date     = start_date,
          end_date       = end_date,
          api_key        = api_key
          )

  }

  # return time series dataframe
  return(sw_ts)


}

