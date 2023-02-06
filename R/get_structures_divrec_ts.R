utils::globalVariables(c("."))
#' Return Structure Daily Diversion/Release Records
#' @description     Make a request to the api/v2/structures/divrec/divrecday/ endpoint to retrieve daily structure diversion/release data for a specified WDID within a specified date range.
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Default is NULL which will return diversions.
#' @param start_date character date to request data start point YYYY-MM-DD. Default start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe with diversion or flow data for CDSS structure of interest
get_structure_divrecday <- function(
    wdid            = NULL,
    wc_identifier   = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "wc_identifier", "start_date", "end_date"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # valid parameters
  div_lst <- c("diversion", "diversions", "div", "divs", "d")
  rel_lst <- c("release", "releases", "rel", "rels", "r")
  wc_id_lst <- c(div_lst, rel_lst)

  # make wc ID lowercase
  wc_identifier <- tolower(wc_identifier)

  # check if parameter is valid
  if(!wc_identifier %in% wc_id_lst) {

    stop(paste0("Invalid `wc_identifier` argument: ", wc_identifier,
                "\nMust be one of the following:\n",
                "Diversions: ",
                paste(c(div_lst), collapse = ", "),  "\nReleases: ",
                paste(c(rel_lst), collapse = ", ")))

  }

  # Base API URL for Daily Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecday/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wdid         <- null_convert(wdid)

  # make sure correctly named wc_identifier
  wc_identifier <- align_wcid(x = wc_identifier)

  # format multiple WDID query
  wdid <- collapse_vect(
    x   = wdid,
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

  # format wcidentifer query
  wc_identifier <- paste0(gsub(":", "%3A",   unlist(strsplit(wc_identifier, " "))), collapse = "+")

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving daily ", wc_identifier, " data"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&wcIdentifier=*", wc_identifier,
      "*&min-dataMeasDate=", start,
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
    cdss_data$datetime <-  as.POSIXct(cdss_data$data_meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

    # # add data from this page to list
    # data_lst[[page_index]] <- cdss_data

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  return(data_df)

}

#' Return Structure Monthly Diversion/Release Records
#' @description Make a request to the api/v2/structures/divrec/divrecmonth/ endpoint to retrieve monthly structure  diversion/release data for a specified WDID within a specified date range.
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Default is NULL which will return diversions.
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe with monthly structure data for the CDSS structure of interest
get_structure_divrecmonth<- function(
    wdid            = NULL,
    wc_identifier   = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "wc_identifier", "start_date", "end_date"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # valid parameters
  div_lst   <- c("diversion", "diversions", "div", "divs", "d")
  rel_lst   <- c("release", "releases", "rel", "rels", "r")
  wc_id_lst <- c(div_lst, rel_lst)

  # make wc ID lowercase
  wc_identifier <- tolower(wc_identifier)

  # check if parameter is valid
  if(!wc_identifier %in% wc_id_lst) {

    stop(paste0("Invalid `wc_identifier` argument: ", wc_identifier,
                "\nMust be one of the following:\n",
                "Diversions: ",
                paste(c(div_lst), collapse = ", "),  "\nReleases: ",
                paste(c(rel_lst), collapse = ", ")))

  }

  # Base API URL for Monthly Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wdid         <- null_convert(wdid)

  # make sure correctly named wc_identifier
  wc_identifier <- align_wcid(x = wc_identifier)

  # format multiple WDID query
  wdid <- collapse_vect(
    x   = wdid,
    sep = "%2C+"
  )

  # reformat and extract valid start date
  start <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%m-%Y",
    sep    = "%2F"
  )

  # reformat and extract valid start date
  end <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%m-%Y",
    sep    = "%2F"
  )

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
  message(paste0("Retrieving monthly ", wc_identifier, " data"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
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
    names(cdss_data)   <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

    # set datetime column
    cdss_data$datetime <- as.Date(paste0(cdss_data$data_meas_date, "-01"))

    # bind data from this page
    data_df            <- rbind(data_df, cdss_data)

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  return(data_df)

}

#' Return Structure Yearly Diversion/Release Records
#' @description Make a request to the api/v2/structures/divrec/divrecyear/ endpoint to retrieve annual structure diversion/release data for a specified WDID within a specified date range.
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Default is NULL which will return diversions.
#' @param start_date character date to request data start point YYYY-MM-DD. Default start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe with yearly structure data for the CDSS structure of interest
get_structure_divrecyear <- function(
    wdid            = NULL,
    wc_identifier   = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "wc_identifier", "start_date", "end_date"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # valid parameters
  div_lst   <- c("diversion", "diversions", "div", "divs", "d")
  rel_lst   <- c("release", "releases", "rel", "rels", "r")
  wc_id_lst <- c(div_lst, rel_lst)

  # make wc ID lowercase
  wc_identifier <- tolower(wc_identifier)

  # check if parameter is valid
  if(!wc_identifier %in% wc_id_lst) {

    stop(paste0("Invalid `wc_identifier` argument: ", wc_identifier,
                "\nMust be one of the following:\n",
                "Diversions: ",
                paste(c(div_lst), collapse = ", "),  "\nReleases: ",
                paste(c(rel_lst), collapse = ", ")))

  }

  # Base API URL for Monthly Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecyear/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wdid         <- null_convert(wdid)

  # make sure correctly named wc_identifier
  wc_identifier <- align_wcid(x = wc_identifier)

  # format multiple WDID query
  wdid <- collapse_vect(
    x   = wdid,
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

  # format wcidentifer query
  wc_identifier <- paste0(gsub(":", "%3A",   unlist(strsplit(wc_identifier, " "))), collapse = "+")

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving yearly ", wc_identifier, " data from CDSS API"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-dataMeasDate=", start_year,
      "&max-dataMeasDate=", end_year,
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

  return(data_df)

}




#' Return diversion/releases record data for administrative structures
#' @description Make a request to the CDSS API /structures/divrec endpoints to get diversion/releases time series data for administrative structures by wdid, within a given date range (start and end dates) and at a specified temporal resolution.
#' @param wdid character vector or list of characters indicating WDID code of structure.
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Default is NULL which will return diversions.
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating the time series time step. Either "day", "month", "year". Default is to return daily time series.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe with diversion records data for CDSS structure of interest
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/structures/divrec/divrecday
#' divrecord_day <- get_structures_divrec_ts(
#'                wdid             = "2302917",
#'                wc_identifier    = "diversion",
#'                start_date       = "2000-01-01",
#'                end_date         = "2005-01-01",
#'                timescale        = "day"
#'                )
#'
#' # plot daily diversion record
#' plot(divrecord_day$data_value~divrecord_day$datetime, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/divrecmonth
#' divrecord_month <- get_structures_divrec_ts(
#'                    wdid             = "2302917",
#'                    wc_identifier    = "diversion",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01",
#'                    timescale        = "month"
#'                  )
#'
#' # plot monthly diversion record
#' plot(divrecord_month$data_value~divrecord_month$datetime, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/divrecyear
#' divrecord_year <- get_structures_divrec_ts(
#'                     wdid             = "2302917",
#'                     wc_identifier    = "diversion",
#'                     start_date       = "2000-01-01",
#'                     end_date         = "2022-01-01",
#'                     timescale        = "year"
#'                     )
#'
#' # plot yearly diversion record
#' plot(divrecord_year$data_value~divrecord_year$data_meas_date, type = "s")
#' }
get_structures_divrec_ts <- function(
    wdid            = NULL,
    wc_identifier   = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    timescale       = NULL,
    api_key         = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key", "wc_identifier", "start_date", "end_date", "timescale"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # list of valid timescales
  day_lst       <- c("day", "days", "daily", "d")
  month_lst     <- c("month", "months", "monthly", "mon", "mons", "m")
  year_lst      <- c("year", "years", "yearly", "annual", "annually", "yr", "yrs", "y")
  timescale_lst <- c(day_lst, month_lst, year_lst)

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
  wdid         <- null_convert(wdid)

  # Retrieve daily divrec data
  if(timescale %in% day_lst) {

    # divrec day
    divrec_df <- get_structure_divrecday(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrec_df)
  }

  # Retrieve monthly divrec data
  if(timescale %in% month_lst) {

    # divrec month
    divrec_df <- get_structure_divrecmonth(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrec_df)
  }

  # Retrieve yearly divrec data
  if(timescale %in% year_lst) {

    # divrec year
    divrec_df <- get_structure_divrecyear(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrec_df)
  }

}


