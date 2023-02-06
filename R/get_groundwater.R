utils::globalVariables(c("."))

#' Search for groundwater water level wells
#' @description Make a request to the groundwater/waterlevels/wells endpoint to retrieve groundwater water level wells data.
#' @param county character, indicating the county to query
#' @param designated_basin character, indicating the  designated basin to query
#' @param division numeric, indicating the water division to query
#' @param management_district character, indicating the management district to query
#' @param water_district numeric, indicating the water district to query
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/groundwater/waterlevels/wells/
#' wl_wells <- get_gw_wl_wells(
#'   county = "ADAMS"
#'  )
#'
#' plot(wl_wells$latitude~wl_wells$longitude)
#' }
get_gw_wl_wells <- function(
    county              = NULL,
    designated_basin    = NULL,
    division            = NULL,
    management_district = NULL,
    water_district      = NULL,
    wellid              = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # Base API
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/waterlevels/wells/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  management_district  <- null_convert(management_district)
  designated_basin     <- null_convert(designated_basin)
  division             <- null_convert(division)
  water_district       <- null_convert(water_district)
  wellid               <- null_convert(wellid)

  # format county name
  if(!is.null(county)) {

    county <- gsub(" ", "+", toupper(county))

  }

  # format management district name
  if(!is.null(management_district)) {

    management_district <- gsub(" ", "+", toupper(management_district))

  }

  # format designated district name
  if(!is.null(designated_basin)) {

    designated_basin <- gsub(" ", "+", toupper(designated_basin))

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving groundwater water level wells"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
      "&wellId=", wellid,
      "&division=", division,
      "&waterDistrict=", water_district,
      "&designatedBasin=", designated_basin,
      "&managementDistrict=", management_district,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # api_key <- NULL
    # check whether to use API key or not
    if(!is.null(api_key)) {

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
    cdss_data$datetime <-  as.POSIXct(cdss_data$measurement_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return individual groundwater water level well measurements
#' @description  Internal - Make a request to the groundwater/waterlevels/wellmeasurements endpoint to retrieve groundwater water level well measurement data.
#' @param wellid character, indicating the Well ID to query
#' @param start_date character date to request data start point YYYY-MM-DD.
#' @param end_date character date to request data end point YYYY-MM-DD. Default is set to the current date function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords internal
#' @return dataframe of groundwater wells within the given query specifications
inner_gw_wl_wellmeasures <- function(
    wellid           = NULL,
    start_date       = "1950-01-01",
    end_date         = Sys.Date(),
    api_key          = NULL
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

  # Base URL groundwater/waterlevels/wellmeasurements/
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/waterlevels/wellmeasurements/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wellid  <- null_convert(wellid)

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
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # message(paste0("Retrieving groundwater well measurements"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-measurementDate=", start,
      "&max-measurementDate=", end,
      "&wellId=", wellid,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # check whether to use API key or not
    if(!is.null(api_key)) {

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
    cdss_data$datetime <-  as.POSIXct(cdss_data$measurement_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return groundwater water level well measurements
#' @description  Make a request to the groundwater/waterlevels/wellmeasurements endpoint to retrieve groundwater water level well measurement data.
#' @param wellid character vector or list of well IDs
#' @param start_date character date to request data start point (YYYY-MM-DD).
#' @param end_date character date to request data end point (YYYY-MM-DD). Default is set to the current date function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/groundwater/waterlevels/wellmeasurements
#' well_measure <- get_gw_wl_wellmeasures(
#'                   wellid = 1274
#'                    )
#'
#' # plot depth to water
#' plot(well_measure$depth_to_water~well_measure$datetime, type = "l")
#'
#' # get data from multiple well IDs
#' multi_well <- get_gw_wl_wellmeasures(
#'                   wellid = c("84", "85", "94")
#'                    )
#'  }
get_gw_wl_wellmeasures <- function(
    wellid           = NULL,
    start_date       = "1950-01-01",
    end_date         = Sys.Date(),
    api_key          = NULL
) {

  # print message
  message(paste0("Retrieving groundwater well measurements"))

  # if only one site given, set verbose to FALSE to avoid superfluous messages
  if(length(wellid) <= 1) {

    verbose = FALSE

    # if multiple sites given, set verbose to TRUE to clarify to user each query made
  } else {

    verbose = TRUE

  }

  # loop over Well IDs and call inner_gw_wl_wellmeasures() function and bind results rows
  data_df <- lapply(1:length(wellid), function(i) {

    if(verbose == TRUE) {

      message(paste0("Well ID: ", wellid[i]))

    }

    tryCatch({
      inner_gw_wl_wellmeasures(
        wellid           = wellid[i],
        start_date       = start_date,
        end_date         = end_date,
        api_key          = api_key
      )

    },
    error = function(e) {

      NULL

    })

  })

  # bind rows of dataframes
  data_df <- do.call(rbind, data_df)

  return(data_df)

}

#' Search for groundwater geophysicallog wells
#' @description Make a request to the groundwater/geophysicallogs/wells endpoint to retrieve groundwater geophysicallog wells data.
#' @param county character, indicating the county to query
#' @param designated_basin character, indicating the  designated basin to query
#' @param division numeric, indicating the water division to query
#' @param management_district character, indicating the management district to query
#' @param water_district numeric, indicating the water district to query
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/groundwater/geophysicallogs/wells/
#' gplog_wells <- get_gw_gplogs_wells(
#'   county = "ADAMS"
#'  )
#'
#' plot(gplog_wells$latitude~gplog_wells$longitude)
#'  }
get_gw_gplogs_wells <- function(
    county              = NULL,
    designated_basin    = NULL,
    division            = NULL,
    management_district = NULL,
    water_district      = NULL,
    wellid              = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/geophysicallogs/wells/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  management_district  <- null_convert(management_district)
  designated_basin     <- null_convert(designated_basin)
  division             <- null_convert(division)
  water_district       <- null_convert(water_district)
  wellid               <- null_convert(wellid)

  # format county name
  if(!is.null(county)) {

    county <- gsub(" ", "+", toupper(county))

  }

  # format management district name
  if(!is.null(management_district)) {

    management_district <- gsub(" ", "+", toupper(management_district))

  }

  # format designated district name
  if(!is.null(designated_basin)) {

    designated_basin <- gsub(" ", "+", toupper(designated_basin))

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving groundwater geophysical log wells"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
      "&wellId=", wellid,
      "&division=", division,
      "&waterDistrict=", water_district,
      "&designatedBasin=", designated_basin,
      "&managementDistrict=", management_district,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # api_key <- NULL
    # check whether to use API key or not
    if(!is.null(api_key)) {

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
    cdss_data$datetime <- as.POSIXct(cdss_data$log_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return Groundwater Geophysical Log picks by well ID
#' @description Make a request to the groundwater/geophysicallogs/wells endpoint to retrieve groundwater geophysical log picks for the given well ID.
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of groundwater geophysical log picks for a given well ID
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/groundwater/geophysicallogs/geoplogpicks/
#' gplogpicks <- get_gw_gplogs_geologpicks(
#'   wellid = 2409
#'  )
#' }
get_gw_gplogs_geologpicks <- function(
    wellid              = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/geophysicallogs/geoplogpicks/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wellid     <- null_convert(wellid)

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving groundwater geophysical log wells geologpicks"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&wellId=", wellid,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # api_key <- NULL
    # check whether to use API key or not
    if(!is.null(api_key)) {

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
