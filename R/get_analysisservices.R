utils::globalVariables(c("."))

#' Return call analysis by WDID from analysis services API
#' @description Makes a request to the analysisservices/callanalysisbywdid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified WDID and priority was out of priority and the downstream call in priority.
#' @param wdid character indicating DWR unique structure identifier code (WDID)
#' @param admin_no character Water Right Administration Number
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @return dataframe of call services by WDID
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @export
get_call_analysis_wdid <- function(
    wdid                = NULL,
    admin_no            = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key", "start_date", "end_date"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbywdid/?"

  # convert arguments to characters if necessary
  wdid      <- null_convert(wdid)
  admin_no  <- null_convert(admin_no)

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
  message(paste0("Retrieving call anaylsis by WDID"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&adminNo=", admin_no,
      "&endDate=", end,
      "&startDate=", start,
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
        message(paste0("Error in call anaylsis by WDID query"))
        message(paste0("Perhaps the URL address is incorrect OR there is no data available."))
        message(paste0("Query:\nAdmin number: ", admin_no,
                       "\nWDID: ", wdid,
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

    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

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

#' Return call analysis by GNIS ID from analysis services API
#' @description Makes a request to the analysisservices/callanalysisbygnisid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified stream/stream mile and priority was out of priority and the downstream call in priority. This can be used when there is not an existing WDID to be analyzed.
#' @param gnis_id character, the GNIS ID to query. Defaults to NULL.
#' @param admin_no character Water Right Administration Number
#' @param stream_mile numeric, stream mile for call analysis
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @return dataframe of call services by GNIS ID
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @export
get_call_analysis_gnisid <- function(
    gnis_id             = NULL,
    admin_no            = NULL,
    stream_mile         = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key", "start_date", "end_date"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbygnisid/?"

  # convert arguments to characters if necessary
  gnis_id     <- null_convert(gnis_id)
  admin_no    <- null_convert(admin_no)
  stream_mile <- null_convert(stream_mile)

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
  message(paste0("Retrieving call anaylsis by GNIS ID"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&adminNo=", admin_no,
      "&endDate=", end,
      "&gnisId=", gnis_id,
      "&startDate=", start,
      "&streamMile=", stream_mile,
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
        message(paste0("Error in call anaylsis by GNIS ID query"))
        message(paste0("Perhaps the URL address is incorrect OR there is no data available."))
        message(paste0("Query:\nGNIS ID: ", gnis_id,
                       "\nAdmin number: ", admin_no,
                       "\nStream mile: ", stream_mile,
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

    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

    # set datetime column
    cdss_data$datetime      <- as.POSIXct(cdss_data$analysis_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return DWR source route framework from analysis services API
#' @description Makes a request to the analysisservices/watersourcerouteframework/ endpoint to retrieve the DWR source route framework reference table for the criteria specified.
#' @param division character, the division to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param gnis_name character, the GNIS Name to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param water_district character, the water district to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of water source route framework
#' @export
  get_source_route_framework <- function(
    division            = NULL,
    gnis_name           = NULL,
    water_district      = NULL,
    api_key             = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteframework/?"

  # convert arguments to characters if necessary
  division       <- null_convert(division)
  gnis_name      <- null_convert(gnis_name)
  water_district <- null_convert(water_district)

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving DWR source route frameworks"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&division=", division,
      "&gnisName=", gnis_name,
      "&waterDistrict=", water_district,
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
        message(paste0("Error in DWR source route framework query"))
        message(paste0("Perhaps the URL address is incorrect OR there is no data available."))
        message(paste0("Query:\nDivision: ", division,
                       "\nGNIS name: ", gnis_name,
                       "\nWater district: ", water_district
        ))
        message(paste0('\nHere is the URL address that was queried:\n'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(paste0('-----------------------------------------'))
        message(e)
        stop()

      }
    )

    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

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

#' 	Returns all WDID(s), and their stream mile, located between two different stream locations on the DWR Water Source Framework
#' @description Makes a request to the analysisservices/watersourcerouteanalysis/ endpoint to retrieve the DWR source route framework analysis data
#' @param lt_gnis_id character or numeric,	lower terminus GNIS ID
#' @param lt_stream_mile character or numeric, lower terminus stream mile
#' @param ut_gnis_id character or numeric, upper terminus GNIS ID
#' @param ut_stream_mile character or numeric, upper terminus stream mile
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of water source route framework analysis
#' @export
get_source_route_analysis<- function(
    lt_gnis_id          = NULL,
    lt_stream_mile      = NULL,
    ut_gnis_id          = NULL,
    ut_stream_mile      = NULL,
    api_key             = NULL
) {

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = as.list(environment()),
    ignore  = c("api_key"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteanalysis/?"

  # convert arguments to characters if necessary
  lt_gnis_id       <- null_convert(lt_gnis_id)
  lt_stream_mile   <- null_convert(lt_stream_mile)
  ut_gnis_id       <- null_convert(ut_gnis_id)
  ut_stream_mile   <- null_convert(ut_stream_mile)

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving DWR source route analysis"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&ltGnisId=", lt_gnis_id,
      "&ltStreamMile=", lt_stream_mile,
      "&utGnisId=", ut_gnis_id,
      "&utStreamMile=", ut_stream_mile,
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
        message(paste0("Error in DWR source route analysis"))
        message(paste0("Perhaps the URL address is incorrect OR there is no data available."))
        message(paste0("Query:\nLower Terminus GNIS ID: ", lt_gnis_id,
                       "\nLower Terminus Stream mile: ", lt_stream_mile,
                       "\nUpper Terminus GNIS ID: ", ut_gnis_id,
                       "\nUpper Terminus Stream mile: ", ut_stream_mile
        ))
        message(paste0('\nHere is the URL address that was queried:\n'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(paste0('-----------------------------------------'))
        message(e)
        stop()

      }
    )

    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

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
