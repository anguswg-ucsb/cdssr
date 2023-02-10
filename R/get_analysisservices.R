utils::globalVariables(c("."))

#' Return call analysis by WDID from analysis services API
#' @description Makes a request to the analysisservices/callanalysisbywdid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified WDID and priority was out of priority and the downstream call in priority.
#' @param wdid character indicating DWR unique structure identifier code (WDID)
#' @param admin_no character Water Right Administration Number
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param batch logical, whether to break date range calls into batches of 1 year. This can speed up data retrieval for date ranges greater than a year. A date range of 5 years would be batched into 5 separate API calls for each year. Default is FALSE, will run a single query for the entire date range.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of call services by WDID
#' @export
get_call_analysis_wdid <- function(
    wdid                = NULL,
    admin_no            = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    batch               = FALSE,
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

  # convert arguments to characters if necessary
  wdid      <- null_convert(wdid)
  admin_no  <- null_convert(admin_no)

  # if function should be run in batch mode
  if(batch == TRUE) {

    # make a dataframe of date ranges to issue GET requests in smaller batches
    date_df <- batch_dates(
      start_date = start_date,
      end_date   = end_date
    )

    # print message
    message(paste0("Retrieving call analysis by WDID"))

    # go through range of dates in date_df and make batch GET requests
    cdss_df <- lapply(1:nrow(date_df), function(i) {

      message(paste0("Batch: ", i, "/", nrow(date_df)))

      inner_call_analysis_wdid(
        wdid       = wdid,
        admin_no   = admin_no,
        start_date = date_df$starts[i],
        end_date   = date_df$ends[i],
        api_key    = api_key
      )

    })

    # bind dataframe rows
    cdss_df <- do.call(rbind, cdss_df)

    # return final binded dataframe
    return(cdss_df)

  } else {

    # print message
    message(paste0("Retrieving call analysis by WDID"))

    cdss_df <- inner_call_analysis_wdid(
      wdid       = wdid,
      admin_no   = admin_no,
      start_date = start_date,
      end_date   = end_date,
      api_key    = api_key
    )

    # return final binded dataframe
    return(cdss_df)

  }

}
  # # base URL
  # base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbywdid/?"
  #
  # # convert arguments to characters if necessary
  # wdid      <- null_convert(wdid)
  # admin_no  <- null_convert(admin_no)

  # # make a dataframe of date ranges to issue GET requests in smaller batches
  # date_df <- batch_dates(
  #   start_date = start_date,
  #   end_date   = end_date
  #   )
  #
  # # print message
  # message(paste0("Retrieving call analysis by WDID"))
  #
  # # go through range of dates in date_df and make batch GET requests
  # cdss_lst <- lapply(1:nrow(date_df), function(i) {
  #
  #   # reformat and extract valid start date
  #   start <- parse_date(
  #     date   = date_df$starts[i],
  #     start  = TRUE,
  #     format = "%m-%d-%Y",
  #     sep    = "%2F"
  #   )
  #
  #   # reformat and extract valid end date
  #   end <- parse_date(
  #     date   = date_df$ends[i],
  #     start  = FALSE,
  #     format = "%m-%d-%Y",
  #     sep    = "%2F"
  #   )
  #
  #   # message(paste0("Start date: ", date_df$starts[i], " - (", start, ")", "\nEnd date: ", date_df$ends[i], "- (", end, ")\n"))
  #
  #   # maximum records per page
  #   page_size  <- 50000
  #
  #   # initialize empty dataframe to store data from multiple pages
  #   data_df    <-  data.frame()
  #
  #   # initialize first page index
  #   page_index <- 1
  #
  #   # Loop through pages until there are no more pages to get
  #   more_pages <- TRUE
  #
  #   # while more pages are available, send get requests to CDSS API
  #   while (more_pages) {
  #
  #     # Construct query URL w/o API key
  #     url <- paste0(
  #       base,
  #       "format=json&dateFormat=spaceSepToSeconds",
  #       "&adminNo=", admin_no,
  #       "&endDate=", end,
  #       "&startDate=", start,
  #       "&wdid=", wdid,
  #       "&pageSize=", page_size,
  #       "&pageIndex=", page_index
  #     )
  #
  #     # check whether to use API key or not
  #     if(!is.null(api_key)) {
  #
  #       # Construct query URL w/ API key
  #       url <- paste0(url, "&apiKey=", api_key)
  #
  #     }
  #
  #     # GET request to CDSS API
  #     tryCatch({
  #
  #       # query CDSS API
  #       cdss_data <- parse_gets(url = url)
  #
  #     },
  #     error = function(e) {
  #
  #       # error message handler
  #       message(
  #         query_error(
  #           arg_lst = input_args,
  #           ignore  = c("url", "e"),
  #           url     = url,
  #           e_msg   = e
  #         )
  #       )
  #
  #       stop()
  #
  #     })
  #
  #     # Extract Result List
  #     cdss_data <- cdss_data$ResultList
  #
  #     # set clean names
  #     names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))
  #
  #     # use sprintf() to extract all true decimal places of admin numbers
  #     cdss_data$analysis_wr_admin_no <- sprintf("%.5f",  cdss_data$analysis_wr_admin_no)
  #     cdss_data$priority_admin_no    <- sprintf("%.5f",  cdss_data$priority_admin_no)
  #
  #     # set datetime column
  #     cdss_data$datetime      <- as.POSIXct(cdss_data$analysis_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  #
  #     # bind data from this page
  #     data_df <- rbind(data_df, cdss_data)
  #
  #     # Check if more pages to get to continue/stop while loop
  #     if (nrow(cdss_data) < page_size) {
  #
  #       more_pages <- FALSE
  #
  #     } else {
  #
  #       page_index <- page_index + 1
  #
  #     }
  #
  #   }
  #   data_df
  # })
  #
  # # bind dataframe rows
  # cdss_lst <- do.call(rbind, cdss_lst)
  #
  # # return final binded dataframe
  # return(cdss_lst)

# }

#' Return single call analysis by WDID from analysis services for a single date range
#' @description Internal function to be called within get_call_analysis_wdid. Makes a request to the analysisservices/callanalysisbywdid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified WDID and priority was out of priority and the downstream call in priority.
#' @param wdid character indicating DWR unique structure identifier code (WDID)
#' @param admin_no character Water Right Administration Number
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords internal
#' @return dataframe of call services by WDID
inner_call_analysis_wdid <- function(
    wdid                = NULL,
    admin_no            = NULL,
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

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbywdid/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

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

  # while more pages are available, send get requests to CDSS API
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

    # use sprintf() to extract all true decimal places of admin numbers
    cdss_data$analysis_wr_admin_no <- sprintf("%.5f",  cdss_data$analysis_wr_admin_no)
    cdss_data$priority_admin_no    <- sprintf("%.5f",  cdss_data$priority_admin_no)

    # set datetime column
    cdss_data$datetime      <- as.POSIXct(cdss_data$analysis_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return call analysis by GNIS ID from analysis services API
#' @description Makes a request to the analysisservices/callanalysisbygnisid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified stream/stream mile and priority was out of priority and the downstream call in priority. This can be used when there is not an existing WDID to be analyzed.
#' @param gnis_id character, the GNIS ID to query. Defaults to NULL.
#' @param admin_no character Water Right Administration Number
#' @param stream_mile numeric, stream mile for call analysis
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param batch logical, whether to break date range calls into batches of 1 year. This can speed up data retrieval for date ranges greater than a year. A date range of 5 years would be batched into 5 separate API calls for each year. Default is FALSE, will run a single query for the entire date range.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of call services by GNIS ID
#' @export
get_call_analysis_gnisid <- function(
    gnis_id             = NULL,
    admin_no            = NULL,
    stream_mile         = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    batch               = FALSE,
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

  # convert arguments to characters if necessary
  gnis_id     <- null_convert(gnis_id)
  admin_no    <- null_convert(admin_no)
  stream_mile <- null_convert(stream_mile)

  # if function should be run in batch mode
  if(batch == TRUE) {

    # make a dataframe of date ranges to issue GET requests in smaller batches
    date_df <- batch_dates(
      start_date = start_date,
      end_date   = end_date
    )

    # print message
    message(paste0("Retrieving call analysis by GNIS ID"))

    # go through range of dates in date_df and make batch GET requests
    cdss_df <- lapply(1:nrow(date_df), function(i) {

      message(paste0("Batch: ", i, "/", nrow(date_df)))

      inner_call_analysis_gnisid(
        gnis_id      = gnis_id,
        admin_no     = admin_no,
        stream_mile  = stream_mile,
        start_date   = date_df$starts[i],
        end_date     = date_df$ends[i],
        api_key      = api_key
      )

    })

    # bind dataframe rows
    cdss_df <- do.call(rbind, cdss_df)

    # return final binded dataframe
    return(cdss_df)

  } else {

    # print message
    message(paste0("Retrieving call analysis by GNIS ID"))

    cdss_df <- inner_call_analysis_gnisid(
      gnis_id      = gnis_id,
      admin_no     = admin_no,
      stream_mile  = stream_mile,
      start_date   = start_date,
      end_date     = end_date,
      api_key      = api_key
    )

    # return final binded dataframe
    return(cdss_df)

  }

}
  # base URL
  # base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbygnisid/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # # make a dataframe of date ranges to issue GET requests in smaller batches
  # date_df <- batch_dates(
  #   start_date = start_date,
  #   end_date   = end_date
  # )
  #
  # # print message
  # message(paste0("Retrieving call analysis by GNIS ID"))
  #
  # # go through range of dates in date_df and make batch GET requests
  # cdss_lst <- lapply(1:nrow(date_df), function(i) {
  #
  #   # reformat and extract valid start date
  #   start <- parse_date(
  #     date   = date_df$starts[i],
  #     start  = TRUE,
  #     format = "%m-%d-%Y",
  #     sep    = "%2F"
  #   )
  #
  #   # reformat and extract valid end date
  #   end <- parse_date(
  #     date   = date_df$ends[i],
  #     start  = FALSE,
  #     format = "%m-%d-%Y",
  #     sep    = "%2F"
  #   )
  #
  #   # message(paste0("Start date: ", date_df$starts[i], " - (", start, ")", "\nEnd date: ", date_df$ends[i], "- (", end, ")\n"))
  #
  #   # maximum records per page
  #   page_size  <- 50000
  #
  #   # initialize empty dataframe to store data from multiple pages
  #   data_df    <-  data.frame()
  #
  #   # initialize first page index
  #   page_index <- 1
  #
  #   # Loop through pages until there are no more pages to get
  #   more_pages <- TRUE
  #
  #   # while more pages are available, send get requests to CDSS API
  #   while (more_pages) {
  #
  #     # Construct query URL w/o API key
  #     url <- paste0(
  #       base,
  #       "format=json&dateFormat=spaceSepToSeconds",
  #       "&adminNo=", admin_no,
  #       "&endDate=", end,
  #       "&gnisId=", gnis_id,
  #       "&startDate=", start,
  #       "&streamMile=", stream_mile,
  #       "&pageSize=", page_size,
  #       "&pageIndex=", page_index
  #     )
  #
  #     # check whether to use API key or not
  #     if(!is.null(api_key)) {
  #
  #       # Construct query URL w/ API key
  #       url <- paste0(url, "&apiKey=", api_key)
  #
  #     }
  #
  #     # GET request to CDSS API
  #     tryCatch({
  #
  #       # query CDSS API
  #       cdss_data <- parse_gets(url = url)
  #
  #     },
  #     error = function(e) {
  #
  #       # error message handler
  #       message(
  #         query_error(
  #           arg_lst = input_args,
  #           ignore  = c("url", "e"),
  #           url     = url,
  #           e_msg   = e
  #         )
  #       )
  #
  #       stop()
  #
  #     })
  #
  #     # Extract Result List
  #     cdss_data <- cdss_data$ResultList
  #
  #     # set clean names
  #     names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))
  #
  #     # use sprintf() to extract all true decimal places of admin numbers
  #     cdss_data$analysis_wr_admin_no <- sprintf("%.5f",  cdss_data$analysis_wr_admin_no)
  #     cdss_data$priority_admin_no    <- sprintf("%.5f",  cdss_data$priority_admin_no)
  #
  #     # set datetime column
  #     cdss_data$datetime      <- as.POSIXct(cdss_data$analysis_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  #
  #     # bind data from this page
  #     data_df <- rbind(data_df, cdss_data)
  #
  #     # Check if more pages to get to continue/stop while loop
  #     if (nrow(cdss_data) < page_size) {
  #
  #       more_pages <- FALSE
  #
  #     } else {
  #
  #       page_index <- page_index + 1
  #
  #     }
  #
  #   }
  #   data_df
  # })
  #
  # # bind dataframe rows
  # cdss_lst <- do.call(rbind, cdss_lst)
  #
  # # return final binded dataframe
  # return(cdss_lst)

# }

#' Return call analysis by GNIS ID from analysis services API
#' @description Internal function to be called within get_call_analysis_gnisid. Makes a request to the analysisservices/callanalysisbygnisid/ endpoint that performs a call analysis that returns a time series showing the percentage of each day that the specified stream/stream mile and priority was out of priority and the downstream call in priority. This can be used when there is not an existing WDID to be analyzed.
#' @param gnis_id character, the GNIS ID to query. Defaults to NULL.
#' @param admin_no character Water Right Administration Number
#' @param stream_mile numeric, stream mile for call analysis
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords internal
#' @return dataframe of call services by GNIS ID
inner_call_analysis_gnisid <- function(
    gnis_id             = NULL,
    admin_no            = NULL,
    stream_mile         = NULL,
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
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # while more pages are available, send get requests to CDSS API
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

    # use sprintf() to extract all true decimal places of admin numbers
    cdss_data$analysis_wr_admin_no <- sprintf("%.5f",  cdss_data$analysis_wr_admin_no)
    cdss_data$priority_admin_no    <- sprintf("%.5f",  cdss_data$priority_admin_no)

    # set datetime column
    cdss_data$datetime      <- as.POSIXct(cdss_data$analysis_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

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

#' Return DWR source route framework from analysis services API
#' @description Makes a request to the analysisservices/watersourcerouteframework/ endpoint to retrieve the DWR source route framework reference table for the criteria specified.
#' @param division character, the division to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param gnis_name character, the GNIS Name to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param water_district character, the water district to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of water source route framework
#' @export
get_source_route_framework <- function(
    division            = NULL,
    gnis_name           = NULL,
    water_district      = NULL,
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

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteframework/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

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
    # data_df <- dplyr::bind_rows(data_df, cdss_data)

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

#' Returns all WDID(s), and their stream mile, located between two different stream locations on the DWR Water Source Framework
#' @description Makes a request to the analysisservices/watersourcerouteanalysis/ endpoint to retrieve the DWR source route framework analysis data
#' @param lt_gnis_id character or numeric,	lower terminus GNIS ID
#' @param lt_stream_mile character or numeric, lower terminus stream mile
#' @param ut_gnis_id character or numeric, upper terminus GNIS ID
#' @param ut_stream_mile character or numeric, upper terminus stream mile
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of water source route framework analysis
#' @export
get_source_route_analysis<- function(
    lt_gnis_id          = NULL,
    lt_stream_mile      = NULL,
    ut_gnis_id          = NULL,
    ut_stream_mile      = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteanalysis/?"

  # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

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

