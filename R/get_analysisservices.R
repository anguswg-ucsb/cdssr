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

  # check if valid wdid and admin_no were given
  if(all(is.null(wdid), is.null(admin_no))) {
    stop(paste0("Invalid 'wdid' and 'admin_no' arguments"))
  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/callanalysisbywdid/?"

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
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nAdmin number: ", admin_no,
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

#' Return DWR source route framework from analysis services API
#' @description Makes a request to the analysisservices/watersourcerouteframework/ endpoint to retrieve the DWR source route framework reference table for the criteria specified.
#' @param division character, the division to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param gnis_name character, the GNIS Name to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param water_district character, the water district to query and retrieve DWR source route frameworks. Defaults to NULL.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of source route framework
#' @export
get_source_route_framework <- function(
    division            = NULL,
    gnis_name           = NULL,
    water_district      = NULL,
    api_key             = NULL
) {

  # check if valid division, gnis_name or water_district was given
  if(all(is.null(division), is.null(gnis_name), is.null(water_district))) {
    stop(paste0("Invalid'division', 'gnis_name', or 'water_district' arguments"))
  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteframework/?"

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
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nDivision: ", division,
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

# get_source_route_analysis<- function(
#     lt_gnis_id          = NULL,
#     lt_stream_mile      = NULL,
#     ut_gnis_id          = NULL,
#     ut_stream_mile      = NULL,
#     api_key             = NULL
# ) {
#   lt_gnis_id          = NULL
#   lt_stream_mile      = NULL
#   ut_gnis_id          = NULL
#   ut_stream_mile      = NULL
#   api_key             = NULL
#
#   snet <- get_source_route_framework(division = 1)
#   # check if valid division, gnis_name or water_district was given
#   if(all(is.null(division), is.null(gnis_name), is.null(water_district))) {
#     stop(paste0("Please enter a valid 'division', 'gnis_name', or 'water_district' to retreive DWR source route framework data for"))
#   }
#
#   # base URL
#   base <- "https://dwr.state.co.us/Rest/GET/api/v2/analysisservices/watersourcerouteframework/?"
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
#   # print message
#   message(paste0("Retrieving DWR source route frameworks from CDSS API..."))
#
#   # while more pages are avaliable, send get requests to CDSS API
#   while (more_pages) {
#
#     # Construct query URL w/o API key
#     url <- paste0(
#       base,
#       "format=json&dateFormat=spaceSepToSeconds",
#       "&division=", division,
#       "&gnisName=", gnis_name,
#       "&waterDistrict=", water_district,
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
#     tryCatch(
#       {
#         # query CDSS API
#         cdss_data <-
#           url %>%
#           httr::GET() %>%
#           httr::content(as = "text") %>%
#           jsonlite::fromJSON() %>%
#           dplyr::bind_rows() %>%
#           .$ResultList
#
#       },
#       error = function(e) {
#         message(paste0("Error in DWR source route framework query"))
#         message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
#         message(paste0("Query:\n----------------------------------",
#                        "\nDivision: ", division,
#                        "\nGNIS name: ", gnis_name,
#                        "\nWater district: ", water_district
#         ))
#         message(paste0('\nHere is the URL address that was queried:\n'))
#         message(paste0(url))
#         message(paste0('And, here is the original error message:'))
#         message(paste0('-----------------------------------------'))
#         message(e)
#         stop()
#
#       }
#     )
#
#     # Tidy data
#     cdss_data <-
#       cdss_data %>%
#       janitor::clean_names()
#
#     # bind data from this page
#     data_df <- dplyr::bind_rows(data_df, cdss_data)
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
#
#   # return final binded dataframe
#   return(data_df)
#
# }
