#' Return daily climate data
#' @description Make a request to the /climatedata/climatestationtsday endpoint to retrieve climate stations daily timeseries data by station number, or Site IDs within a given date range (start and end dates)
#' @param station_number character, climate data station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of climate data station daily timeseries data
get_climate_ts_day <- function(
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # check if parameter is valid
  if(!param %in% c("Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip",
                     "Snow","SnowDepth", "SnowSWE", "Solar","VP", "Wind")) {

    stop(paste0("Invalid `param` argument \nMust be one of: \nEvap, FrostDate, MaxTemp, MeanTemp, MinTemp, Precip, Snow, SnowDepth, SnowSWE, Solar, VP, Wind"))
  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationtsday/?"


  # format multiple USGS ID query string
  if(!is.null(site_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(site_id) == TRUE) {

      site_id <- unlist(site_id)

    }

    site_id <- paste0(unlist(strsplit(site_id, " ")), collapse = "%2C+")

  }

  # reformat dates to MM-DD-YYYY and format for API query
  start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
  end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving climate timeseries data from CDSS API..."))
  message(paste0("Parameter: ", param))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      # "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
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
        message(paste0("Error in climate data daily timeseries query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nStation number: ", station_number,
                       "\nSite ID: ", site_id,
                       "\nParameter: ", param
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
      janitor::clean_names() %>%
      dplyr::mutate(
        datetime   = as.POSIXct(meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
      )

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

#' Return monthly climate data
#' @description Make a request to the /climatedata/climatestationtsmonth endpoint to retrieve climate stations monthly timeseries data by station number, or Site IDs within a given date range (start and end dates)
#' @param station_number character, climate data station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD
#' @param end_date character date to request data end point YYYY-MM-DD
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of climate data station monthly timeseries data
get_climate_ts_month <- function(
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # check if parameter is valid
  if(!param %in% c("Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip",
                   "Snow","SnowDepth", "SnowSWE", "Solar","VP", "Wind")) {

    stop(paste0("Invalid `param` argument \nMust be one of: \nEvap, FrostDate, MaxTemp, MeanTemp, MinTemp, Precip, Snow, SnowDepth, SnowSWE, Solar, VP, Wind"))
  }

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestationtsmonth/?"


  # format multiple USGS ID query string
  if(!is.null(site_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(site_id) == TRUE) {

      site_id <- unlist(site_id)

    }

    site_id <- paste0(unlist(strsplit(site_id, " ")), collapse = "%2C+")

  }

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
  message(paste0("Retrieving climate timeseries data from CDSS API..."))
  message(paste0("Parameter: ", param))

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
        message(paste0("Error in climate data monthly timeseries query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nStation number: ", station_number,
                       "\nSite ID: ", site_id,
                       "\nParameter: ", param
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
      janitor::clean_names() %>%
      dplyr::mutate(
        datetime = dplyr::case_when(                                                                                    # make POSIXct date
          cal_month_num <= 9 ~ as.POSIXct(paste0(cal_year, "-0", cal_month_num, "-01"),  format="%Y-%m-%d", tz = "UTC"),    # add "0" in front of 1 digit months
          TRUE               ~ as.POSIXct(paste0(cal_year, "-", cal_month_num, "-01"),  format="%Y-%m-%d", tz = "UTC")
        )
      )

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

#' Return climate station timeseries data
#' @description Make a request to the /climatedata/climatestationts  endpoints (climatestationtsday, climatestationtsmonth) to retrieve climate station timeseries data by station number or Site IDs within a given date range (start and end dates)
#' @param station_number character, surface water station number
#' @param site_id character vector or list of characters of climate station site IDs
#' @param param character climate variable. One of: "Evap", "FrostDate",  "MaxTemp", "MeanTemp", "MinTemp", "Precip", "Snow", "SnowDepth", "SnowSWE", "Solar","VP", "Wind"
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating data type to return, either "day" or "month". Default is "day".
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of climate station timeseries data
#' @examples
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
#' @export
get_climate_ts <- function(
    timescale           = "day",
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    station_number      = NULL,
    site_id             = NULL,
    param               = NULL,
    api_key             = NULL
) {

  # check which timescale to request data for

  # request daily climate timeseries data
  if(timescale == "day") {

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
  if(timescale == "month") {

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

