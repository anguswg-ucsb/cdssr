#' Return Surface Water Station info
#' @description Make a request to the /surfacewater/surfacewaterstations endpoint to locate surface water stations by AOI, station abbreviation, county, division, station name, USGS ID or water_district.
#' @param aoi 2 column matrix/dataframe of XY coordinates, or SF point or polygon object to search for administrative structures within a given radius
#' @param radius numeric, search radius in miles around a given point (or the centroid of a polygon) to return administrative structures. If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param abbrev character vector or list of characters of station abbreviation
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param station_name character, surface water station name
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param water_district numeric, indicating the water district to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of surface water station info
#' @examples
#' # Retrieve surface water station info from Larimer county
#' sw_stations <- get_sw_stations(
#'                     county = "Larimer"
#'                     )
#'  # plot latitude/longitude of surface water stations
#'  plot(sw_stations$latitude~sw_stations$longitude)
#' @export
get_sw_stations <- function(
    aoi                 = NULL,
    radius              = NULL,
    abbrev              = NULL,
    county              = NULL,
    division            = NULL,
    station_name        = NULL,
    usgs_id             = NULL,
    water_district      = NULL,
    api_key             = NULL
) {

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewaterstations/?"

  # format multiple abbrev query
  if(!is.null(abbrev)) {

    # if abbreviations are in a list, unlist to a character vector
    if(is.list(abbrev) == TRUE) {

      abbrev <- unlist(abbrev)

    }

    abbrev <- paste0(unlist(strsplit(abbrev, " ")), collapse = "%2C+")

  }

  # format multiple USGS ID query string
  if(!is.null(usgs_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(usgs_id) == TRUE) {

      usgs_id <- unlist(usgs_id)

    }

    usgs_id <- paste0(unlist(strsplit(usgs_id, " ")), collapse = "%2C+")

  }

  # extract lat/long coords for query
  if(!is.null(aoi)) {

    # extract coordinates from matrix/dataframe/sf object
    coord_df <- extract_coords(aoi = aoi)

    # check radius is valid and fix if necessary
    radius   <- check_radius(
      aoi    = aoi,
      radius = radius
    )

    # lat/long coords
    lat <- coord_df$lat
    lng <- coord_df$lng

  } else {

    # if NULL aoi given, set coords and radius to NULL
    lat    <- NULL
    lng    <- NULL
    radius <- NULL

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving surface water station data from CDSS API..."))

  # if location based search
  if(all(!is.null(lng), !is.null(lat))) {

    # location search print message
    message(paste0("Location search: \nLatitude: ", lat,
                   "\nLongitude: ", lng,
                   "\nRadius (miles): ", radius))
  }

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&abbrev=", abbrev,
      "&county=", county,
      "&division=", division,
      "&stationName=", station_name,
      "&usgsSiteId=", usgs_id,
      "&waterDistrict=", water_district,
      "&latitude=", lat,
      "&longitude=", lng,
      "&radius=", radius,
      "&units=miles",
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
        message(paste0("Error in surface water station query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nAbbreviation: ", abbrev,
                       "\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nStation name: ", station_name,
                       "\nUSGS ID: ", usgs_id,
                       "\nWater District: ", water_district,
                       "\nLatitude: ", lat,
                       "\nLongitude: ", lng,
                       "\nRadius (miles)", radius
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
        start_date   = as.POSIXct(start_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
        end_date     = as.POSIXct(end_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
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

#' Return daily surface water timeseries data
#' @description Make a request to the /surfacewater/surfacewatertsday endpoint to retrieve surface water stations daily timeseries data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of surface water station daily timeseries data
get_sw_ts_day <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsday/?"

  # format multiple abbrev query
  if(!is.null(abbrev)) {

    # if abbreviations are in a list, unlist to a character vector
    if(is.list(abbrev) == TRUE) {

      abbrev <- unlist(abbrev)

    }

    abbrev <- paste0(unlist(strsplit(abbrev, " ")), collapse = "%2C+")

  }

  # format multiple USGS ID query string
  if(!is.null(usgs_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(usgs_id) == TRUE) {

      usgs_id <- unlist(usgs_id)

    }

    usgs_id <- paste0(unlist(strsplit(usgs_id, " ")), collapse = "%2C+")

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
  message(paste0("Retrieving surface water timeseries data from CDSS API..."))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      # "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
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
        message(paste0("Error in surface water daily timeseries query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nAbbreviation: ", abbrev,
                       "\nStation number: ", station_number,
                       "\nUSGS ID: ", usgs_id
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
#' Return monthly surface water timeseries data
#' @description Make a request to the /surfacewater/surfacewatertsmonth endpoint to retrieve surface water stations monthly timeseries data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate case_when `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of surface water station monthly timeseries data
get_sw_ts_month <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsmonth/?"

  # format multiple abbrev query
  if(!is.null(abbrev)) {

    # if abbreviations are in a list, unlist to a character vector
    if(is.list(abbrev) == TRUE) {

      abbrev <- unlist(abbrev)

    }

    abbrev <- paste0(unlist(strsplit(abbrev, " ")), collapse = "%2C+")

  }

  # format multiple USGS ID query string
  if(!is.null(usgs_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(usgs_id) == TRUE) {

      usgs_id <- unlist(usgs_id)

    }

    usgs_id <- paste0(unlist(strsplit(usgs_id, " ")), collapse = "%2C+")

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
  message(paste0("Retrieving surface water timeseries data from CDSS API..."))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      # "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
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
        message(paste0("Error in surface water monthly timeseries query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nAbbreviation: ", abbrev,
                       "\nStation number: ", station_number,
                       "\nUSGS ID: ", usgs_id
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
          cal_mon_num <= 9 ~ as.POSIXct(paste0(cal_year, "-0", cal_mon_num, "-01"),  format="%Y-%m-%d", tz = "UTC"),    # add "0" in front of 1 digit months
          TRUE             ~ as.POSIXct(paste0(cal_year, "-", cal_mon_num, "-01"),  format="%Y-%m-%d", tz = "UTC")
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

#' Return water year surface water timeseries data
#' @description Make a request to the /surfacewater/surfacewatertswateryear endpoint to retrieve surface water stations water year timeseries data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character vector or list of characters of station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character vector or list of characters of USGS Site IDs
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of annual surface water station timeseries data
get_sw_ts_wyear <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    api_key             = NULL
) {

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertswateryear/?"

  # format multiple abbrev query
  if(!is.null(abbrev)) {

    # if abbreviations are in a list, unlist to a character vector
    if(is.list(abbrev) == TRUE) {

      abbrev <- unlist(abbrev)

    }

    abbrev <- paste0(unlist(strsplit(abbrev, " ")), collapse = "%2C+")

  }

  # format multiple USGS ID query string
  if(!is.null(usgs_id)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(usgs_id) == TRUE) {

      usgs_id <- unlist(usgs_id)

    }

    usgs_id <- paste0(unlist(strsplit(usgs_id, " ")), collapse = "%2C+")

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
  message(paste0("Retrieving surface water water year timeseries data from CDSS API..."))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      # "&fields=abbrev%2Cparameter%2C", date_field, "%2CmeasValue%2CmeasUnit",
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
        message(paste0("Error in surface water water year timeseries query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date,
                       "\nAbbreviation: ", abbrev,
                       "\nStation number: ", station_number,
                       "\nUSGS ID: ", usgs_id
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
      janitor::clean_names()

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

#' Return Surface water timeseries data
#' @description Make a request to the /surfacewater/surfacewaterts/ endpoints (surfacewatertsday, surfacewatertsmonth, surfacewatertswateryear) to retrieve surface water station timeseries data by station abbreviations, station number, or USGS Site IDs within a given date range (start and end dates)
#' @param abbrev character,	station abbreviation
#' @param station_number character, surface water station number
#' @param usgs_id character, USGS Site ID
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param timescale character indicating data type to return, either "day", "month", or "wateryear". Default is "day".
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of surface water station timeseries data
#' @examples
#' # Retrieve surface water daily timeseries
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
#' # Retrieve surface water monthly timeseries
#' sw_ts_month <-
#'  get_sw_ts(
#'   abbrev     = "CLAFTCCO",
#'   start_date = "2000-01-01",
#'   end_date   = "2022-01-01",
#'   timescale  = "month"
#'   )
#' # plot average monthly flow
#' plot(sw_ts_month$avg_q_cfs~sw_ts_month$datetime, type = "s")
#'
#' # Retrieve surface water water year timeseries
#' sw_ts_year <-
#'  get_sw_ts(
#'   abbrev     = "CLAFTCCO",
#'   start_date = "2000-01-01",
#'   end_date   = "2022-01-01",
#'   timescale  = "wateryear"
#'   )
#'
#' # plot average water year flow
#' plot(sw_ts_year$avg_q_cfs~sw_ts_year$water_year, type = "s")
#' @export
get_sw_ts <- function(
    abbrev              = NULL,
    station_number      = NULL,
    usgs_id             = NULL,
    start_date          = "1900-01-01",
    end_date            = Sys.Date(),
    timescale           = "day",
    api_key             = NULL
) {

  # check which timescale to request data for

  # request surface water daily timeseries data
  if(timescale == "day") {

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

  # request surface water monthly timeseries data
  if(timescale == "month") {

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

  # request surface water wateryear timeseries data
  if(timescale == "wateryear") {

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

  # return timeseries dataframe
  return(sw_ts)


}




