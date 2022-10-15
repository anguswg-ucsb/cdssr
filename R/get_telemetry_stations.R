#' Return Telemetry Station info
#' @description Make a request to the telemetrystations/telemetrystation/ endpoint to locate telemetry stations by AOI, station abbreviations, county, division, station abbreviation, GNIS ID, USGS Station ID, or WDID
#' @param aoi 2 column matrix/dataframe of XY coordinates, or SF point or polygon object to search for administrative structures within a given radius
#' @param radius numeric, search radius in miles around a given point (or the centroid of a polygon) to return administrative structures. If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param abbrev character vector or list of characters of station abbreviation
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param gnis_id character, water source - Geographic Name Information System ID
#' @param usgs_id character, indicating USGS Station ID
#' @param water_district numeric, indicating the water district to query
#' @param wdid character indicating WDID code of structure
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate `%>%`
#' @importFrom janitor clean_names
#' @return dataframe of telemetry station info
#' @examples
#' # Retrieve telemetry stations within a county
#' telemetry_station <- get_telemetry_stations(
#'                                county = "Adams"
#'                                )
#'
#' # plot telemetry station locations
#' plot(telemetry_station$latitude~telemetry_station$longitude)
#' @export
get_telemetry_stations <- function(
    aoi                 = NULL,
    radius              = NULL,
    abbrev              = NULL,
    county              = NULL,
    division            = NULL,
    gnis_id             = NULL,
    usgs_id             = NULL,
    water_district      = NULL,
    wdid                = NULL,
    api_key             = NULL
) {

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrystation/?"

  # format multiple Site ID query string
  if(!is.null(abbrev)) {

    # if USGS IDs are in a list, unlist to a character vector
    if(is.list(abbrev) == TRUE) {

      abbrev <- unlist(abbrev)

    }

    abbrev <- paste0(unlist(strsplit(abbrev, " ")), collapse = "%2C+")

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
  message(paste0("Retrieving telemetry station data from CDSS API..."))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&abbrev=", abbrev,
      "&county=", county,
      "&division=", division,
      "&gnisId=", gnis_id,
      "&includeThirdParty=true",
      "&usgsStationId=", usgs_id,
      "&waterDistrict=", water_district,
      "&wdid=", wdid,
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
        message(paste0("Error in telemetry station query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nStation Abbreviation: ", abbrev,
                       "\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nGNIS ID: ", gnis_id,
                       "\nUSGS Station ID: ", usgs_id,
                       "\nWater District: ", water_district,
                       "\nWDID: ", wdid,
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
        meas_date_time     = as.POSIXct(meas_date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
        station_por_start  = as.POSIXct(station_por_start, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
        station_por_end    = as.POSIXct(station_por_end, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
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
