#' Return Climate Station information
#' @description Make a request to the climatedata/climatestations/ endpoint to locate climate stations by AOI, county, division, station name, Site ID or water district.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param station_name character, surface water station name
#' @param site_id character vector or list of characters of climate station site IDs
#' @param water_district numeric, indicating the water district to query
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom sf st_coordinates st_as_sf st_centroid st_geometry_type
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of climate data station info
#' @examples
#' # get climate stations in Adams County, CO
#' climate_stations  <- get_climate_stations(
#'                        county = "Adams"
#'                        )
#'
#' # plot latitude/longitude of climate stations
#' plot(climate_stations$latitude~climate_stations$longitude)
#' @export
get_climate_stations <- function(
    aoi                 = NULL,
    radius              = NULL,
    county              = NULL,
    division            = NULL,
    station_name        = NULL,
    site_id             = NULL,
    water_district      = NULL,
    api_key             = NULL
) {

  # check if valid parameters are given
  if(all(is.null(aoi), is.null(county), is.null(division), is.null(station_name), is.null(site_id), is.null(water_district))) {

    stop(paste0("Invalid `aoi`, 'county', 'division', 'station_name', 'site_id', or 'water_district' arguments"))

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/climatedata/climatestations/?"

  # format multiple site_id query string
  site_id <- collapse_vect(
    x   = site_id,
    sep = "%2C+"
  )

  # check and extract spatial data from 'aoi' and 'radius' args for location search query
  aoi_lst <- check_aoi(
    aoi    = aoi,
    radius = radius
  )

  # lat/long coords
  lat    <- aoi_lst$lat
  lng    <- aoi_lst$lng
  radius <- aoi_lst$radius

  # maximum records per page
  page_size  <- 500000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving climate station data"))

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
      "&county=", county,
      "&division=", division,
      "&stationName=", station_name,
      "&siteId=", site_id,
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
        message(paste0("Error in climate station query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nStation name: ", station_name,
                       "\nSite ID: ", site_id,
                       "\nWater District: ", water_district,
                       "\nLatitude: ", lat,
                       "\nLongitude: ", lng,
                       "\nRadius (miles) ", radius
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
    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

    # add extra columns
    cdss_data$start_date   <- as.POSIXct(cdss_data$start_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    cdss_data$end_date     <- as.POSIXct(cdss_data$end_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

    # bind data from this page
    data_df <- dplyr::bind_rows(data_df, cdss_data)

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  # mask data in the case that a polygon AOI was given, otherwise masking is skipped in original dataset is returned
  data_df <- aoi_mask(
                  aoi = aoi,
                  pts = data_df
                  )

  # return final binded dataframe
  return(data_df)

}


