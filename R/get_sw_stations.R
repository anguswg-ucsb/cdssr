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
