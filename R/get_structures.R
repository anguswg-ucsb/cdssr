#' Return list of administrative structures
#' @description Make a request to the /structures endpoint to locate administrative structures via a spatial search or by division, county, water_district, GNIS, or WDID.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param gnis_id character, water source - Geographic Name Information System ID
#' @param water_district numeric, indicating the water district to query
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS. Defaults to NULL.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of administrative structures
#' @export
#' @examples
#' # Request endpoint: api/v2/structures/
#' divrec_structures <- get_structures(
#'                            county = "Boulder"
#'                            )
#'
#' # plot administrative structure locations
#' plot(divrec_structures$latdecdeg~divrec_structures$longdecdeg)
get_structures <- function(
    aoi                 = NULL,
    radius              = NULL,
    county              = NULL,
    division            = NULL,
    gnis_id             = NULL,
    water_district      = NULL,
    wdid                = NULL,
    api_key             = NULL
) {

  # check if valid parameters are given
  if(all(is.null(aoi), is.null(county), is.null(division), is.null(gnis_id), is.null(water_district), is.null(wdid))) {

    stop(paste0("Invalid 'aoi', 'county', 'division', 'gnis_id', 'water_district', or 'wdid' arguments"))

  }

  # Base API URL
  base <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/structures/?")

  # format multiple WDID query string
  wdid <- collapse_vect(
    x   = wdid,
    sep = "%2C+"
  )

  # check and extract spatial data from 'aoi' and 'radius' args
  aoi_lst <- check_aoi(
    aoi    = aoi,
    radius = radius
  )

  # lat/long coords
  lat    <- aoi_lst$lat
  lng    <- aoi_lst$lng
  radius <- aoi_lst$radius

  # format county name
  if(!is.null(county)) {

    county <- gsub(" ", "+", toupper(county))

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retreiving administrative structures"))

  # if location based search
  if(all(!is.null(lng), !is.null(lat))) {

    # location search print message
    message(paste0("Location search: \nLatitude: ", lat,
                   "\nLongitude: ", lng,
                   "\nRadius (miles): ", radius))
  }

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
      "&division=", division,
      "&gnisId=", gnis_id,
      "&waterDistrict=", water_district,
      "&wdid=", wdid,
      "&latitude=", lat,
      "&longitude=", lng,
      "&radius=", radius,
      "&units=miles",
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # api_key <- NULL
    # check whether to use API key or not
    if(!is.null(api_key)) {

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

        message(paste0("Error in administrative structure query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nGNIS ID: ", gnis_id,
                       "\nWater District: ", water_district,
                       "\nWDID: ", wdid
                       )
                )
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

  # mask data in the case that a polygon AOI was given, otherwise masking is skipped in original dataset is returned
  data_df <- aoi_mask(
                  aoi = aoi,
                  pts = data_df
                  )

  # return final binded dataframe
  return(data_df)

}
