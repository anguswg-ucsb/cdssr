#' Locate all CDSS API Endpoints
#' @description Returns a dataframe with the API endpoints for CDSS REST services
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_table
#' @return dataframe with API endpoint names, URLs and descriptions of each CDSS resource
browse_api <- function() {

  # base CDSS REST API URL
  base_url <- "https://dwr.state.co.us/Rest/"

  # URL to endpoints
  catalog_url <- "https://dwr.state.co.us/rest/get/help"

  # URL to endpoints
  catalog_url  <- url(
    paste0("https://dwr.state.co.us/rest/get/help"),
    "rb"
  )

  # page with table of API endpoints
  page <- rvest::read_html(catalog_url)

  # close URL connection
  close(catalog_url)

  # extract endpoint tables
  api_endpoints <-
    page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    .[c(3:17)] %>%
    do.call(rbind, .)

  # add endpoint column
  api_endpoints$endpoint     <- gsub("GET ", "", api_endpoints$API)

  # add URL column
  api_endpoints$url          <- paste0(base_url, "GET/", api_endpoints$endpoint)

  # select columns
  api_endpoints              <- api_endpoints[, c("Url Generator", "Description", "endpoint", "url")]

  # set names
  names(api_endpoints)       <- c("resource", "description", "endpoint", "url")

  # add URL column
  api_endpoints$endpoint_url <- paste0("https://dwr.state.co.us/Rest/GET/Help/Api/GET-", gsub("/", "-", api_endpoints$endpoint))

  return(api_endpoints)

}

#' Request meta data for a CDSS API endpoint
#' @description Returns the names, descriptions, and types for each response field for a given API endpoint
#' @param endpoint_url character. URL to CDSS API REST Help page detailing the return fields for each endpoint. This URL can be found in the dataframe returned by the browse_api function
#' @param endpoint_path character. full path name of CDSS API resource
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_elements html_table
#' @return dataframe with the endpoint name, field name, a description, the data type, and the endpoint URL
get_resource_meta <- function(
    endpoint_url  = NULL,
    endpoint_path = NULL
) {

  # stop if no URL is given
  if(any(is.null(endpoint_url), is.null(endpoint_path))) {

    stop(paste0("\nInvalid 'endpoint_url' and/or 'endpoint_path'.\n Enter a URL of the following structure:\nhttps://dwr.state.co.us/Rest/GET/Help/Api/GET-<insert-name-of-api-resource>"))

  }

  message(paste0("Getting meta data - ", endpoint_path))

  # Construct URL
  parse_url  <- url(
    paste0(endpoint_url),
    "rb"
  )

  # read HTML from API Parameter Help page
  field_page <- rvest::read_html(parse_url)

  # close URL connection
  close(parse_url)

  # extract help table detailing endpoint parameters
  field_tbl <-
    field_page %>%
    rvest::html_nodes("table") %>%
    rvest::html_elements(xpath = "//*[@class = 'help-page-table']") %>%
    rvest::html_table() %>%
    do.call(rbind, .)

  # rename columns
  names(field_tbl) <- tolower(names(field_tbl))

  # add endpoint path column
  field_tbl$endpoint <- endpoint_path

  # add endpoint URL column
  field_tbl$endpoint_url <- endpoint_url

  # reorder columns
  field_tbl <- field_tbl[c("endpoint", "name", "description", "type", "endpoint_url")]

  # rename columns
  names(field_tbl) <- c("endpoint", "name", "description", "type", "endpoint_url")

  return(field_tbl)
}


#' Error message handling for extract_coords function
#' @description Internal helper function that returns a boilerplate error message used in extract_coords function
#' @return character error message
aoi_error_msg <- function() {

  paste0(
    "Invalid 'aoi' argument, 'aoi' must be one of the following:\n",
    "1. List/Vector of an XY coordinate pair (longitude, latitude)\n",
    "2. 2 column XY coordinate matrix/dataframe (longitude, latitude)\n",
    "3. sf point, polygon, or linestring geometry\n",
    "4. terra SpatVector point, polygon, or linestring geometry"
  )
}

#' Internal function for extracting lat/lng points from SF/Terra geometries
#'
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @importFrom terra crs project is.points is.polygons is.lines vect centroids crds
#' @return dataframe with lat/long columns
parse_aoi <- function(aoi) {

  # if an SF object is provided, convert to Terra before extracting coordinates
  if(class(aoi)[1] == "sf") {

    # message("sf")

    # convert sf to terra SpatVector
    aoi <- terra::vect(aoi)

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }
  }

  # if more than one geometry is provided, stop function
  if(nrow(aoi) > 1) {
    stop(paste0("Invalid number of geometries in 'aoi'\nPlease provide a single point, polygon, or linestring"))
  }

  # get coordinates from SpatVector points/polygons/lines
  if(class(aoi)[1] == "SpatVector") {

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }

    # check if points
    if(terra::is.points(aoi)) {

      # message("terra points")

      # Extract point coordinates
      coords <-
        aoi %>%
        terra::crds()

      # assign lng/lat
      lng <- coords[1]
      lat <- coords[2]

    }

    # check if polygons
    if(terra::is.polygons(aoi)) {

      # message("terra poly")

      # Extract polygon centroid coordinates
      coords <-
        aoi %>%
        terra::centroids(inside = T) %>%
        terra::crds()

      # assign lng/lat
      lng <- coords[1]
      lat <- coords[2]

    }

    # check if lines
    if(terra::is.lines(aoi)) {

      # message("terra line")

      # Extract line centroid coordinates
      coords <-
        aoi %>%
        terra::centroids(inside = T) %>%
        terra::crds()

      lng <- coords[1]
      lat <- coords[2]
    }

  }

  # dataframe of coordinates
  coord_df <- data.frame(
    lng = lng,
    lat = lat
  )

  return(coord_df)

}

#' Extract lat/long coordinates from an AOI
#' @description Internal helper function. Checks that an AOI is of valid type and then extracts the latitude/longitude of the point or centroid of the given polygon.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @importFrom sf st_coordinates st_centroid st_geometry_type
#' @importFrom terra crs project is.points is.polygons is.lines vect centroids crds
#' @importFrom dplyr `%>%`
#' @return dataframe with lat/long columns
extract_coords <- function(
    aoi
) {

  # check if matrix or dataframe before continuing
  if(!any(is.matrix(aoi), is.data.frame(aoi), is.list(aoi), is.numeric(aoi), is.character(aoi),  class(aoi) == 'SpatVector')) {
    stop(aoi_error_msg())
  }

  # if spatial data is provided, check type and try to extract XY coordinates
  if(!is.null(aoi)) {

    # given a 2 column matrix of XY coords
    if(any(all(class(aoi) == "list"), is.numeric(aoi), is.character(aoi))) {

      # message("List/vector")

      # extract lng/lat coords
      lng <- as.numeric(aoi[1])
      lat <- as.numeric(aoi[2])

      if(any(is.null(lat), is.null(lng))) {

        stop(aoi_error_msg())

      }

    }

    # given a 2 column matrix of XY coords
    if(any(is.matrix(aoi))) {

      # extract lng/lat coords
      lng <- as.numeric(aoi[1])
      lat <- as.numeric(aoi[2])

      if(any(is.null(lat), is.null(lng))) {

        stop(aoi_error_msg())

      }
    }

    # check if aoi is 'sf' or 'SpatVector'
    if(class(aoi)[1] == "sf"| class(aoi)[1] == "SpatVector") {

      # message(paste0("Class: ", class(aoi)))

      # extract coordinates from spatial object
      parsed_coords <- parse_aoi(aoi = aoi)

      # set lng/lat values
      lng <- parsed_coords$lng
      lat <- parsed_coords$lat

      if(any(is.null(lat), is.null(lng))) {

        stop(aoi_error_msg())
      }

    }
    # check if aoi is NON sf dataframe
    if(is.data.frame(aoi) & !any(class(aoi) == "sf")) {

      # extract first 2 columns of datafame (X and Y)
      lng <- aoi[,1]
      lat <- aoi[,2]

      if(any(is.null(lat), is.null(lng))) {
        stop(aoi_error_msg())
      }

    }

  }

  # dataframe of coordinates
  coord_df <- data.frame(
    lng = lng,
    lat = lat
  )

  return(coord_df)

}
# extract_coords <- function(
#     aoi,
#     verbose = FALSE
# ) {
#
#   # print class of aoi
#   if(verbose == TRUE){
#     message(paste(class(aoi)))
#   }
#
#   # check if matrix or dataframe before continuing
#   if(!any(is.matrix(aoi), is.data.frame(aoi))) {
#     stop(paste0("Invalid AOI type, must be 2 column matrix/dataframe of XY coordinates, SF POINT/MULTIPOINT or SF POLYGON/MULTIPOLYGON object"))
#   }
#
#   # if spatial data is provided, check type and try to extract XY coordinates
#   if(!is.null(aoi)) {
#
#
#     # given a 2 column mateix of XY coords
#     if(any(is.matrix(aoi) == TRUE)) {
#
#       lng <- aoi[1]
#       lat <- aoi[2]
#
#       if(any(is.null(lat), is.null(lng))) {
#
#         stop(paste0("List must be of length 2 and ordered latitude, longitude"))
#
#       }
#     }
#
#     # given a type of dataframe
#     if(is.data.frame(aoi) == TRUE) {
#
#       # given a SF point or polygon
#       if(any(class(aoi) == "sf")) {
#
#         # if SF object is a point, extract coordinates
#         if(sf::st_geometry_type(aoi) == "POINT" | sf::st_geometry_type(aoi) == "MULTIPOINT") {
#
#           coords <-
#             aoi %>%
#             sf::st_coordinates()
#
#           lng <- coords[1]
#           lat <- coords[2]
#
#         }
#
#         # if SF object is a polygon, find centroid and extract coordinates
#         if(sf::st_geometry_type(aoi) == "POLYGON" | sf::st_geometry_type(aoi) == "MULTIPOLYGON") {
#
#           coords <-
#             aoi %>%
#             sf::st_centroid() %>%
#             sf::st_coordinates()
#
#           lng <- coords[1]
#           lat <- coords[2]
#
#         }
#
#         if(any(is.null(lat), is.null(lng))) {
#
#           stop(paste0("SF object must be of geometry type POINT or POLYGON"))
#         }
#       } else {
#
#         names(aoi) <- toupper(names(aoi))
#
#         lng <- aoi$X
#         lat <- aoi$Y
#
#         if(any(is.null(lat), is.null(lng))) {
#
#           stop(paste0("Must be two column dataframe of XY coordinate"))
#         }
#       }
#     }
#
#   } else {
#
#     stop(paste0("Invalid AOI type, must be 2 column matrix/dataframe of XY coordinates, SF POINT/MULTIPOINT or SF POLYGON/MULTIPOLYGON object"))
#
#   }
#
#   # dataframe of coordinates
#   coord_df <- data.frame(
#     lng = lng,
#     lat = lat
#   )
#
#   return(coord_df)
#
# }

#' Validate radius for location search queries
#' @description Internal helper function. Checks that an AOI is given and that the radius is within valid range of values. Radius must be: 0 < radius < 150.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @return numeric radius value or NULL
check_radius <- function(
    aoi,
    radius
) {


  # if spatial data is provided, check type and try to extract XY coordinates
  if(!is.null(aoi)) {

    # if radius is not NULL, and is larger than 150, set to max of 150. if NULL radius is provided with spatial data, default to 150 miles
    if(!is.null(radius)) {

      # if radius value is over max, set to 150
      if(radius > 150) {
        radius = 150
      }

      # if radius value is under min, set to 1
      if(radius <= 0) {
        radius = 1
      }

      # if no radius given, set to 20 miles
    } else {

      radius = 20

    }

  } else {

    radius <- NULL

  }

  return(radius)

}

#' Extract coordinates and radius values to use in constructing location URL
#' @description Internal helper function. Checks that an AOI is given and that the radius is within valid range of values. Radius must be: 0 < radius < 150.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @return named list containing lat, lng, and radius values
check_aoi <- function(
    aoi    = NULL,
    radius = NULL
) {

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

  # create list to return
  aoi_lst <- list(lat = lat, lng = lng, radius = radius)

  return(aoi_lst)

}

#' Mask points returned from CDSS to only those within polygon AOI
#' @description Internal function for masking out extraneous points that fall outside of bounds of provided 'aoi' polygon. If not 'aoi' is provided to the function, the default behavior is to return the original set of points received from CDSS. Furthermore, if the AOI is anything other than a sf or terra polygon, the function will return the original set of data from CDSS.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry. Default is NULL.
#' @param pts dataframe of points that should be masked to the given aoi. Dataframe must contain "utm_y" and "utm_x"columns
#' @return dataframe containing subset (or original) point data from the CDSS API
aoi_mask <- function(
    aoi = NULL,
    pts = NULL
) {

  # if AOI and pts are NULL, return NULL
  if(all(is.null(aoi), is.null(pts))) {

    return(NULL)

  }

  # if no 'aoi' is given (NULL), just return original pts data. Default behavior
  if(is.null(aoi)) {

    # message("returning original bc AOI is null")

    return(pts)

  }

  # if an SF object is provided, convert to Terra before extracting coordinates
  if(class(aoi)[1] == "sf") {

    # convert sf to terra SpatVector
    aoi <- terra::vect(aoi)

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }
  }

  # get coordinates from SpatVector points/polygons/lines
  if(class(aoi)[1] == "SpatVector") {

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }

    # check if polygons
    if(terra::is.polygons(aoi)) {

      # keep only points within given aoi polygon
      rel_pts <- pts[
                    as.vector(
                      terra::relate(
                        x = aoi,
                        y = terra::project(
                          terra::vect(pts, geom = c("utm_x", "utm_y"), crs  = "epsg:26913"),
                          "epsg:4326"
                        ),
                        "contains"
                      )
                    ),
                  ]

      # message("returning masked subset of points")

      return(rel_pts)

    } else {

      # message("returning original data bc SpatVector is NOT a POLYGON")

      # If SpatVector but NOT polygon, return original data
      return(pts)

    }

  } else {

    # message("returning original data the AOI was not NULL but also not SF/TERRA")

    # If neither sf OR SpatVector, return original data
    return(pts)

  }

}
