#' Extract lat/long coordinates from an AOI
#' @description Internal helper function. Checks that an AOI is of valid type and then extracts the latitude/longitude of the point or centroid of the given polygon.
#' @param aoi 2 column matrix/dataframe of XY coordinates, or SF point or polygon object to search for administrative structures within a given radius
#' @param verbose logical, whether to print messages to console. Default is FALSE, no print.
#' @importFrom sf st_coordinates st_centroid st_geometry_type
#' @importFrom dplyr `%>%`
#' @return dataframe with lat/long columns
extract_coords <- function(
    aoi,
    verbose = FALSE
) {

  # print class of aoi
  if(verbose == TRUE){
    message(paste(class(aoi)))
  }

  # check if matrix or dataframe before continuing
  if(!any(is.matrix(aoi), is.data.frame(aoi))) {
    stop(paste0("Invalid AOI type, must be 2 column matrix/dataframe of XY coordinates, SF POINT/MULTIPOINT or SF POLYGON/MULTIPOLYGON object"))
  }

  # if spatial data is provided, check type and try to extract XY coordinates
  if(!is.null(aoi)) {


    # given a 2 column mateix of XY coords
    if(any(is.matrix(aoi) == TRUE)) {

      lng <- aoi[1]
      lat <- aoi[2]

      if(any(is.null(lat), is.null(lng))) {

        stop(paste0("List must be of length 2 and ordered latitude, longitude"))

      }
    }

    # given a type of dataframe
    if(is.data.frame(aoi) == TRUE) {

      # given a SF point or polygon
      if(any(class(aoi) == "sf")) {

        # if SF object is a point, extract coordinates
        if(sf::st_geometry_type(aoi) == "POINT" | sf::st_geometry_type(aoi) == "MULTIPOINT") {

          coords <-
            aoi %>%
            sf::st_coordinates()

          lng <- coords[1]
          lat <- coords[2]

        }

        # if SF object is a polygon, find centroid and extract coordinates
        if(sf::st_geometry_type(aoi) == "POLYGON" | sf::st_geometry_type(aoi) == "MULTIPOLYGON") {

          coords <-
            aoi %>%
            sf::st_centroid() %>%
            sf::st_coordinates()

          lng <- coords[1]
          lat <- coords[2]

        }

        if(any(is.null(lat), is.null(lng))) {

          stop(paste0("SF object must be of geometry type POINT or POLYGON"))
        }
      } else {

        names(aoi) <- toupper(names(aoi))

        lng <- aoi$X
        lat <- aoi$Y

        if(any(is.null(lat), is.null(lng))) {

          stop(paste0("Must be two column dataframe of XY coordinate"))
        }
      }
    }

  } else {

    stop(paste0("Invalid AOI type, must be 2 column matrix/dataframe of XY coordinates, SF POINT/MULTIPOINT or SF POLYGON/MULTIPOLYGON object"))

  }

  # dataframe of coordinates
  coord_df <- data.frame(
    lng = lng,
    lat = lat
  )

  return(coord_df)

}

#' Validate radius for location search queries
#' @description Internal helper function. Checks that an AOI is given and that the radius is within valid range of values. Radius must be: 0 < radius < 150.
#' @param aoi 2 column matrix/dataframe of XY coordinates, or SF point or polygon object to search for administrative structures within a given radius
#' @param radius numeric, search radius in miles around a given point (or the centroid of a polygon) to return administrative structures. If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param verbose logical, whether to print messages to console. Default is FALSE, no print.
#' @return numeric radius value or NULL
check_radius <- function(
    aoi,
    radius,
    verbose = FALSE
) {

  if(verbose == TRUE){
    message(paste0(radius))
  }

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
#' @param aoi 2 column matrix/dataframe of XY coordinates, or SF point or polygon object to search for administrative structures within a given radius
#' @param radius numeric, search radius in miles around a given point (or the centroid of a polygon) to return administrative structures. If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
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
