#' Check all arguments of a function for any/all NULL values
#' @description Internal utils function for checking a function arguments for any/all invalid/missing arguments necessary to the function it is called within
#' @param arg_lst list of function arguments by calling 'as.list(environment())'
#' @param ignore character vector of function arguments to ignore NULL check
#' @param f character, either "any" or "all" to indicate whether to check for "any" or "all" NULL argument. If "any" then if any of the function arguments are NULL, then an error is thrown. If "all" then all relevant arguments must be NULL for an error to be thrown. Defaults to "any"
#'
#' @return character error statement with NULL arguments listed, or NULL if no error is thrown by NULL values
check_args <- function(
    arg_lst = NULL,
    ignore  = NULL,
    f       = "any"
) {

  # if no function arguments are given, throw an error
  if(is.null(arg_lst)) {

    stop(paste0("provide a list of function arguments by calling 'as.list(environment())', within another function"))

  } else {

    # make sure provided function is either "any" or "all"
    if(!f %in% c("any", "all")) {

      stop(paste0("Invalid 'f' argument, provide either 'any' or 'all' functions to 'f' argument"))

    }

    # match user provided function
    f <- match.fun(f)

    # if certain arguments are specifically supposed to be ignored
    if(!is.null(ignore)) {

      # remove "api_key" from possible arguments
      args <- arg_lst[!names(arg_lst) %in% ignore]
      # args <- arg_lst[names(arg_lst) != ignore]

    } else {

      args <- arg_lst

    }

    # if any/all arguments are NULL, return an error statement. Otherwise return NULL if NULL check is passed
    if(f(sapply(args, is.null))) {

      return(paste0("Invalid or missing ", paste0("'", c(names(args[sapply(args, is.null)])), "'", collapse = ", "), " arguments"))

    } else {

      return(NULL)


    }

  }

}

#' Convert non NULL list/vectors to characters
#' @description Internal function for converting non NULL lists/vectors to characters. When NULL is provided, NULL is returned.
#' @param x list, or vector to convert to character
#' @return character list/vector of the same length as x
null_convert <- function(x) {

  # if NULL is given, return NULL, otherwise convert to character
  if(is.null(x)) {

    return(NULL)

  } else {

    x <- as.character(x)

    return(x)

  }

}


#' Convert function arguments to strings
#' @description Internal function for converting all or specific function arguments to strings
#' @param arg_lst list of function arguments by calling 'as.list(environment())'
#' @param ignore character vector of function arguments to ignore NULL check
#' @param envir an environment. If called within another function to alter the arguments of the outer function, use envir = environment()
#' @return No returns, function reassigns function arguments to strings
str_args <- function(
    arg_lst = NULL,
    ignore  = NULL,
    envir
    ) {

  # if no function arguments are given, throw an error
  if(is.null(arg_lst)) {

    stop(paste0("provide a list of function arguments by calling 'as.list(environment())', within another function"))

  } else {

    # if certain arguments are specifically supposed to be ignored
    if(!is.null(ignore)) {

      # remove "api_key" from possible arguments
      args <- arg_lst[!names(arg_lst) %in% ignore]

    } else {

      args <- arg_lst

    }

    # convert all values to strings
    str_lst  <- sapply(args, toString)

    # if all arguments are NULL, return NULL
    if(all(!nzchar(str_lst))) {

      return(NULL)

    }


    # names and values of non NULL arguments
    lst_names <- names(str_lst[str_lst != ""])
    lst_vals  <- str_lst[str_lst != ""]

    # loop through function arguments and reassign string values
    for(i in 1:length(lst_names)) {

      # reassign function arguments
      assign(
        x        = lst_names[i],
        value    = unname(lst_vals[lst_names[i]]),
        envir    = envir,
        inherits = T
      )
    }

  }

}

# error_status_codes <- function() {
#   return(
#     data.frame(
#       http_response = c("200", "400", "403", "404", "500", "502", "503"),
#       error_code    = c("Ok", "Bad Request", "Forbidden", "Not Found", "Internal_Server_Error", "Connection_Failed", "Server_Unavailable")
#       )
#   )
# }

#' Parse GET URL response
#' @description Internal convenience function for parsing a "text" content of URL GET responses into JSON format
#' @param url character URL if page to retrieve. Default is NULL.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return json formatted list
parse_gets <- function(
    url     = NULL
) {

  # if no URL is given
  if(is.null(url)) {

    stop("Invalid NULL value provided to 'url'")

  }

  # make get request
  resp <- httr::GET(url = url)

  # if request is 200 (OK), return JSON content data
  if(resp$status_code == 200) {

    # get data and convert from JSON
    resp <-
      jsonlite::fromJSON(
        httr::content(
          resp,
          as = "text"
          )
        )

    # return data
    return(resp)

  } else {

    # if request is NOT 200 (not OK), return error from CDSS API

    # get error message out of response
    resp <- httr::content(
      resp,
      as = "text"
    )

    # stop function and return error
    stop(resp)

  }

    # resp <-
    #   httr::content(
    #     httr::GET(url = url),
    #     as = "text"
    #     )

    # if an error is returned, return the error
    # if(grepl("error", resp, ignore.case = TRUE)) {
    #   stop(resp)
    # }

    # # return "URL is properly formatted, but..." response
    # if(resp == "This URL is properly formatted, but returns zero records from CDSS.") {
    #   stop(resp)
    # }

    # get data from JSON format
    # resp <- jsonlite::fromJSON(resp)

    # return(resp)

}

#' GET Request Error message handler
#' @description Internal function for generating dynamic error messages for failed GET requests. Designed to be called within another function and print out the functions input arguments.
#' @param arg_lst list of function arguments by calling 'as.list(environment())'. Default is NULL.
#' @param url character URL if page to retrieve. Default is NULL.
#' @param ignore character vector of function arguments to ignore NULL check. Default is NULL.
#' @param e_msg character error message. Default is NULL.
#' @return character error message that includes the query inputs that led to the error, the requested URL, and the original error message
query_error <- function(
    arg_lst = NULL,
    url     = NULL,
    ignore  = NULL,
    e_msg   = NULL
) {

  # if no function arguments are given, throw an error
  if(is.null(arg_lst)) {

    stop(paste0("provide a list of function arguments by calling 'as.list(environment())', within another function"))

  } else {

    # if certain arguments are specifically supposed to be ignored
    if(!is.null(ignore)) {

      # remove "api_key" from possible arguments
      query_args <- arg_lst[!names(arg_lst) %in% ignore]

    } else {

      query_args <- arg_lst

    }

    return(
      paste0(
        "\n",
        "DATA RETRIEVAL ERROR",
        "\n", "Query: ",
        "\n", paste0(
          paste0(" - ", c(names(query_args))), ": ", c(query_args), collapse = "\n"
        ),
        "\n\nRequested URL:\n", url,
        "\n\n", "Original error message:",
        "\n-----------------------\n\n",
        e_msg
      )
    )
  }
}


#' Collapse a list/vector with a given separator into a single string
#' @description Internal function for generating query string URls
#' @param x list, or vector to collapse into single string
#' @param sep character to separate list/vector items by
#' @return single character string separated by given sep arg
collapse_vect <- function(
    x    = NULL,
    sep  = "%2C+"
) {

  # if no list/vector given, return NULL
  if(is.null(x)) {

    return(NULL)

  }

  # convert to character
  x <- as.character(x)

  # if vect is a list, unlist it
  if(is.list(x)) {

    x <- unlist(x)

  }

  # collapse list/vector into single string w/ seperator
  xstring <- paste0(
    unlist(
      strsplit(x, " ")
    ),
    collapse = sep
  )

  return(xstring)

}

#' Formats, collapses, and extract dates to build query strings
#' @description Internal function for generating date query string URls
#' @param date character date in YYYY-MM-DD format.
#' @param start logical, whether the given date is the starting or ending date
#' @param format character, format to convert the date to. Defaults to MM-DD-YYYY.
#' @param sep character separator to collapse date by. Defaults to "%2F"
#' @return reformated date string
parse_date <- function(
    date   = NULL,
    start  = TRUE,
    format = "%m-%d-%Y",
    sep    = "%2F"
) {

  # if the given date is the starting date or the ending date
  if(start){

    if(is.null(date)) {

      date = "1900-01-01"

    }

    # reformat and extract date
    out_date <- format(as.Date(date, format = "%Y-%m-%d"), format)

    # if an invalid date is given, default to earliest date
    if(is.na(out_date)) {

      out_date <- format(as.Date("1900-01-01", format = "%Y-%m-%d"), format)

    }

    # collapse date with given seperator
    out_date <- gsub("-", sep, out_date)

    return(out_date)

  } else {

    if(is.null(date)) {

      date = Sys.Date()

    }

    # reformat and extract date
    out_date <- format(as.Date(date, format = "%Y-%m-%d"), format)

    # if an invalid date is given, default to earliest date
    if(is.na(out_date)) {

      out_date <- format(as.Date("1900-01-01", format = "%Y-%m-%d"), format)

    }

    # collapse date with given seperator
    out_date <- gsub("-", sep, out_date)

    return(out_date)
  }

}

#' Set wc_identifier name to releases or diversions
#' @description Internal function for getting correct wc_identifier code for querying
#' @param x character indicating whether "diversion" or "release" should be returned. Defaults to NULL and thus "diversion"
#' @return wc_identifier equaling either "diversion" or "release"
align_wcid <- function(x = NULL) {

  # if no wc_identifier given, return "diversion"
  if(is.null(x)) {

    x <- "diversion"

    return(x)

  }

  # if wc ID in the diversions list
  if(x %in% c("diversion", "diversions", "div", "divs", "d")) {

    x <- "diversion"

    return(x)
  }

  # if wc ID in the releases list
  if(x %in% c("release", "releases", "rel", "rels", "r")) {

    x <- "release"

    return(x)

  }

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
#' @importFrom methods is
#' @return dataframe with lat/long columns
parse_aoi <- function(aoi) {

  # if an SF object is provided, convert to Terra before extracting coordinates
  if(methods::is(aoi, "sf")) {

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
  if(methods::is(aoi, "SpatVector")) {

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }

    # check if points
    if(terra::is.points(aoi)) {

      # message("terra points")

      # Extract point coordinates
      coords <- terra::crds(aoi)

      # assign lng/lat
      lng <- coords[1]
      lat <- coords[2]

    }

    # check if polygons
    if(terra::is.polygons(aoi)) {

      # message("terra poly")

      # Extract polygon centroid coordinates
      coords <-
        terra::crds(
          terra::centroids(aoi, inside = T)
          )

      # assign lng/lat
      lng <- coords[1]
      lat <- coords[2]

    }

    # check if lines
    if(terra::is.lines(aoi)) {

      # message("terra line")

      # Extract line centroid coordinates
      coords <-
        terra::crds(
          terra::centroids(aoi, inside = T)
        )

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
#' @importFrom methods is
#' @return dataframe with lat/long columns
extract_coords <- function(
    aoi
) {

  # check if matrix or dataframe before continuing
  if(!any(is.matrix(aoi), is.data.frame(aoi), is.list(aoi), is.numeric(aoi), is.character(aoi),  methods::is(aoi, "SpatVector"))) {
    stop(aoi_error_msg())
  }

  # if spatial data is provided, check type and try to extract XY coordinates
  if(!is.null(aoi)) {

    # given a 2 column matrix of XY coords
    if(any(all(methods::is(aoi, "list")), is.numeric(aoi), is.character(aoi))) {

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
    if(methods::is(aoi, "sf") | methods::is(aoi, "SpatVector")) {

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
    if(is.data.frame(aoi) & !any(methods::is(aoi, "sf"))) {

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
#' @importFrom terra vect crs project is.polygons relate
#' @importFrom methods is
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
  if(methods::is(aoi, "sf")) {

    # convert sf to terra SpatVector
    aoi <- terra::vect(aoi)

    # check CRS is 4326, and transform if necessary
    if(terra::crs(aoi, describe = T)$code != "4326") {

      aoi <- terra::project(aoi, "epsg:4326")

    }
  }

  # get coordinates from SpatVector points/polygons/lines
  if(methods::is(aoi, "SpatVector")) {

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

#' Locate all CDSS API Endpoints
#' @description Returns a dataframe with the API endpoints for CDSS REST services
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
    rvest::html_table(
      rvest::html_nodes(page, "table")
      )

  # extract endpoint tables
  api_endpoints <- api_endpoints[c(3:17)]

  # do.call row bind list together
  api_endpoints <- do.call(rbind, api_endpoints)

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
    rvest::html_table(
      rvest::html_elements(
        rvest::html_nodes(
          field_page, "table"
          ),
        xpath = "//*[@class = 'help-page-table']"
        )
      )

  # do.call row bind list together
  field_tbl <- do.call(rbind, field_tbl)

  # extract help table detailing endpoint parameters
  # field_tbl <-
  #   field_page %>%
  #   rvest::html_nodes("table") %>%
  #   rvest::html_elements(xpath = "//*[@class = 'help-page-table']") %>%
  #   rvest::html_table() %>%
  #   do.call(rbind, .)

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
