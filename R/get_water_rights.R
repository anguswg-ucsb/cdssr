#' Return water rights net amounts data
#' @description Make a request to the /waterrights/netamount endpoint to retrieve water rights net amounts data via a spatial search or by county, division, water district, or WDID, within a given date range (start and end dates). Returns current status of a water right based on all of its court decreed actions.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param water_district numeric, indicating the water district to query
#' @param wdid character, indicating WDID code of structure
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of water right net amounts
#' @export
#'
#' @examples
#' # Water right net amounts within a county
#' county_net <- get_water_rights_netamount(county = "Adams")
#'
#' county_net
#'
#' # Water right net amounts within a division
#' division_net <- get_water_rights_netamount(division = 1)
#'
#' division_net
#'
#' # Water right net amounts for a WDID
#' wdid_net <- get_water_rights_netamount(wdid   = "0100555")
#'
#' wdid_net
#' # Water right net amounts within a 20 mile search radius
#' aoi_net <- get_water_rights_netamount(
#'                   aoi    = data.frame(X = -104.3379, Y = 39.87417),
#'                   radius = 20
#'                   )
#'
#'  aoi_net
get_water_rights_netamount <- function(
    aoi                 = NULL,
    radius              = NULL,
    county              = NULL,
    division            = NULL,
    water_district      = NULL,
    wdid                = NULL,
    api_key             = NULL
) {

  # check if valid wdid and admin_no were given
  if(all(is.null(aoi), is.null(county), is.null(division), is.null(water_district), is.null(wdid))) {
    stop(paste0("Invalid 'aoi', 'county', 'division', 'water_district' or 'wdid' arguments"))
  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/?"

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
  message(paste0("Retrieving water rights net amounts data"))

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
        message(paste0("Error in water rights net amounts data query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nWater district: ", water_district,
                       "\nWDID: ", wdid,
                       "\nLatitude: ", lat,
                       "\nLongitude: ", lng,
                       "\nRadius ", radius
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

  # mask data in the case that a polygon AOI was given, otherwise masking is skipped in original dataset is returned
  data_df <- aoi_mask(
                  aoi = aoi,
                  pts = data_df
                  )

  # return final binded dataframe
  return(data_df)

}

#' Return water rights transactions data
#' @description  Make a request to the /waterrights/transaction endpoint to retrieve water rights transactions data via a spatial search or by county, division, water district, or WDID, within a given date range (start and end dates). Returns List of court decreed actions that affect amount and use(s) that can be used by each water right.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param water_district numeric, indicating the water district to query
#' @param wdid character, indicating WDID code of structure
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%`
#' @return dataframe of water right transactions
#' @export
#'
#' @examples
#'
#' # Water right transactions within a county
#' county_wr <- get_water_rights_trans(county = "Adams")
#'
#' county_wr
#'
#' # Water right transactions within a division
#' division_wr <- get_water_rights_trans(division = 1)
#'
#' division_wr
#'
#' # Water right transactions for a WDID
#' wdid_wr <- get_water_rights_trans(wdid   = "2000502")
#'
#' wdid_wr
#'
#' # Water right transactions within a 20 mile search radius
#' aoi_wr <- get_water_rights_trans(
#'                   aoi    = data.frame(X = -104.3379, Y = 39.87417),
#'                   radius = 20
#'                   )
#' aoi_wr
#'
get_water_rights_trans <- function(
    aoi                 = NULL,
    radius              = NULL,
    county              = NULL,
    division            = NULL,
    water_district      = NULL,
    wdid                = NULL,
    api_key             = NULL
) {

  # check if valid wdid and admin_no were given
  if(all(is.null(aoi), is.null(county), is.null(division), is.null(water_district), is.null(wdid))) {
    stop(paste0("Invalid 'aoi', 'county', 'division', 'water_district' or 'wdid' arguments"))
  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/waterrights/transaction/?"

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
  message(paste0("Retrieving water rights transactions data"))

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
        message(paste0("Error in water rights transaction data query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nCounty: ", county,
                       "\nDivision: ", division,
                       "\nWater district: ", water_district,
                       "\nWDID: ", wdid,
                       "\nLatitude: ", lat,
                       "\nLongitude: ", lng,
                       "\nRadius ", radius
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

  # mask data in the case that a polygon AOI was given, otherwise masking is skipped in original dataset is returned
  data_df <- aoi_mask(
                  aoi = aoi,
                  pts = data_df
                  )

  # return final binded dataframe
  return(data_df)

}






