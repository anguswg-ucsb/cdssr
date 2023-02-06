utils::globalVariables(c("."))
#' Return Telemetry Station information
#' @description Make a request to the telemetrystations/telemetrystation/ endpoint to locate telemetry stations by AOI, station abbreviations, county, division, station abbreviation, GNIS ID, USGS Station ID, or WDID
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param abbrev character vector or list of characters of station abbreviation
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param gnis_id character, water source - Geographic Name Information System ID
#' @param usgs_id character, indicating USGS Station ID
#' @param water_district numeric, indicating the water district to query
#' @param wdid character indicating WDID code of structure
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of telemetry station info
#' @export
#' @examples
#' \dontrun{
#' # Retrieve telemetry stations within a county
#' telemetry_station <- get_telemetry_stations(
#'                                county = "Adams"
#'                                )
#'
#' # plot telemetry station locations
#' plot(telemetry_station$latitude~telemetry_station$longitude)
#' }
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

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # base URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrystation/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   ignore  = c("aoi"),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  division        <- null_convert(division)
  gnis_id         <- null_convert(gnis_id)
  usgs_id         <- null_convert(usgs_id)
  water_district  <- null_convert(water_district)
  wdid            <- null_convert(wdid)

  # format multiple abbrev query string
  abbrev <- collapse_vect(
    x   = abbrev,
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
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving telemetry station data"))

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
    tryCatch({

      # query CDSS API
      cdss_data <- parse_gets(url = url)

    },
    error = function(e) {

      # error message handler
      message(
        query_error(
          arg_lst = input_args,
          ignore  = c("url", "e"),
          url     = url,
          e_msg   = e
        )
      )

      stop()

    })

    # Extract Result List
    cdss_data <- cdss_data$ResultList

    # set clean names
    names(cdss_data) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

    # set datetime column
    cdss_data$meas_date_time     <- as.POSIXct(cdss_data$meas_date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    cdss_data$station_por_start  <- as.POSIXct(cdss_data$station_por_start, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    cdss_data$station_por_end    <- as.POSIXct(cdss_data$station_por_end, format="%Y-%m-%d %H:%M:%S", tz = "UTC")


    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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
