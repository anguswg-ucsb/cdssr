#' Search for groundwater water level wells
#' @description Given search query parameters, a request is made to the api/v2/groundwater/waterlevels/wells endpoint, and a dataframe of groundwater water level wells is returned matching the specified query
#' @param county character, indicating the county to query
#' @param designated_basin character, indicating the  designated basin to query
#' @param division numeric, indicating the water division to query
#' @param management_district character, indicating the management district to query
#' @param water_district numeric, indicating the water district to query
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' # Request endpoint: api/v2/groundwater/waterlevels/wells/
#' wl_wells <- get_gw_wl_wells(
#'   county = "ADAMS"
#'  )
#'  plot(wl_wells$latitude~wl_wells$longitude)
get_gw_wl_wells <- function(
    county              = NULL,
    designated_basin    = NULL,
    division            = NULL,
    management_district = NULL,
    water_district      = NULL,
    wellid              = NULL,
    api_key             = NULL
) {

  # Base API
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/waterlevels/wells/?"

  # if no inputs given, stop function
  if(all(is.null(division), is.null(county), is.null(designated_basin), is.null(water_district), is.null(management_district), is.null(wellid))) {
    stop(paste0("Please enter one of:\nDivision\nCounty\nDesignated Basin\nWater District\nManagement District\nWell ID"))
  }

  # format county name
  if(!is.null(county)) {

    county <- gsub(" ", "+", toupper(county))

  }

  # format management district name
  if(!is.null(management_district)) {

    management_district <- gsub(" ", "+", toupper(management_district))

  }

  # format designated district name
  if(!is.null(designated_basin)) {

    designated_basin <- gsub(" ", "+", toupper(designated_basin))

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Downloading data from CDSS API...\nSearching groundwater water levels wells"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
      "&wellId=", wellid,
      "&division=", division,
      "&waterDistrict=", water_district,
      "&designatedBasin=", designated_basin,
      "&managementDistrict=", management_district,
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

        message(paste0("Error in groundwater water levels wells data search query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nDivision: ", division,
                       "\nWater District: ", water_district,
                       "\nDesignated Basin: ", designated_basin,
                       "\nManagement District: ", management_district,
                       "\nCounty: ", county,
                       "\nWell ID: ", wellid))
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
        datetime       = as.POSIXct(measurement_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
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

#' Return groundwater water level well measurements
#' @description Given query specifications, this function makes a request to this endpoint of the CDSS API: api/v2/groundwater/waterlevels/wellmeasurements and returns a dataframe containing  groundwater water level well measurements
#' @param wellid character, indicating the Well ID to query
#' @param start_date character date to request data start point YYYY-MM-DD.
#' @param end_date character date to request data end point YYYY-MM-DD. Default is set to the current date function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' # Request endpoint: api/v2/groundwater/waterlevels/wellmeasurements
#' well_measure <- get_gw_wl_wellmeasures(
#'                   wellid = 1274
#'                    )
#'
#' # plot depth to water
#' plot(well_measure$depth_to_water~well_measure$datetime, type = "l")
get_gw_wl_wellmeasures <- function(
    wellid           = NULL,
    start_date       = "1950-01-01",
    end_date         = Sys.Date(),
    api_key          = NULL
) {

  # if no Well ID given, stop function
  if(is.null(wellid)) {
    stop(paste0("Please enter a valid Well ID"))
  }

  # Base URL groundwater/waterlevels/wellmeasurements/
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/waterlevels/wellmeasurements/?"

  # reformat dates to MM-DD-YYYY and format for API query
  start <- gsub("-", "%2F", format(as.Date(start_date, '%Y-%m-%d'), "%m-%d-%Y"))
  end   <- gsub("-", "%2F", format(as.Date(end_date, '%Y-%m-%d'), "%m-%d-%Y"))

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  message(paste0("Downloading data from CDSS API...\nGroundwater well measurements"))
  # message(paste0("Downloading data from CDSS API...\n---------------------------------\nGroundwater well measurements"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-measurementDate=", start,
      "&max-measurementDate=", end,
      # "&min-modified=", end,
      "&wellId=", wellid,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

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

        message(paste0("Error in data retrieval of groundwater well measurements"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------\nWell ID: ", wellid,
                       "\nStart date: ", start_date,
                       "\nEnd date: ", end_date
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

    # Tidy data
    cdss_data <-
      cdss_data %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        datetime       = as.POSIXct(measurement_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
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

#' Search for groundwater geophysicallog wells
#' @description Given search query parameters, a request is made to the api/v2/groundwater/geophysicallogs/wells endpoint, and a dataframe of groundwater geophysicallog wells is returned matching the specified query
#' @param county character, indicating the county to query
#' @param designated_basin character, indicating the  designated basin to query
#' @param division numeric, indicating the water division to query
#' @param management_district character, indicating the management district to query
#' @param water_district numeric, indicating the water district to query
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe of groundwater wells within the given query specifications
#' @export
#' @examples
#' # Request endpoint: api/v2/groundwater/geophysicallogs/wells/
#' gplog_wells <- get_gw_gplogs_wells(
#'   county = "ADAMS"
#'  )
#'  plot(gplog_wells$latitude~gplog_wells$longitude)
get_gw_gplogs_wells <- function(
    county              = NULL,
    designated_basin    = NULL,
    division            = NULL,
    management_district = NULL,
    water_district      = NULL,
    wellid              = NULL,
    api_key             = NULL
) {

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/geophysicallogs/wells/?"

  # if no inputs given, stop function
  if(all(is.null(division), is.null(county), is.null(designated_basin), is.null(water_district), is.null(management_district), is.null(wellid))) {
    stop(paste0("Please enter one of:\nDivision\nCounty\nDesignated Basin\nWater District\nManagement District\nWell ID"))
  }

  # format county name
  if(!is.null(county)) {

    county <- gsub(" ", "+", toupper(county))

  }

  # format management district name
  if(!is.null(management_district)) {

    management_district <- gsub(" ", "+", toupper(management_district))

  }

  # format designated district name
  if(!is.null(designated_basin)) {

    designated_basin <- gsub(" ", "+", toupper(designated_basin))

  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Downloading data from CDSS API...\nSearching groundwater geophysical log wells"))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
      "&wellId=", wellid,
      "&division=", division,
      "&waterDistrict=", water_district,
      "&designatedBasin=", designated_basin,
      "&managementDistrict=", management_district,
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

        message(paste0("Error in groundwater geophysical logs well data search query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nDivision: ", division,
                       "\nWater District: ", water_district,
                       "\nDesignated Basin: ", designated_basin,
                       "\nManagement District: ", management_district,
                       "\nCounty: ", county,
                       "\nWell ID: ", wellid))
        message(paste0('\nHere is the URL address that was queried:\n'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(paste0('-----------------------------------------'))
        message(e)
        stop()

      }
    )

    # Tidy geophysicallogs wells data
    cdss_data <-
      cdss_data %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        datetime       = as.POSIXct(log_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
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

#' Return Groundwater Geophysical Log picks by well ID
#' @description Given a specified well ID, a request is made to api/v2/groundwater/geophysicallogs/geoplogpicks, and a dataframe of groundwater geophysical log picks for the given well ID is returned
#' @param wellid character, indicating the Well ID to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe of groundwater geophysical log picks for a given well ID
#' @export
#' @examples
#' # Request endpoint: api/v2/groundwater/geophysicallogs/geoplogpicks/
#' gplogpicks <- get_gw_gplogs_geologpicks(
#'   wellid = 2409
#'  )
#'  gplogpicks
get_gw_gplogs_geologpicks <- function(
    wellid              = NULL,
    api_key             = NULL
) {

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/groundwater/geophysicallogs/geoplogpicks/?"

  # if no inputs given, stop function
  if(all(is.null(wellid))) {
    stop(paste0("Please enter a valid 'wellid'"))
  }

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df = data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Downloading data from CDSS API..."))

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&wellId=", wellid,
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

        message(paste0("Error in groundwater geophysical log picks well data search query"))
        message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
        message(paste0("Query:\n----------------------------------",
                       "\nWell ID: ", wellid)
                )
        message(paste0('\nHere is the URL address that was queried:\n'))
        message(paste0(url))
        message(paste0('And, here is the original error message:'))
        message(paste0('-----------------------------------------'))
        message(e)
        stop()

      }
    )

    # Tidy geophysicallogs picks wells data
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

#' #' Return groundwater well measurements or search well water levels/geophysical logs
#' #' @description Make a request to CDSS API /groundwater endpoints to get well measurement data or to search for groundwater wells (water level wells or geo physical log records)
#' #' @param type character indicating the type of data to search for. Either "wellmeasurements", "waterlevels", or "geophysicallogs". Default is "wellmeasurements".
#' #' @param wellid character, indicating the Well ID to query
#' #' @param start_date character date to request data start point YYYY-MM-DD.
#' #' @param end_date character date to request data end point YYYY-MM-DD. Default is set to the current date function is run.
#' #' @param county character, indicating the county to query
#' #' @param designated_basin character, indicating the  designated basin to query
#' #' @param division numeric, indicating the water division to query
#' #' @param management_district character, indicating the management district to query
#' #' @param water_district numeric, indicating the water district to query
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' #' @importFrom httr GET content
#' #' @importFrom jsonlite fromJSON
#' #' @importFrom dplyr bind_rows rename mutate
#' #' @importFrom janitor clean_names
#' #' @return dataframe with groundwater well data
#' #' @examples
#' #' # Request endpoint: api/v2/groundwater/waterlevels/wellmeasurements
#' #' well_measure <- get_groundwater(
#' #' type   = "wellmeasurements",
#' #'   wellid = 1274
#' #'  )
#' #'
#' #' # plot depth to water
#' #' plot(well_measure$depth_to_water~well_measure$datetime, type = "l")
#' #'
#' #' # Request endpoint: api/v2/groundwater/waterlevels/wells
#' #' water_levels <- get_groundwater(
#' #'   type     = "waterlevels",
#' #'   division = 2
#' #'  )
#' #'
#' #' # number of unique well IDs from query
#' #' length(unique(water_levels$well_id))
#' #'
#' #' # Request endpoint: api/v2/groundwater/geophysicallogs/wells
#' #' geophysicallogs <- get_groundwater(
#' #'   type     = "geophysicallogs",
#' #'   division = 2
#' #' )
#' #'
#' #' # number of unique well IDs from query
#' #' length(unique(geophysicallogs$well_id))
#' get_groundwater <- function(
#'     type                = "wellmeasurements",
#'     wellid              = NULL,
#'     start_date          = "1950-01-01",
#'     end_date            = Sys.Date(),
#'     county              = NULL,
#'     designated_basin    = NULL,
#'     division            = NULL,
#'     management_district = NULL,
#'     water_district      = NULL,
#'     api_key             = NULL
#' ) {
#'
#'   # check if search is orrectly inputed
#'   if(!type %in% c("wellmeasurements", "waterlevels", "geophysicallogs")) {
#'
#'     message(paste0("Please enter correct search type: 'wellmeasurements', 'waterlevels' or 'geophysicallogs'"))
#'
#'   }
#'
#'   # if groundwater well measurements desired
#'   if(type == "wellmeasurements") {
#'     gw_measure <- get_gw_waterlevels_wellmeasures(
#'                             wellid           = wellid,
#'                             start_date       = start_date,
#'                             end_date         = end_date,
#'                             api_key          = api_key
#'                             )
#'
#'     return(gw_measure)
#'   }
#'
#'   # if groundwater waterlevels well search
#'   if(type == "waterlevels") {
#'
#'     # waterlevels well search
#'     # gw_waterlevels <- get_groundwater_well_search(
#'     #                         search              = "waterlevels",
#'     #                         division            = division,
#'     #                         county              = county,
#'     #                         designated_basin    = designated_basin,
#'     #                         water_district      = water_district,
#'     #                         management_district = management_district,
#'     #                         wellid              = wellid,
#'     #                         api_key             = api_key
#'     #                       )
#'
#'     # waterlevels well search
#'     gw_waterlevels <- get_gw_waterlevels_wells(
#'                             division            = division,
#'                             county              = county,
#'                             designated_basin    = designated_basin,
#'                             water_district      = water_district,
#'                             management_district = management_district,
#'                             wellid              = wellid,
#'                             api_key             = api_key
#'                           )
#'
#'     return(gw_waterlevels)
#'   }
#'
#'   # if groundwater geophysicallogs well search
#'   if(type == "geophysicallogs") {
#'
#'     # geophysicallogs well search
#'     # gw_geophysicallogs <- get_groundwater_well_search(
#'     #                         search              = "geophysicallogs",
#'     #                         division            = division,
#'     #                         county              = county,
#'     #                         designated_basin    = designated_basin,
#'     #                         water_district      = water_district,
#'     #                         management_district = management_district,
#'     #                         wellid              = wellid,
#'     #                         api_key             = api_key
#'     #                       )
#'
#'     # geophysicallogs well search
#'     gw_geophysicallogs <-  get_gw_geophysicallogs_wells(
#'                               division            = division,
#'                               county              = county,
#'                               designated_basin    = designated_basin,
#'                               water_district      = water_district,
#'                               management_district = management_district,
#'                               wellid              = wellid,
#'                               api_key             = api_key
#'                             )
#'
#'     return(gw_geophysicallogs)
#'   }
#'
#' }
