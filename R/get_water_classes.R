utils::globalVariables(c("."))
#' Returns list of waterclasses
#' @description Make a request to the /structures/divrec/waterclasses endpoint to identify water classes via a spatial search or by division, county, water_district, GNIS, or WDID.
#' @param wdid character vector or list of characters indicating WDID code of structure
#' @param county character, indicating the county to query
#' @param division numeric, indicating the water division to query
#' @param water_district numeric, indicating the water district to query
#' @param wc_identifier character, series of water class codes that provide the location of the diversion, the SOURCE of water, the USE of the water and the administrative operation required to make the diversion. The Water Class, combined with a daily, monthly or annual volume, constitutes a Diversion Record.
#' @param aoi list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry
#' @param radius numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.
#' @param gnis_id character, water source - Geographic Name Information System ID
#' @param start_date character date of first measurement in the well’s period of record (YYYY-MM-DD). Default is NULL.
#' @param end_date character date of last measurement in the well’s period of record (YYYY-MM-DD). Default is NULL.
#' @param divrectype character, type of record: "DivComment", "DivTotal", "RelComment", "RelTolal", "StageVolume", or "WaterClass".
#' @param ciu_code character, current in use code of structure
#' @param timestep character, timestep, one of "day", "month", "year"
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS. Defaults to NULL.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of administrative structures
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: /structures/divrec/waterclasses
#' wc <- get_water_classes(
#'                    county     = "Boulder",
#'                    start_date = "1999-01-01",
#'                    end_date   = "2005-01-01",
#'                    timestep   = "year"
#'                    )
#' }
get_water_classes <- function(
    wdid                = NULL,
    county              = NULL,
    division            = NULL,
    water_district      = NULL,
    wc_identifier       = NULL,
    aoi                 = NULL,
    radius              = NULL,
    gnis_id             = NULL,
    start_date          = NULL,
    end_date            = NULL,
    divrectype          = NULL,
    ciu_code            = NULL,
    timestep            = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "start_date", "end_date", "aoi", "radius",
                "ciu_code", "divrectype", "gnis_id", "timestep"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # validate divrectype argument
  divrectype <- valid_divrectype(divrectype)

  # validate timestep argument
  timestep   <- valid_timesteps(timestep)

  # Base API URL
  base       <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/waterclasses/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   ignore  = c("aoi"),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  division        <- null_convert(division)
  gnis_id         <- null_convert(gnis_id)
  water_district  <- null_convert(water_district)

  # if wc_identifier is given, add * to indicate "contains" this string
  if(!is.null(wc_identifier)) {

    wc_id <- align_wcid(wc_identifier)

  }

  # if start_date is NULL, return NULL
  if(is.null(start_date)) {

    start <- NULL

  } else {

    # reformat and extract valid start date
    start <- parse_date(
      date        = start_date,
      start       = TRUE,
      format      = "%m-%d-%Y",
      sep         = "%2F"
    )

  }

  # if end_date is NULL, return NULL
  if(is.null(end_date)) {

    end <- NULL

  } else {

    # reformat and extract valid end date
    end <- parse_date(
      date        = end_date,
      start       = FALSE,
      format      = "%m-%d-%Y",
      sep         = "%2F"
    )

  }

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
  message(paste0("Retrieving structure water classes"))

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
      "timestep=", timestep,
      "&format=json&dateFormat=spaceSepToSeconds",
      "&ciuCode=", ciu_code,
      "&county=", county,
      "&division=", division,
      "&divrectype=", divrectype,
      "&min-porEnd=", end,
      "&min-porStart=", start,
      "&gnisId=", gnis_id,
      "&waterDistrict=", water_district,
      "&wcIdentifier=", wc_id,
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

#' Validate correct timestep argument for get_water_classes function
#' @description Internal function for checking timestep arguments for get_water_classes()
#' @param timestep character, timestep indicating, daily, monthly, or annual timestep. Default is NULL and will return "day"
#' @noRd
#' @keywords internal
#' @return character, "day", "month", or "year"
valid_timesteps <- function(
    timestep = NULL
    ) {

  # list of valid timescales
  day_lst       <- c("day", "days", "daily", "d")
  month_lst     <- c("month", "months", "monthly", "mon", "mons", "m")
  year_lst      <- c('year', 'years', 'yearly', 'annual', 'annually', 'yr', 'y')

  timestep_lst <- c(day_lst, month_lst, year_lst)

  # check if type is NULL, default timescale to "day"
  if(is.null(timestep)) {

    # set timescale to "day"
    timestep <- "day"

  }

  # convert timescale to lowercase
  timestep <- tolower(timestep)

  # check if type is correctly inputed
  if(!timestep %in% timestep_lst) {

    stop(paste0("Invalid `timestep` argument: '", timestep, "'",
                "\nPlease enter one of the following valid timesteps:",
                "\nDay: ", paste0("'", c(day_lst), "'", collapse = ", "),
                "\nMonth: ", paste0("'", c(month_lst), "'", collapse = ", "),
                "\nYear: ", paste0("'", c(year_lst), "'", collapse = ", ")
    )
    )
  }

  # check if given timestep is in day_lst and set timestep to "day"
  if(timestep %in% day_lst) {

    # set timescale to "day"
    timestep <- "day"

  }

  # check if given timestep is in month_lst and set timestep to "month"
  if(timestep %in% month_lst) {

    # set timescale to "month"
    timestep <- "month"

  }

  # check if given timestep is in year_lst and set timestep to "year"
  if(timestep %in% year_lst) {

    # set timescale to "month"
    timestep <- "year"

  }

  return(timestep)


}

#' Validate correct divrectype argument for get_water_classes function
#' @description Internal function for checking divrectype arguments for get_water_classes()
#' @param divrectype character, divrectype
#' @noRd
#' @keywords internal
#' @return character, "DivComment", "DivTotal", "RelComment", "RelTolal", "StageVolume", "WaterClass"
valid_divrectype <- function(
    divrectype = NULL
) {

  # check if type is NULL, default timescale to "day"
  if(is.null(divrectype)) {

    # set timescale to "day"
    divrectype <- NULL

    return(divrectype)
  }

  # list of available divrectypes
  divrectype_lst <- c("DivComment", "DivTotal", "RelComment", "RelTolal", "StageVolume", "WaterClass")

  # if a divrectype argument is provided (not NULL)
  if(!is.null(divrectype)) {

    # if divrectype matches any of the list, return correctly formatted divrectype
    if(any(tolower(divrectype_lst) %in% tolower(divrectype))) {

      divrectype <- divrectype_lst[tolower(divrectype_lst) %in% tolower(divrectype)]

    }

    # check if divrectype is a valid divrectype
    if(!divrectype %in% divrectype_lst | !tolower(divrectype) %in% tolower(divrectype_lst)) {

      stop(paste0("Invalid `divrectype` argument: '", divrectype, "'",
                  "\nPlease enter one of the following valid 'divrectype' arguments:",
                  "\n", paste0("'", c(divrectype_lst), "'", collapse = "\n")
      ))

    }

  }

  return(divrectype)

}

