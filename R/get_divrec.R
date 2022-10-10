#' Request Diversion Records data
#' @description Make a request to CDSS API structures/divrec endpoints to get diversion records data
#' @param type character indicating the type of data to request. Either "divrecday", "divrecmonth", or "stagevolume". Default is "divrecday".
#' @param wdid character indicating WDID code of structure
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. Defaults to "diversion", not used in stagevolume requests.
#' @param start_date character date to request data start point YYYY-MM-DD.
#' @param end_date character date to request data end point YYYY-MM-DD. Default is set to the current date function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe with diversion records data for CDSS structure of interest
#' @export
#' @examples
#' # Request endpoint: api/v2/structures/divrec/divrecday
#' divrecord_day <- get_divrec(
#'                type             = "divrecday",
#'                wdid             = "2302917",
#'                wc_identifier    = "diversion",
#'                start_date       = "2000-01-01",
#'                end_date         = "2005-01-01"
#'                )
#'
#' # plot daily diversion record
#' plot(divrecord_day$value~divrecord_day$datetime, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/divrecmonth
#' divrecord_month <- get_divrec(
#'                    type             = "divrecmonth",
#'                    wdid             = "2302917",
#'                    wc_identifier    = "diversion",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01"
#'                  )
#'
#' # plot monthly diversion record
#' plot(divrecord_month$value~divrecord_month$date, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/stagevolume
#' stage_vol <- get_divrec(
#'                    type             = "stagevolume",
#'                    wdid             =  "0303732",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01"
#'                  )
#'
#' # plot stage
#' plot(stage_vol$stage~stage_vol$datetime, type = "s")
#'
#' # plot volume
#' plot(stage_vol$volume~stage_vol$datetime, type = "s")
get_divrec <- function(
    type            = "divrecday",
    wdid            = NULL,
    wc_identifier   = "diversion",
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # check if type is NULL
  if(is.null(type)) {

    stop(paste0("Please enter correct diversion record type: 'divrecday', 'divrecmonth' or 'stagevolume'"))

  }

  # check if type is correctly inputed
  if(!type %in% c("divrecday", "divrecmonth", "stagevolume")) {

    stop(paste0("Please enter correct diversion record type: 'divrecday', 'divrecmonth' or 'stagevolume'"))

  }

  # if groundwater well measurements desired
  if(type == "divrecday") {

    divrecord_day <- get_structure_divrecday(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrecord_day)
  }

  # if groundwater waterlevels well search
  if(type == "divrecmonth") {

    # waterlevels well search
    divrecord_month <- get_structure_divrecmonth(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrecord_month)
  }

  # if groundwater geophysicallogs well search
  if(type == "stagevolume") {

    # geophysicallogs well search
    stage_vol <- get_structure_stage(
      wdid             = wdid,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(stage_vol)
  }

}
