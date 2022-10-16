#' Request Diversion Records data for administrative structures
#' @description Make a request to CDSS API structures/divrec endpoints to get diversion/releases/stage/volume data for administrative structures. For more information go here https://dwr.state.co.us/rest/get/help#Datasets&#DiversionRecordsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0&#gettingstarted&#jsonxml
#' @param wdid character vector or list of characters indicating WDID code of structure. Only a single WDID can be used when type = "stagevolume".
#' @param wc_identifier character indicating whether "diversion" or "release" should be returned. A wc_identifier does not need to be given in stagevolume requests. Defaults to "diversion".
#' @param type character indicating the type of data to request. Either "day", "month", "year", or "stagevolume". Default is "day".
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom janitor clean_names
#' @return dataframe with diversion records data for CDSS structure of interest
#' @export
#' @examples
#' # Request endpoint: api/v2/structures/divrec/divrecday
#' divrecord_day <- get_structures_divrec(
#'                wdid             = "2302917",
#'                type             = "day",
#'                wc_identifier    = "diversion",
#'                start_date       = "2000-01-01",
#'                end_date         = "2005-01-01"
#'                )
#'
#' # plot daily diversion record
#' plot(divrecord_day$data_value~divrecord_day$datetime, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/divrecmonth
#' divrecord_month <- get_structures_divrec(
#'                    wdid             = "2302917",
#'                    type             = "month",
#'                    wc_identifier    = "diversion",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01"
#'                  )
#'
#' # plot monthly diversion record
#' plot(divrecord_month$data_value~divrecord_month$datetime, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/divrecyear
#' divrecord_year <- get_structures_divrec(
#'  wdid             = "2302917",
#'  type             = "year",
#'  wc_identifier    = "diversion",
#'  start_date       = "2000-01-01",
#'  end_date         = "2022-01-01"
#'  )
#'
#' # plot yearly diversion record
#' plot(divrecord_year$data_value~divrecord_year$data_meas_date, type = "s")
#'
#' # Request endpoint: api/v2/structures/divrec/stagevolume
#' stage_vol <- get_structures_divrec(
#'                    wdid             =  "0303732",
#'                    type             = "stagevolume",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01"
#'                  )
#'
#' # plot stage
#' plot(stage_vol$stage~stage_vol$datetime, type = "s")
#'
#' # plot volume
#' plot(stage_vol$volume~stage_vol$datetime, type = "s")
get_structures_divrec <- function(
    wdid            = NULL,
    wc_identifier   = "diversion",
    type            = "day",
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # check if type is NULL
  if(is.null(type)) {

    stop(paste0("Please enter correct diversion record type: 'day', 'month' or 'stagevolume'"))

  }

  # check if type is correctly inputed
  if(!type %in% c("day", "month", "year", "stagevolume")) {

    stop(paste0("Please enter correct diversion record type: 'day', 'month', 'year', or 'stagevolume'"))

  }

  # Retrieve daily divrec data
  if(type == "day") {

    # divrec day
    divrecord_day <- get_structure_divrecday(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrecord_day)
  }

  # Retrieve monthly divrec data
  if(type == "month") {

    # divrec month
    divrecord_month <- get_structure_divrecmonth(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrecord_month)
  }

  # Retrieve yearly divrec data
  if(type == "year") {

    # divrec year
    divrecord_year <- get_structure_divrecyear(
      wdid             = wdid,
      wc_identifier    = wc_identifier,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(divrecord_year)
  }

  # Retrieve stage/volume divrec data
  if(type == "stagevolume") {

    # if more than one WDID is provided, use the first WDID
    if(length(wdid) > 1) {

      wdid <- wdid[1]

    }

    # divrec stagevolume
    stage_vol <- get_structure_stage(
      wdid             = wdid,
      start_date       = start_date,
      end_date         = end_date,
      api_key          = api_key
    )

    return(stage_vol)
  }

}
