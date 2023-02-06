utils::globalVariables(c("."))
#' Return individual Structure stage/volume Records
#' @description Internal function - Make a request to the api/v2/structures/divrec/stagevolume/ endpoint to retrieve structure stage/volume data for a specified WDID within a specified date range.
#' @param wdid character indicating WDID code of structure
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords internal
#' @return dataframe with stage/volume data for CDSS structure of interest
inner_structure_stage_ts <- function(
    wdid            = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("api_key", "start_date", "end_date"),
    f       = "all"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # Base API URL for Daily Diversion Records
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/stagevolume/?"

  # # convert arguments to strings if necessary
  # str_args(
  #   arg_lst = as.list(environment()),
  #   envir   = environment()
  # )

  # convert arguments to characters if necessary
  wdid    <- null_convert(wdid)

  # reformat and extract valid start date
  start <- parse_date(
    date   = start_date,
    start  = TRUE,
    format = "%m-%d-%Y",
    sep    = "%2F"
  )

  # reformat and extract valid end date
  end <- parse_date(
    date   = end_date,
    start  = FALSE,
    format = "%m-%d-%Y",
    sep    = "%2F"
  )

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <- data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # while more pages are avaliable, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&min-dataMeasDate=", start,
      "&max-dataMeasDate=", end,
      "&wdid=", wdid,
      "&pageSize=", page_size,
      "&pageIndex=", page_index
    )

    # check whether to use API key or not
    if(!is.null(api_key)) {

      # construct query URL w/ API key
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
    names(cdss_data)   <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(cdss_data))))

    # set datetime column
    cdss_data$datetime <- as.POSIXct(cdss_data$data_meas_date, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

    # bind data from this page
    data_df            <- rbind(data_df, cdss_data)

    # Check if more pages to get to continue/stop while loop
    if (nrow(cdss_data) < page_size) {

      more_pages <- FALSE

    } else {

      page_index <- page_index + 1

    }

  }

  return(data_df)

}

#' Return Structure stage/volume records
#' @description Make a request to the api/v2/structures/divrec/stagevolume/ endpoint to retrieve structure stage/volume data for WDID(s) within a specified date range.
#' @param wdid character vector or list of WDID codes
#' @param start_date character date to request data start point YYYY-MM-DD. Default is start date is "1900-01-01".
#' @param end_date character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.
#' @param api_key character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @return dataframe with stage/volume data for CDSS structure(s) of interest
#' @export
#' @examples
#' \dontrun{
#' # Request endpoint: api/v2/structures/divrec/stagevolume
#' stage_vol <- get_structure_stage_ts(
#'                    wdid             = "0303732",
#'                    start_date       = "2000-01-01",
#'                    end_date         = "2005-01-01"
#'                  )
#'
#' # plot stage
#' plot(stage_vol$stage~stage_vol$datetime, type = "s")
#'
#' # plot volume
#' plot(stage_vol$volume~stage_vol$datetime, type = "s")
#'
#' # get stage/volume data for multiple WDIDs
#' multi_wdids <- get_structure_stage_ts(
#'                wdid          = c("0504010", "0504020", "0504021"),
#'                start_date    = "2021-06-01",
#'                end_date      = "2021-08-01"
#'                )
#' }
get_structure_stage_ts <- function(
    wdid            = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = NULL
) {

  # print message
  message(paste0("Retrieving stage/volume data"))

  # if only one site given, set verbose to FALSE to avoid superfluous messages
  if(length(wdid) <= 1) {

    verbose = FALSE

  # if multiple sites given, set verbose to TRUE to clarify to user each query made
  } else {

    verbose = TRUE

  }

  # loop over WDIDs and call inner_structure_stage_ts function and bind results rows
  data_df <- lapply(1:length(wdid), function(i) {

    if(verbose == TRUE) {

      message(paste0("WDID: ", wdid[i]))

    }

    tryCatch({
      inner_structure_stage_ts(
        wdid            = wdid[i],
        start_date      = start_date,
        end_date        = end_date,
        api_key         = api_key
      )

    },
    error = function(e) {

      NULL

    })

  })

  # bind rows of dataframes
  data_df <- do.call(rbind, data_df)


  return(data_df)

}
