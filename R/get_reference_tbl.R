utils::globalVariables(c("."))

#' Return county reference table
#' @description Make a request to the referencetables/county/ endpoint to retrieve a reference table of counties. The reference table functions can help provide information to use while querying other endpoints.
#' @param county character, (optional) indicating the county to query, if no county is given, entire county dataframe is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of Colorado counties
get_ref_county <- function(
    county              = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/county/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving county reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&county=", county,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return water districts reference table
#' @description Make a request to the referencetables/waterdistrict/ endpoint to retrieve a reference table of water districts The reference table functions can help provide information to use while querying other endpoints.
#' @param division character, (optional) indicating the division to query, if no division is given, dataframe of all water districts is returned
#' @param water_district character, (optional) indicating the water district to query, if no ater district is given, dataframe of all water districts is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of Colorado water districts, indicating the division the water district is in and its name
get_ref_waterdistricts <- function(
    division            = NULL,
    water_district      = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/waterdistrict/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving water districts reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&division=", division,
      "&waterDistrict=", water_district,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return water divisions reference table
#' @description Make a request to the referencetables/waterdivision/ endpoint to retrieve a reference table of water divisions. The reference table functions can help provide information to use while querying other endpoints.
#' @param division character, (optional) indicating the division to query, if no division is given, dataframe of all water divisions is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of Colorado water divisions and water division names
get_ref_waterdivisions <- function(
    division            = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/waterdivision/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving water division reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&division=", division,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return management districts reference table
#' @description Make a request to the referencetables/managementdistrict/ endpoint to retrieve a reference table of management districts. The reference table functions can help provide information to use while querying other endpoints.
#' @param management_district character, (optional) indicating the management district to query, if no management district is given, dataframe of all management districts is returned
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of Colorado management districts
get_ref_managementdistricts <- function(
    management_district  = NULL,
    api_key              = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/managementdistrict/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving management districts reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json&dateFormat=spaceSepToSeconds",
      "&managementDistrictName=", management_district,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return designated basin reference table
#' @description Make a request to the referencetables/designatedbasin/ endpoint to retrieve a reference table of designated basins. The reference table functions can help provide information to use while querying other endpoints.
#' @param designated_basin character, (optional) indicating the  designated basin to query character, if no designated basin is given, all designated basins dataframe is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of Colorado designated basins
get_ref_designatedbasins <- function(
    designated_basin    = NULL,
    api_key             = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/designatedbasin/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving designated basin reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json",
      "&designatedBasinName=", designated_basin,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return telemetry parameter reference table
#' @description Make a request to the referencetables/telemetryparams/ endpoint to retrieve a reference table of parameters available at telemetry stations. The reference table functions can help provide information to use while querying other endpoints.
#' @param param character, (optional) indicating the parameter to query character, if no parameter is given, all parameter dataframe is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of telemetry station parameters
get_ref_telemetry_params <- function(
    param      = NULL,
    api_key    = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/telemetryparams/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving telemetry parameter reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json",
      "&parameter=", param,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return climate station parameter reference table
#' @description Make a request to the referencetables/climatestationmeastype/ endpoint to retrieve a reference table of parameters available at climate stations. The reference table functions can help provide information to use while querying other endpoints.
#' @param param character, (optional) indicating the parameter to query character, if no parameter is given, all parameter dataframe is returned
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of climate station parameters
get_ref_climate_params <- function(
    param      = NULL,
    api_key    = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/climatestationmeastype/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving climate parameter reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json",
      "&measType=", param,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return Diversion Record Types reference table
#' @description Make a request to the referencetables/divrectypes/ endpoint to retrieve a reference table of diversion record types available. The reference table functions can help provide information to use while querying other endpoints.
#' @param divrectype character, (optional) diversion record type to query, if no divrectype is given, a dataframe with all diversion record types is returned.
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of diversion record types
get_ref_divrectypes <- function(
    divrectype  = NULL,
    api_key     = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/divrectypes/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving diversion record types reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json",
      "&divRecType=", divrectype,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return Station Flag reference table
#' @description Make a request to the referencetables/stationflags/ endpoint to retrieve a reference table of station flag descriptions. The reference table functions can help provide information to use while querying other endpoints.
#' @param flag character, (optional) short code for the flag to query, if no flag is given, a dataframe with all flags is returned.
#' @param api_key character, (optional). If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of station flags and flag descriptions
get_ref_stationflags<- function(
    flag     = NULL,
    api_key  = NULL
) {

  # list of function inputs
  input_args <- as.list(environment())

  # base API URL
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/referencetables/stationflags/?"

  # maximum records per page
  page_size  <- 50000

  # initialize empty dataframe to store data from multiple pages
  data_df    <-  data.frame()

  # initialize first page index
  page_index <- 1

  # Loop through pages until there are no more pages to get
  more_pages <- TRUE

  # print message
  message(paste0("Retrieving station flags reference table"))

  # while more pages are available, send get requests to CDSS API
  while (more_pages) {

    # Construct query URL w/o API key
    url <- paste0(
      base,
      "format=json",
      "&flag=", flag,
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

    # bind data from this page
    data_df <- rbind(data_df, cdss_data)

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

#' Return reference table information, which may help with other searches
#' @description Makes requests to the /referencetables/ endpoints and returns helpful reference tables. Reference tables can help identify valid inputs for querying CDSS API resources using cdssr. For more detailed information visit: https://dwr.state.co.us/rest/get/help#Datasets&#ReferenceTablesController&#gettingstarted&#jsonxml.
#' @param table_name character,name of the reference table to return. Must be one of: ("county", "waterdistricts", "waterdivisions", "designatedbasins", "managementdistricts", "telemetryparams", "climateparams", "divrectypes", "flags"). Defaults to NULL
#' @param api_key character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @return dataframe of CDSS endpoint Reference Table
#' @export
#' @examples
#' \dontrun{
#' # Retrieve station flag reference table
#' flag_tbl <- get_reference_tbl(
#'    table_name = "flags"
#'    )
#'
#' # Retrieve available telemetry station parameters
#' telemetry_params <- get_reference_tbl(
#'    table_name = "telemetryparams"
#'    )
#'
#' # Retrieve available climate station parameters
#' climate_params <- get_reference_tbl(
#'    table_name = "climateparams"
#'    )
#'
#' # Retrieve water districts
#' water_districts <- get_reference_tbl(
#'    table_name = "waterdistricts"
#'    )
#' }
get_reference_tbl <- function(
    table_name           = NULL,
    api_key              = NULL
) {

  # list of available tables
  tbl_lst <- c("county", "waterdistricts", "waterdivisions", "designatedbasins",
               "managementdistricts", "telemetryparams", "climateparams", "divrectypes", "flags")

  # check if table_name is a valid table name
  if(is.null(table_name)) {

    stop(paste0("Invalid `table_name` argument: '", table_name, "'",
                "\nPlease enter one of the following valid reference tables:",
                "\n", paste0("'", c(tbl_lst), "'", collapse = "\n")
    )
    )

  }

  # check if table_name is a valid table name
  if(!table_name %in% tbl_lst) {

    stop(paste0("Invalid `table_name` argument: '", table_name, "'",
                "\nPlease enter one of the following valid reference tables:",
                "\n", paste0("'", c(tbl_lst), "'", collapse = "\n")
                )
         )

  }

  # retrieve county reference table
  if(table_name == "county") {

    ref_table <-
      get_ref_county(
        api_key        = api_key
      )

  }

  # retrieve water districts reference table
  if(table_name == "waterdistricts") {

    ref_table <-
      get_ref_waterdistricts(
        api_key        = api_key
      )

  }

  # retrieve water divisions reference table
  if(table_name == "waterdivisions") {

    ref_table <-
      get_ref_waterdivisions(
        api_key        = api_key
      )

  }

  # retrieve management districts reference table
  if(table_name == "managementdistricts") {

    ref_table <-
      get_ref_managementdistricts(
        api_key        = api_key
      )

  }

  # retrieve designated basins reference table
  if(table_name == "designatedbasins") {

    ref_table <-
      get_ref_designatedbasins(
        api_key        = api_key
      )

  }

  # retrieve telemetry station parameters reference table
  if(table_name == "telemetryparams") {

    ref_table <-
      get_ref_telemetry_params(
        api_key        = api_key
      )

  }

  # retrieve climate station parameters reference table
  if(table_name == "climateparams") {

    ref_table <-
      get_ref_climate_params(
        api_key        = api_key
      )

  }

  # retrieve diversion record types reference table
  if(table_name == "divrectypes") {

    ref_table <-
      get_ref_divrectypes(
        api_key        = api_key
      )

  }

  # retrieve station flags reference table
  if(table_name == "flags") {

    ref_table <-
      get_ref_stationflags(
        api_key        = api_key
      )

  }

  # return desired reference table
  return(ref_table)


}
