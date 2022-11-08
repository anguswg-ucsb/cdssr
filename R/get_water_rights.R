# get_water_rights_netamount <- function(
#     county              = NULL,
#     division            = NULL,
#     water_district      = NULL,
#     wdid                = NULL,
#     api_key             = NULL
# ) {
#   county              = "Adams"
#   division            = NULL
#   water_district      = NULL
#   wdid                = NULL
#   # wdid = "0105426"
#   api_key             = NULL
#   # check if valid wdid and admin_no were given
#   if(all(is.null(county), is.null(division), is.null(water_district), is.null(wdid))) {
#     stop(paste0("Please enter a valid 'county', division', 'water_district' or 'wdid' to retrieve water rights net amounts data"))
#   }
#
#   # base URL
#   base <- "https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/?"
#
#   # maximum records per page
#   page_size  <- 50000
#
#   # initialize empty dataframe to store data from multiple pages
#   data_df    <-  data.frame()
#
#   # initialize first page index
#   page_index <- 1
#
#   # Loop through pages until there are no more pages to get
#   more_pages <- TRUE
#
#   # print message
#   message(paste0("Retrieving water rights net amounts data from CDSS API..."))
#
#   # while more pages are avaliable, send get requests to CDSS API
#   while (more_pages) {
#
#     # Construct query URL w/o API key
#     url <- paste0(
#       base,
#       "format=json&dateFormat=spaceSepToSeconds",
#       "&county=", county,
#       "&division=", division,
#       "&waterDistrict=", water_district,
#       "&wdid=", wdid,
#       "&pageSize=", page_size,
#       "&pageIndex=", page_index
#     )
#
#     # check whether to use API key or not
#     if(!is.null(api_key)) {
#
#       # Construct query URL w/ API key
#       url <- paste0(url, "&apiKey=", api_key)
#
#     }
#
#     # GET request to CDSS API
#     tryCatch(
#       {
#         # query CDSS API
#         cdss_data <-
#           url %>%
#           httr::GET() %>%
#           httr::content(as = "text") %>%
#           jsonlite::fromJSON() %>%
#           dplyr::bind_rows() %>%
#           .$ResultList
#
#       },
#       error = function(e) {
#         message(paste0("Error in water rights net amounts data query"))
#         message(paste0("Perhaps the URL address is incorrect OR there are no data available."))
#         message(paste0("Query:\n----------------------------------",
#                        "\nCounty: ", county,
#                        "\nDivision: ", division,
#                        "\nWater district: ", water_district,
#                        "\nWDID: ", wdid
#         ))
#         message(paste0('\nHere is the URL address that was queried:\n'))
#         message(paste0(url))
#         message(paste0('And, here is the original error message:'))
#         message(paste0('-----------------------------------------'))
#         message(e)
#         stop()
#
#       }
#     )
#
#     # Tidy data
#     cdss_data <-
#       cdss_data %>%
#       janitor::clean_names()
#
#     # bind data from this page
#     data_df <- dplyr::bind_rows(data_df, cdss_data)
#
#     # Check if more pages to get to continue/stop while loop
#     if (nrow(cdss_data) < page_size) {
#
#       more_pages <- FALSE
#
#     } else {
#
#       page_index <- page_index + 1
#
#     }
#
#   }
#
#   # return final binded dataframe
#   return(data_df)
#
# }
#
#
#
# stream_area <-
#   data_df %>%
#   dplyr::filter(gnis_id == "00181805")
#
#
#
# #
# # Beneficial uses of water right: • 0 - STORAGE • 1 - IRRIGATION • 2 - MUNICIPAL • 3 - COMMERCIAL • 4- INDUSTRIAL • 5 - RECREATION • 6 - FISHERY • 7 - FIRE • 8 - DOMESTIC • 9 - STOCK • A - AUGMENTATION • B - SUB-BASIN EXPORT • C - CHANGE OF USE RETURN FLOW • E - EVAPORATIVE • F - FEDERAL RESERVED • G - GEOTHERMAL • H - HOUSEHOLD USE ONLY • K - SNOW MAKING • M - MINIMUM STREAMFLOW • P - POWER GENERATION • Q - OTHER • R - RECHARGE • S - EXPORT FROM STATE • T - TRANSMOUNTAIN EXPORT • W - WILDLIFE • X - ALL BENEFICIAL USES
#
# tmp <-
# # stream_area %>%
#   data_df %>%
#   dplyr::filter(grepl("A", decreed_uses))
#
# unique(tmp$appropriation_date)
#
# tmp %>%
#   dplyr::mutate(
#     appropriation_date = as.Date(appropriation_date)
#   ) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(x = appropriation_date, y = net_conditional)) +
#   ggplot2::facet_wrap(~structure_type)
#
# # plot(tmp$appropriation_date~)
# tmp %>%
#   dplyr::group_by(decreed_uses, )
#







