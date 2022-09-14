#' Locate all CDSS API Endpoints
#' @description Returns a dataframe with the API endpoints for CDSS REST services
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom dplyr bind_rows mutate select
#' @return dataframe with API endpoint names, URLs and descriptions of each CDSS resource
#' @export
browse_api <- function() {

  # base CDSS REST API URL
  base_url <- "https://dwr.state.co.us/Rest/"

  # URL to endpoints
  catalog_url <- "https://dwr.state.co.us/rest/get/help"

  # page with table of API endpoints
  page <- rvest::read_html(catalog_url)

  # extract endpoint tables
  tbl <-
    page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    .[c(3:17)] %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      endpoint = gsub("GET ", "", API),
      url      = paste0(base_url, "GET/", endpoint)
    ) %>%
    dplyr::select(resource = "Url Generator", description = "Description", endpoint, url) %>%
    dplyr::mutate(
      param_url =  paste0("https://dwr.state.co.us/Rest/GET/Help/Api/GET-", gsub("/", "-", endpoint))
    )

  return(tbl)

}











