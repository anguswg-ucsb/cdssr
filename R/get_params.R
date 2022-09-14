#' Retrieve parameter definitions for CDSS API Endpoints
#' @description Returns the names, descriptions, and types for each parameter in a given API endpoint
#' @param param_url character. URL to CDSS API REST Help page detailing the parameter fields for each endpoint. This URL can be found in the dataframe returned by the browse_api function
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_elements html_table
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' @return dataframe with the parameter name for querying, a description, and the parameter type
#' @export
get_params <- function(param_url = NULL) {

  # stop if no URL is given
  if(is.null(param_url)) {

    stop(paste0("\nInvalid URL. Enter a URL of the following structure:\nhttps://dwr.state.co.us/Rest/GET/Help/Api/GET-<insert-name-of-api-resource>\n\nParameter URLs can be found by using 'browse_api()'"))

  }

  # read HTML from API Parameter Help page
  param_page <- rvest::read_html(param_url)

  # extract help table detailing endpoint parameters
  param_tbl <-
    param_page %>%
    rvest::html_nodes("table") %>%
    rvest::html_elements(xpath = "//*[@class = 'help-page-table']") %>%
    # .[[1]] %>%
    rvest::html_table() %>%
    dplyr::bind_rows() %>%
    stats::setNames(c(tolower(names(.))))

  return(param_tbl)
}
