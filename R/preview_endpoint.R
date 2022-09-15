#' Retrieve descriptions of expected return fields for an API endpoint
#' @description Returns the names, descriptions, and types for each response field for a given API endpoint
#' @param endpoint_url character. URL to CDSS API REST Help page detailing the return fields for each endpoint. This URL can be found in the dataframe returned by the browse_api function
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_elements html_table
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' @return dataframe with the field name, a description, and the data type
#' @export
preview_endpoint <- function(endpoint_url = NULL) {

  # stop if no URL is given
  if(is.null(endpoint_url)) {

    stop(paste0("\nInvalid URL. Enter a URL of the following structure:\nhttps://dwr.state.co.us/Rest/GET/Help/Api/GET-<insert-name-of-api-resource>\n\nEndpoint URLs can be found by using 'browse_api()'"))

  }

  # read HTML from API Parameter Help page
  field_page <- rvest::read_html(endpoint_url)

  # extract help table detailing endpoint parameters
  field_tbl <-
    field_page %>%
    rvest::html_nodes("table") %>%
    rvest::html_elements(xpath = "//*[@class = 'help-page-table']") %>%
    rvest::html_table() %>%
    dplyr::bind_rows() %>%
    stats::setNames(c(tolower(names(.))))

  return(field_tbl)
}
