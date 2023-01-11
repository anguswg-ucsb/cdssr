## code to prepare `api_endpoints` dataset goes here
#' @title List of CDSS API endpoints
#' @description Dataframe containing a list of all CDSS REST API endpoints
#' @importFrom rvest read_html html_nodes html_table
#' @export

# retrieve table describing CDSS API endpoints
api_endpoints <- browse_api()

usethis::use_data(api_endpoints, overwrite = TRUE)
