#' Function that given a dataset transforms it into a list, unless it already is a list
#'
#' @param data data
#'
#' @examples
#'
#' @return data storred in a list
#'
#' @export
#'

list_data <- function(data){
  if (is.data.frame(data)) {
    data <- list(data)
  }
  
  return(data)
}