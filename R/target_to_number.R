#' Function that given the target and data transforms the target to the column number in the data, when given as string
#'
#' @param data data
#' @param target target 
#'
#' @examples
#'
#' @return target as numeric value
#'
#' @export
#'

target_to_number <- function(data, target){
  
  if (!is.numeric(target)) {
    c_names <- colnames(data[[1]])
    target <- which(c_names == target)
  }
  
  return(target)
}