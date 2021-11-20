#' Function that given a dataset, a variable and a value partitions the dataset at specified cut point
#'
#' @param data data
#' @param variable variable
#' @param value value
#'
#' @examples
#' partition(mtcars, 1, 20)
#'
#' @return list of partitions
#'
#' @export
#'

partition <- function(data, variable, value){
  p1 <- data[data[,variable] < value,]
  p2 <- data[data[,variable] >= value,]
  
  part <- list(p1, p2)
  
  return(part)
}