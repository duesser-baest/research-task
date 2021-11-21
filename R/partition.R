#' Function that given a dataset, a variable and a value partitions the dataset at specified cut point
#'
#' @param data data
#' @param split list of variable and value
#'
#' @examples
#'
#' @return list of partitions
#'
#' @export
#'

partition <- function(data, split){
  
  variable <- split[[1]]
  value <- split[[2]]
  
  p1 <- data[data[,variable] < value,]
  p2 <- data[data[,variable] >= value,]
  
  part <- list(p1, p2)
  
  return(part)
}
