#' Function that given a dataset and the target variable returns all possible split points
#'
#' @param data data
#' @param target target variable
#'
#' @examples
#'
#' @return matrix of possible spliting points
#'
#' @export
#'
#' @import rlist

split_list <- function(data, target){
  
  data <- data[,-target]
  
  c_names <- colnames(data)
  
  list <- list()
  
  for (j in 1:ncol(data)) {
    
    if (is.numeric(data[,j])) {
      
      # sorted unique values
      suv <- sort(unique(data[,j]))
      
      # split points
      sp <- c(rep(0,(length(suv) -1)))
      
      for (i in 1:length(sp)) {
        sp[i] <- mean(suv[i:(i+1)])
      }
      
      list <- rlist::list.append(list, sp)
      
    } else stop("can only deal with numeric variables for now")
    
  }
  
  names(list) <- c_names
  
  
  list <- list[!is.na(list)]
  
  return(list)
}