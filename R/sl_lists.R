#' Function that given a split_list returns the loss value to all corresponding splitting points
#' 
#' @param data data
#' @param target target variable
#'
#' @examples
#'
#' @return two lists, one containing the splitting points, one containing the loss
#'
#' @export
#' 

sl_lists <- function(data, target){
  
  s_l <- split_list(data, target)
  
  l_l <- s_l
  
  nam <- names(s_l)
  
  for (j in 1:length(s_l)) {
    for (i in 1:length(s_l[[j]])) {
      l_l[[j]][i] <- loss_function(target = target, data = partition(data = data, split = list(nam[j], s_l[[j]][i])))
    }
  }
  
  lists <- list(s_l, l_l)
  
  return(lists)
}