#' Function that given data and target variable returns frist optimal split point
#' 
#' @param data data
#' @param target target variable
#'
#' @examples
#'
#' @return split point
#'
#' @export
#' 
#' @import purrr



find_split <- function(data, target){
  
  find_split_df <- function(data, target){
    # create split/lost lists for given data
    sl <- sl_lists(data, target)
    
    # finds highest improvement in loss measues
    min_value <- purrr::reduce(sl[[2]], min)
    
    # finds variable
    var <- which(sapply(sl[[2]], function(x) min_value %in% x))
    
    # in case multiple splits yield same loss value
    if (length(var) > 1) {
      var <- var[1]
    }
    
    name <- names(var)
    
    # finds index 
    idx <- which.min(sl[[2]][[var]])
    
    # finds splitting value
    sv <- sl[[1]][[var]][idx]
    
    split <- list(name, sv)
    return(split)
  }
  
  splits <- lapply(data, find_split_df, target = target)
  
  return(splits)
}