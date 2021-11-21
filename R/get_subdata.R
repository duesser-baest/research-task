#' Function, that given data builds the next step
#'
#' @param data partitioned data
#' @param target target
#'
#' @examples
#'
#' @return list of partitions
#'
#' @export
#'

get_subdata <- function(data, target){
  
  split <- find_split(data, target)
  
  matrix <- mapply(partition, data, split)
  
  possible_data <- list()
  
  if (length(data) == 1) {
    next_data <- matrix[,1]
  } else {
    for (j in 1:length(data)) {
      possible_data[[j]] <- c(matrix[,j], data[-j])
    }
    
    losses <- sapply(possible_data, loss_function, target = target)
    
    min <- which.min(losses)
    
    next_data <- possible_data[[min]]
  }

  return(next_data)
}