#' Function that given a target variable, a partition and a task type returns a loss value
#'
#' @param partitionition partition
#' @param target target variable
#' @param task_type task type
#'
#' @examples
#'
#' @return list of partitionitions
#'
#' @export
#'


loss_function <- function(partition, target, task_type = "regr"){
  l <- 0
  
  if (task_type == "regr") {
    # measure is sum of squared residuals
    for (p in 1:length(partition)) {
      l <- l + sum((partition[[p]][,target] - mean(partition[[p]][,target])) ^2)
    }
  } else stop("loss function for this task type not jet implemented")
  
  return(l)
}