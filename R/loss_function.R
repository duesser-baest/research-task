#' Function that given a target variable, a data and a task type returns a loss value
#'
#' @param data data
#' @param target target variable
#' @param task_type task type
#'
#' @examples
#'
#' @return list of data
#'
#' @export
#'


loss_function <- function(data, target, task_type = "regr"){
  l <- 0
  
  if (task_type == "regr") {
    # measure is sum of squared residuals
    for (p in 1:length(data)) {
      l <- l + sum((data[[p]][,target] - mean(data[[p]][,target])) ^2)
    }
  } else stop("loss function for this task type not jet implemented")
  
  return(l)
}
