#' Function that given a dataset and a target variable returns the task type
#'
#' @param data dataset
#' @param target target variable
#'
#' @examples
#'
#' @return task type
#'
#' @export
#'

task_type <- function(data, target){

  if (is.numeric(data[,target])) {
    return("regr")
  } else {
    return("classif")
  }
}