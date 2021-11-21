library(R6)

Node <- R6Class(
  classname = "Node",
  public = list(
    data = NULL,
    target = NULL,
    current_loss = NULL,
    possible_splits = NULL,
    subdata = NULL,
    resulting_split = NULL,
    min_leaf_size = NULL,
    initialize = function(data = NA, target = NA, min_leaf_size = 5){
      self$data <- list_data(data)
      self$target <- target_to_number(self$data, target)
      self$current_loss <- loss_function(self$data, self$target)
      self$possible_splits <- find_split(self$data, self$target)
      self$subdata <- get_subdata(self$data, self$target)
      self$min_leaf_size <- min_leaf_size
      self$greet()
    },
    greet = function(){
      cat(paste0("Node initialized"))
    }
  )
)

n <- Node$new(data = mtcars, target = "mpg")
n
n$possible_splits
n$data
n$subdata

n_1 <- Node$new(n$subdata, target = "mpg")
n_1$subdata


length(n$possible_splits)
