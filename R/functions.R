##### loss functions ####
#' Calculates the residual sum of squares
#'
#' @param y dependent variable vector
#'
#' @return loss value
#'
#' @export

loss_RSS <- function(y){
  sum((y - mean(y)) ** 2)
}

#### loss function given name and value pair ####
#' Calculates loss of a split given name-value pair
#'
#' @param measure performance measure
#' @param node node
#' @param name variable name
#' @param value value
#'
#' @return loss value
#'
#' @export

loss_nv <- function(measure, node, name, value){
  
  if (measure == "RMSE") {
    loss <- loss_RSS(node$y[node$X[name] < value]) +
      loss_RSS(node$y[node$X[name] > value])
  } else {stop("Measure not supported in loss_nv")}
  
  return(loss)
}

##### helper function for calc_model_loss #####
#' Creates df to store loss values
#'
#' @param node node
#'
#' @return dataframe
#'
#' @export


loss_helper <- function(node){
  if(node$leaf){
    # TODO delete this. for debug only
    if (length(node$y) == 0) {
      print(node)
      stop("a node should never have an empty y-list")
    }
    
    df <- data.frame(y_hat = mean(node$y), y = node$y)
  } else {df <- data.frame()}
  
  return(df)
}

##### calculates model loss given measure #####
#' Calvulates RMSE loss of model
#'
#' @param model model
#' @param measure measure
#'
#' @return loss 
#' 
#' @importFrom dplyr bind_rows
#'
#' @export

calc_model_loss <- function(model, measure){
  if (measure == "RMSE") {
    loss_df <- dplyr::bind_rows(lapply(model, loss_helper))
    
    loss <- sqrt(mean((loss_df$y - loss_df$y_hat) **2))
  } else stop("measure not supported in calc_model_loss")
  
  return(loss)
}

##### initializes an empty node ####
#' Initializes new node for model
#'
#' @param X independent data
#' @param y dependent variable
#' @param name name stores heritage
#' @param generation generation (kept to track max_tree_depth)
#' @param parent parent
#' @param parent_split parent split
#' @param loss loss of node
#'
#' @return node
#'
#' @export

init_node <- function(X, y, name, generation, parent, parent_split, loss){
  node <- list(
    X = X,
    y = y,
    name = name, # this will store a bit string (either 1 or 2) of integers of length "generation", root name = 0
    generation = generation, # this will store an integer
    parent = parent, # this will store the name of parent
    parent_split = parent_split, # can eather be 1 or 2; root = 0
    best_split = NULL, # split is a list of two (variable, value)
    loss = loss,
    is_terminated = FALSE,
    leaf = FALSE
  )
}

##### split loss data frame #####
#' Creates a data frame storing name split and loss in a node
#'
#' @param node node
#' @param min_leaf_size minimal allowed leaf size
#' @param measure measure
#' @param restr_la_var whether or not the dataframe should be restricted to carry only one split per variable
#'
#' @return df with name, value and loss
#'
#' @importFrom dplyr arrange
#'
#' @export

sl_df <- function(node, min_leaf_size, measure, restr_la_var){
  # check if splits are eligable according to min leaf size restriction
  if (nrow(node$X) < (2 * min_leaf_size)) {
    return(data.frame())
  }
  
  # sort values in variables
  srt <- apply(node$X, 2, sort)
  
  # lower and upper boundary due to min leaf size restriction
  lb <- min_leaf_size
  ub <- nrow(node$X) - min_leaf_size + 1
  
  # reduce to applicable splits
  srt_red <- srt[c(lb:ub),]
  
  # get only unique values
  suv <- apply(srt_red, 2, unique)
  
  obtain_splits <- function(vector){ # find all splitting points
    spv <- sapply(seq_along(vector), function(x) mean(vector[c(x, x + 1)]))[-length(vector)]
    
    ifelse(length(spv == 0), return(spv), return(NA))
  }
  
  split_list <- lapply(suv, obtain_splits) # get a list with all possible splitting points
  
  split_list <- split_list[!is.na(split_list)] # remove all list elements with NA
  
  if (length(split_list) == 0) { # if there are no possible splits, return node as terminated and as leaf
    return(data.frame())
  }
  
  name <- unlist(sapply(1:length(split_list), function(i) rep(names(split_list[i]), times = length(split_list[[i]]))))
  
  # in case all names that are applicable occur in the same frequency, name will be a matrix. the curious reader can note that finding this error took almost a full day and occurred only after seemingly being ready to benchmark the learner in mlr3 and not, as one would hope when writing this (one of the first) functions. in case anyone but myself ever reads this - i appreciate that you take such an in depth look into my project :)
  name <- as.vector(name)
  
  value <- unlist(split_list, use.names = FALSE)
  
  loss <- unlist(lapply(1:length(value), function(i) loss_nv(measure, node, name[i], value[i])))
  
  splits <- data.frame(name = name, value = value, loss = loss)
  
  splits_ordered <- switch (measure,
                            RMSE = dplyr::arrange(splits, loss)
                            # if measure maximizes, dplyr::arrange(df, desc(loss))
  )
  
  # if only splits should be considered that are from diferent variables, the splits get subset.
  if (isTRUE(restr_la_var)) {
    splits_ordered <- splits_ordered[!duplicated(splits_ordered$name),]
  }
  
  return(splits_ordered)
}

##### find children #####
#' Creates offspring and updates parent
#'
#' @param node node
#' @param split applied split
#' @param measure measure
#'
#' @return list with updated parent and both children
#'
#' @export

find_children <- function(node, split, measure){
  
  if (nrow(split) == 0) {
    return(list(node))
  } else if (nrow(split) > 1) {
    stop("too many observations in split")
  }
  
  name <- split$name[1]
  
  sv <- split$value[1]
  
  # adjust parent
  node$best_split <- as.list(split[1, 1:2])
  
  # returns original split( altered "terminated") as well as both child splits, if applicable
  node$is_terminated <- TRUE
  
  # initialize children
  lc_name <- paste0(node$name, "1")
  rc_name <- paste0(node$name, "2")
  
  lc_X <- node$X[node$X[name] < sv,]
  rc_X <- node$X[node$X[name] > sv,]
  
  lc_y <- node$y[node$X[name] < sv]
  rc_y <- node$y[node$X[name] > sv]
  
  c_generation <- node$generation + 1
  
  parent <- node$name
  
  
  if (measure == "RMSE") {
    lc_loss <- loss_RSS(lc_y)
    rc_loss <- loss_RSS(rc_y)
  }
  
  left_child <- init_node(X = lc_X,
                          y = lc_y,
                          name = lc_name,
                          generation = c_generation,
                          parent = parent,
                          parent_split = as.list(split[1, 1:2]),
                          loss = lc_loss)
  
  right_child <- init_node(X = rc_X,
                           y = rc_y,
                           name = rc_name,
                           generation = c_generation,
                           parent = parent,
                           parent_split = as.list(split[1, 1:2]),
                           loss = rc_loss)
  
  
  
  return(list(node, left_child, right_child))
}

##### terminate at maximal tree depth #####
#' Terminated a node, if maximal tree depth has been reached
#'
#' @param node node
#' @param max_tree_depth maximal tree depth
#'
#' @return terminated node that is now a leaf
#'
#' @export

terminate_mtd <- function(node, max_tree_depth){
  if (node$generation == max_tree_depth) {
    node$is_terminated <- TRUE
    node$leaf <- TRUE
  }
  
  return(node)
}

##### terminates a tree now by terminating a node and making it a leaf #####
#' Terminates a node and makes it a leaf without condition
#'
#' @param node node
#'
#' @return terminated node
#'
#' @export

terminate_now <- function(node){
  if (!node$is_terminated) {
    node$is_terminated <- TRUE
    node$leaf <- TRUE
  }
  
  return(node)
}

##### translates decimal to ids of length look_ahead with range 1:consider #####
#' Translates decimal to ids of length look_ahead with range ! trough consider
#'
#' @param consider considered amount of splits
#' @param look_ahead amount of future splits to be tried
#'
#' @return ids
#'
#' @export

id_creator <- function(consider, look_ahead){
  
  if (look_ahead == 0) {
    stop("id_creator called with look_ahead = 0")
  }
  
  id_helper_rep <- function(counter, digits, range){
    rep(1:range, times = range ** (counter - 1), each = range ** (digits - counter))
  }
  
  id_helper_combine <- function(index, id_matrix){
    paste(id_matrix[index,], collapse = "")
  }
  
  id_matrix <- sapply(1:look_ahead, id_helper_rep, digits = look_ahead, range = consider)
  
  #if consider == 1, no matrix but vector is returned, this statement fixes this apropriatly
  if (!is.matrix(id_matrix)) { 
    id_matrix <- matrix(data = id_matrix, nrow = (consider ** look_ahead))
  }
  
  ids <- sapply(1:(consider ** look_ahead), id_helper_combine, id_matrix = id_matrix)
  
  return(ids)
}

##### build la model and return loss #####
#' Builds a look ahead model given a path obtained by the id_creator, trying the n-best split at any point and returns the loss of the corresponding model
#'
#' @param path path of splits to be exected
#' @param model main model 
#' @param max_tree_depth maximal tree depth
#' @param min_leaf_size minimal leaf size
#' @param measure measure
#' @param look_ahead depth of looking ahead
#' @param restr_la_var whether or not the dataframe should be restricted to carry only one split per variable
#'
#' @return model loss
#' 
#' @importFrom rlist list.append
#'
#' @export

build_la_model <- function(path, 
                           model, 
                           max_tree_depth,
                           min_leaf_size, 
                           measure, 
                           look_ahead,
                           restr_la_var){
  
  model_terminated <- FALSE
  
  la_counter <- 1
  
  ####begin while
  while (isFALSE(model_terminated || !(la_counter <= look_ahead))) {
    #terminate and make leafs to all nodes that have reached max_tree depth
    model <- lapply(model, terminate_mtd, max_tree_depth = max_tree_depth)
    
    # look at what nodes are not terminated yet
    term_info <- sapply(model, function(node) node$is_terminated)
    
    # break current loop and set model_terminated to true, if all nodes are terminated
    if (all(term_info)) {
      model_terminated <- TRUE
      break
    }
    
    # apply find_children to first non-terminated node
    
    # find first non-terminated node
    i <- which.min(term_info)
    
    # find best split
    splits <- sl_df(model[[i]], min_leaf_size, measure, restr_la_var)
    
    # check if any splits are applicable
    
    if (nrow(splits) == 0) { # if no splits are applicable, terminate current node and make it a leaf
      model[[i]]$is_terminated <- TRUE
      model[[i]]$leaf <- TRUE
    } else { # if splits are applicable continue splitting
      
      index <- as.integer(substr(path, la_counter, la_counter))
      
      # in case there are not this many possible splits, the model will terminate
      if (index > nrow(splits)) {
        model_terminated <- TRUE
        break
      }
      
      split <- splits[index,]
      
      res <- find_children(model[[i]], split, measure)
      
      # update parent node in model
      model[[i]] <- res[[1]]
      
      # if current parent is no leaf, add children to model
      if (!model[[i]]$leaf ) {
        model <- rlist::list.append(model, res[[2]], res[[3]])
      }
    }
    
    la_counter <- la_counter + 1
  }
  
  # make every unterminated node a leaf and terminate the whole model
  model <- lapply(model, terminate_now)
  
  loss <- calc_model_loss(model, measure = measure)
  
  return(loss)
}

##### get the best la split depending on look ahead and consider parameters #####
#' Gets the best split from all possible considered la-models
#'
#' @param model input model
#' @param consider considered splits in each level
#' @param look_ahead depth of looking ahead
#' @param max_tree_depth maximal tree depth
#' @param min_leaf_size minimal leaf size
#' @param measure measure
#' @param restr_la_var whether or not the dataframe should be restricted to carry only one split per variable
#'
#' @return split number
#'
#' @export

get_la_split <- function(model,
                         consider,
                         look_ahead,
                         max_tree_depth,
                         min_leaf_size,
                         measure,
                         restr_la_var){
  
  df <- data.frame(id = id_creator(consider = consider, 
                                   look_ahead = look_ahead), 
                   loss = -1)
  
  # for all ids: apply decision tree and save loss to df
  
  df$loss <- sapply(df$id, build_la_model,
                    model = model,
                    max_tree_depth = max_tree_depth,
                    min_leaf_size = min_leaf_size,
                    measure = measure,
                    look_ahead = look_ahead,
                    restr_la_var = restr_la_var)
  
  # check if any value was not changed. 
  if(any(df$loss == -1)) stop("build_la_tree did not work propperly")
  
  min <- which.min(df$loss)
  
  id <- df$id[min] 
  
  result <- as.integer(substr(id, 1, 1))
  
  if (result > consider) stop("resulting best split out of bounds")
  
  return(result)
}


##### create preprocessing list #####
#' Helper function used in both building models and predicting on them. The result is passed within the model. The output maps the order of factor input features depending on y to their internally used number
#'
#' @param X independent variables
#' @param y dependent variable vector
#'
#' @return preprocessing list
#' 
#' @importFrom dplyr arrange
#' @importFrom stats aggregate
#'
#' @export


create_pp_list <- function(X, y){
  a_means_list <- function(variable, target){
    if (is.numeric(variable)) {
      return(NULL)
    } 
    
    variable <- as.character(variable)
    means <- stats::aggregate(x = target, by = list(variable), FUN = mean)
    
    # variable name "x" is assigned by default, package issue is due to this unsure how to resolve it
    a_means <- dplyr::arrange(means, x)
    
    return(a_means)
  }
  
  pp <- lapply(X, a_means_list, target = y)
  
  return(pp)
}

##### make X numeric #####
#' Makes all non numeric variables numeric for internal use
#'
#' @param X independent variables
#' @param pp preprocessing list that maps from factor to numeric values
#'
#' @return data frame containing only numeric variables
#'
#' @export

make_X_numeric <- function(X, pp){
  
  make_var_numeric <- function(variable, pp){
    if (is.numeric(variable)) {
      return(variable)
    }
    
    variable <- as.character(variable)
    variable <- sapply(variable, function(obs) which(pp == obs))
    
    return(variable)
  }
  
  numeric_list <- lapply(seq_along(X), function(j) make_var_numeric(X[[j]], pp[[j]]))
  
  numeric_df <- as.data.frame(numeric_list)
  
  colnames(numeric_df) <- colnames(X)
  
  return(numeric_df)
}

##### main function #####
#' Function that calculates the residual sum of squares
#'
#' @param data data
#' @param target target variable, eather numeric or as string
#' @param task_type task type
#' @param look_ahead hyperparameter how far to look into future splits
#' @param consider hyperparameter how many splits to consider in each future split
#' @param restr_la_var whether or not the dataframe should be restricted to carry only one split per variable
#' @param cap_la sets the maximum depth of the tree when look_ahead should still be applied
#' @param max_tree_depth hyperparameter to choose the maximal depth of the tree
#' @param min_leaf_size hyperparameter to choose the minimal size of a leaf allowed
#' @param measure measure
#' @param prune_parameter hyperparameter that is used for pruning the model

#'
#' @return model containing the used time, decission tree, preprocessing list and RMSE loss
#' 
#' @importFrom rlist list.append
#'
#' @export

la_tree <- function(data, 
                    target, 
                    task_type = "regr", 
                    look_ahead = 2, 
                    consider = 5, 
                    restr_la_var = TRUE,
                    cap_la = 3, 
                    max_tree_depth = 6,
                    min_leaf_size = 5, 
                    measure = "RMSE",
                    prune_parameter = NULL){
  
  start_time <- Sys.time()
  
  if (task_type != "regr") {
    stop(paste0("task type ", task_type, " not supported"))
  }
  
  if (!is.numeric(look_ahead)) {
    stop(paste0("look ahead ", look_ahead, " not supported as it is not numeric"))
  } else {as.integer(look_ahead)}
  
  if (!(look_ahead >= 1)) {
    stop("look_ahead must have a value of at least 1")
  }
  
  if (!is.numeric(consider)) {
    stop(paste0("look ahead strategy", consider, " not supported as it is not numeric"))
  } else {as.integer(consider)}
  
  if (!(consider <= 9 && consider > 0)) {
    stop("consider value must be between 1 and 9")
  }
  
  if (consider == 1) { # if only the best split will be considered greedy splitting is applied anyways so look_ahead can be set to 1
    look_ahead <- 1
  }
  
  if (measure != "RMSE") {
    stop(paste0("measure ", measure, "not supported"))
  }
  
  if (!is.null(prune_parameter)) {
    stop(paste0("prune parameter", prune_parameter, "not supported"))
  }
  
  # split data into X and y
  if (!is.data.frame(data)) {
    class <- class(data)
    stop(paste0("object of class -", class, "- is not supported as data input"))
  }
  
  if (!is.logical(restr_la_var)) {
    stop(paste0(restr_la_var, " must be boolean"))
  }
  
  data <- as.data.frame(data) # make sure data is of type base::data.frame
  
  if (!is.numeric(target)) {
    if (!(target %in% colnames(data))) {
      stop("target not found")
    }
    
    target <- which(colnames(data) == target)
  }
  
  if (!(target %in% 1:ncol(data))) {
    stop("target not found")
  }
  
  X <- data[,-target]
  y <- data[,target]
  
  # creates a preprocessing list that stores the information how to transform the data that will be used in the predict function also
  ppl <- create_pp_list(X,y)
  
  X <- make_X_numeric(X, ppl)
  
  if (measure == "RMSE") {
    loss <- loss_RSS(y)
  }
  
  # initialize root node
  root <- init_node(X = X, 
                    y = y, 
                    name = "0",
                    generation = 0,
                    parent = "0",
                    parent_split = "0",
                    loss = loss)
  
  # initialize model - storage of all nodes
  model <- vector("list")
  model <- rlist::list.append(model, root)
  
  model_terminated <- FALSE
  
  #build tree
  while (isFALSE(model_terminated)) {
    
    #terminate and make leafs to all nodes that have reached max_tree depth
    model <- lapply(model, terminate_mtd, max_tree_depth = max_tree_depth)
    
    # look at what nodes are not terminated yet
    term_info <- sapply(model, function(node) node$is_terminated)
    
    # break current loop and set model_terminated to true, if all nodes are terminated
    if (all(term_info)) {
      model_terminated <- TRUE
      break
    }
    
    # apply find_children to first non-terminated node
    
    # find first non-terminated node
    i <- which.min(term_info)
    
    splits <- sl_df(model[[i]], min_leaf_size, measure, restr_la_var)
    
    # check if any splits are applicable
    if (nrow(splits) == 0) { # if no splits are applicable, terminate current node and make it a leaf
      model[[i]]$is_terminated <- TRUE
      model[[i]]$leaf <- TRUE
    } else { # if splits are applicable continue splitting
      
      # if no look_ahead is used (look_ahead == 1), greedy splitting is applied
      # if cap_la is reached, no look ahead splits are preformed after this point and only greedy splits are aplied from this depth on
      if (look_ahead == 1 || model[[i]]$generation > cap_la) { 
        index <- 1 
      } else {
        # in order to propperly look ahead a strict subtree from the current node on must be considered. previous implementations had the error of having model = model in get_la_split which is nonsensical as for deeper trees there might no longer be a dependency between the initial and la splits
        la_model <- vector("list")
        la_model <- rlist::list.append(la_model, model[[i]])
        index <- get_la_split(model = la_model,
                              consider = consider,
                              look_ahead = look_ahead,
                              max_tree_depth = max_tree_depth,
                              min_leaf_size = min_leaf_size,
                              measure = measure,
                              restr_la_var = restr_la_var)
      }
      
      # find split corresponding to chosen index
      split <- splits[index,]
      
      # apply split
      res <- find_children(model[[i]], split, measure)
      
      # update parent node in model
      model[[i]] <- res[[1]]
      
      # if current parent is no leaf, add children to model
      if (!model[[i]]$leaf ) {
        model <- rlist::list.append(model, res[[2]], res[[3]])
      }
    }
  }
  
  # calculate loss
  loss <- calc_model_loss(model = model, 
                          measure = measure)
  
  end_time <- Sys.time()
  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  return_list <- list(
    target = colnames(data)[target],
    loss = loss,
    time_elapsed = time_elapsed,
    preprocessing_list = ppl,
    model = model
  )
  
  return(return_list)
}

##### predicts individual observations #####
#' Predicts individual observatons gets an id vector obtained from the node names in the model. This can be obtained by running (id_vec <- sapply(model, function(node) node$name)). This is needed to travel trough the tree
#'
#' @param observation dataframe containing all X values of one observation
#' @param model model
#' @param id_vec vector of node id's
#'
#' @return estimate of dependen variable
#'
#' @export

predict_obs <- function(observation, model, id_vec){
  ## helper get model id'
  id <- "0"
  
  # termination condiiton for while loop
  reached_leaf <- FALSE
  
  # travel down decision tree until reaching a leaf
  while (isFALSE(reached_leaf)) {
    # get index of next node
    idx <- which(id_vec == id)
    
    # when a leaf is reached, store y_hat and set reached_leaf to true ending while
    if (model[[idx]]$leaf) {
      y_hat <- mean(model[[idx]]$y)
      reached_leaf <- TRUE
      break
    }
    
    # find spliting point
    var <- model[[idx]]$best_split$name
    val <- model[[idx]]$best_split$value
    
    # see which is the next node to travel to
    if (observation[,var] < val) {
      child <- 1
    } else child <- 2
    
    # updates the next leaf to go to
    id <- paste0(id, child)
  }
  
  return(y_hat)
}

##### predicts new observations #####
#' Predicts the dependent variable based on model obtained from la_tree on new X data
#'
#' @param newdata eather whole data or only independent variables
#' @param model_list model and preprocessing list from la_tree
#'
#' @return vector ob predictions
#'
#' @export

predict_la_tree <- function(newdata, model_list){
  
  if (!is.data.frame(newdata)) {
    class <- class(newdata)
    stop(paste0("object of class -", class, "- is not supported in predict as newdata"))
  }
  
  newdata <- as.data.frame(newdata) # make sure data is of type base::data.frame
  
  target <- model_list$target
  
  # check if the data still includes the target variable and throw it out if yes
  if (target %in% colnames(newdata)) {
    target <- which(colnames(newdata) == target)
    
    X <- newdata[,-target]
  } else {
    X <- newdata
  }
  # unpack output
  model <- model_list$model
  ppl <- model_list$preprocessing_list
  
  # creates the data sturcture that the model was trained on
  X <- make_X_numeric(X, ppl)
  
  # vector storing the names of each node
  id_vec <- sapply(model, function(node) node$name)
  
  # predicts all observations
  y_hat <- sapply(1:nrow(X), function(i) predict_obs(X[i,], model, id_vec))
  
  # this is necessary for mlr3 to be able to interpret the prediction vector.
  names(y_hat) <- 1:nrow(X)
  
  return(y_hat)
}
