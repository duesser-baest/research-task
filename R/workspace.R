# ### TODO #####
# 
# # include factor variables
# # get main funciton into mlr3
# # pruning
# 
# ##### load libraries #####
# 
# rm(list = ls())
# 
# library(rlist)
# library(dplyr)
# library(researchTask)
# 
# library(mlr3verse)
# 
# ##### workspace ####
# 
# task <- tsk("kc_housing")
# data <- as.data.table(task$data())
# data <- na.omit(data)
# data <- as.data.frame(data)
# 
# 
# # data <- mtcars
# target <- 1
# min_leaf_size <- 1
# max_tree_depth <- 2
# measure <- "RMSE"
# look_ahead <- 1
# consider <- 1
# 
# output <- la_tree(data = data,
#                   target = target,
#                   look_ahead = look_ahead,
#                   max_tree_depth = max_tree_depth,
#                   min_leaf_size = min_leaf_size,
#                   measure = measure,
#                   consider = consider)
# 
# predict_la_tree(newdata = data, model_list = output)
# 
# ##### playground #####
# 
# task <- tsk("mtcars")
# lrn <- lrn("regr.rpart")
# lrn <- lrn("regr.la_tree")
# 
# lrn$train(task)
# lrn$model
# p <-lrn$predict(task)
# p$data$response
# 
# rpart::predict
# 
# t1 <- stats::predict(lrn$model, task$data())
# 
# predict_la_tree(lrn$model, task$data())
# 
