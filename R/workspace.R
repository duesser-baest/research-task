# ##### load libraries #####
# 
# rm(list = ls())
#
# library(rlist)
# library(dplyr)
# library(researchTask)
# library(mlr3verse)
#
# ##### workspace ####
# 
# task <- tsk("kc_housing")
# data <- as.data.table(task$data())
# data <- na.omit(data)
# data <- as.data.frame(data)
# 
# data <- mtcars
# target <- 1
# min_leaf_size <- 5
# max_tree_depth <- 4
# measure <- "RMSE"
# look_ahead <- 2
# consider <- 9
# restr_la_var <- TRUE
# cap_la <- 3
# 
# output <- la_tree(data = data,
#                   target = target,
#                   look_ahead = look_ahead,
#                   max_tree_depth = max_tree_depth,
#                   min_leaf_size = min_leaf_size,
#                   measure = measure,
#                   consider = consider,
#                   restr_la_var = restr_la_var,
#                   cap_la = cap_la)
# 
# predict_la_tree(newdata = data, model_list = output)
# 


