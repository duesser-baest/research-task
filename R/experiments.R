# ##### Experiments 1 benchmark hyperparameters ####
# 
# rm(list = ls())
# 
# # run out of mlr3extralearners repo
# devtools::document()
# library(researchTask)
# library(mlr3verse)
# 
# taskmb <- tsk("moneyball")
# datamb <- taskmb$data() %>% as.data.frame()
# datamb <- datamb[,-c(10,11,13)]
# datamb <- na.omit(datamb)
# taskmb <- TaskRegr$new(id = "moneyball", backend = datamb, target = "rs")
# 
# taskkc <- tsk("kc_housing")
# datakc <- taskkc$data() %>% as.data.frame()
# datakc <- datakc[,-c(5, 11, 19)]
# taskkc <- TaskRegr$new(id = "kc", backend = datakc, target = "price")
# 
# 
# taskbh <- tsk("boston_housing")
# databh <- taskbh$data() %>% as.data.frame()
# databh <- databh[,-17] # town for boston
# taskbh <- TaskRegr$new(id = "boston", backend = databh, target = "medv")
# 
# 
# taskmtc <- tsk("mtcars")
# 
# taskbs <- tsk("bike_sharing")
# databs <- taskbs$data() %>% as.data.frame()
# databs <- databs[,-3] # date for bike_sharing
# taskbs <- TaskRegr$new(id = "bike", backend = databs, target = "count")
# 
# la_1 <- lrn("regr.la_tree",
#             look_ahead = 1,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 4,
#             restr_la_var = FALSE)
# 
# la_1$id <- "la_1"
# 
# la_2 <- lrn("regr.la_tree",
#             look_ahead = 2,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 4,
#             restr_la_var = FALSE)
# 
# la_2$id <- "la_2"
# 
# la_3 <- lrn("regr.la_tree",
#             look_ahead = 3,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 4,
#             restr_la_var = FALSE)
# 
# la_3$id <- "la_3"
# 
# la_2r <- lrn("regr.la_tree",
#             look_ahead = 2,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 4,
#             restr_la_var = TRUE)
# 
# la_2r$id <- "la_2r"
# 
# la_3r <- lrn("regr.la_tree",
#             look_ahead = 3,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 4,
#             restr_la_var = TRUE)
# 
# la_3r$id <- "la_3r"
# 
# la_2c <- lrn("regr.la_tree",
#             look_ahead = 2,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 2,
#             restr_la_var = FALSE)
# 
# la_2c$id <- "la_2c"
# 
# la_3c <- lrn("regr.la_tree",
#             look_ahead = 3,
#             consider = 9,
#             max_tree_depth = 5,
#             min_leaf_size = 15,
#             cap_la = 2,
#             restr_la_var = FALSE)
# 
# la_3c$id <- "la_3c"
# 
# la_2rc <- lrn("regr.la_tree",
#              look_ahead = 2,
#              consider = 9,
#              max_tree_depth = 5,
#              min_leaf_size = 15,
#              cap_la = 2,
#              restr_la_var = TRUE)
# 
# la_2rc$id <- "la_2rc"
# 
# la_3rc <- lrn("regr.la_tree",
#             look_ahead = 3,
#              consider = 9,
#              max_tree_depth = 5,
#              min_leaf_size = 15,
#              cap_la = 2,
#              restr_la_var = TRUE)
# 
# la_3rc$id <- "la_3rc"
# 
# learners <- list(
#   la_1, la_2, la_3, la_2r, la_3r, la_2c, la_3c, la_2rc, la_3rc
# )
# 
# resampling <- rsmp("cv", folds = 3)
# measure <- msr("regr.rmse")
# 
# designbh <- benchmark_grid(tasks = taskbh, learners = learners, resamplings = resampling)
# designmb <- benchmark_grid(tasks = taskmb, learners = learners, resamplings = resampling)
# designmtc <- benchmark_grid(tasks = taskmtc, learners = learners, resamplings = resampling)
# 
# bmrmtc <- benchmark(designmtc)
# save(bmrmtc, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmrmtc.RData")
# bmrmtc_plot <- bmrmtc %>% autoplot(measure = measure)
# bmrmtc$aggregate(measure = measure)
# bmrbh <- benchmark(designbh)
# 
# save(bmrbh, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmrbh.RData")
# bmrbh_plot <- bmrbh %>% autoplot(measure = measure)
# bmrbh$aggregate(measure = measure)
# bmrmb <- benchmark(designmb)
# 
# save(bmrmb, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmrmb.RData")
# bmrmb_plot <- bmrmb %>% autoplot(measure = measure)
# bmrmb$aggregate(measure = measure)
# 
# library(gridExtra)
# bmr <- grid.arrange(bmrmtc_plot, bmrbh_plot, bmrmb_plot, nrow = 3)
# 
# save(bmr, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmr.RData")
# 
# # ##### Experiments 2 benchmark against rpart and ranger ####
# 
# l_rpart <- lrn("regr.rpart", minsplit = 2, maxdepth = 5)
# l_rpart$id <- "rpart"
# l_ranger <- lrn("regr.ranger", min.node.size = 2, max.depth = 5, num.trees = 1000)
# l_ranger$id <- "ranger"
# la_bm1 <- lrn("regr.la_tree", max_tree_depth = 5, min_leaf_size = 2, look_ahead = 1)
# la_bm1$id <- "la_tree1"
# la_bm3r <- lrn("regr.la_tree", max_tree_depth = 5, min_leaf_size = 2, look_ahead = 3, restr_la_var = TRUE, cap_la = 5, consider = 9)
# la_bm3r$id <- "la_tree3r"
# 
# lrns <- list(l_rpart, l_ranger, la_bm1, la_bm3r)
# 
# design2mtc <- designmtc <- benchmark_grid(tasks = taskmtc, learners = lrns, resamplings = resampling)
# bmr2mtc <- benchmark(design2mtc)
# bmr2mtc_plot <- bmr2mtc %>% autoplot(measure = measure)
# save(bmr2mtc, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmr2mtc.RData")
# 
# l_rpart <- lrn("regr.rpart", minsplit = 10, maxdepth = 5)
# l_rpart$id <- "rpart"
# l_ranger <- lrn("regr.ranger", min.node.size = 10, max.depth = 5, num.trees = 1000)
# l_ranger$id <- "ranger"
# la_bm1 <- lrn("regr.la_tree", max_tree_depth = 5, min_leaf_size = 10, look_ahead = 1)
# la_bm1$id <- "la_tree1"
# la_bm3r <- lrn("regr.la_tree", max_tree_depth = 5, min_leaf_size = 10, look_ahead = 3, restr_la_var = TRUE, cap_la = 5, consider = 9)
# la_bm3r$id <- "la_tree3r"
# 
# lrns <- list(l_rpart, l_ranger, la_bm1, la_bm3r)
# 
# design2bh <- benchmark_grid(tasks = taskbh, learners = lrns, resamplings = resampling)
# bmr2bh <- benchmark(design2bh)
# bmr2bh_plot <- bmr2bh %>% autoplot(measure = measure)
# save(bmr2bh, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmr2bh.RData")
# 
# design2mb <- benchmark_grid(tasks = taskmb, learners = lrns, resamplings = resampling)
# bmr2mb <- benchmark(design2mb)
# bmr2mb_plot <- bmr2mb %>% autoplot(measure = measure)
# save(bmr2mb, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmr2mb.RData")
# 
# library(gridExtra)
# bmr2 <- grid.arrange(bmr2mtc_plot, bmr2bh_plot, bmr2mb_plot, nrow = 3)
# save(bmr2, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bmr2.RData")
# 
# # ##### Experiments 3 interpretable machine learning ####
# 
# load("C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/bike.RData")
# 
# taskbs2 <- TaskRegr$new(id = "bike", backend = bike, target = "cnt")
# 
# l_rpart$train(taskbs2)
# l_ranger$train(taskbs2)
# la_bm3r$train(taskbs2)
# 
# library(iml)
# library(gridExtra)
# 
# 
# predictor_rpart <- Predictor$new(
#   l_rpart,
#   data = taskbs2$data(),
#   y = taskbs2$target_names
# )
# 
# predictor_ranger <- Predictor$new(
#   l_ranger,
#   data = taskbs2$data(),
#   y = taskbs2$target_names
# )
# 
# predictor_la_tree <- Predictor$new(
#   la_bm3r,
#   data = taskbs2$data(),
#   y = taskbs2$target_names
# )
# 
# ale_temp_rpart <- FeatureEffect$new(
#   predictor = predictor_rpart,
#   feature = "temp",
#   method = "ale"
# )
# 
# ale_temp_rpart_plot <- ale_temp_rpart$plot()
# ale_temp_rpart_plot$labels$x <- "Temperature (rpart)"
# ale_temp_rpart_plot$coordinates$limits$y <- c(-1250, 750)
# 
# ale_temp_ranger <- FeatureEffect$new(
#   predictor = predictor_ranger,
#   feature = "temp",
#   method = "ale"
# )
# 
# ale_temp_ranger_plot <- ale_temp_ranger$plot()
# ale_temp_ranger_plot$labels$x <- "Temperature (ranger)"
# ale_temp_ranger_plot$coordinates$limits$y <- c(-1250, 750)
# 
# ale_temp_la_tree <- FeatureEffect$new(
#   predictor = predictor_la_tree,
#   feature = "temp",
#   method = "ale"
# )
# 
# ale_temp_la_tree_plot <- ale_temp_la_tree$plot()
# ale_temp_la_tree_plot$labels$x <- "Temperature (la_tree)"
# ale_temp_la_tree_plot$coordinates$limits$y <- c(-1250, 750)
# 
# ale <- grid.arrange(ale_temp_rpart_plot, ale_temp_ranger_plot, ale_temp_la_tree_plot, nrow = 3)
# save(ale, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/ale.RData")
# 
# # shapley values
# 
# shapley_rpart <- Shapley$new(
#   predictor = predictor_rpart,
#   x.interest = as.data.frame(taskbs2$data(103))
# )
# 
# shapley_ranger <- Shapley$new(
#   predictor = predictor_ranger,
#   x.interest = as.data.frame(taskbs2$data(103))
# )
# 
# shapley_la_tree <- Shapley$new(
#   predictor = predictor_la_tree,
#   x.interest = as.data.frame(taskbs2$data(103))
# )
# 
# shapley_rpart_plot <- shapley_rpart$plot()
# shapley_ranger_plot <- shapley_ranger$plot()
# shapley_la_tree_plot <- shapley_la_tree$plot()
# shapley_rpart_plot$labels$y <- "Shapley values obs 103 (rpart)"
# shapley_ranger_plot$labels$y <- "Shapley values obs 103 (ranger)"
# shapley_la_tree_plot$labels$y <- "Shapley values obs 103 (la_tree)"
# 
# shapley <- grid.arrange(shapley_rpart_plot, shapley_ranger_plot, shapley_la_tree_plot, nrow = 3)
# save(shapley, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/shapley.RData")
# 
# ale_hum_rpart <- FeatureEffect$new(
#   predictor = predictor_rpart,
#   feature = "hum",
#   method = "ale"
# )
# 
# ale_hum_rpart_plot <- ale_hum_rpart$plot()
# ale_hum_rpart_plot$labels$x <- "Humidity (rpart)"
# 
# ale_hum_ranger <- FeatureEffect$new(
#   predictor = predictor_ranger,
#   feature = "hum",
#   method = "ale"
# )
# 
# ale_hum_ranger_plot <- ale_hum_ranger$plot()
# ale_hum_ranger_plot$labels$x <- "Humidity (ranger)"
# 
# ale_hum_la_tree <- FeatureEffect$new(
#   predictor = predictor_la_tree,
#   feature = "hum",
#   method = "ale"
# )
# 
# ale_hum_la_tree_plot <- ale_hum_la_tree$plot()
# ale_hum_la_tree_plot$labels$x <- "Humidity (la_tree)"
# 
# ale_hum <- grid.arrange(ale_hum_rpart_plot, ale_hum_ranger_plot, ale_hum_la_tree_plot, nrow = 3)
# save(ale_hum, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/ale_hum.RData")
# 
# 
# # shapley values
# 
# shapley106_rpart <- Shapley$new(
#   predictor = predictor_rpart,
#   x.interest = as.data.frame(taskbs2$data(106))
# )
# 
# shapley106_ranger <- Shapley$new(
#   predictor = predictor_ranger,
#   x.interest = as.data.frame(taskbs2$data(106))
# )
# 
# shapley106_la_tree <- Shapley$new(
#   predictor = predictor_la_tree,
#   x.interest = as.data.frame(taskbs2$data(106))
# )
# 
# shapley106_rpart_plot <- shapley106_rpart$plot()
# shapley106_ranger_plot <- shapley106_ranger$plot()
# shapley106_la_tree_plot <- shapley106_la_tree$plot()
# shapley106_rpart_plot$labels$y <- "Shapley values obs 106 (rpart)"
# shapley106_ranger_plot$labels$y <- "Shapley values obs 106 (ranger)"
# shapley106_la_tree_plot$labels$y <- "Shapley values obs 106 (la_tree)"
# 
# shapley106 <- grid.arrange(shapley106_rpart_plot, shapley106_ranger_plot, shapley106_la_tree_plot, nrow = 3)
# save(shapley106, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Research Task in Data Analytics/Resuls Experiments/shapley106.RData")

