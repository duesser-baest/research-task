# s <- find_split(data_2, 1)
# 
# m <- mapply(partition, data_2, s)
# 
# pd <- list()
# 
# pd[[1]] <- rlist::list.append()
# 
# m[,1]
# data_2[[-3]]
# data_1[[-2]]
# data_2[[1]]
# 
# l <- list(1:10, 11:20, 21:30)
# l[-3]
# l2 <- list(1:10, 11:20)
# l2[-1]
# 
# list.append(l,l2)
# 
# ################
# s <- find_split(data_2, 1)
# m <- mapply(partition, data_2, s)
# pd <- list()
# length(data_1)
# 
# 
# m[,1][[1]] %>% nrow()
# data_2[-1][[2]] %>% nrow()
# 
# c(m[,1], data_2[-1]) %>% class
# 
# data_1[-2]
# rlist::list.append(m[,1], data_2[-1])
# 
# target <- 1
# 
# data <- list(mtcars)
# 
# loss <- loss_function(data = data, target = target)
# 
# split <- find_split(data, target)
# 
# data_1 <- partition(data = data[[1]], variable = split[[1]][[1]], value = split[[1]][[2]])
# 
# loss_1 <- loss_function(data = data_1, target = target)
# 
# split_1 <- find_split(data_1, target)
# 
# data_21p <- partition(data = data_1[[1]], variable = split_1[[1]][[1]], value = split_1[[1]][[2]])
# data_21 <- rlist::list.append(data_21p, data_1[[2]])
# 
# data_22p <- partition(data_1[[2]], split_1[[2]][[1]], split_1[[2]][[2]])
# data_22 <- rlist::list.append(data_22p, data_1[[1]])
# 
# loss_21 <- loss_function(data_21, target)
# loss_22 <- loss_function(data_22, target)
# ##################
# 
# sp <- find_split(mtcars, 1)
# sp
# p <- partition(mtcars, sp[[1]], sp[[2]])
# p
# d1 <- p[[1]]
# d1
# d2 <- p[[2]]
# d2
# sl_lists(d2, 1)
# 
# # to find best split in lower level, the best split in each partition has to be evaluated on the loss of all resulting sub partitions
# 
# sp2.1 <- find_split(d1, 1)
# sp2.2 <- find_split(d2, 1)
# 
# split_list(d, 1)
# sl <- sl_lists(d, 1)
# 
# p2.1s <- partition(d1, sp2.1[[1]], sp2.1[[2]])
# p2.1 <- rlist::list.append(p2.1s, d2)
# p2.1
# p2.2s <- partition(d2, sp2.2[[1]], sp2.2[[2]])
# p2.2 <- rlist::list.append(p2.2s, d1)
# p2.2
# 
# 
# loss_function(p2.1, 1)
# loss_function(p2.2, 1)

