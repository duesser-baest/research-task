build_tree <- function(){}
# 
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
# 
# 
