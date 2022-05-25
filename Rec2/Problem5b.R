set.seed(2) # to reproduce
M <- 100 # repeated samplings,x fixed but new errors
nord <- 20
x <- seq(from = -2, to = 4, by = 0.1)
truefunc <- function(x){
  return(2^x)
}
true_y <- truefunc(x)
error <- matrix(rnorm(length(x)*M, mean = 0, sd = 2), nrow = M, byrow = TRUE)
testerror <- matrix(rnorm(length(x)*M, mean = 0, sd = 2), nrow = M, byrow = TRUE)
ymat <- matrix(rep(true_y, M), byrow = T, nrow = M) + error
testymat <- matrix(rep(true_y, M), byrow=T, nrow=M) + testerror
predictions_list <- lapply(1:nord, matrix, data = NA, nrow = M, ncol = ncol(ymat))
for(i in 1:nord){
  for(j in 1:M){
    predictions_list[[i]][j, ] <- predict(lm(ymat[j,] ~ poly(x, i, raw = TRUE)))
  }
}
trainMSE <- lapply(1:nord, function(poly_degree) rowMeans((predictions_list[[poly_degree]] - ymat)^2))
testMSE <- lapply(1:nord, function(poly_degree) rowMeans((predictions_list[[poly_degree]] - testymat)^2))

library(tidyverse) # The tidyverse contains ggplot2, as well as tidyr and dplyr,
# which we can use for dataframe manipulation.
# Convert each matrix in the list form wide to long (because that is the best format for ggplot2)
list_train_MSE <- lapply(1:nord, function(poly_degree) cbind(error = trainMSE[[poly_degree]],
                                                             poly_degree,
                                                             error_type = "train",
                                                             simulation_num = 1:M))
list_test_MSE <- lapply(1:nord, function(poly_degree) cbind(error = testMSE[[poly_degree]],
                                                            poly_degree,
                                                            error_type = "test",
                                                            simulation_num = 1:M))
# Now predictions_list is a list with 20 entries, where each entry is a matrix
# with 100 rows, where each row is the predicted polynomial of that degree.
stacked_train <- NULL
for (i in 1:nord) {
  stacked_train <-
    rbind(stacked_train, list_train_MSE[[i]])
}
stacked_test <- NULL
for (i in 1:nord) {
  stacked_test <-
    rbind(stacked_test, list_test_MSE[[i]])
}
stacked_errors_df <- as.data.frame(rbind(stacked_train, stacked_test))
# This is already on long format.
stacked_errors_df$error <- as.numeric(stacked_errors_df$error)
stacked_errors_df$simulation_num <- as.integer(stacked_errors_df$simulation_num)
stacked_errors_df$poly_degree <- as.integer(stacked_errors_df$poly_degree)
p.all_lines <- ggplot(data = stacked_errors_df, aes(x = poly_degree, y = error, group = simulation_num)) +
  geom_line(aes(color = simulation_num)) +
  facet_wrap(~ error_type) +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw() +
  theme(legend.position = "none")
5
p.bars <- ggplot(stacked_errors_df, aes(x = as.factor(poly_degree), y = error)) +
  geom_boxplot(aes(fill = error_type)) +
  scale_fill_discrete(name = "Error type") +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw()
# Here we find the average test error and training error across the repeated simulations.
# The symbol "%>%" is called a pipe, and comes from the tidyverse packages,
# which provide convenient functions for working with data frames.
means_across_simulations <- stacked_errors_df %>%
  group_by(error_type, poly_degree) %>%
  summarise(mean = mean(error))
p.means <- ggplot(means_across_simulations, aes(x = poly_degree, y = mean)) +
  geom_line(aes(color = error_type)) +
  scale_color_discrete(name = "Error type") +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw()
library(patchwork) # The library patchwork is the best way of combining ggplot2 objects.
# You could also use the function ggarrange from the ggpubr package.
p.all_lines / (p.bars + p.means)

meanmat <- matrix(ncol = length(x), nrow = nord)
varmat <- matrix(ncol = length(x), nrow = nord)
for (j in 1:nord){
  meanmat[j,] <- apply(predictions_list[[j]], 2, mean) # we now take the mean over the M simulations - to mimic E and Var at each x value and each poly model
  varmat[j,] <- apply(predictions_list[[j]], 2, var)
}
# nord times length(x)
bias2mat <- (meanmat - matrix(rep(true_y, nord), byrow = TRUE, nrow = nord))^2 #here the truth is final

df <- data.frame(x = rep(x, each = nord), poly_degree = rep(1:nord, length(x)),
                 bias2 = c(bias2mat), variance = c(varmat),
                 irreducible_error = rep(4, prod(dim(varmat)))) #irr is just 1
df$total <- df$bias2 + df$variance + df$irreducible_error
df_long <- pivot_longer(df, cols = !c(x, poly_degree), names_to = "type")
df_select_poly <- filter(df_long, poly_degree %in% c(1, 2, 10, 20))
ggplot(df_select_poly, aes(x = x, y = value, group = type)) +
  geom_line(aes(color = type)) +
  facet_wrap(~poly_degree, scales = "free", labeller = label_both) +
  theme_bw()

df_select_x <- filter(df_long, x %in% c(-1, 0.5, 2, 3.5))
ggplot(df_select_x, aes(x = poly_degree, y = value, group = type)) +
  geom_line(aes(color = type)) +
  facet_wrap(~x, scales = "free", labeller = label_both) +
  theme_bw()

