##
## Weight over time
library(MASS)
library(dplyr)
library(tidyr)

means_c <- c(95.36, 94.46, 93.27, 93.51, 94.11)
means_i <- c(94.95, 92.03, 89.21, 89.4, 89.84)
cov <- matrix(0.5, nrow = 5, ncol = 5)
diag(cov) <- 1
covc <- cov *116.3378
covi <- cov *145.3471
set.seed(235)
control <- MASS::mvrnorm(n = 55, mu = means_c, Sig = covc) %>%
  data.frame() %>%
  mutate(group = "control", 
         id = 1:55)

intervention <- MASS::mvrnorm(n = 55, mu = means_i, Sig = covi) %>%
  data.frame() %>%
  mutate(group = "intervention",
         id = 56:110)

weightloss_r <- bind_rows(control, intervention) 
set.seed(789)
weightloss <- MAR(weightloss_r, alpha = 0.3, 
                  pattern =  matrix(c(1,1,1,1,0,1,1,
                                      1,1,1,0,0,1,1,
                                      1,1,0,0,0,1,1,
                                      1,0,0,0,0,1,1),
                                    byrow = T, nrow = 4),
                  f = c(0.4, 0.3, 0.2, 0.1))

colnames(weightloss) <- colnames(weightloss_r)


weightloss <- 
weightloss %>%
  pivot_longer(cols = X1:X5, names_to = "month", values_to = "weight") %>%
  mutate(month = ifelse(month == "X1", 0, month),
         month = ifelse(month == "X2", 3, month),
         month = ifelse(month == "X3", 6, month),
         month = ifelse(month == "X4", 12, month),
         month = ifelse(month == "X5", 24, month),
         month = as.numeric(month),
         group = ifelse(group == 1, "control", "intervention"))


usethis::use_data(weightloss, overwrite = T)
