
library(dplyr)

set.seed(13534)
gender = ifelse(runif(n = 142) > 0.5, "man", "woman")
resid <- rnorm(n = 142, mean = 0, sd = 3)
resid2 <- rnorm(n = 142, mean = 0, sd = 1)
wman <- rnorm(142, 85,10)
wwom <- rnorm(142, 75,10)
x <- data.frame(gender, resid, resid2, wman, wwom) %>%
  mutate(weight = ifelse(gender == "man", wman,wwom)) %>%
  mutate(performance = 25 + (weight * -0.2) + resid) %>%
  mutate(performance = ifelse(gender == "man", performance + 2 + resid2, performance)) %>%
  mutate(performance = ifelse(performance < 0, 0, performance),
         performance = ifelse(performance > 15, 15, performance)) %>%
  dplyr::select(gender, weight, performance)



summary(x)
cor(x[,2:3])
plot(x$weight, x$performance)



ggplot(x, aes(x = weight, y = performance, group = gender, color = gender)) +
  geom_point()

#create missings

perform <- MAR(x, alpha = 0.25, pattern = matrix(c(1,0,1,
                                                   1,1,0,
                                                   1,0,0),
                                                 byrow = T, nrow = 3), f= c(0.4, 0.4, 0.2))

colnames(perform) <- colnames(x)
perform$gender <- factor(perform$gender, labels = c("Man", "Woman"))


usethis::use_data(perform, overwrite =  T)
