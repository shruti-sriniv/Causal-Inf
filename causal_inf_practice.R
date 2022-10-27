library(tidyverse)
library(broom)
library(ggplot2)

# Create the binary dataset
binary_a_df <- data.frame(
  a = c(rep(1, 8), rep(0, 8)),
  y = c(200, 150, 220, 110, 50, 180, 90, 170, 
        170, 30, 70, 110, 80, 50, 10, 20)
)

#Plot out the data grouped by a on a scatterplot 
#ggplot(binary_a_df, aes(a, y)) +
 #geom_point(size = 4, col = "white", fill = "#E69F00", shape = 21) +
  #scale_x_continuous(breaks = c(0, 1), expand = expand_scale(.5)) + 
  #theme_minimal(base_size = 20)

#Find the mean, median, standard deviation, min and max of the data grouped by a
binary_a_df %>% 
  group_by(a) %>% 
  summarize(
    n = n(), 
    mean = mean(y), 
    sd = sd(y), 
    minimum = min(y), 
    maximum = max(y)
  ) %>% 
  #Print this data in an organized table 
  knitr::kable(digits = 2)

#Create the categorical dataset 
categorical_a_df <- data.frame(a = sort(rep(1:4, 4)),
                               y = c(110, 80, 50, 40, 170, 30, 70, 50, 
                                     110, 50, 180, 130, 200, 150, 220, 210))

#Plot out the categorical dataset 
#ggplot(categorical_a_df, aes(a, y)) +
 #geom_point(size = 4, col = "white", fill = "#E69F00",  shape = 21) +
  #scale_x_continuous(breaks = 1:4, expand = expand_scale(.25)) + 
  #theme_minimal(base_size = 20)

#Summarize for categorical dataset
categorical_a_df %>% 
  group_by(a) %>% 
  summarize(
    n = n(), 
    mean = mean(y), 
    sd = sd(y), 
    minimum = min(y), 
    maximum = max(y)
  ) %>% 
  knitr::kable(digits = 2)

#Create a continuous dataset 
continuous_a_df <- data.frame(
  a = c(3, 11, 17, 23, 29, 37, 41, 53, 
        67, 79, 83, 97, 60, 71, 15, 45),
  y = c(21, 54, 33, 101, 85, 65, 157, 120, 
        111, 200, 140, 220, 230, 217, 11, 190)
)

#Create a linear regression line for this dataset
ggplot(continuous_a_df, aes(a, y)) +
  geom_point(size = 4, col = "white", fill = "red", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, col = "blue", size = 1.2) +
  theme_minimal(base_size = 20) 

linear_regression <- lm(y ~ a, data = continuous_a_df)

linear_regression %>% 
  # get the confidence intervals using `conf.int = TRUE`
  tidy(conf.int = TRUE) %>% 
  # drop the test statistic and P-value
  select(-statistic, -p.value) %>% 
  knitr::kable(digits = 2)

#Predict when a's value is 90 
linear_regression %>% 
  predict(newdata = data.frame(a = 90))

#Linear model for binary dataset
bin_lin <- lm(y ~ a, data = binary_a_df) 

bin_lin %>% 
  tidy(conf.int = TRUE) %>%   
  select(-statistic, -p.value) %>% 
  knitr::kable(digits = 2)

#Creating the quadratic equation 
smoothed_regression <- lm(y ~ a + I(a^2), data = continuous_a_df) 

smoothed_regression %>% 
  tidy(conf.int = TRUE) %>% 
  select(-statistic, -p.value) %>% 
  #  remove `I()` from the term name
  mutate(term = ifelse(term == "I(a^2)", "a^2", term)) %>% 
  knitr::kable(digits = 2) 

ggplot(continuous_a_df, aes(a, y)) +
  geom_point(size = 4, col = "white", fill = "red", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, col = "blue", formula = y ~ x + I(x^2), size = 1.2) + 
  theme_minimal(base_size = 20)

smoothed_regression %>% 
  predict(newdata = data.frame(a=90))




