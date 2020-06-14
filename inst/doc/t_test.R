## ----settings, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE------------
library(ggplot2)
library(dplyr)
devtools::load_all()

## ----glimpse-gss-actual, warning = FALSE, message = FALSE---------------------
dplyr::glimpse(gss)

## ----plot-1-sample, echo = FALSE----------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = hours) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::labs(x = "hours: Number of Hours Worked",
                y = "Number of Responses") +
  ggplot2::scale_x_continuous(breaks = seq(0, 90, 10))

## ----calc-obs-stat-1-sample, warning = FALSE, message = FALSE-----------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

## ----generate-null-1-sample, warning = FALSE, message = FALSE-----------------
# generate the null distribution
null_distribution_1_sample <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "t")

## ----visualize-1-sample, warning = FALSE, message = FALSE---------------------
# visualize the null distribution and test statistic!
null_distribution_1_sample %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")

## ----p-value-1-sample, warning = FALSE, message = FALSE-----------------------
# calculate the p value from the test statistic and null distribution
p_value_1_sample <- null_distribution_1_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_1_sample

## ----t-test-wrapper, message = FALSE, warning = FALSE-------------------------
t_test(gss, response = hours, mu = 40)

## ----t-stat-wrapper, message = FALSE, warning = FALSE-------------------------
t_stat(gss, response = hours, mu = 40)

## ----plot-2-sample, echo = FALSE----------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = college, y = hours) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "college: Whether the Respondent has a College Degree",
                y = "hours: Number of Hours Worked")

## ----calc-obs-stat-2-sample, warning = FALSE, message = FALSE-----------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(hours ~ college) %>%
  calculate(stat = "t", order = c("degree", "no degree"))

observed_statistic

## ----generate-null-2-sample, warning = FALSE, message = FALSE-----------------
# generate the null distribution with randomization
null_distribution_2_sample_permute <- gss %>%
  specify(hours ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("degree", "no degree"))

## ----generate-null-2-sample-theoretical, warning = FALSE, message = FALSE-----
# generate the null distribution with the theoretical t
null_distribution_2_sample_theoretical <- gss %>%
  specify(hours ~ college) %>%
  hypothesize(null = "independence") %>%
  # generate() isn't used for the theoretical version!
  calculate(stat = "t", order = c("degree", "no degree"))

## ----visualize-2-sample, warning = FALSE, message = FALSE---------------------
# visualize the randomization-based null distribution and test statistic!
null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")

## ----visualize-2-sample-theoreticql, warning = FALSE, message = FALSE---------
# visualize the theoretical null distribution and test statistic!
null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_statistic,
                direction = "two-sided")

## ----visualize-2-sample-both, warning = FALSE, message = FALSE----------------
# visualize both null distributions and test statistic!
null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(observed_statistic,
                direction = "two-sided")

## ----p-value-2-sample, warning = FALSE, message = FALSE-----------------------
# calculate the p value from the randomization-based null 
# distribution and the observed statistic
p_value_2_sample <- null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_2_sample

## ----2-sample-t-test-wrapper, message = FALSE, warning = FALSE----------------
t_test(x = gss, 
       formula = hours ~ college, 
       order = c("degree", "no degree"),
       alternative = "two-sided")

