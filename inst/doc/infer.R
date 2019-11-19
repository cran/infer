## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 3.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE-------
library(devtools)
library(dplyr)
devtools::load_all()

## ----load-gss, warning = FALSE, message = FALSE--------------------------
# load in the dataset
data(gss)

# take a look at its structure
str(gss)

## ----specify-example, warning = FALSE, message = FALSE, cache = TRUE-----
gss %>%
  specify(response = age)

## ----specify-one, warning = FALSE, message = FALSE, cache = TRUE---------
gss %>%
  specify(response = age) %>%
  class()

## ----specify-two, warning = FALSE, message = FALSE, cache = TRUE---------
# with the named arguments
gss %>%
  specify(age ~ partyid)

# as a formula
gss %>%
  specify(response = age, explanatory = partyid)

## ----specify-success, warning = FALSE, message = FALSE, cache = TRUE-----
# specifying for inference on proportions
gss %>%
  specify(response = college, success = "degree")

## ----hypothesize-independence, warning = FALSE, message = FALSE, cache = TRUE----
gss %>%
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")

## ----hypothesize-40-hr-week, warning = FALSE, message = FALSE, cache = TRUE----
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)

## ----generate-point, warning = FALSE, message = FALSE, cache = TRUE------
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap")

## ----generate-permute, warning = FALSE, message = FALSE, cache = TRUE----
gss %>%
  specify(partyid ~ age) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

## ----calculate-point, warning = FALSE, message = FALSE, cache = TRUE-----
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

## ----specify-diff-in-means, warning = FALSE, message = FALSE, cache = TRUE----
gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))

## ----utilities-examples--------------------------------------------------
# find the point estimate
point_estimate <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean") %>%
  dplyr::pull()

# generate a null distribution
null_dist <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean")

## ----visualize, warning = FALSE, message = FALSE-------------------------
null_dist %>%
  visualize()

## ----visualize2, warning = FALSE, message = FALSE------------------------
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two_sided")

## ----get_p_value, warning = FALSE, message = FALSE-----------------------
# get a two-tailed p-value
p_value <- null_dist %>%
  get_p_value(obs_stat = point_estimate, direction = "two_sided")

p_value

## ----get_conf, message = FALSE, warning = FALSE--------------------------
# start with the null distribution
null_dist %>%
  # calculate the confidence interval around the point estimate
  get_confidence_interval(point_estimate = point_estimate,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")

