## ----settings, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE------------
library(ggplot2)
library(dplyr)
devtools::load_all()

## ----glimpse-gss-actual, warning = FALSE, message = FALSE---------------------
dplyr::glimpse(gss)

## ----plot-f, echo = FALSE-----------------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = partyid, y = age) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_brewer(type = "qual") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                     vjust = .5)) +
    ggplot2::labs(x = "partyid: Political Party Affiliation",
                  y = "age: Age of Respondent")

## ----calc-obs-stat-f, warning = FALSE, message = FALSE------------------------
# calculate the observed statistic
observed_f_statistic <- gss %>%
  specify(age ~ partyid) %>%
  calculate(stat = "F")

## ----generate-null-f, warning = FALSE, message = FALSE------------------------
# generate the null distribution using randomization
null_distribution <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

## ----visualize-f, warning = FALSE, message = FALSE----------------------------
# visualize the null distribution and test statistic!
null_distribution %>%
  visualize() + 
  shade_p_value(observed_f_statistic,
                direction = "greater")

## ----visualize-f-theor, warning = FALSE, message = FALSE----------------------
# visualize the theoretical null distribution and test statistic!
gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_f_statistic,
                direction = "greater")

## ----visualize-indep-both, warning = FALSE, message = FALSE-------------------
# visualize both null distributions and the test statistic!
null_distribution %>%
  visualize(method = "both") + 
  shade_p_value(observed_f_statistic,
                direction = "greater")

## ----p-value-indep, warning = FALSE, message = FALSE--------------------------
# calculate the p value from the observed statistic and null distribution
p_value <- null_distribution %>%
  get_p_value(obs_stat = observed_f_statistic,
              direction = "greater")

p_value

