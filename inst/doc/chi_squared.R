## ----settings, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE------------
library(ggplot2)
library(dplyr)
devtools::load_all()

## ----glimpse-gss-actual, warning = FALSE, message = FALSE---------------------
dplyr::glimpse(gss)

## ----plot-indep, echo = FALSE-------------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = finrela, fill = college) +
  ggplot2::geom_bar(position = "fill") +
  ggplot2::scale_fill_brewer(type = "qual") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                     vjust = .5)) +
    ggplot2::labs(x = "finrela: Self-Identification of Income Class",
                  y = "Proportion")

## ----calc-obs-stat-indep, warning = FALSE, message = FALSE--------------------
# calculate the observed statistic
observed_indep_statistic <- gss %>%
  specify(college ~ finrela) %>%
  calculate(stat = "Chisq")

## ----generate-null-indep, warning = FALSE, message = FALSE--------------------
# generate the null distribution using randomization
null_distribution_simulated <- gss %>%
  specify(college ~ finrela) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq")

## ----generate-null-indep-t, warning = FALSE, message = FALSE------------------
# generate the null distribution by theoretical approximation
null_distribution_theoretical <- gss %>%
  specify(college ~ finrela) %>%
  hypothesize(null = "independence") %>%
  # note that we skip the generation step here!
  calculate(stat = "Chisq")

## ----visualize-indep, warning = FALSE, message = FALSE------------------------
# visualize the null distribution and test statistic!
null_distribution_simulated %>%
  visualize() + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

## ----visualize-indep-theor, warning = FALSE, message = FALSE------------------
# visualize the theoretical null distribution and test statistic!
gss %>%
  specify(college ~ finrela) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

## ----visualize-indep-both, warning = FALSE, message = FALSE-------------------
# visualize both null distributions and the test statistic!
null_distribution_simulated %>%
  visualize(method = "both") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

## ----p-value-indep, warning = FALSE, message = FALSE--------------------------
# calculate the p value from the observed statistic and null distribution
p_value_independence <- null_distribution_simulated %>%
  get_p_value(obs_stat = observed_indep_statistic,
              direction = "greater")

p_value_independence

## ----chisq-indep-wrapper, message = FALSE, warning = FALSE--------------------
chisq_test(gss, college ~ finrela)

## ----gof-plot, echo = FALSE---------------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = finrela) +
  ggplot2::geom_bar() +
  ggplot2::geom_hline(yintercept = 466.3, col = "red") +
  ggplot2::labs(x = "finrela: Self-Identification of Income Class",
                y = "Number of Responses")

## ----observed-gof-statistic, warning = FALSE, message = FALSE-----------------
# calculating the null distribution
observed_gof_statistic <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  calculate(stat = "Chisq")

## ----null-distribution-gof, warning = FALSE, message = FALSE------------------
# generating a null distribution, assuming each income class is equally likely
null_distribution_gof <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  generate(reps = 1000, type = "simulate") %>%
  calculate(stat = "Chisq")

## ----visualize-indep-gof, warning = FALSE, message = FALSE--------------------
# visualize the null distribution and test statistic!
null_distribution_gof %>%
  visualize() + 
  shade_p_value(observed_gof_statistic,
                direction = "greater")

## ----get-p-value-gof, warning = FALSE, message = FALSE------------------------
# calculate the p-value
p_value_gof <- null_distribution_gof %>%
  get_p_value(observed_gof_statistic,
              direction = "greater")

p_value_gof

## ----chisq-gof-wrapper, message = FALSE, warning = FALSE----------------------
chisq_test(gss, 
           response = finrela,
           p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6))

