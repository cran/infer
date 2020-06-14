## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5, 
                      message = FALSE, warning = FALSE) 
options(digits = 4)

## ----load-packages, echo = FALSE----------------------------------------------
library(dplyr)
devtools::load_all()

## ----load-gss-----------------------------------------------------------------
# load in the dataset
data(gss)

# take a look at its structure
dplyr::glimpse(gss)

## -----------------------------------------------------------------------------
x_bar <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = x_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = x_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
t_bar <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

## -----------------------------------------------------------------------------
t_bar <- gss %>%
  t_stat(response = hours, mu = 40)

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000) %>%
  calculate(stat = "t")

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn, method = "both") +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = t_bar, direction = "two-sided")

## -----------------------------------------------------------------------------
gss %>%
  t_test(response = hours, mu = 40)

## -----------------------------------------------------------------------------
x_tilde <- gss %>%
  specify(response = age) %>%
  calculate(stat = "median")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(response = age) %>%
  hypothesize(null = "point", med = 40) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "median")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = x_tilde, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = x_tilde, direction = "two-sided")

## -----------------------------------------------------------------------------
p_hat <- gss %>%
  specify(response = sex, success = "female") %>%
  calculate(stat = "prop")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(response = sex, success = "female") %>%
  hypothesize(null = "point", p = .5) %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = p_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = p_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  dplyr::mutate(is_female = (sex == "female")) %>%
  specify(response = is_female, success = "TRUE") %>%
  hypothesize(null = "point", p = .5) %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")

## ----prop_test_1_grp----------------------------------------------------------
prop_test(gss,
          college ~ NULL,
          p = .2)

## -----------------------------------------------------------------------------
d_hat <- gss %>% 
  specify(college ~ sex, success = "no degree") %>%
  calculate(stat = "diff in props", order = c("female", "male"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "diff in props", order = c("female", "male"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
r_hat <- gss %>% 
  specify(college ~ sex, success = "no degree") %>%
  calculate(stat = "ratio of props", order = c("female", "male"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "ratio of props", order = c("female", "male"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = r_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = r_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
or_hat <- gss %>% 
  specify(college ~ sex, success = "no degree") %>%
  calculate(stat = "odds ratio", order = c("female", "male"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "odds ratio", order = c("female", "male"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = or_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = or_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
z_hat <- gss %>% 
  specify(college ~ sex, success = "no degree") %>%
  calculate(stat = "z", order = c("female", "male"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "z", order = c("female", "male"))

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>%  
  calculate(stat = "z", order = c("female", "male"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn, method = "both") +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = z_hat, direction = "two-sided")

## ----prop_test_2_grp----------------------------------------------------------
prop_test(gss, 
          college ~ sex,  
          order = c("female", "male"))

## -----------------------------------------------------------------------------
Chisq_hat <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  calculate(stat = "Chisq")

## -----------------------------------------------------------------------------
Chisq_hat <- gss %>%
  chisq_stat(response = finrela,
             p = c("far below average" = 1/6,
                   "below average" = 1/6,
                   "average" = 1/6,
                   "above average" = 1/6,
                   "far above average" = 1/6,
                   "DK" = 1/6))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
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

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  calculate(stat = "Chisq")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "both") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
chisq_test(gss, 
           response = finrela,
           p = c("far below average" = 1/6,
                 "below average" = 1/6,
                 "average" = 1/6,
                 "above average" = 1/6,
                 "far above average" = 1/6,
                 "DK" = 1/6))

## -----------------------------------------------------------------------------
Chisq_hat <- gss %>%
  specify(formula = finrela ~ sex) %>% 
  calculate(stat = "Chisq")

## -----------------------------------------------------------------------------
Chisq_hat <- gss %>%
  chisq_stat(formula = finrela ~ sex)

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(finrela ~ sex) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "Chisq")

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
  specify(finrela ~ sex) %>%
  hypothesize(null = "independence") %>% 
  calculate(stat = "Chisq")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn, method = "both") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = Chisq_hat, direction = "greater")

## -----------------------------------------------------------------------------
gss %>%
  chisq_test(formula = finrela ~ sex)

## -----------------------------------------------------------------------------
d_hat <- gss %>% 
  specify(age ~ college) %>% 
  calculate(stat = "diff in means", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
t_hat <- gss %>% 
  specify(age ~ college) %>% 
  calculate(stat = "t", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
visualize(null_distn, method = "both") +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = t_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
d_hat <- gss %>% 
  specify(age ~ college) %>% 
  calculate(stat = "diff in medians", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
null_distn <- gss %>%
  specify(age ~ college) %>% # alt: response = age, explanatory = season
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in medians", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
F_hat <- gss %>% 
  specify(age ~ partyid) %>%
  calculate(stat = "F")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "F")

## -----------------------------------------------------------------------------
null_distn_theoretical <- gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   calculate(stat = "F")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = F_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

## -----------------------------------------------------------------------------
visualize(null_distn, mdthod = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = F_hat, direction = "greater")

## -----------------------------------------------------------------------------
slope_hat <- gss %>% 
  specify(hours ~ age) %>% 
  calculate(stat = "slope")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
   specify(hours ~ age) %>% 
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "slope")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = slope_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = slope_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
correlation_hat <- gss %>% 
  specify(hours ~ age) %>% 
  calculate(stat = "correlation")

## -----------------------------------------------------------------------------
null_distn <- gss %>%
   specify(hours ~ age) %>% 
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "correlation")

## -----------------------------------------------------------------------------
visualize(null_distn) +
  shade_p_value(obs_stat = correlation_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
null_distn %>%
  get_p_value(obs_stat = correlation_hat, direction = "two-sided")

## -----------------------------------------------------------------------------
x_bar <- gss %>% 
  specify(response = hours) %>%
  calculate(stat = "mean")

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(response = hours) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "mean")

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- get_ci(boot, type = "se", point_estimate = x_bar)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
t_hat <- gss %>% 
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(response = hours) %>%
   hypothesize(null = "point", mu = 40) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t")

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = t_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
p_hat <- gss %>% 
   specify(response = sex, success = "female") %>%
   calculate(stat = "prop")

## -----------------------------------------------------------------------------
boot <- gss %>%
 specify(response = sex, success = "female") %>%
 generate(reps = 1000, type = "bootstrap") %>%
 calculate(stat = "prop")

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = p_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
d_hat <- gss %>%
  specify(hours ~ college) %>%
  calculate(stat = "diff in means", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(hours ~ college) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "diff in means", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = d_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
t_hat <- gss %>%
  specify(hours ~ college) %>%
  calculate(stat = "t", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(hours ~ college) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t", order = c("degree", "no degree"))

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = t_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
d_hat <- gss %>% 
  specify(college ~ sex, success = "degree") %>%
  calculate(stat = "diff in props", order = c("female", "male"))

## -----------------------------------------------------------------------------
boot <- gss %>%
  specify(college ~ sex, success = "degree") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("female", "male"))

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = d_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
z_hat <- gss %>% 
  specify(college ~ sex, success = "degree") %>%
  calculate(stat = "z", order = c("female", "male"))

## -----------------------------------------------------------------------------
boot <- gss %>%
  specify(college ~ sex, success = "degree") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "z", order = c("female", "male"))

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = z_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
slope_hat <- gss %>% 
  specify(hours ~ age) %>%
  calculate(stat = "slope")

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(hours ~ age) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "slope")

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = slope_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## -----------------------------------------------------------------------------
correlation_hat <- gss %>% 
  specify(hours ~ age) %>%
  calculate(stat = "correlation")

## -----------------------------------------------------------------------------
boot <- gss %>%
   specify(hours ~ age) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "correlation")

## -----------------------------------------------------------------------------
percentile_ci <- get_ci(boot)

## -----------------------------------------------------------------------------
visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)

## -----------------------------------------------------------------------------
standard_error_ci <- boot %>%
  get_ci(type = "se", point_estimate = correlation_hat)

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

