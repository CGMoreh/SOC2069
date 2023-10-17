library(tidyverse)
library(easystats)
library(ggformula)


set.seed(123)

n_observations <- 60e6
beta1 <- 1.5

df_reg <-
  tibble(
    x = runif(n = n_observations, min = 1, max = 100),
    z = rbinom(n = n_observations, size = 1, prob = .65),
    e = rnorm(n = n_observations, mean = 0, sd = 500),
    y = x * beta1 + e
  ) |>
  mutate(
    x = round(x, 2),
    y = round(y, 2)
  ) 


df_reg_sample <- df_reg |> 
  slice_sample(n = 10000)


describe_distribution(df_reg)
describe_distribution(df_reg_sample)

gf_density( ~ e, data = df_reg_sample)

gf_point(y ~ x, data = df_reg_sample) |> gf_lm()


lm(y ~ x, data = df_reg_sample) |> model_parameters()
