library(tidyverse)
library(easystats)
library(ggformula)
library(ggeffects)


## Population data simulation --------------------------------------------------------------------------

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



## Interaction effect simulation (numeric/indicator*indicator) ------------------------------------------------------

set.seed(123)

n <-  80

# simulate a covariate

# x <- rnorm(n, 0, 1)

x <- sample(c(0,1), size = n, replace = T)

# simualate a binary moderator
z <- sample(c(0,1), size = n, replace = T)

beta0 <- 77.320
beta1 <- 0.141
beta2 <- 1.863
beta3 <- -14.007
sigma0 <- 1.2

# mu
mu <- beta0 + beta1*x + beta2*z + beta3*(x*z) 

y <- rnorm(n, mu, sigma0)

dd <- data.frame(y=y, x=x, z=z)

mod <- lm(y ~ x * z, data = dd) 

compare_parameters(mod)
model_parameters(mod)

ggeffects::ggpredict(mod,
                     terms = c("x", "z")) |> plot()




## Simulate data for a mixed model ----------------------------------------------------------------------------------

See: https://stats.stackexchange.com/questions/488188/what-are-the-steps-to-simulate-data-for-a-linear-model-with-random-slopes-and-ra




