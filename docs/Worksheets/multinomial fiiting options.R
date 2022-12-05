
library(tidyverse)
library(mosaic)
library(sjmisc)
library(jtools)

ukhls <- readRDS(url("https://cgmoreh.github.io/SOC2069/SOC2069-Statistical-analysis/ukhls_w8.rds"))


### For logit slides


frq(ukhls$urban_dv)


favstats( ~ age_dv, data = ukhls)     














#### multinomial options
glm(scwhorusex ~ age_dv + sex, family = "poisson", etastart="0", data=ukhls)

library(MASS)
pacman::p_load(nnet, VGAM)

vglm(as.factor(scwhorusex) ~ age_dv + sex, data=ukhls, family = multinomial) -> mod

multinom(as.factor(scwhorusex) ~ age_dv + sex, data=ukhls) -> mod

summary(mod)

stargazer::stargazer(mod, type = "text")
