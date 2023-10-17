
#' Example from: https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html?q=logistic#an-example-with-a-simple-logistic-regression-model

library(tidyverse)
library(ggeffects)
library(easystats)
pacman::p_load(margins)

#' https://www.analyticsvidhya.com/wp-content/uploads/2015/12/d5.jpg


##' `rbinom` with probability vectors
###' see also: https://stackoverflow.com/questions/42926683/how-does-prob-argument-in-rbinom-work-when-prob-is-a-vector
###' So the two `y`'s below are similar in their proportions

set.seed(123)
y <- rbinom(300, 1, c(0.3, 0.7))
# y <- c(rbinom(150, 1, 0.3), rbinom(150, 1, 0.7))
x <- rnorm(300, 2)
y_1 <- y == 1
x[y_1] <- x[y_1] + rnorm(sum(y_1), 3)

d <- data.frame(x, y)

model2 <- glm(y ~ x, family = binomial(), data = d)

coef(model2)

coef(model2)["x"]

model2

summary(model2)

parameters::model_parameters(model2)





#' Odds ratios

##' see: https://stats.oarc.ucla.edu/stata/faq/how-do-i-interpret-odds-ratios-in-logistic-regression/
##' see: https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

parameters::model_parameters(model2, exponentiate = TRUE, digits = 4, remo)


#' The regression coefficient for x (on the logit-scale) is 2.641. 
#' However, for a logistic regression, this “slope” is not constant across all values of x, 
#' because we have non-linear transformations here. This becomes clearer by looking at the predicted probabilities:

predicted_prob <- ggpredict(model2, "x [all]")

  ggplot(d, aes(x, y)) +
  geom_point()

  
plot(predicted_prob, ci = FALSE)

plot(ggpredict(model2, "x [all]"), show_data = T, show_ci = F) # ggplot2 error?


#' Using the `margins` package

margins::marginal_effects(model2)

margins::margins(model2) # only point estimate of AME
margins::margins_summary(model2) # same as marginaleffects::avg_slopes()


#' marginaleffects

library(marginaleffects)


marginaleffects::marginaleffects(model2)
marginaleffects::avg_comparisons(model2)
marginaleffects::avg_predictions(model2)


marginaleffects::avg_slopes(model2) # same as margins::margins_summary() AME estimate



#' The AME estimate (average slopes in marginaleffects) indicates “the contribution of each variable on the outcome scale”, 
#' i.e. the “change in the predicted probability that the outcome equals 1” (see vignettes from the margins package). 
#' On average, a unit-change in x changes the predicted probability that the outcome equals 1 by 15.4%.

#' I personally find it less intuitive to interpret average marginal effects, in particular for non-Gaussian models, 
#' because it is harder to understand an average effect where we actually have varying effects across the range of the focal term. 
#' Instead, I rather prefer to look at predictions at different values of the focal term(s), which is what ggeffects returns by default:


ggpredict(model2, "x")


#' For x = -2, the predicted probability that y = 1, as estimated by our model, is zero. 
#' For x = 10, the probability is 100%. In essence, what ggpredict() returns, are not average marginal effects, 
#' but rather the predicted values at different values of x (possibly adjusted for co-variates, also called non-focal terms). 
#' This makes clear that marginal effects require predictions: 
#' The marginal effect would be the difference between any two adjacent predicted values 
#' (if these are close enough in case of non-linear relationship between our outcome and the focal variable).


#' Example

smoking <- data.frame(
  sex = factor(c("male", "female", "female", "male", "female", "female",
                 "male", "female", "female", "male", "male", "female",
                 "female"),
               levels = c("male", "female")),
  smoking = factor(c("no", "yes", "yes", "yes", "yes", "no", "no", "yes",
                     "yes", "no", "no", "no", "yes"),
                   levels = c("no", "yes")),
  age = c(10, 45, 50, 40, 45, 12, 14, 55, 60, 10, 14, 50, 40)
)

100 * prop.table(table(smoking$sex, smoking$smoking), margin = 3)

model5 <- glm(smoking ~ sex, family = binomial(), data = smoking)

#' Looking at the odds ratio for "sex"
model_parameters(model5, exponentiate = TRUE)

#' Looking at the predicted probabilities for "sex"
ggpredict(model5, "sex")

#' The reference category for sex is male, 
#' so we can estimate the average marginal effects for female persons using margins():


margins::margins(model5)
margins::margins_summary(model5)

marginaleffects::avg_slopes(model5) 



#' Interpretation: 
#' the change in the predicted probability that the outcome equals 1 for female persons is 0.55, i.e. 55%.
#' This is exactly the difference between the predicted probabilities for male and female persons.









#' Probability, odds and log_odds

probs <- c(0.01, 0.05, 0.1, 0.2, 0.33334, 0.4, 0.5, 0.6, 0.66667, 0.8, 0.9, 0.95, 0.99)
odds <- probs / (1 - probs)
log_odds <- log(odds)

p_lagged <- lag(probs, 1)
odds_lagged <- p_lagged / (1 - p_lagged)
odds_ratios <- odds/odds_lagged

table <- data.frame(probs, odds, log_odds)

plot_1 <- ggplot(d, aes(x = log_odds, y = odds)) +
  geom_line() +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
  labs(title = "odds versus log-odds") + 
  theme(text = element_text(size = 25))  

plot_2 <- ggplot(d, aes(x = odds, y = probs)) +
  geom_line() +
  labs(title = "probability versus odds") + 
  theme(text = element_text(size = 25)) 

plot_3 <- ggplot(d, aes(x = log_odds, y = probs)) +
  geom_line() +
  geom_hline(aes(yintercept = 0.5),
             colour = "gray",
             linetype = "dashed") +
  geom_vline(aes(xintercept = 0.0),
             colour = "gray",
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
  labs(title = "probability versus log-odds") + 
  theme(text = element_text(size = 25)) 

plot_4 <- ggplot(d, aes(x = odds_ratios, y = odds)) +
  geom_line() +
  labs(title = "odds versus Odds Ratios") + 
  theme(text = element_text(size = 25)) 

table |> kableExtra::kbl(digits = 2) |> kableExtra::kable_styling(font_size = 25)

library(patchwork)

plot_1 + 
  plot_2 + 
  plot_3

