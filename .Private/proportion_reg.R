# Based on Chapter on Beta reg from https://rcompanion.org/handbook/J_02.html

# Example in book

Data = read.table(header=TRUE, stringsAsFactors=TRUE, text="
Class            Grade  Pass  Fail
 'Bully Hill'    12     14     6
 'Keuka Lake'    12     15     5
 'Heron Hill'    11     18     2
 'Castel Grisch' 11     10    10
 'Red Newt'      10     17     3
 'Finger Lakes'  10      9    11
 'Bellview'       9     12     8
 'Auburn Road'    9      8    12
 'Balic'          8     10    10
 'Cape May'       8      8    12
 'Hawk Haven'     7     12     8
 'Natali'         7      4    16
")

# Transform data for logistic regression

Trials = cbind(Data$Pass, Data$Fail)         # Successes, Failures

# Then fit regression with this Trials "integer"
model.log1 = glm(Trials ~ Grade,
                data = Data,
                family = binomial(link="logit"))


# But how to transform the original dataframe?

Data <- add_row(
  Data |> group_by(Class, Grade) |> reframe(Passed= seq(1, Pass)) |> mutate(Passed = 1),
  Data |> group_by(Class, Grade) |> reframe(Passed= seq(1, Fail)) |> mutate(Passed = 0)
  ) |>  arrange(Class)
  

## Example for Trust ~ Inequality data


# Packages and data

pacman::p_load(tidyverse,             # metapackage; general data management (especially the `dplyr` package)
               easystats,             # metapackage; data management and model summaries
               gtsummary,             # data and model summary tables
               ggformula,             # formula syntax to simplify `ggplot2` graphics
               sjlabelled,            # work with labelled survey data from other statistical packages
               ggeffects,             # extract and plot marginal/conditional/interaction effects from models
               marginaleffects,       # extract and plot marginal/conditional/interaction effects from models
               latex2exp,             # math annotation on graphs
               betareg,               # Beta regression for proportional data
               ggpubr,                # GGplot functions (combine and arrange plots on page)
               patchwork              # same
)                


inequality <- data_read("https://github.com/CGMoreh/SOC2069/raw/main/Data/for_analysis/lab3macro.rds")

# remove missing, arrange by Region, set up graphing aesthetics

selected_countries <- c("United Kingdom", "Nicaragua") 

inequality <- inequality |> 
  drop_na(trust_pct, s80s20) |> 
  arrange(Region) |> 
  mutate(
    trust_prop = structure(trust_pct/100, label = "Prop. people agreein that 'most people can be trusted'"),
    non_trust_prop = structure(1-trust_prop, label = "Prop. people agreeing that 'one cannot be too careful'"),
    # aesthetics
    colours = case_when(country == selected_countries[1] ~ "#009C6B",
                        country == selected_countries[2] ~ "#FF8916",
                        TRUE ~ "black"),
    sizes = case_when(country %in% selected_countries ~ 2.5, 
                      TRUE ~ 1.5),
    transparency = case_when(country %in% selected_countries ~ 1,
                             TRUE ~ 0.1)
  )


Trust_logit = cbind(inequality$trust_prop, inequality$non_trust_prop)

test <- glm(Trust_logit ~ s80s20, data = inequality, family = binomial(link = "logit"))
# But also works directly on proportions!!
# 3 ways of providing binomial data: https://stats.stackexchange.com/a/26779/373890
 
mod_prop_logit <- glm(trust_prop ~ s80s20, data = inequality, family = binomial(link = "logit"))
mod_prop_logitw <- glm(trust_prop ~ s80s20, data = inequality, family = binomial(link = "logit"), weights = trust_prop + (1 - trust_prop))




mod <- lm(trust_pct ~ s80s20, data = inequality)

mod_prop_beta <- betareg(trust_prop ~ s80s20, data = inequality)
mod_prop_reg <- lm(trust_prop ~ s80s20, data = inequality)

compare_parameters(test, mod_prop_logit, mod_prop_logitw)


mod_m <- lm(trust_pct ~ s80s20 + I(pop*1e-06) + Region, data = inequality)
mod_x <- lm(trust_pct ~ s80s20*Region + I(pop*1e-06), data = inequality)




plot0 <-  ggeffects::ggpredict(mod, terms = c("s80s20 [all]")) |> 
  plot(limits = c(-25, 60), breaks = round(c(seq(-20, 60, 10)), 10), 
       show_title = FALSE, show_x_title = FALSE, show_y_title = FALSE) +
  labs(title = "Predicted % of Trusting (lm)") +
  theme(plot.title = element_text(size=10))

plot1 <-  ggeffects::ggpredict(mod_prop_reg, terms = c("s80s20 [all]")) |> 
  plot(limits = c(-.25, .6), breaks = round(c(seq(-0.2, 0.6, 0.1)), 1), 
       show_title = FALSE, show_x_title = FALSE, show_y_title = FALSE) +
  labs(title = "Predicted proportion of Trusting (lm)") +
  theme(plot.title = element_text(size=10))

plot2 <-  ggeffects::ggpredict(mod_prop_beta, terms = c("s80s20 [all]")) |> 
  plot(limits = c(-.25, .6), breaks = round(c(seq(-0.2, 0.6, 0.1)), 1), 
       show_title = FALSE, show_x_title = FALSE, show_y_title = FALSE) +
  labs(title = "Predicted proportion of Trusting (betareg)") +
  theme(plot.title = element_text(size=10))

plot3 <- ggeffects::ggpredict(mod_logit, terms = c("s80s20 [all]")) |> 
  plot(show_title = FALSE, show_x_title = FALSE, show_y_title = FALSE) +
  scale_y_continuous(labels = scales::number, 
                     limits = c(-.25, .6), breaks = round(c(seq(-0.2, 0.6, 0.1)), 1)) +
  labs(title = "Predicted probabilities of Trusting (logit)") +
  theme(plot.title = element_text(size=10))


ggarrange(plot0, plot1, plot2, plot3) |> annotate_figure(bottom = "Inequality", left = "Trust")


inequality <- inequality |> 
  data_modify(fitted = case_when(country %in% selected_countries ~ mod$fitted.values))

compare_parameters(mod, mod_m, mod_x, mod_prop)