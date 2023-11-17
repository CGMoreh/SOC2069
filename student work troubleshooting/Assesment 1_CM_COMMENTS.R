install.packages("gtsummary")
library(gtsummary)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggformula")
library(ggformula)
install.packages("easystats")
library(easystats)
install.packages("ggplot")
library(ggplot2)

evs2017 <- data(evs2017, convert_factors = FALSE)     
#######################################  ABOVE SCODE IS NOT CORRECT
ex1 <- evs2017 |> 
  select(v199, v200, v201, v202, v203, v204) 
###################################### ABOVE YOU ARE NOT INCLUDING THE GENDER/SEX VARIABLE, SO YOU DON'T HAVE ACCESS TO IT IN THE REDUCED DATASET! YOU NEED TO ALSO KEEP GENDER
str(ex1)

# Bar Chart: Willingness to Take Personal Action by Gender
ggplot(data = ex1, aes(x = v200, fill = v202)) +
  geom_bar(position = "dodge") +
  labs(title = "Willingness to Take Personal Action by Gender",
       # x = "Gender",  
       ####################################  ABOVE CODE IS NOT CORRECT; HERE YOU ARE JUST ADDING A LABEL, WHICH CAN BE ANY TEXT; YOU ARE NOT ADDING ANY DATA ON GENDER TO THE PLOT.
       # y = "Count"
       )

# Scatter Plot: Relationship between Age and Belief in Human-Induced Climate Change
ggplot(data = ex1, aes(x = v199, y = v201)) +
  geom_point() +
  labs(title = "Relationship between Age and Belief in Human-Induced Climate Change",
       x = "Age",
       y = "Belief in Human-Induced Climate Change")

# Boxplot: Cost of Climate Change Mitigation by Socio-Economic Status
ggplot(data = ex1, aes(x = v203, y = v204)) +
  geom_boxplot() +
  labs(title = "Cost of Climate Change Mitigation by Socio-Economic Status",
       x = "Socio-Economic Status",
       y = "Cost of Climate Change Mitigation")

# Linear Regression: Willingness to Take Personal Action by Age and Gender
model1 <- lm(v200 ~ v199 + v202, data = ex1)
summary(model1)

# Linear Regression: Belief in Human-Induced Climate Change by Age
model2 <- lm(v201 ~ v199, data = ex1)
summary(model2)

# Linear Regression: Cost of Climate Change Mitigation by Socio-Economic Status
model3 <- lm(v204 ~ v203, data = ex1)
summary(model3) 

install.packages("ggpredect") 
####################################### ABOVE CODE IS INCRRECT; THERE IS NO "GGPREDECT" PACKAGE
library(ggpredect)
# Select the variables needed for this exercise:
ex1 <- evs2017 |> 
  select(
    v199,  # I would give part of my income if I were certain that the money would be used to prevent environmental pollution
    v200,  # It is just too difficult for someone like me to do much about the environment
    v201,  # There are more important things to do in life than protect the environment
    v202,  # There is no point in doing what I can for the environment unless others do the same
    v203,  # Many of the claims about environmental threats are exaggerated
    v204   # Protecting the environment vs. Economic growth and job creation
  )

# `View` the codebook for the selected variables:
data_codebook(ex1) |> View()

# Recode variables to match the specified categories:
ex1 <- evs2017 |> 
  ############################### THIS IS NOT WHAT YOU WANT!!!! WITH THIS CODE YOU ARE OVERWRITING THE ex1 SMALL DATASET WITH ALL VARIABLES FROM evs2017 AGAIN.....
  
  data_modify(
    v199 = recode_values(v199,
                         recode = list("strongly agree" = 1,
                                       "agree" = 2,
                                       "neither agree nor disagree" = 3,
                                       "disagree" = 4,
                                       "strongly disagree" = 5,
                                       "DK" = 8,
                                       "NA" = 9)),
    v200 = recode_values(v200,
                         recode = list("strongly agree" = 1,
                                       "agree" = 2,
                                       "neither agree nor disagree" = 3,
                                       "disagree" = 4,
                                       "strongly disagree" = 5,
                                       "DK" = 8,
                                       "NA" = 9)),
    v201 = recode_values(v201,
                         recode = list("strongly agree" = 1,
                                       "agree" = 2,
                                       "neither agree nor disagree" = 3,
                                       "disagree" = 4,
                                       "strongly disagree" = 5,
                                       "DK" = 8,
                                       "NA" = 9)),
    v202 = recode_values(v202,
                         recode = list("strongly agree" = 1,
                                       "agree" = 2,
                                       "neither agree nor disagree" = 3,
                                       "disagree" = 4,
                                       "strongly disagree" = 5,
                                       "DK" = 8,
                                       "NA" = 9)),
    v203 = recode_values(v203,
                         recode = list("strongly agree" = 1,
                                       "agree" = 2,
                                       "neither agree nor disagree" = 3,
                                       "disagree" = 4,
                                       "strongly disagree" = 5,
                                       "DK" = 8,
                                       "NA" = 9)),
    v204 = recode_values(v204,
                         recode = list("Protecting environment should be given priority" = 1,
                                       "Economic growth and job creation should be the top priority" = 2,
                                       "Other answer" = 3,
                                       "DK" = 8,
                                       "NA" = 9))
  )

# `View` the codebook for the recoded dataset:
data_codebook(ex1) |> View()                            ###### THIS WORKS FOR ME
# Fit a multiple regression model with interactions:
mex1 <- lm(willingness ~ age * socio_economic_status * gender, data = ex5.1) ################ THIS GIVES AN ERROR BECAUSE YOU DO NOT HAVE THE "ex5.1" DATA OBJECT DEFINED; THIS IS JUST A COPY-PASTE FROM A WORKSHEET; YOU ONLY HAVE "eX1" DEFINED IN THIS SCRIPT

mex1 <- lm(willingness ~ age + socio_economic_status + socio_economic_status*willingness , data = ex1)


############## YOU ALSO DO NOT HAVE A VARIABLE CALLED "willingness" DEFINED IN THE SCRIPT, SO YOU CANNOT USE AN OBJECT THAT DOES NOT EXIST. tHAT'S THE ISSUE


model_parameters(mex1)
ggpredict(mex1, terms = c("socio_economic_status", "willingness")) |> 
   plot()
ggpredict(mex1, terms = c("willingness", "socio_economic_status")) |> 
  plot()
ggpredict(mex1, terms = c("willingness", "socio_economic_status")) |> 
  plot(connect.lines = TRUE)

