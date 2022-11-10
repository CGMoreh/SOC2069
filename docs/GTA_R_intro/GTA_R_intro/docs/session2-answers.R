# Challenge activities

#Work on these challenges on your own or in small groups this afternoon and we will go over possible solutions together at 16:00.

## Exercise 1

#Plot where earthquakes occur using latitude and longitude columns in `quakes`.
#```{r ex1a, warning=FALSE, message=FALSE}
quakes %>%
  ggplot(aes(x=long,y=lat)) +
  geom_point()
#```

#Take this plot and colour points differently if they are below 175 and above 175. You will need to add a new column to the dataset that you pipe into your ggplot. Look at the chunk labeled `group work 1` for hints.
#```{r ex1b, warning=FALSE, message=FALSE}
# use this line of code at the end of your code to choose the two colours you use
# scale_colour_manual(values=c("red", "blue"))
quakes %>%
  mutate(group = case_when(long<175~"low",
                           TRUE~"high")) %>%
  ggplot(aes(x=long,y=lat, colour=group)) +
  geom_point() +
  scale_colour_manual(values=c("red", "blue"))
#```

## Exercise 2

#Using the code in the chunk labelled `low-med-high`, make a box and whisker plot of how deep each of your groups in `magText` are.
#```{r ex2}
quakes %>% #
  mutate(magText = case_when(mag <  "5" ~ "four",
                             mag >= "6" ~ "six",
                             TRUE ~ "five")) %>%
  ggplot(aes(x=magText, y=depth)) +
  geom_boxplot(aes(fill=magText))
#```

## Exercise 3

#Read through the code below and make comments on it. Your comments should help you make sense of what it is doing. Use Google and the help documents (as well as anything else you can think of) to reverse-engineer the code.
#```{r ex3, message=FALSE, echo=TRUE}
quakes %>%
  group_by(mag) %>%
  summarise(numberOfRows = n(),
            stationMean = mean(stations),
            stationSD = sd(stations),
            stationSE = stationSD/sqrt(numberOfRows),
            depthMean = mean(depth),
            depthSD = sd(depth),
            depthSE = depthSD/sqrt(numberOfRows)) %>%
  ggplot(aes(x = mag)) +
  geom_point(aes(y = stationMean),
             colour="red")+
  geom_path(aes(y = stationMean),
            colour = "red")+
  geom_ribbon(aes(ymin = stationMean - stationSD,
                  ymax = stationMean + stationSD),
              fill="red",
              alpha=.2) +
  geom_ribbon(aes(ymin = stationMean - stationSE,
                  ymax = stationMean + stationSE),
              fill = "red",
              alpha = .5) +
  geom_point(aes(y = depthMean),
             colour = "blue")+
  geom_path(aes(y = depthMean),
            colour = "blue")+
  geom_ribbon(aes(ymin = depthMean - depthSD,
                  ymax = depthMean + depthSD),
              fill = "blue",
              alpha = .2) +
  geom_ribbon(aes(ymin = depthMean - depthSE,
                  ymax = depthMean + depthSE),
              fill = "blue",
              alpha = .5) +
  theme_bw() +
  ylab("depth OR number of stations reporting") +
  ggtitle("How does magnitude interact with depth and number of stations reporting?")
#```

