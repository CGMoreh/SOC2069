# Libraries
library(tidyverse)
library(ggrepel)
library(strengejacke)
library(easystats)


# library(gridExtra)
# library(plotly)
# library(hrbrthemes)
# library(viridis)
# library(gapminder)

# devtools::install_github("piersyork/owidR")
# library(owidR)


##### SSC7001 logo (YSJ) -------------------------------------------------------------------

a <- c("S", "S", "C", "7", "0", "0", "1", "M", 
       "A", "D", "V", "A", "N", "C", "E", "D", 
       "R", "E", "S", "E", "A", "R", "C", "H", 
       "M", "E", "T", "H", "O", "D", "S", "1")

y <- c(5,5,5,5,5,5,5,5,
       4,4,4,4,4,4,4,4,
       3,3,3,3,3,3,3,3,
       2,2,2,2,2,2,2,2       )

x <- c(2,3,4,5,6,7,8,9,
       2,3,4,5,6,7,8,9,
       2,3,4,5,6,7,8,9,
       2,3,4,5,6,7,8,9)

group <- c("a","a","a","b","b","b","b","a","c","c","c","c","c","c","c","c",
           "d","d","d","d","d","d","d","d","e","e","e","e","e","e","e","b")

group2 <- c(1,1,1,2,2,2,2,1,
            3,3,3,3,3,3,3,3,
            4,4,4,4,4,4,4,4,
            5,5,5,5,5,5,5,2)



logo_data <- tibble(a, y, x, group)

cols <- c("1" = "#333300", "2" = "#990000", "3" = "#003300", "4" = "#003333", "5" = "#000033")


## Logo plots

ggplot(logo_data, aes(x, y, label=a))  +
    geom_point(aes(color = group, fill = group),
    shape = 22,
    size = 30) +
  geom_text(aes(label=a), size=20) +   
  scale_color_manual(values = c("#C4961A", "#FC4E07", "#D16103", "#52854C", "#293352"))


# + expand_limits(x=1, y=1)


ggplot(logo_data, aes(x, y)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_point(aes(color = group), shape = 15, size = 25, show.legend = F) +
  scale_color_manual(values = c("#C4961A", "#FC4E07", "#D16103", "#52854C", "#293352")) +
  geom_text(aes(label=a), size=20, 
            colour = ifelse(logo_data$group=="c" | logo_data$group=="e", "white", "black"),
            fontface = 2) + 
  expand_limits(x=c(1.8,9.2), y=c(1.6, 5.4)) +
  scale_x_continuous(breaks=seq(1.5, 9.5, 0.5)) +
  scale_y_continuous(breaks=seq(1.5, 5.5, 0.5))

### Front slide logo

ggplot(logo_data, aes(x, y)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_point(aes(color = group), shape = 15, size = 15, show.legend = F) +
  scale_color_manual(values = c("#C4961A", "#FC4E07", "#D16103", "#52854C", "#293352")) +
  geom_text(aes(label=a), size=12, 
            colour = ifelse(logo_data$group=="c" | logo_data$group=="e", "white", "black"),
            fontface = 2) + 
  expand_limits(x=c(-10.1, 9.1), y=c(-5, 5.2)) +
  scale_x_continuous(breaks=seq(-10, 10, 1)) +
  scale_y_continuous(breaks=seq(-10, 6, 1))

### Front slide logo2

ggplot(logo_data, aes(x, y)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(aes(color = group), shape = 15, size = 11, show.legend = F) +
  scale_color_manual(values = c("#C4961A", "#FC4E07", "#D16103", "#52854C", "#293352")) +
  geom_text(aes(label=a), size=9, 
            colour = ifelse(logo_data$group=="c" | logo_data$group=="e", "white", "black"),
            fontface = 2) + 
  expand_limits(x=c(-11, 9), y=c(-10, 5.2)) +
  scale_x_continuous(breaks=seq(-11, 9, 2)) +
  scale_y_continuous(breaks=seq(-10, 6, 2))

# ggsave("ssc7001_logo.png")






#####
#####
##### SOC2069 logo --------------------------------------------------------------


a <- c("R", "E", "S", "E", "A", "R", "C", "H", "I", "N", "G",
       "S", "O", "C", "I", "A", "L", "L", "I", "F", "E", "1",
       " ", " ", "S", "O", "C", "2", "0", "6", "9", " ", " ")

y <- c(3,3,3,3,3,3,3,3,3,3,3,
       2,2,2,2,2,2,2,2,2,2,2,
       1,1,1,1,1,1,1,1,1,1,1 )

x <- c(2,3,4,5,6,7,8,9,10,11,12,
       2,3,4,5,6,7,8,9,10,11,12,
       2,3,4,5,6,7,8,9,10,11,12)

group <- c("a","a","a","a","a","a","a","a","a","a","a",
           "b","b","b","b","b","b","c","c","c","c","d",
           " "," ","b","b","b","d","d","d","d"," "," ")

logo_data <- tibble(a, y, x, group)


# logo 

ggplot(logo_data, aes(x, y)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_point(aes(color = group), shape = 15, size = 25, show.legend = F) +
  scale_color_manual(values = c("#99999900", "#C4961A", "#293352", "#52854C", "#D16103" )) + 
  geom_text(aes(label=a), size=20, 
            colour = ifelse(logo_data$group=="b" | logo_data$group=="d", "#FCFFFFFF", "#1e1e1e"),
            fontface = 2) + 
  expand_limits(x=c(1.8,12.2), y=c(0.6, 3.4)) +
  # scale_x_continuous(breaks=seq(1.5, 12.5, 0.5)) +
  # scale_y_continuous(breaks=seq(0.5,3.5,0.5)) +
  scale_y_continuous(breaks=seq(0, 5 , 1), limits = c(-0.5, 4.5)) +
  scale_x_continuous(breaks=seq(-2, 16, 1), limits = c(-2, 16)) +
  # theme_void() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        #panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        # plot.margin = margin(t = 0,  # Top margin
        #                      r = 0,  # Right margin
        #                      b = 0,  # Bottom margin
        #                      l = 0) # Left margin
  )

dev.print(png, "SOC2069_logo.png", width = 890, height = 260, bg = "transparent", res = 95.5)

dev.print(png, "SOC2069_logo_noaxis.png", width = 890, height = 260, bg = "transparent", res = 95.5)

### Empty ggplot canvas for backgrounds

ggplot(logo_data, aes(x, y)) +
  expand_limits(x=c(-15.1, 10.1), y=c(-10, 3.5)) +
  scale_x_continuous(breaks=seq(-15, 12, 1)) +
  scale_y_continuous(breaks=seq(-10, 6, 1)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

dev.print(png, "ggplot_background.png", width = 1200, height = 675, bg = "transparent", res = 100)

### can also change the colouring of the background
p + theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'),
          panel.grid.major = element_line(color = 'red', linetype = 'dotted'),
          panel.grid.minor = element_line(color = 'green', size = 2))

# Simple front slide logo

ggplot(logo_data, aes(x, y)) +
  geom_point(aes(color = group), shape = 15, size = 15, show.legend = F) +
  scale_color_manual(values = c("#99999900", "#C4961A", "#293352", "#52854C", "#D16103" )) + 
  geom_text(aes(label=a), size=11, 
            colour = ifelse(logo_data$group=="b" | logo_data$group=="d", "white", "#1e1e1e"),
            # colour = ifelse(logo_data$group=="b" , "#d16103",
            #                 ifelse(logo_data$group== "d", "#293352", 
            #                 "white")),
            fontface = 2) + 
  expand_limits(x=c(-15.1, 12), y=c(-10, 3.5)) +
  scale_x_continuous(breaks=seq(-16, 15, 1)) +
  scale_y_continuous(breaks=seq(-10, 5, 1)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )

dev.print(png, "SOC2069_slide_logo.png", width = 1407, height = 725, bg = "transparent", res = 100)

#### Data logos ------------------------------

# Gapminder slide logo

data <- gapminder %>% 
  filter(year=="2007") %>% 
  dplyr::select(-year)

data %>%
  mutate(pop=pop/100000,
         lifeExp=lifeExp/10,
         # lifeExp = ifelse(gdpPercap > 9100, lifeExp*1.1, lifeExp),
         # lifeExp = ifelse(gdpPercap > 15000, lifeExp*1.15, lifeExp),
         # lifeExp = ifelse(gdpPercap < 2000, lifeExp*0.9, lifeExp),
         # lifeExp = ifelse(lifeExp > 7, lifeExp*1.15, lifeExp),
         # lifeExp = ifelse(lifeExp > 6, lifeExp*1.05, lifeExp),
         gdpPercap=gdpPercap/2000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.8) +
  scale_size(range = c(3, 20), name="Population (M)") +
  scale_colour_manual(values=c("#293352", "#C4961A", "#52854C", "#D16103", "#99999900")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none") +
  scale_x_continuous(breaks = seq(0, 35, 1.8), expand = c(0, 0.1)) +
  scale_y_continuous(breaks = seq(0, 8, 0.7), expand = c(0, 0.15)) +
  expand_limits(x=c(0, 25), y=c(5.5, 8))
  
dev.print(png, "gapminder_background.png", width = 1407, height = 725, bg = "transparent", res = 100)



data %>%
  mutate(pop=pop/100000,
         lifeExp=lifeExp/10,
         gdpPercap=gdpPercap/2000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(y=gdpPercap, x=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.8) +
  scale_size(range = c(3, 20), name="Population (M)") +
  scale_colour_manual(values=c("#293352", "#C4961A", "#52854C", "#D16103", "#99999900")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none") +
  scale_y_continuous(breaks = seq(0, 25, 5), expand = c(0, 0.4)) +
  scale_x_continuous(breaks = seq(0, 8, 0.4), expand = c(0, 0.1)) +
  expand_limits(y=c(3, 25), x=c(4, 8))

dev.print(png, "gapminder_background_inverse.png", width = 1407, height = 725, bg = "transparent", res = 100)



# WVS trust/gdp logo

data <- read_rds("Data/data_for_logo.rds") |> 
  mutate(gdp = GDPpercap2,
         ln_gdp = log(gdp),
         lnx_gdp = log(log(log(gdp))),
         exp_trust = (trust_pct^2)
         ) |>
  drop_na()


data %>%
  filter(country != "China") |> 
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x=lnx_gdp, y=exp_trust, size = pop, color = Region)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(3, 15), name="Population") +
  scale_colour_manual(values=c("#293352", "#C4961A", "#52854C", "#D16103", "#99999900", "#9a372d", "#2d9aa4")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none"
  ) +
  scale_x_continuous(breaks = seq(0.7, 0.88, 0.02), limits = c(0.7, 0.88)) +
  scale_y_continuous(breaks = seq(0, 6050, 1000), limits = c(0, 6050))
# xlim(7.5, 11.15) +
# expand_limits(x=c(7.5, 11), y=c(0, 80))

dev.print(png, "site_pics/trust_background.png", width = 1407, height = 725, bg = "transparent", res = 100)



data %>%
  filter(country != "China") |> 
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x=trust_pct, y=ln_gdp, size = pop, color = Region)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(3, 15), name="Population") +
  scale_colour_manual(values=c("#293352", "#C4961A", "#52854C", "#D16103", "#99999900", "#9a372d", "#2d9aa4")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none"
  ) +
  scale_y_continuous(breaks = seq(7.5, 11.2, 0.6), limits = c(7.5, 11.1)) +
  scale_x_continuous(breaks = seq(0, 80, 10))
# xlim(7.5, 11.15) +
# expand_limits(x=c(7.5, 11), y=c(0, 80))

dev.print(png, "site_pics/inverse_trust_background.png", width = 1407, height = 725, bg = "transparent", res = 100)









#####
#####
##### NEW TITLE LOGO "Sociological Methodology Applied to Trust" ------------------------------------------------------------

a <- c("?", "!", "|", ">", "A", "P", "P", "L", "I", "E", "D", "(", 
       "S", "O", "C", "I", "O", "L", "O", "G", "I", "C", "A", "L", 
       "M", "E", "T", "H", "O", "D", "O", "L", "O", "G", "Y", ")",
       "_", "|", ">", "T", "O", "(", "T", "R", "U", "S", "T", ")")

y <- c(5,5,5,5,5,5,5,5,5,5,5,5, 
       4,4,4,4,4,4,4,4,4,4,4,4,
       3,3,3,3,3,3,3,3,3,3,3,3,
       2,2,2,2,2,2,2,2,2,2,2,2)

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,
       1,2,3,4,5,6,7,8,9,10,11,12,
       1,2,3,4,5,6,7,8,9,10,11,12,
       1,2,3,4,5,6,7,8,9,10,11,12)

group <- c("y", "y", "z", "z", "a", "a", "a", "a", "a", "a", "a", "q",
           "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
           "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "q",
           "", "z", "z", "a", "a", "q", "c", "c", "c", "c", "c", "q")

group2 <- c(9,9,8,8,1,1,1,1,1,1,1,7,
            2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,7,
            0,8,8,1,1,7,3,3,3,3,3,7)


logo_data <- tibble(a, y, x, group)

colours <- c( "9" = "#a90606",
              "8" = "#638cb3",
              "1" = "#8c8279",
              "7" = "#cf9930",
              "2" = "#160202",
              "0" = "#cf9930",
              "3" = "#aaaaaa")
  
  
ggplot(logo_data, aes(x, y)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(aes(color = group), shape = 15, size = 20, show.legend = F) +
  scale_color_manual(values = c("#a90606", "#638cb3", "#8c8279", "#cf9930", "#160202", "#cf9930", "#aaaaaa")) +
  geom_text(aes(label=a), size=9, 
            colour = ifelse(logo_data$group=="c" | logo_data$group=="e", "white", "black"),
            fontface = 2) +



  expand_limits(x=c(-11, 9), y=c(-10, 5.2)) +
  scale_x_continuous(breaks=seq(-11, 9, 2)) +
  scale_y_continuous(breaks=seq(-10, 6, 2))
