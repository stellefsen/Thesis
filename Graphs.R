library(tidyverse)
library(psych)
library(jtools)

#Long dataset
kinship_long <- read_csv("kinship_long.csv")
kinship_long$Relationship <- factor(kinship_long$Relationship)

#Wide dataset
kinshipstudy <- read_csv("kinship_wide.csv")

#TimeGiven ~ Relationship and Culture
#Individualism

TimeIndPlot <- ggplot(kinship_long, aes(x = IND_C,
                                        y = TimeGiven,
                                        fill = Relationship)) + 
  geom_point(data = kinship_long,
             aes(color = Relationship)) + #Color of the points (dots) = which Relationship
  geom_smooth(method = 'lm', formula = y ~ x) + #Graph lines according to TimeGiven ~ IND_C
  theme_apa(legend.use.title = TRUE, #Makes the graph in APA style
            legend.font.size = 12, #Can change font sizes to what you want
            x.font.size = 20,
            y.font.size = 20) +
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 14),
        strip.text.y = element_text(size = 18)) +
  expand_limits(y = c(0, 14)) + #Since range of time is 0 to 14 hours
  scale_x_continuous(breaks = seq(-3, 2, by = 1)) + #Show full range of IND_C
  scale_y_continuous(breaks = seq(0, 14, by = 2)) + #Show full range of TimeGiven, tick marks in 2's
  xlab("\nIndividualism (centered)") +
  ylab("Time Given to Each Relationship (Hours)")

TimeIndPlot

#Save the plot as actual image files

#SVG: Can resize image and keep the quality, but can only use in certain programs
ggsave(TimeIndPlot, width = 10, height = 10, filename = "TimeIndPlot.svg",  bg = "transparent")
#PNG: Like a normal image file, but it means might lose image quality if make too big
ggsave(TimeIndPlot, width = 10, height = 10, filename = "TimeIndPlot.png",  bg = "transparent")

#Collectivism
TimeColPlot <- ggplot(kinship_long, aes(x = COL_C,
                                        y = TimeGiven,
                                        fill = Relationship)) + 
  geom_point(data = kinship_long,
             aes(color = Relationship)) +
  geom_smooth(method = 'lm', formula = y ~ x) +
  theme_apa(legend.use.title = TRUE,
            legend.font.size = 12,
            x.font.size = 20,
            y.font.size = 20) +
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 14),
        strip.text.y = element_text(size = 18)) +
  expand_limits(y = c(0, 14)) +
  scale_x_continuous(breaks = seq(-3, 2, by = 1)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  xlab("\nCollectivism (centered)") +
  ylab("Time Given to Each Relationship (Hours)")

TimeColPlot

#SVG
ggsave(TimeColPlot, width = 10, height = 10, filename = "TimeColPlot.svg",  bg = "transparent")
#PNG
ggsave(TimeColPlot, width = 10, height = 10, filename = "TimeColPlot.png",  bg = "transparent")

#MoneyGiven ~ Relationship and Culture
#Individualism
MoneyIndPlot <- ggplot(kinship_long, aes(x = IND_C,
                                        y = MoneyGiven,
                                        fill = Relationship)) + 
  geom_point(data = kinship_long,
             aes(color = Relationship)) +
  geom_smooth(method = 'lm', formula = y ~ x) +
  theme_apa(legend.use.title = TRUE,
            legend.font.size = 12,
            x.font.size = 20,
            y.font.size = 20) +
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 14),
        strip.text.y = element_text(size = 18)) +
  #expand_limits(y = c(0, 2000)) +
  scale_x_continuous(breaks = seq(-3, 2, by = 1)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 200)) +
  xlab("\nIndividualism (centered)") +
  ylab("Money Given to Each Relationship ($)")

MoneyIndPlot

#SVG
ggsave(MoneyIndPlot, width = 10, height = 10, filename = "MoneyIndPlot.svg",  bg = "transparent")
#PNG
ggsave(MoneyIndPlot, width = 10, height = 10, filename = "MoneyIndPlot.png",  bg = "transparent")


#Collectivism
MoneyColPlot <- ggplot(kinship_long, aes(x = COL_C,
                                         y = MoneyGiven,
                                         fill = Relationship)) + 
  geom_point(data = kinship_long,
             aes(color = Relationship)) +
  geom_smooth(method = 'lm', formula = y ~ x) +
  theme_apa(legend.use.title = TRUE,
            legend.font.size = 12,
            x.font.size = 20,
            y.font.size = 20) +
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 14),
        strip.text.y = element_text(size = 18)) +
  #expand_limits(y = c(0, 2000)) +
  scale_x_continuous(breaks = seq(-3, 2, by = 1)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 200)) +
  xlab("\nCollectivism (centered)") +
  ylab("Money Given to Each Relationship ($)")

MoneyColPlot

#SVG
ggsave(MoneyColPlot, width = 10, height = 10, filename = "MoneyColPlot.svg",  bg = "transparent")
#PNG
ggsave(MoneyColPlot, width = 10, height = 10, filename = "MoneyColPlot.png",  bg = "transparent")
