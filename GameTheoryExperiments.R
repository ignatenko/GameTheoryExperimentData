# libraries

library(ggplot2)
library(tidyverse)


# Set the path to working directory

setwd("/Users/anymac/Documents/GitHub/GameTheoryExperimentData")

getwd()# Read the data

data1 <- read.csv("resultsGame1.csv")

summary(data1)

data1$id <- factor(data1$id)
data1$name <- factor(data1$name)
data1$age <- factor(data1$age)
data1$round <- factor(data1$round)
class(data1$round)

  
# First game visualization 
statistics <- data1 %>%
  group_by(id, round)  %>%
  summarise(av = mean(choice), Icount = sum(choice > 66)/n() * 100, median = median(choice), win = 0.66666 * av, cnt = n()) %>%
  ungroup()

# First game visualization 
statistics2 <- data1 %>%
  group_by(type, round)  %>%
  summarise(av = mean(choice), Icount = sum(choice > 66)/n() * 100, median = median(choice), win = 0.66666 * av, cnt = n()) %>%
  ungroup()


data <- data1[data1$round != 3,]

data2 <- subset(data1, round != 3)

ggplot(data2, aes(round, choice)) + 
  geom_boxplot() + 
  facet_wrap(.~type, ncol = 3)


schools <- c()

ggplot(data1,aes(choice, fill = round)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(.~type, ncol = 3) 

 choice1 <- subset(statistics2, round == 1)
 choice2 <- subset(statistics2, round == 2)
 
ggplot(choice1, aes(x = choice1$win, y = choice2$win, color = type)) + 
  geom_point(aes(x = choice1$win, y = choice2$win )) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Winning choice first round") + 
  ylab("Winning choice second round") + 
  xlim(1,50) + ylim(1,50)
  
#Second game

data2 <- read.csv("resultsGame2.csv")

data2$choice <- as.integer(data2$choice)
  
ggplot(data2, aes(type, choice)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 50)


statistics3 <- data2 %>%
  group_by(type)  %>%
  summarise(av = mean(choice), MaxChoice = sum(choice == 100)/n() * 100, MinChoice = sum(choice == 1)/n() * 100, cnt = n()) %>%
  ungroup()

data3 <- read.csv("resultsGame3.csv")

data3$choice <- as.integer(data3$choice)

ggplot(data3,aes(choice)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(.~id, ncol = 3) 

data4 <- read.csv("resultsGame4.csv")

ggplot(data4,aes(choice)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(.~id, ncol = 4) 

