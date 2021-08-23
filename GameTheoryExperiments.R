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
  group_by(type, round)  %>%
  summarise(av = mean(choice), Icount = sum(choice > 66)/n() * 100, median = median(choice), win = 0.66666 * av, cnt = n()) %>%
  ungroup()

library(data.table)
neworder <- c(1,2, 3, 6, 5, 7, 4)
setcolorder(statistics, neworder)
statistics

# First game visualization 
statistics2 <- data1 %>%
  group_by(id, round)  %>%
  summarise(av = mean(choice), win = 0.66666 * av, Zlevel = sum(choice > 50)/n() * 100, median = median(choice), cnt = n(), h = sum(choice > 90)/n() * 100) %>%
  ungroup()



data1 <- data1[data1$round != 3,]

data2 <- subset(data1, round != 3)

# statistics of choices

ggplot(data2, aes(round, choice)) + 
  geom_boxplot() +
  facet_wrap(.~type, ncol = 3) 

# remove data with only one round

data1 <- subset(data2, type != "Adults")
data1 <- subset(data2, type != "Adults (facebook online)")
data1 <- subset(data1, id != 7)
data1 <- subset(data1, type == "Math lyceum")

# histogram of choices
# add lines of k-level reasoning

spikes <- c("50", "33", "22", "14")

ggplot(data1,aes(choice, fill = round)) + 
  geom_histogram(bins = 20, position="dodge") + 
  geom_vline(xintercept = 50, linetype="dotted", 
             color = "blue", size=0.5) + 
  geom_vline(xintercept = 33, linetype="dotted", 
             color = "blue", size=0.5) + 
  geom_vline(xintercept = 22, linetype="dotted", 
             color = "blue", size=0.5) + 
  geom_vline(xintercept = 14, linetype="dotted", 
             color = "blue", size=0.5) + 
  facet_wrap(.~type, ncol = 3) 

# Calculate number of -1 level, 0 level, 1 - level 

## Gaussian and Student t are much closer to each other than
## to the uniform:
x = subset(data1, id == '2')
x = subset(x, round == '2')
x = x$choice

y = subset(data1, id == '20')
y = subset(y, round == '2')
y = y$choice

ks.test(x,y)


wilcox.test(x, y, paired = TRUE, alternative = "two.sided")

t.test(x, y, alternative="two.sided", var.equal=FALSE)

mded(distr1 = x, distr2 = y, detail = TRUE)
out

number_levels <- data1 %>%
  group_by(id, round,type) %>%
  summarise(level_m_choices= length(choice[choice > 50]), 
            level_1_choices= length(choice[(choice <= 50 & choice > 18)]),
            level_2_choices= length(choice[(choice <= 18 & choice > 5)]),
            level_inf_choices= length(choice[(choice <= 5)]),
            count = n()) %>%
  ungroup()

ggplot(statistics2, aes(h, id, fill = round)) + 
  geom_col(aes(h), position = "dodge")


df <- filter(number_levels, id %in% c(1,2,4,8,9,11,12,14,15,16,17))

df1 <- df %>%
  group_by(id) %>%
  summarise(diff_b = round((level_m_choices[round == 2] - level_m_choices[round == 1]) / mean(count),2) * 100,
            diff_m = round((level_1_choices[round == 2] - level_1_choices[round == 1]) / mean(count),2) * 100,
            diff_h = round((level_2_choices[round == 2] - level_2_choices[round == 1]) / mean(count),2) * 100,
            diff_inf = round((level_inf_choices[round == 2] - level_inf_choices[round == 1]) / mean(count),2) * 100            
            ) %>%
  ungroup()
  
number_levels2 <- subset(number_levels, type == "Alternative humanitarian")

p1 <- ggplot(number_levels2,aes(fill = round)) +  
  geom_boxplot(aes(x = type, y = level_m_choices)) + 
  #facet_wrap(.~id, ncol = 3) + 
  theme_minimal()

p2 <- ggplot(number_levels2,aes(fill = round)) +  
  geom_boxplot(aes(x = type, y = level_1_choices)) + 
  #facet_wrap(.~id, ncol = 3) + 
  theme_minimal()

p3 <- ggplot(number_levels2,aes(fill = round)) +  
  geom_boxplot(aes(x = type, y = level_2_choices)) + 
  #facet_wrap(.~id, ncol = 3) + 
  theme_minimal() 

p4 <- ggplot(number_levels2,aes(fill = round)) +  
  geom_boxplot(aes(x = type, y = level_inf_choices)) + 
  #facet_wrap(.~id, ncol = 3) + 
  theme_minimal()

grid.arrange(p1,p2,p3,p4, nrow = 2)
 
ggplot(number_levels,aes(x = level_1_choices,y = level_2_choices, color = type, shape = round)) +  
  geom_point(aes(x = level_1_choices,y = level_2_choices)) + 
#  facet_wrap(.~type, ncol = 3) + 
  theme_minimal() 

ggplot(number_levels,aes(x = level_2_choices,y = level_inf_choices, color = type, shape = round)) +  
  geom_point(aes(x = level_2_choices,y = level_inf_choices)) + 
#  facet_wrap(.~type, ncol = 3) + 
  theme_minimal()

ggplot(number_levels,aes(x = level_m_choices,y = level_inf_choices, color = type, shape = round,size = 2)) +  
  geom_point(aes(x = level_m_choices,y = level_inf_choices)) + 
  xlab("Choices in [50, 100]") + ylab("Choices in [1, 5]") +
#  facet_wrap(.~type, ncol = 3) + 
  theme_minimal()

# prediction function depending on size
ggplot(statistics2, aes(win, cnt, color = round, size = 2)) + 
  xlab("Winning choice") + ylab("Number of participants") +
  geom_point()

ggplot(statistics2, aes(Zlevel, h, color = round, size = 2)) + 
  xlab("Winning choice") + ylab("Number of participants") +
  geom_point()

# First round vs second round
 choice1 <- subset(statistics2, round == 1)
 choice2 <- subset(statistics2, round == 2)
 
ggplot(choice1, aes(x = win, y = choice2$win)) + 
  geom_point(aes(x = win, y = choice2$win ), size = 5) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Winning choice first round") + 
  ylab("Winning choice second round") + 
  xlim(1,50) + ylim(1,50)
  
#Second game

data2 <- read.csv("resultsGame2.csv")

data2$choice <- as.integer(data2$choice)
  
ggplot(data2, aes(type, choice)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 50) + 
  coord_flip()


statistics3 <- data2 %>%
  group_by(type)  %>%
  summarise(av = mean(choice), MaxChoice = sum(choice == 100)/n() * 100, MinChoice = sum(choice == 1)/n() * 100, cnt = n()) %>%
  ungroup()

ggplot(statistics3, aes(MaxChoice, MinChoice, color = type)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point()


data3 <- read.csv("resultsGame3.csv")

data3$choice <- as.integer(data3$choice)

ggplot(data3,aes(choice)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(.~type, ncol = 3) 





data4 <- read.csv("resultsGame4.csv")

ggplot(data4,aes(choice)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(.~id, ncol = 4) 

123454320/2*2 == 123454320
123454321/2*2 == 123454321
123454322/2*2 == 123454322
