# Question#1
library(tidyverse)
#Importing the data and viewing it
data("ToothGrowth")
View(ToothGrowth)

#Checking the data
summary(ToothGrowth)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

plotly::ggplotly(ggplot(ToothGrowth, aes(x = dose,
                                         y = len )) + geom_boxplot())
ggplot(ToothGrowth, aes(x = supp,
                        y = len )) + geom_boxplot()
#Hypothesis Testing:
t.test(subset(ToothGrowth, dose == 0.5)$len,
       subset(ToothGrowth, dose == 1)$len,
       alternative = "less")
#The p value we get from this testing is 6.342e-08

t.test(subset(ToothGrowth, dose == 1)$len,
       subset(ToothGrowth, dose == 2)$len,
       alternative = "less")
#The p value we get from this testing is 9.532e-06
#As we can see that as the dosage increases the the length of the teeth also increases.

t.test(subset(ToothGrowth, supp == "OJ")$len,
       subset(ToothGrowth, supp == "VC")$len,
       alternative = "greater")
#The p value we get from this testing is 0.03032
#We can see that supplement OJ has a more effect on the teeth growth than VC.


#Question#2
#Importing the data and viewing it
data_fish_diet <- read.csv("E:/KarachiAI Course/CSV Files/fish-diet.csv", header=TRUE)
view(data_fish_diet)

ggplot(data_fish_diet, aes(x = fish_in_diet, fill = cancer)) + geom_bar(position = 'stack')

#We can see from the graph that the people who don't eat fish had cancer and their number is more than the people who eats fish.

#Hypothesis testing:

chisq.test(table(data_fish_diet$fish_in_diet, data_fish_diet$cancer))
#The value of p-value is 0.2985 which tell us that there is no relation between the cancer and fish in diet.
