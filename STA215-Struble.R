# Set Working Directory
setwd("H:/sta215")

# Load "haven" package
library("haven")

# Load "dplyr" package for data without outliers
library(dplyr)

#Load raw_data 
raw_data<-read.csv("raw_data.csv")

#Table 1: Descriptive Statistics

#Quantitative Variable 1
mean(raw_data$dexter_inner_thoughts)
sd(raw_data$dexter_inner_thoughts)
summary(raw_data$dexter_inner_thoughts)

#Quantitative variable 2
mean(raw_data$dead_body_count)
sd(raw_data$dead_body_count)
summary(raw_data$dead_body_count)

#Quantitative variable 3
mean(raw_data$detectives_at_scence)
sd(raw_data$detectives_at_scence)
summary(raw_data$detectives_at_scence)

#Qualitative Variable 1
table(raw_data$lighting)
summary(raw_data$lighting)

#Qualitative Variable 2
table(raw_data$tension)
summary(raw_data$tension)

#Qualitative Variable 3
table(raw_data$gruesomeness)
summary(raw_data$gruesomeness)

#Table 2: Contingency Table
table(raw_data$tension,raw_data$lighting)
chisq.test(raw_data$tension,raw_data$lighting)
#chi-squared test returns a warning message because there are cells in the table that are less than 5 

#Figure 1: Box Plot
model <- lm(dexter_inner_thoughts ~ gruesomeness, data = raw_data)
summary(model)
aov_result <- aov(dexter_inner_thoughts ~ gruesomeness, data = raw_data)
summary(aov_result)
boxplot(dexter_inner_thoughts ~ gruesomeness, data = raw_data, xlab = "Gruesome?", ylab = "Dexter's Inner Thoughts", main = "Boxplot of Dexter's Inner Thoughts and Gruesomeness", col = "red") 

#Figure 2: Scatter Plot
plot(dead_body_count ~ number_characters, data = raw_data, xlab = "Number of Characters", ylab = "Dead Body Count", main = "Scatterplot of Number of Characters and Dead Bodies")

# Add x line and y line for means
meany <- mean(raw_data$dead_body_count)
meanx <- mean(raw_data$number_characters)

abline(v = meanx, lty=2, col = "black")
abline(h = meany,lty=2, col = "black")

# Add the linear regression line to the scatter plot
linear_relationship <- lm(dead_body_count ~ number_characters, data = raw_data)
summary(linear_relationship)

abline(linear_relationship, col = "red")

#Remove outliers for the number of characters
#Done to see if the presence of outliers affect the result
dataset_withoutoutlier <- raw_data %>%
  filter(raw_data$number_characters < 15)

#New plot without outliers
plot(dead_body_count ~ number_characters, data = dataset_withoutoutlier, xlab = "Number of Characters", ylab = "Dead Body Count", main = "Scatterplot of Number of Characters and Dead Bodies Without Outliers")

#Add linear regression line to new plot
linear_regression <- lm(dead_body_count ~ number_characters, data = dataset_withoutoutlier)
summary(linear_regression)

abline(linear_regression, col = "red")


#Figure 3: Residual Plot
# Plot the residuals
plot(raw_data$number_characters, residuals(linear_relationship), main = "Residuals vs. Number of Characters", xlab = "Number of Characters", ylab = "Residuals")

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

#plot the residuals for data without outliers
plot(dataset_withoutoutlier$number_characters, residuals(linear_regression), main = "Residuals vs. Number of Characters Without Outliers", xlab = "Number of Characters", ylab = "Residuals")

#Add a horizontal line at zero to indicate the baseline for new plot
abline(h = 0, col = "red")
