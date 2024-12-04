# Set Working Directory
setwd("H:/sta215")

# install "haven" package
install.packages("haven")

# Load "haven" package
library("haven")

#Load raw_data 
raw_data<-read.csv("raw_data.csv")

#Table 1: Descriptive Statistics

# Quantitative Variable 1
mean(raw_data$dexter_screentime)
sd(raw_data$dexter_screentime)
summary(raw_data$dexter_screentime)

#Quantitative Variable 2
mean(raw_data$dexter_inner_thoughts)
sd(raw_data$dexter_inner_thoughts)
summary(raw_data$dexter_inner_thoughts)

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

#Figure 1: Box Plot
model <- lm(dexter_inner_thoughts ~ gruesomeness, data = raw_data)
summary(model)
aov_result <- aov(dexter_inner_thoughts ~ gruesomeness, data = raw_data)
summary(aov_result)
boxplot(dexter_inner_thoughts ~ gruesomeness, data = raw_data)

#Figure 2: Scatter Plot
# ylab labels the y axis and xlab labels the x axis and main title the graph
plot(dead_body_count ~ detectives_at_scence, data = raw_data, xlab = "Number of Detectives at Scene", ylab = "Dead Body Count", main = "Relationship Between Detectives at Scene and Body Count")
linear <- lm(dead_body_count ~ detectives_at_scence, data = raw_data)
summary(linear)
abline(linear, col = "red")

#Figure 3: Residual Plot
#h=0 is the abline sets  y-value for the horizontal line, the line helps identify whether residuals are evenly distributed above and below 0, which is expected for a well-fitting linear model.
residuals <- resid(linear)
plot(raw_data$detectives_at_scence, residuals, ylab = "Residuals", xlab = "Detectives at Scene", main = "Residual Plot")
abline(h=0,col = "blue")
