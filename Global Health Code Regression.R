# make sure all programs are in place
library(tidyverse)
library(ProbBayes)
library(mcmcr)
library(readxl)
library(runjags)
library(ggplot2)

setwd("~/Desktop")
df <-read.csv("Global_Health_data.csv")
 df <- subset(df, select = c(20,22,26,36,38:40, 43:45, 47:50))
df <- df[-c(1:2),]
view(df)

# Prep for Regression

  # filter the data
df <- subset(df, df$Q14 != "")
df <- subset(df, df$Q2 != "")
df <- subset(df, df$Q3 != "")
df <- subset(df, df$Q10 != "")
df <- subset(df, df$Q12 != "")
view(df)

  # put data into proper form
scores = c("1st year" = 1, "2nd year" = 2, "3rd year" = 3, "4th year" = 4, "5th year" = 5)
df$Q2. <- scores[as.factor(df$Q2)]
response = c("Yes" = 0, "Unsure" = 1, "No" = 2)
df$Q14. <- response[as.factor(df$Q14)]
age = c("18-20" = 0, "21-23" = 1, "24-26" = 2)
df$Q3. <- age[as.factor(df$Q3)]
df <- mutate(df, Q10. = ifelse(Q10 == "Yes", 1, 0))
df <- mutate(df, Q12. = ifelse(Q12 == "Yes", 1, 0))
view(df)

# plots
  # Bar Graph of Q14
ggplot(data=df, aes(x = df$Q14)) + geom_bar(fill = 'blue')
  # Bar Graph of Q2
ggplot(data=df, aes(x = df$Q2)) + geom_bar(fill = 'blue')
  # Bar Graph of Q3
ggplot(data=df, aes(x = df$Q3)) + geom_bar(fill = 'blue')
  # Bar Graph of Q10
ggplot(data=df, aes(x = df$Q10)) + geom_bar(fill = 'blue')
  # Bar Graph of Q12
ggplot(data=df, aes(x = df$Q12)) + geom_bar(fill = 'blue')
  # Q2 with Q14
ggplot(df, aes(x=Q2 , col = Q14)) + geom_bar()
  # Q3 with Q14
ggplot(df, aes(x=Q3 , col = Q14)) + geom_bar()
  # Q10 with Q14
ggplot(df, aes(x=Q10 , col = Q14)) + geom_bar()
  # Q12 with Q14
ggplot(df, aes(x=Q12 , col = Q14)) + geom_bar()

# Regression for Q14

# Frequentest Regression
model <- lm(Q14. ~ Q2. + Q3. + Q10. + Q12., data=df)
summary(model)
# R-squared is 0.09264
  
# Bayes Regression
modelString <-"
model {
## sampling
for (i in 1:N){
   y[i] ~ dnorm(beta0 + beta1*x[i] + beta2*x2[i], invsigma2)
}
## priors
beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
beta2 ~ dnorm(mu2, g2)
beta3 ~ dnorm(mu3, g3)
beta4 ~ dnorm(mu4, g4)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}"

y <- df$Q14.  
x <-  df$Q2.
x2 <- df$Q3.
x3 <- df$Q10.
x4 <- df$Q12.
N <- length(y)  
the_data <- list("y" = y, "x" = x, "x2" = x2,"x3" = x3, "x4" = x4, "N" = N,
                 "mu0" = 0, "g0" = 0.0001,
                 "mu1" = 0, "g1" = 0.0001,
                 "mu2" = 0, "g2" = 0.0001,
                 "mu3" = 0, "g3" = 0.0001,
                 "mu4" = 0, "g4" = 0.0001,
                 "a" = 1, "b" = 1)


posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1","beta2", "beta3", "beta4", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 250000)

plot(posterior, vars = "beta1") 

summary(posterior$mcmc[[1]], digits = 3)

summary(posterior)


# Extra for future purposes

# Q14 in chart form
df2 %>% 
  filter(Q14 != "") %>%
  group_by(Q14) %>%
  summarise(
    count = n()
  ) %>%
  view()

# Q14 by Q2
df3 <- df2 %>% 
  group_by(Q2) %>%
  count(Q14) %>%
  view()
ggplot(data=df1, aes(x = Q2, main = "Global Health Career Interest?", xlab = "Options")) + geom_bar(fill = 'blue')
