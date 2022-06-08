library(ggplot2)
library(tidyverse)
library(rjags)
library(plyr)
library(ProbBayes)
library(bayesplot)
library(rstanarm)

#Loading in dataframe and keeping important variables
df <-read.csv("Global_Health_data.csv")
df <- subset(df, select = c(20,22,26,36,38:40, 43:45, 47:50))
df <- df[-c(1:2),]
view(df)

#Task 1 -  T test of answers on Q18 by splitting on answers of Q14
#Bar Graph of Q18
df18 <- df %>% 
  filter(Q18 != "") %>%
view()

ggplot(df18, aes(x=Q18)) +
  geom_bar(fill = "blue") +
  labs(title = "Are there enough Global Health Opportunities?",
       y = "total votes")

#Bar Graph of Q14
df14 <- df %>% 
  filter(Q14 != "") #%>% 
#view()

ggplot(data=df14, aes(x = df14$Q14, 
                    main = "Global Health Career Interest?",
                    xlab = "Options")) + 
  geom_bar(fill = 'blue')


#Q18 Responses: No is 0, Yes is 1, I don't know is .5
DF18 <- df18 %>%
  mutate(Q18 = case_when(
    Q18 == "No" ~ 0,
    Q18 == "Yes" ~ 1,
    Q18 == "I don't know" ~ .5
  )) %>%
#Q14 Responses: No is 0, Yes is 1, Unsure is also 0
  mutate(Q14 = case_when(
    Q14 == "No" ~ 0,
    Q14 == "Yes" ~ 1,
    Q14 == "Unsure" ~ 0
  ))

#Fix confusing NAs
DF18$Q18[is.na(DF18$Q18)] = .5
DF18$Q14[is.na(DF18$Q14)] = 0
view(DF18)

#df of those interested in global health
Interested_df18 <- DF18 %>%
  filter(Q14 == 1) %>%
  view()

#df of those uninterested or not sure in global health
Uninterested_df18 <- DF18 %>%
  filter(Q14 == 0) %>%
  view()

# Frequentist Approach
# Two Sample t-test is significant
# p-value of .03273 and 95% Confidence Interval (-.353, -.016)
# In other words, those interested in global health say there are not enough
# Global Health opportunities (22.9%) compared with those who are not interested
# in global health (41.4%)
t.test(Interested_df18$Q18, Uninterested_df18$Q18)

#Bayesian Approach
cat("
model {
## sampling

for(i in 1:n0){
y_0[i] ~ dnorm(mu_0, phi0)
}
for(i in 1:n1){
y_1[i] ~ dnorm(mu_1, phi1)
}
## priors

mu_0 ~ dnorm(0, .0001)
mu_1 ~ dnorm(0, .0001)
phi0 ~ dgamma(.1,.1)
phi1 ~ dgamma(.1,.1)

##interests

diff <- mu_0 - mu_1
#ratio <- mu_0/mu_1
}
", file = "normal-normal.jags")


n0 = length(Interested_df18$Q18)
n1 = length(Uninterested_df18$Q18)

inits <- list(mu_0 = 1, mu_1 = 1, phi0 = .1, phi1 = .1)
data <- list("y_0" = Interested_df18$Q18, "y_1" = Uninterested_df18$Q18, "n0" = n0, "n1" = n1)

jm <- jags.model("normal-normal.jags", data = data, quiet = TRUE)

samps <- coda.samples(jm, variable.names = c("diff"), n.iter = 10000)

ests <- summary(samps, quantiles = c(.025, .975))
ests
#Quantiles in 95% Interval (-.364, -.009) This also suggests that the difference
#is significant
#graphs of difference
mcmc_hist(samps)
plot(samps)
#Below worked for me earlier but for some reason didn't work today
#mcmc_areas(samps, "mu", prob = .95, point.est = "mean") + 
 # labs(x = expression(mu), title = "Posterior")

#Previous Stuff Not Sure how legit

# Q14 by Q2
df1 <- df %>% 
  group_by(Q2) %>%
  count(Q14) %>%
  view()
ggplot(data=df1, aes(x = df1$Q2,
                     main = "Global Health Career Interest?",
                     xlab = "Options")) + 
  geom_bar(fill = 'blue')


t.test()

df %>% 
  filter(Q14 != "") %>%
  group_by(Q14) %>%
  summarise(
    count = n()
  ) %>%
    view()

