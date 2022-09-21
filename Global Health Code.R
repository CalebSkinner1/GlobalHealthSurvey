library(ggplot2)
library(tidyverse)
library(rjags)
library(plyr)
library(ProbBayes)
library(bayesplot)
library(rstanarm)
library(forcats)

#Loading in dataframe and keeping important variables
df0 <- read.csv("Global_Health_data.csv") %>% 
  select(20,22,26,36,38:40, 43:45, 47:50) %>% view()
df <- df0[-c(1:2),] %>% 
  mutate(Q17 = recode(Q17,
                      'Strongly Agree ' = 'Strongly Agree',
                      'Undecided ' = 'Undecided',
                      'Strongly Disagree ' = 'Strongly Disagree'),
         Q22 = recode(Q22,
                      'Strongly Agree ' = 'Strongly Agree',
                      'Undecided ' = 'Undecided',
                      'Strongly Disagree ' = 'Strongly Disagree')
         ) %>% view()

#Task 2 -  T test of answers on Q18 by splitting on answers of Q14
#Bar Graph of Q18
df18 <- df %>% 
  filter(Q18 != "") %>%
view()

ggplot(df18) +
  geom_bar(aes(Q18), fill = 'cornsilk3') +
  labs(title = "Are there enough Global Health Opportunities?",
       y = "total votes")

#Bar Graph of Q14
df14 <- df %>% 
  filter(Q14 != "") # %>% view()

Q14_Graph <- ggplot(df14, aes(Q14)) +
  geom_bar(fill = 'cornsilk3') +
  labs(title = "Global Health Career Interest?", y = "Options")


#Q18 Responses: No is 0, Yes is 1, I don't know is .5
DF18 <- df18 %>%
  mutate(Q18 = case_when(
    Q18 == "No" ~ 0,
    Q18 == "Yes" ~ 1,
    Q18 == " I don't know" ~ .5
  )) %>%
#Q14 Responses: No is 0, Yes is 1, Unsure is also 0
  mutate(Q14 = case_when(
    Q14 == "No" ~ 0,
    Q14 == "Yes" ~ 1,
    Q14 == "Unsure" ~ 0
  )) %>% view()

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
# p-value of .004335 and 95% Confidence Interval (-.234, -.044)
# In other words, those interested in global health say there are not enough
# Global Health opportunities (25.7%) compared with those who are not interested
# in global health (39.7%)
# 161 total observations
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
# Quantiles in 95% Interval (-.238, -.044) This also suggests that the difference
# is significant
# graphs of difference
mcmc_hist(samps)
plot(samps)
# Below worked for me earlier but for some reason didn't work today
# mcmc_areas(samps, "mu", prob = .95, point.est = "mean") + 
# labs(x = expression(mu), title = "Posterior")

# Task 2 -  T test of answers on Q17 by splitting on answers of Q14
# Bar Graph of Q17
df17 <- df %>% 
  filter(Q17 != "") %>% view()

df17 %>%
  mutate(Q17 = factor(Q17, levels=c("Strongly Agree", "Agree", "Undecided",
                           "Disagree", "Strongly Disagree"))) %>%
  ggplot(aes(Q17)) +
    geom_bar(fill = "cornsilk3") +
    labs(title = "Should all Pre-med students take a global health class?",
       y = "total votes")

# Bar Graph of Q14
Q14_Graph

# Q17 Responses: Strongly Disagree to Strongly Agree on scale of (-2 to 2)
DF17 <- df17 %>%
  mutate(Q17 = case_when(
    Q17 == 'Strongly Disagree' ~ -2,
    Q17 == 'Disagree' ~ -1,
    Q17 == 'Undecided' ~ 0,
    Q17 == 'Agree' ~ 1,
    Q17 == 'Strongly Agree' ~ 2
  )) %>%
# Q14 Responses: No is 0, Yes is 1, Unsure is also 0
  mutate(Q14 = case_when(
    Q14 == 'No ' ~ 0,
    Q14 == 'Yes' ~ 1,
    Q14 == 'Unsure' ~ 0
  )) %>% view()

# boxplot of the data
New_df17 <- df17 %>%
  mutate(Q17 = case_when(
    Q17 == 'Strongly Disagree' ~ -2,
    Q17 == 'Disagree' ~ -1,
    Q17 == 'Undecided' ~ 0,
    Q17 == 'Agree' ~ 1,
    Q17 == 'Strongly Agree' ~ 2
  )) %>% view()

ggplot(New_df17, aes(Q14, Q17, color = Q14)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 3) +
  labs(x = "Are you interested in pursuing a career in global health?",
       y = "Pre-Meds should take class",
       title = "Global Health pursual vs class requirements")

# dotplot of the data
ggplot(New_df17, aes(Q14, Q17, color = Q14)) +
  geom_violin(trim = FALSE) +
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  labs(x = "Are you interested in pursuing a career in global health?",
       y = "Pre-Meds should take class",
       title = "Global Health pursual vs class requirements")

# df of those interested in global health
Interested_df17 <- DF17 %>%
  filter(Q14 == 1) %>% view()

#df of those uninterested or not sure in global health
Uninterested_df17 <- DF17 %>%
  filter(Q14 == 0) %>% view()

# Frequentist Approach
# Two Sample t-test is significant
# p-value of .0088 and 95% Confidence Interval (.092, .630)
# In other words, those interested in global health do have a significantly 
# different opinion on whether all pre-med students should take a global health course
# The overall means are 1.41 for Interested Students and 1.05 for Uninterested students
# with 1 being agree and 2 being strongly agree
t.test(Interested_df17$Q17, Uninterested_df17$Q17)

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


n0 = length(Interested_df17$Q17)
n1 = length(Uninterested_df17$Q17)

inits <- list(mu_0 = 1, mu_1 = 1, phi0 = .1, phi1 = .1)
data <- list("y_0" = Interested_df17$Q17, "y_1" = Uninterested_df17$Q17, "n0" = n0, "n1" = n1)

jm <- jags.model("normal-normal.jags", data = data, quiet = TRUE)

samps <- coda.samples(jm, variable.names = c("diff"), n.iter = 10000)

ests <- summary(samps, quantiles = c(.025, .975))
ests
# Quantiles in 95% Interval (.092, .636) This also suggests that the difference
# is significant
# graphs of difference
mcmc_hist(samps)
plot(samps)
# Below worked for me earlier but for some reason didn't work today
# mcmc_areas(samps, "mu", prob = .95, point.est = "mean") + 
# labs(x = expression(mu), title = "Posterior")

#Task 3 -  T test of answers on Q22 by splitting on answers of Q14
#Bar Graph of Q22
df22 <- df %>% 
  filter(Q22 != "") %>%
  view()

df22 %>% mutate(Q22 = factor(Q22, levels=c("Strongly Agree", "Agree", "Undecided",
                                  "Disagree", "Strongly Disagree"))) %>%
  ggplot(aes(Q22)) +
  geom_bar(fill = "cornsilk3") +
  labs(title = "Practicing Clinal Medicine is the most important step of global health?",
       y = "total votes")

#Bar Graph of Q14
Q14_Graph

#Q22 Responses: Strongly Disagree to Strongly Agree on scale of (-2 to 2)
DF22 <- df22 %>%
  mutate(Q22 = case_when(
    Q22 == 'Strongly Disagree' ~ -2,
    Q22 == 'Disagree' ~ -1,
    Q22 == 'Undecided' ~ 0,
    Q22 == 'Agree' ~ 1,
    Q22 == 'Strongly Agree' ~ 2
  )) %>%
  #Q14 Responses: No is 0, Yes is 1, Unsure is also 0
  mutate(Q14 = case_when(
    Q14 == 'No' ~ 0,
    Q14 == 'Yes' ~ 1,
    Q14 == 'Unsure' ~ 0
  )) %>% view()

#boxplot of the data
New_df22 <- df22 %>%
  mutate(Q22 = case_when(
    Q22 == 'Strongly Disagree' ~ -2,
    Q22 == 'Disagree' ~ -1,
    Q22 == 'Undecided' ~ 0,
    Q22 == 'Agree' ~ 1,
    Q22 == 'Strongly Agree' ~ 2
  )) %>% view()


ggplot(New_df22, aes(Q14, Q22, color = Q14)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 3) +
  labs(x = "Are you interested in pursuing a career in global health?",
       y = "Clinical medicine is important")


# dotplot of the data
ggplot(New_df22, aes(Q14, Q22, color = Q14)) +
  geom_violin(trim = FALSE) +
  geom_jitter(shape = 16, position = position_jitter(.05)) +
  labs(x = "Are you interested in pursuing a career in global health?",
       y = "Clinical medicine is important")

# df of those interested in global health
Interested_df22 <- DF22 %>%
  filter(Q14 == 1) %>% view()

# df of those uninterested or not sure in global health
Uninterested_df22 <- DF22 %>%
  filter(Q14 == 0) %>%
  view()

# Frequentist Approach
# Two Sample t-test is insignificant
# p-value of .3914 and 95% Confidence Interval (-.466, .184)
# In other words, those interested in global health do not have a significantly 
# different opinion on whether practicing clinical medicine is the most important part of global health
# The overall means are .226 for Interested Students and .085 for Uninterested students
# with 0 being undecided, 1 being agree, and 2 being strongly agree
t.test(Interested_df22$Q22, Uninterested_df22$Q22)

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


n0 = length(Interested_df22$Q22)
n1 = length(Uninterested_df22$Q22)

inits <- list(mu_0 = 1, mu_1 = 1, phi0 = .1, phi1 = .1)
data <- list("y_0" = Interested_df22$Q22, "y_1" = Uninterested_df22$Q22, "n0" = n0, "n1" = n1)

jm <- jags.model("normal-normal.jags", data = data, quiet = TRUE)

samps <- coda.samples(jm, variable.names = c("diff"), n.iter = 10000)

ests <- summary(samps, quantiles = c(.025, .975))
ests
# Quantiles in 95% Interval (-.4653, .1875) This also suggests that the difference
# is insignificant
# graphs of difference
mcmc_hist(samps)
plot(samps)
# Below worked for me earlier but for some reason didn't work today
# mcmc_areas(samps, "mu", prob = .95, point.est = "mean") + 
# labs(x = expression(mu), title = "Posterior")





#Old Stuff not sure how legit this is

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

