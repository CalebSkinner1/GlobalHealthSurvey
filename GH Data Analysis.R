library(ggplot2)
library(tidyverse)
library(rjags)
library(ProbBayes)
library(bayesplot)
library(rstanarm)
library(forcats)
library(readr)
library(stringr)
library(scales)
library(readxl)

setwd("~/Desktop/Research")

# loading df- helpful for viewing the questions
df <- read.csv("Global_Health_data.csv") %>%
  select(20,22:24,26,28,30,36,38:41,43:45,47:50) %>% view()

# removing excess rows and fixing bad responses
df <- df[-c(1:2),] %>% 
  mutate(Q17 = recode(Q17,
                      'Strongly Agree ' = 'Strongly Agree',
                      'Undecided ' = 'Undecided',
                      'Strongly Disagree ' = 'Strongly Disagree'),
         Q18 = recode (Q18, "I don't know" = 'Unsure'),
         Q22 = recode(Q22,
                      'Strongly Agree ' = 'Strongly Agree',
                      'Undecided ' = 'Undecided',
                      'Strongly Disagree ' = 'Strongly Disagree'),
         Q14 = recode(Q14,
                      'No ' = 'No'),
         Q13 = recode(Q13,
                      '15+ years ' = '15+ years',
                      '5 - 10 years ' = '5 - 10 years',
                      '10 - 15 years ' = '10 - 15 years',
                      '1 - 5 years ' = '1 - 5 years',
                      'Less than one year ' = 'Less than one year'
         ),
         Q6 = recode(Q6,
                     'Pre-professional track - only choose this option if you do not have another major that fits into one of the above categories ' = 'Pre-Professional Track')
  ) %>% view()

df$Q13[49]

# Responses to whether or not there are enough opportunities in global health (Q18)
# bar graph of Q18
df %>% filter(Q18 != "") %>%
  group_by(Q18) %>%
  summarise(
    Counts = n()
  ) %>% ggplot(aes(Q18, Counts)) +
  geom_bar(stat = "identity", fill = 'cornsilk3') +
  labs(title = "Sufficient Opportunities in Global Health", x = "Responses") +
  geom_text(aes(label = signif(Counts)))

df18 <- df %>% 
  filter(Q18 != "") %>% view()
# set yes = 1, unsure = .5, and no = 0 for t tests
df18ttest <- df$Q18 %>%
  mutate(Q18 = case_when(
    Q18== "Unsure" ~ .5,
    Q18 == "Yes" ~ 1,
    Q18 == "No" ~ 0)
  ) %>% view()

# t test of Opportunities in Global Health by lived outside U.S.
t.test(df18ttest %>% filter(Q12 == "No") %>% select(Q18),
       df18ttest %>% filter(Q12 == "Yes") %>% select(Q18))
  # remove unsure responses
t.test(df18ttest %>% filter(Q12 == "No", Q18 != .5) %>% select(Q18),
       df18ttest %>% filter(Q12 == "Yes", Q18 != .5) %>% select(Q18))

