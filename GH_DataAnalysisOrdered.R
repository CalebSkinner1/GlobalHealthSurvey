# Caleb Skinner
# Last Modified: October 17

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

# loading df- helpful for viewing the questions
df0 <- read.csv("Global_Health_data.csv") %>%
  select(20,22:24,26,28,30,36,38:41,43:45,47:50) %>% view()

# removing excess rows and fixing bad responses
df <- df0[-c(1:2),] %>% 
  mutate(Q17 = recode(Q17,
                      'Strongly Agree ' = 'Strongly Agree',
                      'Undecided ' = 'Undecided',
                      'Strongly Disagree ' = 'Strongly Disagree'),
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

# What do the respondents look like?
{
  # year in undergraduate program
  df %>% filter(Q2 != "") %>%
    group_by(Q2) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q2, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Year in Undergraduate Program", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 2)
  
  # age
  df %>% filter(Q3 != "") %>%
    group_by(Q3) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q3, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Age", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4)
  
  # ethnicity
  df %>% filter(Q29 != "") %>%
    group_by(Q29) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(reorder(Q29, -Counts), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Ethnicity", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 3) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 13))
  
  # gender identity
  df %>% filter(Q9 != "") %>%
    group_by(Q9) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(reorder(Q9, -Counts), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Gender Identity", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 13))
  
  # undergraduate university
  df %>% filter(Q4 != "") %>%
    group_by(Q4) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(reorder(Q4, -Counts), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Undergraduate University", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 3) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 10))
  
  # degree type
  df %>% filter(Q5 != "") %>%
    group_by(Q5) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(reorder(Q5, -Counts), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Degree Type", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
  
  # category of major
  df %>% filter(Q6 != "") %>%
    group_by(Q6) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(reorder(Q6, -Counts), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Major", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
  
  # born outside the US
  df %>% filter(Q10 != "") %>%
    group_by(Q10) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q10, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Born Abroad", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4)
  
  # lived outside the US
  df %>% filter(Q12 != "") %>%
    group_by(Q12) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q12, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Lived Abroad", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 4)
  
  # how long lived outside the US
  df %>% filter(Q13 != "") %>%
    group_by(Q13) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(x = factor(Q13, level = c('Less than one year',
                                           '1 - 5 years',
                                           '5 - 10 years',
                                           '10 - 15 years',
                                           '15+ years')), Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Lived Abroad Length", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = .3) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 10))
  }

# How do pre-medical students perceive a career in global health?
{
  # Responses to pursing a career in global health (Q14)
  df14 <- df %>% 
    filter(Q14 != "") %>% view()
  # set yes = 1, unsure = .5, and no = 0 for t tests
  df14ttest <- df14 %>%
    mutate(Q14 = case_when(
      Q14 == "Unsure" ~ .5,
      Q14 == "Yes" ~ 1,
      Q14 == "No" ~ 0)
    ) %>% view()
  
  # Bar Graph
  df14 %>%
    group_by(Q14) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q14, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Global Health Career Interest?", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 2)
  
  # Career in Global Health by lived outside the US
  df14 %>%
    filter(Q12 != "") %>% 
    group_by(Q12, Q14) %>%
    summarise(
      Counts = n()
    ) %>%
    ggplot(aes(Q12, Counts, fill = Q14)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Global Health Career Interest by Lived outside US", x = "Lived outside US?",
         fill = "Global Health Interest:") +
    theme(legend.position = "bottom") +
    geom_text(aes(label = signif(Counts)), vjust = -.1, position = position_dodge(width = .9))
  
  # t test of Career in Global Health by lived outside the US
    # Keep unsure as .5
    # results are significant. True difference of means is not equal to 0.
    # p-value = .00035
    # mean of not lived outside US: .622
    # mean of lived outside US: .845
    t.test(df14ttest %>% filter(Q12 == "No") %>% select(Q14),
           df14ttest %>% filter(Q12 == "Yes") %>% select(Q14))
    
    # Remove unsure observations
    # results are significant. True difference of means is not equal to 0.
    # p-value = .00102
    # mean of not lived outside US: .722
    # mean of lived outside US: .955
    t.test(df14ttest %>% filter(Q12 == "No", Q14 != .5) %>% select(Q14),
           df14ttest %>% filter(Q12 == "Yes", Q14 != .5) %>% select(Q14))
  
  # How long lived outside the US
  df14 %>%
    filter(Q13 != "") %>% 
    group_by(Q13, Q14) %>%
    summarise(
      Counts = n()
    ) %>%
    ggplot(aes(Q13, Counts, fill = Q14)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Global Health Career Interest by length of time abroad", x = "How long lived outside US?",
         fill = "Global Health Interest:") +
    theme(legend.position = "bottom") +
    geom_text(aes(label = signif(Counts)), vjust = -.1, position = position_dodge(width = .9))
  
  # Career in Global Health by born outside the US
  df14 %>%
    filter(Q10 != "") %>% 
    group_by(Q10, Q14) %>%
    summarise(
      Counts = n()
    ) %>%
    ggplot(aes(Q10, Counts, fill = Q14)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Global Health Career Interest by Born outside US", x = "Born outside US?",
         fill = "Global Health Interest:") +
    theme(legend.position = "bottom") +
    geom_text(aes(label = signif(Counts)), vjust = -.1, position = position_dodge(width = .9))
  
  # t test of Career in Global Health by lived outside the US
    # Keep unsure as .5
    # results are significant. True difference of means is not equal to 0.
    # p-value = .00042
    # mean of not lived outside US: .630
    # mean of lived outside US: .857
    t.test(df14ttest %>% filter(Q10 == "No") %>% select(Q14),
           df14ttest %>% filter(Q10 == "Yes") %>% select(Q14))
  
    # Remove unsure observations
    # results are significant. True difference of means is not equal to 0.
    # p-value = 2.848e-.07 (very small)
    # mean of not lived outside US: .729
    # mean of lived outside US: 1.0
    t.test(df14ttest %>% filter(Q10 == "No", Q14 != .5) %>% select(Q14),
           df14ttest %>% filter(Q10 == "Yes", Q14 != .5) %>% select(Q14))
  
  # Career in Global Health by undergraduate major
  df14 %>%
    filter(Q6 != "") %>% 
    group_by(Q6, Q14) %>%
    summarise(
      Counts = n()
    ) %>%
    ggplot(aes(Q6, Counts, fill = Q14)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Global Health Career Interest by Major", x = "Major",
         fill = "Global Health Interest:") +
    theme(legend.position = "bottom") +
    geom_text(aes(label = signif(Counts)), vjust = -.1, position = position_dodge(width = .9)) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 11))
  }

# Which aspects of the global health field interest you when considering your medical career?
{
  # views the responses of question 15
  df14 %>% select(Q15) # %>% view()
  
  # Total Results
    # converts the list into several columns
    df15 <- df14 %>% filter(Q23 != "") %>% mutate(
      "Health Equity" = str_count(Q15, "Health equity"),
      "Public Health" = str_count(Q15, "Public health"),
      "Travel Opportunities" = str_count(Q15, "Travel opportunities"),
      "Provide for Underserved Communities" = str_count(Q15, "Want to provide healthcare to underserved communities"),
      "Uninterested" = str_count(Q15, "Not interested in the global health field"),
      "Research Opportunities" = str_count(Q15, "Research opportunities"),
      "Non-Profit/Healthcare Admin" = str_count(Q15, "Non-profit and healthcare administration"),
      "Cross-Cultural Experiences" = str_count(Q15, "Cross-cultural experiences"),
      "Number_of_Fields_Selected" = str_count(Q15, ",") + 1 - str_count(Q15, "Not interested in the global health field")
      ) %>% view()
  
    # creates a tibble of the totals
    df15g <- tibble(
      "Fields" = c("Health Equity", "Public Health", "Travel Opportunities", 
                  "Provide for Underserved Communities", "Uninterested", "Research Opportunities", 
                  "Non-Profit/Healthcare Admin","Cross-Cultural Experiences"),
      "Interested" = c(sum(df15$`Health Equity`), sum(df15$`Public Health`), sum(df15$`Travel Opportunities`),
                      sum(df15$`Provide for Underserved Communities`), sum(df15$`Uninterested`),
                      sum(df15$`Research Opportunities`), sum(df15$`Non-Profit/Healthcare Admin`),
                      sum(df15$`Cross-Cultural Experiences`))) %>%
      mutate(
        "Uninterested" = nrow(df15) - Interested,
        "Percent" = percent((Interested / nrow(df15)), accuracy = .1)
      ) # %>% view()
  
    # creates a barplot of each of the Global Health Interest Fields
    # and includes percent selected
    ggplot(df15g, aes(reorder(Fields, -Interested), Interested)) +
      geom_bar(stat = "identity", fill = 'cornsilk3') +
      labs(title = "Interest in Global Health Fields", x = "Fields") +
      geom_text(aes(label = Percent), nudge_y = 3) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
  
    # total number of selections for respondent
    df15 %>% group_by(Number_of_Fields_Selected) %>%
      summarise(
        Count = n()
      ) %>% 
      ggplot(aes(Number_of_Fields_Selected, Count)) +
      geom_bar(stat = "identity", fill = 'cornsilk3') +
      labs(title = "Interest in Global Health Fields", x = "Total Fields Selected") +
      geom_text(aes(label = signif(Count)), nudge_y = 1)
  
  # Results by Lived Outside US
    # converts the list into several columns
    df15Lived <- df15 %>% filter(Q12 == "Yes") %>% view()
    df15NotLived <- df15 %>% filter(Q12 == "No") %>% view()
    
    # creates a tibble of the totals
    df15gLive <- tibble(
      "Fields" = c("Health Equity", "Health Equity", "Public Health", "Public Health",
                   "Travel Opportunities", "Travel Opportunities", "Provide for Underserved Communities",
                   "Provide for Underserved Communities", "Uninterested", "Uninterested","Research Opportunities", 
                   "Research Opportunities","Non-Profit/Healthcare Admin","Non-Profit/Healthcare Admin",
                   "Cross-Cultural Experiences", "Cross-Cultural Experiences"),
      Lived_Abroad = c("Yes", "No", "Yes", "No","Yes", "No","Yes", "No",
                         "Yes", "No","Yes", "No","Yes", "No","Yes", "No"),
      "Interested" = c(sum(df15Lived$`Health Equity`), sum(df15NotLived$`Health Equity`),
                       sum(df15Lived$`Public Health`), sum(df15NotLived$`Public Health`),
                       sum(df15Lived$`Travel Opportunities`), sum(df15NotLived$`Travel Opportunities`),
                       sum(df15Lived$`Provide for Underserved Communities`), sum(df15NotLived$`Provide for Underserved Communities`),
                       sum(df15Lived$`Uninterested`),sum(df15NotLived$`Uninterested`),
                       sum(df15Lived$`Research Opportunities`), sum(df15NotLived$`Research Opportunities`),
                       sum(df15Lived$`Non-Profit/Healthcare Admin`), sum(df15NotLived$`Non-Profit/Healthcare Admin`),
                       sum(df15Lived$`Cross-Cultural Experiences`), sum(df15NotLived$`Cross-Cultural Experiences`))
      ) %>% mutate(
        Decimal = as.double(if_else(Lived_Abroad == "Yes", Interested/nrow(df15Lived), Interested/nrow(df15NotLived))),
        Percent = percent(Decimal,accuracy = .1)
      ) # %>% view()
    
    # creates a barplot of each of the Global Health Interest Fields and Compares
    # Lived in US to not Lived in US
      # TOTALS
      ggplot(df15gLive, aes(reorder(Fields, -Interested), Interested, fill = Lived_Abroad)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Interest in Global Health Fields", x = "Fields", fill = "Lived Abroad?") +
        geom_text(aes(label = Interested), vjust = -.1, position = position_dodge(width = .9)) +
        scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
      # PERCENTS
      ggplot(df15gLive, aes(reorder(Fields, -Decimal), Decimal, fill = Lived_Abroad)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Respondents' interest in the following Global Health Fields",
             x = "Fields", fill = "Lived Abroad?", y = "Percent") +
        geom_text(aes(label = Percent), vjust = -.3, position = position_dodge(width = .9)) +
        scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    
    # total number of selections for respondent
    full_join(df15Lived %>% 
                 group_by(Number_of_Fields_Selected) %>% 
                 summarise(Yes = n()/nrow(df15Lived)), 
               df15NotLived %>% group_by(Number_of_Fields_Selected) %>% 
                 summarise(No = n()/nrow(df15NotLived))) %>% pivot_longer(
                   c(`Yes`, `No`), names_to = "Lived_Abroad", values_to = "Count") %>% mutate(
                     Percent = percent(Count, accuracy= .01)) %>%
      ggplot(aes(Number_of_Fields_Selected, Count, fill = Lived_Abroad)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Total Fields Interested In", x = "Total Fields Selected", 
           y= "Percent", fill = "Lived Abroad") +
      geom_text(aes(label = Percent), vjust = -.3, position = position_dodge(width = .9))
    
    
  # Results by Born Outside US
    # converts the list into several columns
    df15Born <- df15 %>% filter(Q10 == "Yes") #%>% view()
    df15NotBorn <- df15 %>% filter(Q10 == "No") #%>% view()
    
    # creates a tibble of the totals
    df15gBorn <- tibble(
      "Fields" = c("Health Equity", "Health Equity", "Public Health", "Public Health",
                   "Travel Opportunities", "Travel Opportunities", "Provide for Underserved Communities",
                   "Provide for Underserved Communities", "Uninterested", "Uninterested","Research Opportunities", 
                   "Research Opportunities","Non-Profit/Healthcare Admin","Non-Profit/Healthcare Admin",
                   "Cross-Cultural Experiences", "Cross-Cultural Experiences"),
      Born_Abroad = c("Yes", "No", "Yes", "No","Yes", "No","Yes", "No",
                       "Yes", "No","Yes", "No","Yes", "No","Yes", "No"),
      "Interested" = c(sum(df15Born$`Health Equity`), sum(df15NotBorn$`Health Equity`),
                       sum(df15Born$`Public Health`), sum(df15NotBorn$`Public Health`),
                       sum(df15Born$`Travel Opportunities`), sum(df15NotBorn$`Travel Opportunities`),
                       sum(df15Born$`Provide for Underserved Communities`), sum(df15NotBorn$`Provide for Underserved Communities`),
                       sum(df15Born$`Uninterested`),sum(df15NotBorn$`Uninterested`),
                       sum(df15Born$`Research Opportunities`), sum(df15NotBorn$`Research Opportunities`),
                       sum(df15Born$`Non-Profit/Healthcare Admin`), sum(df15NotBorn$`Non-Profit/Healthcare Admin`),
                       sum(df15Born$`Cross-Cultural Experiences`), sum(df15NotBorn$`Cross-Cultural Experiences`))
    ) %>% mutate(
      Decimal = as.double(if_else(Born_Abroad == "Yes", Interested/nrow(df15Born), Interested/nrow(df15NotBorn))),
      Percent = percent(Decimal,accuracy = .1)
    ) # %>% view()
    
    # creates a barplot of each of the Global Health Interest Fields and Compares
    # Lived in US to not Lived in US
    # TOTALS
    ggplot(df15gBorn, aes(reorder(Fields, -Interested), Interested, fill = Born_Abroad)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Interest in Global Health Fields", x = "Fields", fill = "Born Abroad?") +
      geom_text(aes(label = Interested), vjust = -.1, position = position_dodge(width = .9)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    # PERCENTS
    ggplot(df15gBorn, aes(reorder(Fields, -Decimal), Decimal, fill = Born_Abroad)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Respondents' interest in the following Global Health Fields",
           x = "Fields", fill = "Born Abroad?", y = "Percent") +
      geom_text(aes(label = Percent), vjust = -.3, position = position_dodge(width = .9)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    
    # total number of selections for respondent
    full_join(df15Born %>% 
                 group_by(Number_of_Fields_Selected) %>% 
                 summarise(Yes = n()/nrow(df15Born)), 
               df15NotBorn %>% group_by(Number_of_Fields_Selected) %>% 
                 summarise(No = n()/nrow(df15NotBorn))) %>% pivot_longer(
                   c(`Yes`, `No`), names_to = "Born_Abroad", values_to = "Count") %>% mutate(
                     Percent = percent(Count, accuracy= .01)) %>%
      ggplot(aes(Number_of_Fields_Selected, Count, fill = Born_Abroad)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Total Fields Interested In", x = "Total Fields Selected", 
           y= "Percent", fill = "Born Abroad") +
      geom_text(aes(label = Percent), vjust = -.3, position = position_dodge(width = .9))
}











