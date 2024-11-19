# Global Health Data Analysis
# Author: Caleb Skinner
# Last Modified: November 13

library(tidyverse)
library(rjags)
library(rstanarm)
library(scales)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(waffle)
library(gtsummary)


# loading df- helpful for viewing the questions
df0 <- read.csv("Global_Health_data.csv") %>%
  select(20,22:24,26,28,30,36,38:45,47:50) %>% view()

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
  )

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

  # Overall Responses to pursuing a career in Global Health (Q14) -----------
  
  df14 <- df %>% 
    filter(Q14 != "") # %>% view()
  # set yes = 1, unsure = .5, and no = 0 for t tests
  df14ttest <- df14 %>%
    mutate(Q14 = case_when(
      Q14 == "Unsure" ~ .5,
      Q14 == "Yes" ~ 1,
      Q14 == "No" ~ 0)
    ) # %>% view()
  
  # Bar Graph
  df14 %>%
    group_by(Q14) %>%
    summarise(
      Counts = n()
    ) %>% ggplot(aes(Q14, Counts)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Global Health Career Interest?", x = "Responses") +
    geom_text(aes(label = signif(Counts)), nudge_y = 2)
  
  
  # Career in Global Health by lived outside the US -------------------------
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
  
# Career in Global Health by born outside the US --------------------------
  
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
  
  # t test of Career in Global Health by born outside the US
    # Keep unsure as .5
    # results are significant. True difference of means is not equal to 0.
    # p-value = .00042
    # mean of not lived outside US: .630
    # mean of lived outside US: .857
    t.test(df14ttest %>% filter(Q10 == "No") %>% select(Q14),
           df14ttest %>% filter(Q10 == "Yes") %>% select(Q14))
  
  # t test of Career in Global Health by born outside the US
    # remove unsure values
    # results are significant. True difference of means is not equal to 0.
    # p-value = .00
    # mean of not lived outside US: .729
    # mean of lived outside US: 1.0000
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

  # Total Results -----------------------------------------------------------

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
      ) # %>% view()
  
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

  # Results by Lived Outside the US -----------------------------------------
    
    # converts the list into several columns
    df15Lived <- df15 %>% filter(Q12 == "Yes") # %>% view()
    df15NotLived <- df15 %>% filter(Q12 == "No") # %>% view()
    
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
    
    # Chi-Squared Test- testing independence of the two distributions - percents
    # the p-value is .2745, cannot conclude that the variations are distinct
     df15gLiveChi <- df15gLive %>% select(Fields, Lived_Abroad, Percent) %>% 
      pivot_wider(names_from = Lived_Abroad, values_from = Percent) # %>% view()
     chisq.test(df15gLiveChi$Yes, df15gLiveChi$No)
      
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
    
    # Chi-Squared Test- testing independence of the two distributions - total number of fields
    # the p-value is .2745, cannot conclude that the variations are distinct
    df15gLiveChiTotals <- full_join(df15Lived %>% 
                group_by(Number_of_Fields_Selected) %>% 
                summarise(Yes = n()/nrow(df15Lived)), 
              df15NotLived %>% group_by(Number_of_Fields_Selected) %>% 
                summarise(No = n()/nrow(df15NotLived))) # %>% view()
    chisq.test(df15gLiveChiTotals$Yes, df15gLiveChiTotals$No)
    

  # Results by born outside the US ------------------------------------------
    
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
    
    # Chi-Squared Test- testing independence of the two distributions - percents
    # the p-value is .3287, cannot conclude that the variations are distinct
    df15gBornChi <- df15gBorn %>% select(Fields, Born_Abroad, Percent) %>% 
      pivot_wider(names_from = Born_Abroad, values_from = Percent) # %>% view()
    chisq.test(df15gBornChi$Yes, df15gBornChi$No)
    
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
    
    # Chi-Squared Test- testing independence of the two distributions - total number of fields
    # the p-value is .2627, cannot conclude that the variations are distinct
    df15gBornChiTotals <- full_join(df15Born %>% 
                                      group_by(Number_of_Fields_Selected) %>% 
                                      summarise(Yes = n()/nrow(df15Born)), 
                                    df15NotLived %>% group_by(Number_of_Fields_Selected) %>% 
                                      summarise(No = n()/nrow(df15NotBorn))) # %>% view()
    chisq.test(df15gBornChiTotals$Yes, df15gBornChiTotals$No)
    
}

# Physicians who work within global health (domestic or international) should
# have educational training in which THREE academic disciplines?
{
  df %>% filter(Q16 != "")  %>% select(Q16) # %>% view()
  

  # Results of overall education training preference --------------------------
    
  # converts the list into several columns
    df16 <- df %>% filter(Q16 != "") %>% mutate(
      Economics = str_count(Q16, "Economics"),
      Business = str_count(Q16, "Business"),
      Law = str_count(Q16, "Law"),
      Anthropology = str_count(Q16, "Anthropology"),
      Policy_Advocacy = str_count(Q16, "Policy and Advocacy"),
      Public_Health = str_count(Q16, "Public Health"),
      Sociology = str_count(Q16, "Sociology"),
      Language = str_count(Q16, "Language"),
      Ethics = str_count(Q16, "Ethics"),
      Total_Academic_Disciplines = str_count(Q16, ",") + 1
    ) # %>% view()
  
    # creates a tibble of the totals
    df16g <- tibble(
      Disciplines = c("Economics", "Business", "Law", "Anthropology", "Policy_Advocacy",
                   "Public_Health", "Sociology", "Language", "Ethics"),
      Preferred = c(sum(df16$Economics), sum(df16$Business), sum(df16$Law), sum(df16$Anthropology),
                       sum(df16$Policy_Advocacy), sum(df16$Public_Health),
                       sum(df16$Sociology), sum(df16$Language),
                       sum(df16$Ethics))) %>%
      mutate(
        "Uninterested" = nrow(df16) - Preferred,
        "Percent" = percent((Preferred / nrow(df16)), accuracy = .1)
      ) # %>% view()

    # creates a barplot of each of the academic disciplines for education training
    # and includes percent selected
    ggplot(df16g, aes(reorder(Disciplines, -Preferred), Preferred)) +
      geom_bar(stat = "identity", fill = 'cornsilk3') +
      labs(title = "Important Academic Disciplines", x = "Disciplines") +
      geom_text(aes(label = Percent), nudge_y = 3) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    
    # total number of selections for respondent
    df16 %>% group_by(Total_Academic_Disciplines) %>%
      summarise(
        Count = n()
      ) %>% 
      ggplot(aes(Total_Academic_Disciplines, Count)) +
      geom_bar(stat = "identity", fill = 'cornsilk3') +
      labs(title = "Total Academic Disciplines Selected", x = "Total Discplines Selected") +
      geom_text(aes(label = signif(Count)), nudge_y = 1) +
      scale_x_continuous(breaks = seq(1,9, by = 1))
    

    # Academic disciplines preferred by Interest in Global Health --------------

    df16Int <- df16 %>% filter(Q14 == "Yes") #%>% view()
    df16NotInt <- df16 %>% filter(Q14 == "No") #%>% view()
    df16UnsureInt <- df16 %>% filter(Q14 == "Unsure") #%>% view()
    
    df16gInt <- tibble(
      Disciplines = c("Economics", "Economics", "Economics", "Business", "Business", 
                        "Business", "Law", "Law", "Law", "Anthropology", "Anthropology", 
                        "Anthropology", "Policy_Advocacy", "Policy_Advocacy", "Policy_Advocacy",
                        "Public_Health", "Public_Health", "Public_Health", "Sociology",
                        "Sociology", "Sociology", "Language", "Language", "Language",
                        "Ethics", "Ethics", "Ethics"),
      GH_Interest = c("Yes", "No", "Unsure", "Yes", "No", "Unsure", "Yes", "No", "Unsure",
                      "Yes", "No", "Unsure", "Yes", "No", "Unsure", "Yes", "No", "Unsure",
                      "Yes", "No", "Unsure", "Yes", "No", "Unsure", "Yes", "No", "Unsure"),
      Preferred = c(sum(df16Int$Economics),sum(df16NotInt$Economics),sum(df16UnsureInt$Economics),
                       sum(df16Int$Business),sum(df16NotInt$Business),sum(df16UnsureInt$Business),
                       sum(df16Int$Law),sum(df16NotInt$Law),sum(df16UnsureInt$Law),
                       sum(df16Int$Anthropology),sum(df16NotInt$Anthropology),sum(df16UnsureInt$Anthropology),
                       sum(df16Int$Policy_Advocacy),sum(df16NotInt$Policy_Advocacy),sum(df16UnsureInt$Policy_Advocacy),
                       sum(df16Int$Public_Health),sum(df16NotInt$Public_Health),sum(df16UnsureInt$Public_Health),
                       sum(df16Int$Sociology),sum(df16NotInt$Sociology),sum(df16UnsureInt$Sociology),
                       sum(df16Int$Language),sum(df16NotInt$Language),sum(df16UnsureInt$Language),
                       sum(df16Int$Ethics),sum(df16NotInt$Ethics),sum(df16UnsureInt$Ethics)
                       )
    ) %>% mutate(
      Decimal = as.double(if_else(GH_Interest == "Yes", Preferred/nrow(df16Int), 
                                  if_else(GH_Interest == "No", Preferred/nrow(df16NotInt), Preferred/nrow(df16UnsureInt)))),
      Percent = percent(Decimal,accuracy = .1)
      ) # %>% view()
    
    # creates a barplot of each of the Academic Disciplines and Compares
    # Interested in Global Health to not Interested
    # TOTALS
    ggplot(df16gInt, aes(reorder(Disciplines, -Preferred), Preferred, fill = GH_Interest)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Preferred Academic Disciplines", x = "Disciplines", fill = "Interest in Global Health") +
      geom_text(aes(label = Preferred), vjust = -.2, position = position_dodge(width = .9)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    
    # PERCENTS
    ggplot(df16gInt, aes(reorder(Disciplines, -Decimal), Decimal, fill = GH_Interest)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Respondents' prefereance in the following Academic Disciplines",
           x = "Disciplines", fill = "Global Health Interest", y = "Percent") +
      geom_text(aes(label = Percent), vjust = -.3, position = position_dodge(width = .9)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
    
    # Chi-Squared Test- testing independence of the two distributions - percents
    # This tests to see if the distribution of Nos is significantly different from
    # the distributions of Yes's
    # the p-value is .2303, cannot conclude that the variations are distinct
    
    df16gIntChi <- df16gInt %>% select(Disciplines, GH_Interest, Percent) %>% 
      pivot_wider(names_from = GH_Interest, values_from = Percent) # %>% view()
    chisq.test(df16gIntChi$Yes, df16gIntChi$No)
    
    # Academic disciplines preferred by major
    # refiltering out the data
    df16major <- df16 %>% mutate(
      Economics = replace(Economics,Economics == 1, "Economics"),
      Business = replace(Business,Business == 1, "Business"),
      Law = replace(Law,Law == 1, "Law"),
      Anthropology = replace(Anthropology,Anthropology == 1, "Anthropology"),
      Policy_Advocacy = replace(Policy_Advocacy,Policy_Advocacy == 1, "Policy_Advocacy"),
      Public_Health = replace(Public_Health,Public_Health == 1, "Public_Health"),
      Sociology = replace(Sociology,Sociology == 1, "Sociology"),
      Language = replace(Language,Language == 1, "Language"),
      Ethics = replace(Ethics,Ethics == 1, "Ethics")
      ) %>% pivot_longer(c(Economics, Business, Law, Anthropology, Policy_Advocacy,
                           Public_Health, Sociology, Language, Ethics), 
                         names_to = "Academic_Disciplines", values_to = "Disciplines") %>%
      filter(Disciplines != 0, Q6 != "") %>% select(-Q23, -Academic_Disciplines) # %>% view()
    
    totalsmajor <- df16major %>% group_by(Q6) %>% summarise(
      Totals = n()
    ) # %>% view()
    
    df16majorp <- df16major %>% group_by(Q6, Disciplines) %>% summarise(
      Count = n()
    ) %>% group_by(Q6) %>%
      mutate("Percent" = Count/sum(Count)) # %>% view()
      
    # Total Count of Disciplines by Major
    ggplot(df16majorp, aes(Q6, Count, fill = Disciplines)) +
      geom_bar(stat = "identity", position = "dodge") +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 13)) +
      labs(x = "Major")
      
    # Total Percent of Disciplines by Major
    ggplot(df16majorp, aes(reorder(Q6, -Count), Percent, fill = Disciplines)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 13)) +
      labs(x = "Major")
    
}

# Which of the following do you perceive as reasons it is difficult for undergraduate
# pre-medical students to learn more about global health and gain experience in
# the field prior to medical school?
{
  df %>% filter(Q19 != "")  %>% select(Q19) %>% distinct() # %>% view()
  
  df19 <- df %>% filter(Q19 != "") %>% mutate(
    Faculty = str_count(Q19, "Lack of global health faculty"),
    Extracurricular = str_count(Q19, "Lack of extracurricular opportunities"),
    Busyness = str_count(Q19, "Time constraints"),
    Financial = str_count(Q19, "Financial barriers"),
    Research = str_count(Q19, "research opportunities"),
    Ethical_Issues = str_count(Q19, "Ethical issues"),
    Physician_Mentorship = str_count(Q19, "Lack of mentorship"),
    Other = str_count(Q19, "Other"),
    Total_Problems = rowSums(across(where(is.numeric)))
  ) # %>% view()
  df19g <- df19 %>% summarise(
    Faculty = sum(Faculty),
    Extracurricular = sum(Extracurricular),
    Busyness = sum(Busyness),
    Financial = sum(Financial),
    Research = sum(Research),
    Ethical_Issues = sum(Ethical_Issues),
    Physician_Mentorship = sum(Physician_Mentorship),
    Other = sum(Other)
  ) %>%
    t() # %>% view()
  df19g <- rownames_to_column(df19g,var = "rowname")
  colnames(df19g) <- c("Problems", "Count")
  df19g <- df19g %>% mutate(
    Percent = percent(Count/nrow(df %>% filter(Q19 != "")), accuracy = .1)
  ) # %>% view()
  
  # creates a barplot of each of the academic disciplines for education training
  # and includes percent selected
  ggplot(df19g, aes(reorder(Problems, -Count), Count)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Hinderances to Global Health Recognition", x = "Hinderances") +
    geom_text(aes(label = Percent), nudge_y = 3) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 12))
  
  # total number of selections for respondent
  df19 %>% group_by(Total_Problems) %>%
    summarise(
      Count = n()
    ) %>% 
    ggplot(aes(Total_Problems, Count)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Total Hinderances to Global Health Selected", x = "Total Hinderances Selected") +
    geom_text(aes(label = signif(Count)), nudge_y = 1) +
    scale_x_continuous(breaks = seq(1,9, by = 1))
}

# What do you think should be required for pre-medical students to engage in 
# clinical global health work and/or mission trips? (select all that apply)
{
  df %>% filter(Q20 != "")  %>% select(Q20) %>% distinct() %>% view()
  
  df20 <- df %>% filter(Q20 != "") %>% mutate(
    Clinical_Certification = str_count(Q20, "clinical certification"),
    Cultural_Education = str_count(Q20, "Education on the community"),
    Local_Language = str_count(Q20, "Knowledge of the local language"),
    Medical_Ethics = str_count(Q20, "Medical ethics education"),
    Volunteer_Experience = str_count(Q20, "volunteer experience"),
    Nonclinical_role = str_count(Q20, "non-clinical role on the mission trip"),
    No_Undergrad_Mission_Trips = str_count(Q20, "Undergraduates should never"),
    Total_Conditions = rowSums(across(where(is.numeric)))
  ) # %>% view()
  df20g <- df20 %>% summarise(
    "Clinical Certification" = sum(Clinical_Certification),
    "Cultural Education" = sum(Cultural_Education),
    "Local Language Training" = sum(Local_Language),
    "Medical Ethics Education" = sum(Medical_Ethics),
    "Volunteer Experience" = sum(Volunteer_Experience),
    "Non-clinical role on mission trip" = sum(Nonclinical_role),
    "Undergraduates should not engage in Mission Trips" = sum(No_Undergrad_Mission_Trips)
  ) %>%
    t() %>% as.data.frame() %>% rownames_to_column(var = "rowname") %>% view()
  colnames(df20g) <- c("Conditions", "Count")
  df20g <- df20g %>% mutate(
    Percent = percent(Count/nrow(df %>% filter(Q19 != "")), accuracy = .1)
  ) # %>% view()
  
  # creates a barplot of each of the conditions to engage in global health work/mission trip
  # and includes percent selected
  ggplot(df20g, aes(reorder(Conditions, -Count), Count)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Requirements for pre-medical students to engage in 
         clinical global health work and/or mission trips", x = "Requirements") +
    geom_text(aes(label = Percent), nudge_y = 3) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
  
  # total number of selections for respondent
  df20 %>% group_by(Total_Conditions) %>%
    summarise(
      Count = n()
    ) %>% 
    ggplot(aes(Total_Conditions, Count)) +
    geom_bar(stat = "identity", fill = 'cornsilk3') +
    labs(title = "Total Conditions to engage in Global Health Work/Mission Trips",
         x = "Total Conditions Selected") +
    geom_text(aes(label = signif(Count)), nudge_y = 1) +
    scale_x_continuous(breaks = seq(1,9, by = 1))
}

# Short-term mission trips are an effective way to address global health challenges.
{
  df %>% filter(Q21 != "") %>% select(Q21) %>% distinct() %>% view()
  
}


