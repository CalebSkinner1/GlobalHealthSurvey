library(ggplot2)
library(tidyverse)

setwd("~/Desktop")
df <-read.csv("Global_Health_data.csv")
view(df)
df <- subset(df, select = c(20,22,26,36,38:40, 43:45, 47:50))
df <- df[-c(1:2),]

#Bar Graph of Q14
df <- df %>% 
  filter(Q14 != "") %>% view()
ggplot(data=df, aes(x = df$Q14, main = "Global Health Career Interest?", xlab = "Options")) + geom_bar(fill = 'blue')

# Q14 by Q2
df1 <- df %>% 
  group_by(Q2) %>%
  count(Q14) %>%
  view()
ggplot(data=df1, aes(x = df1$Q2, main = "Global Health Career Interest?", xlab = "Options")) + geom_bar(fill = 'blue')


t.test()

df %>% 
  filter(Q14 != "") %>%
  group_by(Q14) %>%
  summarise(
    count = n()
  ) %>%
    view()

