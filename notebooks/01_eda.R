library(tidyverse)
library(dplyr)
library(ggplot2)

load("../data/processed/df_clean.RData")
## accidents per year
acc_year <- df %>%
  count(year) %>%
  arrange(year)

ggplot(acc_year, aes(x = year, y = n)) +
  geom_line() +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red")

## before vs after
acc_period <- df %>%
  count(period)

ggplot(acc_period, aes(x = period, y = n)) +
  geom_col()

## accidents per day
weekday_data <- df %>%
  group_by(period, weekday) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

weekday_data$weekday <- factor(weekday_data$weekday,
                               levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

ggplot(weekday_data, aes(x = weekday, y = prop, fill = period)) +
  geom_col(position = "dodge")


## via type
viaType <- df %>%
  group_by(period, D_TIPUS_VIA) %>%
  summarise(n = n(), .groups = "drop")

viaType <- viaType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(viaType, aes(x = D_TIPUS_VIA, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


