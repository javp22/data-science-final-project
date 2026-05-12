library(tidyverse)

load("../data/processed/df_clean.RData")

## before vs after
prop.table(table(df$period))

acc_period <- df %>%
  count(period) %>%
  mutate(prop = n / sum(n))

ggplot(acc_period, aes(x = period, y = prop, fill = period)) +
  geom_col() +
  labs(
    title = "Proportion of accidents by period",
    x = "Period",
    y = "Proportion"
  )

## monthly distribution
monthly_props <- df %>%
  group_by(period, month) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(monthly_props, aes(x = month, y = prop, color = period)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly distribution of accidents",
    x = "Month",
    y = "Proportion"
  )

## accidents per year
acc_year <- df %>%
  count(year) %>%
  arrange(year)

ggplot(acc_year, aes(x = year, y = n)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
  labs(
    title = "Number of accidents per year",
    x = "Year",
    y = "Number of accidents"
  )

## accidents per day
weekday_data <- df %>%
  group_by(period, weekday) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

weekday_data$weekday <- factor(weekday_data$weekday,
                               levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

ggplot(weekday_data, aes(x = weekday, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribution of accidents by weekday",
    x = "Weekday",
    y = "Proportion"
  )

## via type
viaType <- df %>%
  group_by(period, D_TIPUS_VIA) %>%
  summarise(n = n(), .groups = "drop")

viaType <- viaType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(viaType, aes(x = D_TIPUS_VIA, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by road type",
    x = "Road type",
    y = "Proportion"
  ) 

## severity 
severity <- df %>%
  group_by(period, D_GRAVETAT) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(severity, aes(x = D_GRAVETAT, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Severity distribution by period",
    x = "Severity",
    y = "Proportion"
  ) + coord_flip()

## hour of the day
hour_props <- df %>%
  mutate(hour = lubridate::hour(hor)) %>%
  group_by(period, hour) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(hour_props, aes(x = hour, y = prop, color = period)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly distribution of accidents",
    x = "Hour of day",
    y = "Proportion"
  )

## nom com
comType <- df %>%
  group_by(period, nomCom) %>%
  summarise(n = n(), .groups = "drop")

comType <- comType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(comType, aes(x = nomCom, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by comuna type",
    x = "Road type",
    y = "Proportion"
  ) 


## nom dem
demType <- df %>%
  group_by(period, nomDem) %>%
  summarise(n = n(), .groups = "drop")

demType <- demType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(demType, aes(x = nomDem, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by demarcation type",
    x = "Road type",
    y = "Proportion"
  ) 

## nom mun
MunType <- df %>%
  group_by(period, nomMun) %>%
  summarise(n = n(), .groups = "drop")

MunType <- MunType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(MunType, aes(x = nomMun, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by municipie type",
    x = "Road type",
    y = "Proportion"
  ) 

## nmort 
nMortType <- df %>%
  group_by(period, F_MORTS) %>%
  summarise(n = n(), .groups = "drop")

nMortType <- nMortType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(nMortType, aes(x = F_MORTS, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by quantity of deads",
    x = "Road type",
    y = "Proportion"
  ) 

## D_LLUMINOSITAT 
lul <- df %>%
  group_by(period, D_LLUMINOSITAT) %>%
  summarise(n = n(), .groups = "drop")

lul <- lul %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(lul, aes(x = D_LLUMINOSITAT, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by D_LLUMINOSITAT",
    x = "Road type",
    y = "Proportion"
  ) 

## D_LLUMINOSITAT 
speed <- df %>%
  group_by(period, C_VELOCITAT_VIA) %>%
  summarise(n = n(), .groups = "drop")

speed <- speed %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(speed, aes(x = C_VELOCITAT_VIA, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Accident distribution by speed",
    x = "Road type",
    y = "Proportion"
  ) 