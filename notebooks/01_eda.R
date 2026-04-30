library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
# data reading
## upload data
df <- read.csv("../data/raw/Accidents_de_trànsit_amb_morts_o_ferits_greus_a_Catalunya_20260427.csv")

glimpse(df)
summary(df)
skim(df)

# data loading
## date chr to date format
df <- clean_names(df)
df$dat <- dmy(df$dat)

## create variable that indicates if the information is before or after 2021-09-01
df$period <- ifelse(df$dat >= as.Date("2021-09-01"), "After", "Before")

## time variables
df$year <- year(df$dat)
df$month <- month(df$dat)
df$weekday <- wday(df$dat, label = TRUE)
df$weekend <- df$weekday %in% c("Sat", "Sun")

## imbalance
table(df$period)
prop.table(table(df$period))

# EDA
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
  group_by(period, d_tipus_via) %>%
  summarise(n = n(), .groups = "drop")

viaType <- viaType %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(viaType, aes(x = d_tipus_via, y = prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
