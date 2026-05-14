library(tidyverse)
library(skimr)
library(lubridate)

# data reading
## upload data
df <- read.csv("../data/raw/Accidents_de_trànsit_amb_morts_o_ferits_greus_a_Catalunya_20260427.csv")

glimpse(df)
summary(df)
skim(df)

# data cleaning
## date
df$dat <- as.character(df$dat)
df$dat <- trimws(df$dat)
df$dat <- dmy(df$dat)

## create variable that indicates if the information is before or after 2021-09-01
## and need to be factor
df$period <- ifelse(df$dat >= as.Date("2021-09-01"), "After", "Before")
df$period <- factor(df$period)

##pk
df$pk <- gsub(",", ".", df$pk)
df$pk <- as.numeric(df$pk)

missing_values <- c(
  "Sense especificar",
  "Sense Especificar",
  "No n'hi ha",
  "Sense funció especial"
)

### clean data that is not useful
df$pk[df$pk == 999999] <- NA
df$C_VELOCITAT_VIA[df$C_VELOCITAT_VIA > 150] <- NA
# replace across all character columns
df <- df %>%
  mutate(
    across(
      where(is.character),
      ~na_if(.x, "Sense especificar")
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~na_if(.x, "Sense Especificar")
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~na_if(.x, "No n'hi ha")
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~na_if(.x, "Sense funció especial")
    )
  )




## hor
df <- df %>%
  mutate(
    hor = str_replace(hor, ",", ":"),
    hor = ifelse(str_detect(hor, ":"), hor, paste0(hor, ":00")),
    hor = sprintf("%05s", hor),
    hor = hm(hor)
  )

## categorical variables
df$zona <- as.factor(df$zona)
df$D_TIPUS_VIA <- as.factor(df$D_TIPUS_VIA)
df$D_GRAVETAT <- as.factor(df$D_GRAVETAT)
df$grupHor <- as.factor(df$grupHor)
df$tipAcc <- as.factor(df$tipAcc)

## time variables
df$year <- year(df$dat)
df$month <- month(df$dat)
df$weekday <- wday(df$dat, label = TRUE)
df$weekend <- df$weekday %in% c("Sat", "Sun")

## imbalance
table(df$period)
prop.table(table(df$period))

df <- df %>%
  mutate(
    hour = hour(hor),
    weekday = factor(weekday,
                     levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
    
    season = case_when(
      month %in% c(12,1,2) ~ "Winter",
      month %in% c(3,4,5) ~ "Spring",
      month %in% c(6,7,8) ~ "Summer",
      TRUE ~ "Autumn"
    ),
    
    is_weekend = if_else(weekday %in% c("Sat","Sun"), "Weekend", "Weekday"),
    
    is_peak_hour = if_else(hour %in% c(7,8,9,17,18,19,20),
                           "Peak", "Non-peak"),
    
    is_night = if_else(hour >= 22 | hour <= 6,
                       "Night", "Day"),
    
    covid_period = if_else(year %in% c(2020, 2021),
                           "COVID", "Normal"),
    
    years_since_2010 = year - 2010,
    
    affected_highway = case_when(
      str_detect(via, "AP-7") ~ "AP-7",
      str_detect(via, "AP-2") ~ "AP-2",
      str_detect(via, "C-32") ~ "C-32",
      str_detect(via, "C-33") ~ "C-33",
      TRUE ~ "Other"
    )
  )

head(df)

glimpse(df)
summary(df)

save(df, file = "../data/processed/df_clean.RData")

