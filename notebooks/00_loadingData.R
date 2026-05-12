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


### clean data that is not useful
df$pk[df$pk == 999999] <- NA
df$C_VELOCITAT_VIA[df$C_VELOCITAT_VIA > 150] <- NA
df$D_FUNC_ESP_VIA[df$D_FUNC_ESP_VIA =="Sense funció especial"] <- NA
df$D_CIRCULACIO_MESURES_ESP[df$D_CIRCULACIO_MESURES_ESP =="No n'hi ha"] <- NA
df$D_CARRIL_ESPECIAL[df$D_CARRIL_ESPECIAL =="No n'hi ha"] <- NA
df$D_BOIRA[df$D_BOIRA =="No n'hi ha"] <- NA
df$D_INFLUIT_BOIRA[df$D_INFLUIT_BOIRA =="Sense especificar"] <- NA
df$D_CARACT_ENTORN[df$D_CARACT_ENTORN =="Sense Especificar"] <- NA

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

head(df)

glimpse(df)
summary(df)

save(df, file = "../data/processed/df_clean.RData")

