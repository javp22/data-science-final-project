library(tidyverse)

load("../data/processed/df_clean.RData")

# accidents per year
acc_year <- df %>%
  count(year)

ggplot(acc_year, aes(year, n)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
  labs(
    title = "Accidents per year",
    x = "Year",
    y = "Count"
  )

# seasonality
season_props <- df %>%
  count(period, season) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(season_props, aes(season, prop, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Accidents by season",
    x = "Season",
    y = "Proportion"
  )

# monthly distribution
monthly_props <- df %>%
  count(period, month) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(monthly_props, aes(month, prop, color = period)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly distribution",
    x = "Month",
    y = "Proportion"
  )

# weekday
weekday_props <- df %>%
  count(period, weekday) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(weekday_props, aes(weekday, prop, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Weekday distribution",
    x = "Weekday",
    y = "Proportion"
  )

# hour
hour_props <- df %>%
  count(period, hour) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(hour_props, aes(hour, prop, color = period)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly distribution",
    x = "Hour",
    y = "Proportion"
  )

# peak hour
peak_props <- df %>%
  count(period, is_peak_hour) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(peak_props, aes(is_peak_hour, prop, fill = period)) +
  geom_col(position = "dodge")

# weekend
weekend_props <- df %>%
  count(period, is_weekend) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(weekend_props, aes(is_weekend, prop, fill = period)) +
  geom_col(position = "dodge")

# night/day
night_props <- df %>%
  count(period, is_night) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(night_props, aes(is_night, prop, fill = period)) +
  geom_col(position = "dodge")

# affected highways
road_focus <- df %>%
  count(period, affected_highway) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(road_focus, aes(affected_highway, prop, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Affected highways",
    x = "Highway",
    y = "Proportion"
  )

# road type
road_type <- df %>%
  count(period, D_TIPUS_VIA) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(road_type, aes(D_TIPUS_VIA, prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# speed
speed <- df %>%
  count(period, C_VELOCITAT_VIA) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(speed, aes(C_VELOCITAT_VIA, prop, fill = period)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# urban/rural
zone <- df %>%
  count(period, zona) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(zone, aes(zona, prop, fill = period)) +
  geom_col(position = "dodge")

# severity
severity <- df %>%
  count(period, D_GRAVETAT) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(severity, aes(D_GRAVETAT, prop, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip()

# fatalities
fatalities <- df %>%
  count(period, F_MORTS) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(fatalities, aes(as.factor(F_MORTS), prop, fill = period)) +
  geom_col(position = "dodge")

# victims
victims <- df %>%
  count(period, F_VICTIMES) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(victims, aes(as.factor(F_VICTIMES), prop, fill = period)) +
  geom_col(position = "dodge")

# vehicles
vehicles <- df %>%
  select(period,
         F_UNITATS_IMPLICADES,
         F_VEH_LLEUGERS_IMPLICADES,
         F_VEH_PESANTS_IMPLICADES) %>%
  pivot_longer(-period,
               names_to = "variable",
               values_to = "value")

ggplot(vehicles, aes(value, fill = period)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free")

# accident type
acc_type <- df %>%
  count(period, D_SUBTIPUS_ACCIDENT) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(acc_type,
       aes(reorder(D_SUBTIPUS_ACCIDENT, prop), prop, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip()

# environment
env_vars <- c(
  "D_LLUMINOSITAT",
  "D_CLIMATOLOGIA",
  "D_SUPERFICIE",
  "D_SENTITS_VIA",
  "D_INTER_SECCIO"
)

for(v in env_vars){
  
  temp <- df %>%
    count(period, .data[[v]]) %>%
    group_by(period) %>%
    mutate(prop = n / sum(n))
  
  print(
    ggplot(temp, aes(.data[[v]], prop, fill = period)) +
      geom_col(position = "dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste("Distribution:", v),
        x = v,
        y = "Proportion"
      )
  )
}

# covid effect
covid_effect <- df %>%
  count(period, covid_period) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

ggplot(covid_effect, aes(covid_period, prop, fill = period)) +
  geom_col(position = "dodge")

# missing values
missing_data <- df %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_prop"
  ) %>%
  arrange(desc(missing_prop))

vars_to_remove <- missing_data %>%
  filter(missing_prop > 0.3) %>%
  pull(variable)

print(vars_to_remove)

save(
  df,
  acc_year,
  season_props,
  monthly_props,
  weekday_props,
  hour_props,
  road_focus,
  road_type,
  speed,
  zone,
  severity,
  fatalities,
  victims,
  acc_type,
  covid_effect,
  missing_data,
  vars_to_remove,
  file = "../data/processed/eda_objects.RData"
)
