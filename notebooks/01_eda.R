library(tidyverse)

# create folder if it does not exist
dir.create("../plots/eda", recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 1. TEMPORAL PATTERNS
# =========================================================

acc_year <- df %>%
  count(year)

p_acc_year <- ggplot(acc_year, aes(year, n)) +
  geom_col(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_vline(xintercept = 2021, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = acc_year$year) +
  labs(title = "Accidents by year", x = "Year", y = "Count") +
  theme_minimal()

ggsave(
  "../plots/eda/accidents_by_year.png",
  plot = p_acc_year,
  width = 8,
  height = 5,
  dpi = 300
)

# season
season_props <- df %>%
  count(period, season) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n) * 100)

p_season <- ggplot(season_props, aes(period, prop, fill = season)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(title = "Seasonal distribution", x = "", y = "Percentage", fill = "Season") +
  theme_minimal()

ggsave(
  "../plots/eda/season_distribution.png",
  plot = p_season,
  width = 8,
  height = 5,
  dpi = 300
)

# weekday
weekday_props <- df %>%
  count(period, weekday) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_weekday <- ggplot(weekday_props, aes(weekday, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Weekday distribution", x = "Day", y = "Percentage") +
  theme_minimal()

ggsave(
  "../plots/eda/weekday_distribution.png",
  plot = p_weekday,
  width = 8,
  height = 5,
  dpi = 300
)

# hour
hour_props <- df %>%
  count(period, hour) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_hour <- ggplot(hour_props, aes(hour, prop, color = period)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Hourly distribution", x = "Hour", y = "Proportion") +
  theme_minimal()

ggsave(
  "../plots/eda/hourly_distribution.png",
  plot = p_hour,
  width = 8,
  height = 5,
  dpi = 300
)

# peak hour
peak_props <- df %>%
  count(period, is_peak_hour) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_peak <- ggplot(peak_props, aes(is_peak_hour, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Peak vs Non-Peak Hour", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/peak_hour_distribution.png",
  plot = p_peak,
  width = 8,
  height = 5,
  dpi = 300
)

# weekend
weekend_props <- df %>%
  count(period, is_weekend) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_weekend <- ggplot(weekend_props, aes(is_weekend, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Weekend vs Weekday", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/weekend_distribution.png",
  plot = p_weekend,
  width = 8,
  height = 5,
  dpi = 300
)

# night/day
night_props <- df %>%
  count(period, is_night) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_night <- ggplot(night_props, aes(is_night, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Day vs Night", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/night_distribution.png",
  plot = p_night,
  width = 8,
  height = 5,
  dpi = 300
)

# =========================================================
# 2. ROAD INFRASTRUCTURE
# =========================================================

road_focus <- df %>%
  count(period, affected_highway) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_road_focus <- ggplot(road_focus,
                       aes(x = reorder(affected_highway, -prop),
                           prop,
                           fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Affected highways", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/affected_highways.png",
  plot = p_road_focus,
  width = 8,
  height = 5,
  dpi = 300
)

# road type
road_type <- df %>%
  count(period, D_TIPUS_VIA) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_road_type <- ggplot(road_type,
                      aes(x = reorder(D_TIPUS_VIA, -prop),
                          prop,
                          fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Road type distribution", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/road_type_distribution.png",
  plot = p_road_type,
  width = 8,
  height = 5,
  dpi = 300
)

# speed
speed <- df %>%
  filter(!is.na(C_VELOCITAT_VIA)) %>%
  mutate(speed_bin = cut(
    C_VELOCITAT_VIA,
    breaks = c(0, 50, 90, 120, 150),
    include.lowest = TRUE
  )) %>%
  count(period, speed_bin) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_speed <- ggplot(speed, aes(speed_bin, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Speed distribution", x = "Speed range", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/speed_distribution.png",
  plot = p_speed,
  width = 8,
  height = 5,
  dpi = 300
)

# zone
zone <- df %>%
  count(period, zona) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_zone <- ggplot(zone, aes(zona, prop, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Urban vs Rural", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/urban_rural_distribution.png",
  plot = p_zone,
  width = 8,
  height = 5,
  dpi = 300
)

# =========================================================
# 3. ENVIRONMENTAL CONDITIONS
# =========================================================

climate <- df %>%
  count(period, D_CLIMATOLOGIA) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_climate <- ggplot(climate,
                    aes(x = reorder(D_CLIMATOLOGIA, -prop),
                        prop,
                        fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Weather conditions", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/weather_conditions.png",
  plot = p_climate,
  width = 9,
  height = 6,
  dpi = 300
)

# climate influence
climate_influence_vars <- c(
  "D_INFLUIT_BOIRA",
  "D_INFLUIT_ESTAT_CLIMA",
  "D_INFLUIT_INTEN_VENT",
  "D_INFLUIT_VISIBILITAT",
  "D_INFLUIT_LLUMINOSITAT"
)

climate_influence <- df %>%
  select(period, all_of(climate_influence_vars)) %>%
  pivot_longer(-period,
               names_to = "variable",
               values_to = "value") %>%
  count(period, variable, value) %>%
  group_by(period, variable) %>%
  mutate(prop = n / sum(n))

p_climate_influence <- ggplot(climate_influence,
                              aes(x = value,
                                  y = prop,
                                  fill = period)) +
  geom_col(position = "dodge") +
  facet_wrap(~variable) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Environmental influence factors",
       x = "",
       y = "Percentage",
       fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "../plots/eda/environmental_influence.png",
  plot = p_climate_influence,
  width = 12,
  height = 8,
  dpi = 300
)

# =========================================================
# 4. SEVERITY & OUTCOMES
# =========================================================

severity <- df %>%
  count(period, D_GRAVETAT) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_severity <- ggplot(severity,
                     aes(D_GRAVETAT, prop, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity", x = "", y = "Percentage", fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/severity_distribution.png",
  plot = p_severity,
  width = 8,
  height = 5,
  dpi = 300
)

# outcomes
outcomes <- df %>%
  select(period, F_MORTS, F_VICTIMES) %>%
  pivot_longer(
    cols = c(F_MORTS, F_VICTIMES),
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(period, variable, value) %>%
  group_by(period, variable) %>%
  mutate(prop = n / sum(n))

p_outcomes <- ggplot(outcomes,
                     aes(x = as.factor(value),
                         y = prop,
                         fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Accident outcomes",
    x = "Count",
    y = "Proportion",
    fill = "Period"
  ) +
  theme_minimal()

ggsave(
  "../plots/eda/accident_outcomes.png",
  plot = p_outcomes,
  width = 10,
  height = 6,
  dpi = 300
)

# accident type
acc_type <- df %>%
  count(period, D_SUBTIPUS_ACCIDENT) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n))

p_acc_type <- ggplot(acc_type,
                     aes(reorder(D_SUBTIPUS_ACCIDENT, prop),
                         prop,
                         fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Accident type",
       x = "",
       y = "Percentage",
       fill = "Period") +
  theme_minimal()

ggsave(
  "../plots/eda/accident_type_distribution.png",
  plot = p_acc_type,
  width = 10,
  height = 7,
  dpi = 300
)

# =========================================================
# 5. VEHICLES
# =========================================================

vehicles <- df %>%
  select(period,
         F_UNITATS_IMPLICADES,
         F_VEH_LLEUGERS_IMPLICADES,
         F_VEH_PESANTS_IMPLICADES) %>%
  pivot_longer(-period,
               names_to = "variable",
               values_to = "value") %>%
  count(period, variable, value) %>%
  group_by(period, variable) %>%
  mutate(prop = n / sum(n))

p_vehicles <- ggplot(vehicles,
                     aes(x = as.factor(value),
                         y = prop,
                         fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Vehicle involvement distribution",
    x = "Number of vehicles involved",
    y = "Proportion",
    fill = "Period"
  ) +
  theme_minimal()

ggsave(
  "../plots/eda/vehicle_involvement.png",
  plot = p_vehicles,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\nAll plots saved in ../plots/eda\n")