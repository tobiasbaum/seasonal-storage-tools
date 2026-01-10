library(dplyr)
library(tidyr)
library(ggplot2)

csv <- read.csv("imputed_data_0.csv", sep=";", na.strings = "NaN")
csv$summer <- csv$dateMonth >= 5 & csv$dateMonth <= 8
csv$winter <- csv$dateMonth >= 11 | csv$dateMonth <= 2

summary(csv)
summary(csv[csv$summer,])
summary(csv[csv$winter,])

# Alle numerischen Spalten aggregiert nach dateMonth
df_means <- csv %>%
  group_by(dateMonth) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -dateMonth,
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  filter(variable != "dateMs") %>%
  filter(variable != "storageEnergyLevel")

# Plotten
ggplot(df_means, aes(x = dateMonth, y = mean_value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Mittelwerte je Monat",
       x = "Monat",
       y = "Mittelwert")