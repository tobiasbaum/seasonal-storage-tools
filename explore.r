library(dplyr)
library(tidyr)
library(ggplot2)

csv <- read.csv("imputed_data_0.csv", sep=";", na.strings = "NaN")
csv$summer <- csv$dateMonth >= 5 & csv$dateMonth <= 8
csv$winter <- csv$dateMonth >= 11 | csv$dateMonth <= 2

summary(csv)
summary(csv[csv$summer,])
summary(csv[csv$winter,])

summerData <- csv[csv$summer,]
winterData <- csv[csv$winter,]

hist(summerData$export)
hist(winterData$imported)
sum(winterData$imported > 0.01 & winterData$storageEnergyLevel > 1) # Fremdbezug trotz Speicher
sum(winterData$imported > 0.01 & winterData$storageEnergyLevel <= 1) # Fremdbezug bei leerem Speicher
sum(winterData$imported <= 0.01) # kein Fremdbezug
summary(winterData[winterData$imported > 0.01 & winterData$storageEnergyLevel > 1,]) # Fremdbezug trotz Speicher
summary(winterData[winterData$imported > 0.01 & winterData$storageEnergyLevel <= 1,]) # Fremdbezug bei leerem Speicher

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

# Energiebedarf Winter
energyWinter <- sum(winterData$consumption) / 4
# Energiebedarf Sommer
energySummer <- sum(summerData$consumption) / 4
energyHeating <- energyWinter - energySummer
shareHeating <- energyHeating / energyWinter
# Importbedarf Winter
sum(winterData$imported) / 4

# Gesamteinspeisung Sommer
sum(summerData$export) / 4
max(summerData$export)
sum(pmin(summerData$export, 6)) / 4
sum(pmin(summerData$export, 5)) / 4
sum(pmin(summerData$export, 3)) / 4
sum(pmin(summerData$export, 2)) / 4