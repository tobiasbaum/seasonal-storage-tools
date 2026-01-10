library(dplyr)
library(ggplot2)
library(mice)

csvRaw <- read.csv("solar.csv", sep=";", na.strings = "NaN")
csvRaw$dow <- as.factor(csvRaw$dow)
csvRaw$dateH <- substr(csvRaw$date, 1, 14);

cloudCover <- read.csv("cloudCover.csv", sep=";", na.strings = "NaN")
cloudCover$dateH <- substr(cloudCover$time, 1, 14);
csvRaw <- csvRaw %>%
  left_join(cloudCover %>% select(dateH, cloud_cover), by = "dateH")

# Deklination: -23.45° Wintersonnenwende, 0° Frühlingsanfang, 23.45° SSW
csvRaw$deklination <- 23.45*cos(csvRaw$daysSummer / 365 * 2 * pi)
# Mittagssonnenwinkel = 90° - Breitengrad + Deklination
csvRaw$mittagssonnenwinkel <- 90 - 52 + csvRaw$deklination
# 1h = 15°
phi <- 52 / 360 * 2 * pi
epsilon <- 23.45 / 360 * 2 * pi
# Sonnenscheinformel, Uni Jena
csvRaw$sonnenwinkel <- asin(
  sin(epsilon)*sin(phi)*cos(2*pi*csvRaw$daysSummer/365) +
  cos(phi)*sqrt(1 - (sin(epsilon)*cos(2*pi*csvRaw$daysSummer/365))**2)*cos(2*pi*csvRaw$minutesNoon/24/60)) / 2 / pi * 360
csvRaw$sonnenwinkel <- pmax(0, csvRaw$sonnenwinkel)

csvRaw$cloud_cover_day <- csvRaw$cloud_cover
csvRaw$cloud_cover_day[which(csvRaw$sonnenwinkel <= 0)] <- "nacht"
csvRaw$cloud_cover_day <- as.factor(csvRaw$cloud_cover_day)

# remove outliers
#csv <- csvRaw[!is.na(csvRaw$production) & csvRaw$production < 80,]
csv <- csvRaw
csv$production <- NULL
# set outliers to NA
csv$solarProduction[which(csv$solarProduction >= 30)] <- NA
csv$consumption[which(csv$consumption >= 90)] <- NA
# wrap around dst
csv[csv$minutesNoon < -751,]$minutesNoon <- csv[csv$minutesNoon < -751,]$minutesNoon + 24 * 60

csv$weekend <- csv$dow == "6_SAT" | csv$dow == "7_SUN"
csv$summer <- csv$daysSummer > -45 & csv$daysSummer < 45
csv$winter <- csv$daysSummer < -135 | csv$daysSummer > 135
csv$distSummer <- abs(csv$daysSummer)
csv$distNoon <- abs(csv$minutesNoon)

# Dachneigung: 34°
csv$flaechenfaktorVert <- pmax(0, cos((90 - csv$sonnenwinkel - 34)/360*2*pi))
# Dachausrichtung: 196° (180° wäre Süd)
csv$flaechenfaktorHoriz <- pmax(0, cos(csv$minutesNoon/24/60*2*pi - 16/360*2*pi))
csv$flaechenfaktor <- csv$flaechenfaktorHoriz * csv$flaechenfaktorVert

write.csv(csv, 'filtered_data.csv')


createProfile <- function(csvData, fromDay1, toDay1, fromDay2, toDay2) {
  relevantDays <- csvData %>% 
    filter((daysSummer >= fromDay1 & daysSummer < toDay1) | (daysSummer >= fromDay2 & daysSummer < toDay2)) %>%
    filter(!is.na(consumption))
  profileData <- relevantDays %>%
     group_by(minutesNoon) %>% 
     summarise(consumption = mean(consumption, na.rm = TRUE))
  quality <- 0
  for (t in -50:45) {
    time <- t * 15 - 1
    profileValue <- profileData[profileData$minutesNoon == time,]$consumption
    quality <- quality + mean((relevantDays[relevantDays$minutesNoon == time,]$consumption - profileValue)^2)
  }
  return(list(profileData, quality))
}

determineBestSplits <- function(csvData) {
  bestStartSpring <- 0
  bestStartSummer <- 0
  bestStartAutumn <- 0
  bestStartWinter <- 0
  bestCold <- NA
  bestMiddle <- NA
  bestWarm <- NA
  bestTotalQual <- 99999999
  for (startSpring in seq(-10, 0, by=1)) {
    for (startSummer in seq(7, 17, by=1)) {
      for (startAutumn in seq(18, 27, by=1)) {
        for (startWinter in seq(75, 86, by=1)) {
          curCold <- createProfile(csvData, -170, startSpring, startWinter, 193)
          curMiddle <- createProfile(csvData, startSpring, startSummer, startAutumn, startWinter)
          curWarm <- createProfile(csvData, startSummer, startAutumn, -1, -1)
          curTotalQual <- curCold[[2]] + curMiddle[[2]] + curWarm[[2]]
          print(paste(startSpring, startSummer, startAutumn, startWinter, curTotalQual, bestTotalQual))
          if (curTotalQual < bestTotalQual) {
            print("new best")
            bestTotalQual <- curTotalQual
            bestCold <- curCold
            bestMiddle <- curMiddle
            bestWarm <- curWarm
            bestStartSpring <- startSpring
            bestStartSummer <- startSummer
            bestStartAutumn <- startAutumn
            bestStartWinter <- startWinter
          }
        }
      }
    }
  }
  return(list(
    list(bestStartSpring, bestStartSummer, bestStartAutumn, bestStartWinter), 
    bestCold[1],
    bestMiddle[1],
    bestWarm[1],
    bestTotalQual))
}
bestResult <- determineBestSplits(csv)



csvNoCloud <- csv[csv$cloud_cover == 'none',]
csvNoCloud$datetime <- csvNoCloud$daysSummer * 24 * 60 + csvNoCloud$minutesNoon
csvNoCloudSummerDays <- csvNoCloud[csvNoCloud$daysSummer > -10 & csvNoCloud$daysSummer < 10,]
factor <- max(csvNoCloudSummerDays$solarProduction, na.rm=TRUE) / max(csvNoCloudSummerDays$flaechenfaktor, na.rm = TRUE)
ggplot(csvNoCloudSummerDays) + 
  geom_point(aes(x=minutesNoon,y=factor*flaechenfaktor)) +
  geom_point(aes(x=minutesNoon,y=flaechenfaktorVert),colour="green") +
  geom_point(aes(x=minutesNoon,y=flaechenfaktorHoriz),colour="blue") +
  geom_point(aes(x=minutesNoon,y=solarProduction),colour="red")
csvNoCloudNoon <- csvNoCloud[csvNoCloud$minutesNoon > -40 & csvNoCloud$minutesNoon < 40,]
ggplot(csvNoCloudNoon) + 
  geom_point(aes(x=daysSummer,y=factor*flaechenfaktor)) +
  geom_point(aes(x=daysSummer,y=flaechenfaktorVert),colour="green") +
  geom_point(aes(x=daysSummer,y=flaechenfaktorHoriz),colour="blue") +
  geom_point(aes(x=daysSummer,y=solarProduction),colour="red")
  
# imputation for solarProduction
csv$solarProduction[which(csv$sonnenwinkel <= 0 & is.na(csv$solarProduction))] <- 0
csv <- complete(mice(csv, method = "cart"))

prod_model <- lm(solarProduction ~ sonnenwinkel + cloud_cover_day + minutesNoon, data = csv)
summary(prod_model)
prod_model2 <- lm(solarProduction ~ flaechenfaktor + cloud_cover_day, data = csv)
summary(prod_model2)


boxplot(solarProduction ~ cloud_cover_day, data = csv,
        xlab = "Cloud cover",
        ylab = "Solar Production",
        main = "Box and Whiskers Plot of Production by cloud cover",
        col = "lightblue")

perDay <- csv %>%
  group_by(daysSummer, dow) %>%
  summarise(
    solarProduction = sum(solarProduction, na.rm = TRUE),
    consumption = sum(consumption, na.rm = TRUE),
    max_energyLevel = max(storageEnergyLevel, na.rm = TRUE),
    msw = max(mittagssonnenwinkel)
  )

boxplot(solarProduction ~ dow, data = perDay,
        xlab = "Day of Week (dow)",
        ylab = "Solar Production",
        main = "Box and Whiskers Plot of Production by Day of Week",
        col = "lightblue")

boxplot(consumption ~ dow, data = perDay,
        xlab = "Day of Week (dow)",
        ylab = "Consumption",
        main = "Box and Whiskers Plot of Consumption by Day of Week",
        col = "pink")

ggplot(perDay, aes(x = daysSummer, y = msw)) +
  geom_point() +
  labs(title = "Deklination vs Days in Summer",
       x = "Days in Summer",
       y = "Deklination") +
  theme_minimal()

ggplot(perDay, aes(x = daysSummer, y = solarProduction)) +
  geom_point() +
  geom_smooth(col = "blue") + 
  labs(title = "Solar Production vs Days in Summer",
       x = "Days in Summer",
       y = "Solar Production") +
  theme_minimal()

ggplot(perDay, aes(x = daysSummer, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Days in Summer",
       x = "Days in Summer",
       y = "Consumption") +
  theme_minimal()

ggplot(perDay, aes(x = daysSummer, y = max_energyLevel)) +
  geom_point() +
  geom_smooth(col = "green") + 
  labs(title = "Max store energy level vs Days in Summer",
       x = "Days in Summer",
       y = "Energy level") +
  theme_minimal()

perTime <- csv %>%
  group_by(minutesNoon) %>%
  summarise(
    solarProduction = mean(solarProduction, na.rm = TRUE, trim = 0.1),
    consumption = mean(consumption, na.rm = TRUE, trim = 0.1),
    energyLevel = mean(storageEnergyLevel, na.rm = TRUE, trim = 0.1)
  )

perTimeWeekend <- csv[csv$weekend,] %>%
  group_by(minutesNoon) %>%
  summarise(
    solarProduction = mean(solarProduction, na.rm = TRUE, trim = 0.1),
    consumption = mean(consumption, na.rm = TRUE, trim = 0.1),
    energyLevel = mean(storageEnergyLevel, na.rm = TRUE, trim = 0.1)
  )

perTimeWeekday <- csv[!csv$weekend,] %>%
  group_by(minutesNoon) %>%
  summarise(
    solarProduction = mean(solarProduction, na.rm = TRUE, trim = 0.1),
    consumption = mean(consumption, na.rm = TRUE, trim = 0.1),
    energyLevel = mean(storageEnergyLevel, na.rm = TRUE, trim = 0.1)
  )

perTimeSummer <- csv[csv$summer,] %>%
  group_by(minutesNoon) %>%
  summarise(
    solarProduction = mean(solarProduction, na.rm = TRUE, trim = 0.1),
    consumption = mean(consumption, na.rm = TRUE, trim = 0.1),
    energyLevel = mean(storageEnergyLevel, na.rm = TRUE, trim = 0.1)
  )

perTimeWinter <- csv[csv$winter,] %>%
  group_by(minutesNoon) %>%
  summarise(
    solarProduction = mean(solarProduction, na.rm = TRUE, trim = 0.1),
    consumption = mean(consumption, na.rm = TRUE, trim = 0.1),
    energyLevel = mean(storageEnergyLevel, na.rm = TRUE, trim = 0.1)
  )

ggplot(perTime, aes(x = minutesNoon, y = solarProduction)) +
  geom_point() +
  geom_smooth(col = "blue") + 
  labs(title = "Production vs Time",
       x = "Minutes noon",
       y = "Solar Production") +
  theme_minimal()

ggplot(perTimeWinter, aes(x = minutesNoon, y = solarProduction)) +
  geom_point() +
  geom_smooth(col = "blue") + 
  labs(title = "Production vs Time in winter",
       x = "Minutes noon",
       y = "Solar Production") +
  theme_minimal()

ggplot(perTimeSummer, aes(x = minutesNoon, y = solarProduction)) +
  geom_point() +
  geom_smooth(col = "blue") + 
  labs(title = "Production vs Time in summer",
       x = "Minutes noon",
       y = "Solar Production") +
  theme_minimal()

ggplot(perTime, aes(x = minutesNoon, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Time",
       x = "Minutes noon",
       y = "Consumption") +
  theme_minimal()

ggplot(perTimeWeekend, aes(x = minutesNoon, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Time on weekends",
       x = "Minutes noon",
       y = "Consumption") +
  theme_minimal()

ggplot(perTimeWeekday, aes(x = minutesNoon, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Time on weekdays",
       x = "Minutes noon",
       y = "Consumption") +
  theme_minimal()

ggplot(perTimeSummer, aes(x = minutesNoon, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Time in summer",
       x = "Minutes noon",
       y = "Consumption") +
  theme_minimal()

ggplot(perTimeWinter, aes(x = minutesNoon, y = consumption)) +
  geom_point() +
  geom_smooth(col = "pink") + 
  labs(title = "Consumption vs Time in winter",
       x = "Minutes noon",
       y = "Consumption") +
  theme_minimal()

ggplot(perTime, aes(x = minutesNoon, y = energyLevel)) +
  geom_point() +
  geom_smooth(col = "green") + 
  labs(title = "Energy Level vs Time",
       x = "Minutes noon",
       y = "Energy Level") +
  theme_minimal()

ggplot(perTimeWinter, aes(x = minutesNoon, y = energyLevel)) +
  geom_point() +
  geom_smooth(col = "green") + 
  labs(title = "Energy Level vs Time in winter",
       x = "Minutes noon",
       y = "Energy Level") +
  theme_minimal()

ggplot(perTimeSummer, aes(x = minutesNoon, y = energyLevel)) +
  geom_point() +
  geom_smooth(col = "green") + 
  labs(title = "Energy Level vs Time in summer",
       x = "Minutes noon",
       y = "Energy Level") +
  theme_minimal()

ggplot(data = csv, aes(x = daysSummer, y = minutesNoon / 15, fill = consumption)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red", "pink")) +
  labs(title = "Heatmap of Consumption by distSummer and distNoon",
       x = "distSummer",
       y = "distNoon",
       fill = "Consumption") +
  theme_minimal()

ggplot(data = csv, aes(x = daysSummer, y = minutesNoon / 15, fill = storageEnergyLevel)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red", "pink")) +
  labs(title = "Heatmap of Energy Level by distSummer and distNoon",
       x = "distSummer",
       y = "distNoon",
       fill = "storageEnergyLevel") +
  theme_minimal()

ggplot(data = csv, aes(x = daysSummer, y = minutesNoon / 15, fill = solarProduction)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red", "pink")) +
  labs(title = "Heatmap of Solar Production by distSummer and distNoon",
       x = "distSummer",
       y = "distNoon",
       fill = "solarProduction") +
  theme_minimal()

ggplot(data = csv, aes(x = daysSummer, y = minutesNoon / 15, fill = sonnenwinkel)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red", "pink")) +
  labs(title = "Heatmap of sonnenwinkel by distSummer and distNoon",
       x = "distSummer",
       y = "distNoon",
       fill = "sonnenwinkel") +
  theme_minimal()

cor(csv$sonnenwinkel, csv$solarProduction, use="complete.obs")