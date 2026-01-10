csvRaw <- read.csv("solar.csv", sep=";", na.strings = "NaN")

csv <- csvRaw
csv$production <- NULL
# remove outliers
csv <- csv[is.na(csv$solarProduction) | csv$solarProduction < 30,]
csv <- csv[is.na(csv$consumption) | csv$consumption < 90,]
csv <- csv[is.na(csv$export) | is.na(csv$solarProduction) | csv$export < csv$solarProduction + 1,]

write.csv(csv, 'filtered_data.csv')
