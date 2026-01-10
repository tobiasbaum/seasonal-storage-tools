csvRaw <- read.csv("solar.csv", sep=";", na.strings = "NaN")

# remove outliers
csv <- csvRaw
csv$production <- NULL
# set outliers to NA
csv <- csv[csv$solarProduction < 30,]
csv <- csv[csv$consumption < 90,]
csv <- csv[csv$export < csv$solarProduction + 1,]

write.csv(csv, 'filtered_data.csv')
