
#setwd("/storage/homes/michael/Seattle/")
setwd("~/Documents/Seattle/BikeData/BikeData/")

library(ggplot2)
library(reshape)
library(reshape2)
library(lubridate)
library(car)
library(plyr)
library(foreign)
library(RSocrata)

source("bikeDataFunctions.R")

#Read the data from the SDOT website
bike <- readBikeData()

#Need to remove bad data from Fremont bridge counter
ggplot(bike[bike$Location == "Fremont", ]) + theme_bw() + 
  geom_point(aes(x = Date, y = value))
bike[which(bike$Location == "Fremont" & bike$value > 1000), ]
#Need to take out April 23, 25, 28, and 29
badrows <- which(bike$Location == "Fremont" & bike$date %in% as.Date(c("2014-04-23", "2014-04-25", "2014-04-26","2014-04-27","2014-04-28", "2014-04-29", "2012-10-02")))
bike <- bike[-badrows,]
ggplot(bike[bike$Location == "Fremont", ]) + theme_bw() + 
  geom_point(aes(x = Date, y = value))

#Merge in the weather data
bikeplus <- mergeWeather(bike, wfile = "WASeattle24233hrly.dta")
if(nrow(bikeplus) != nrow(bike)) stop("Error in merging weather")
bike <- bikeplus
rm(bikeplus)


#Look at just 2014
bike <- bike[which(bike$date >= "2014-01-01" & bike$date < "2015-01-01"), ]

#Add a variable for proportion of daily trips that fell in that hour
bikeplus <- addFracByDay(bike)
if(nrow(bike) != nrow(bikeplus)) stop("Error in calculating trip fractions by day")
bike <- bikeplus
rm(bikeplus)

#We want to make a daily dataset... first, though we need to make sure that
#we're not losing too much data. Looks like most of it is Chief Sealth
sum(is.na(bike$value))
table(bike$Long_Location[is.na(bike$value)])
daily <- makeDaily(bike)

print(test <- hourlyPlot(bike, "Ped.Total", type = "count", means = TRUE))

#Make a bunch of daily and hourly plots by each bike.total, ped.south, etc...
lapply(unique(bike$variable), function(var) {
  var2 <- gsub("\\.", "", var)
  #Plot of hourly counts
  ggsave(file = paste(var2, "Hourly.png", sep = ""), 
         hourlyPlot(bike, var, type = "count"))
  
  #Plot of mean hourly counts
  ggsave(file = paste(var2, "HourlyMean.png", sep = ""), 
         hourlyPlot(bike, var, type = "count", means = TRUE))
  
  #Plot of hourly frequencies
  ggsave(file = paste(var2, "HourlyFreq.png", sep = ""), 
         hourlyPlot(bike, var, type = "freq"))
  
  #Plot mean hourly frequences
  ggsave(file = paste(var2, "HourlyFreqMean.png", sep = ""), 
         hourlyPlot(bike, var, type = "freq", means = TRUE))
  
  #Plot of daily counts
  ggsave(file = paste(var2, "Daily.png", sep = ""), 
         dailyPlot(daily, var))
  
  #Plot of daily counts by weather
  ggsave(file = paste(var2, "DailyWeather.png", sep = ""), 
         weatherPlot(daily, var))
})



bike$Month <- as.character(bike$Date, format = "%B")
bike$Month <- factor(bike$Month, levels = c("January", "February", "March", 
                                            "April", "May", "June", "July",
                                            "August", "September", "October",
                                            "November", "December"))

#Now make plots specific to one counter

lapply(unique(bike$Location), function(loc) {
  if(!file.exists(loc)) {
    dir.create(loc)
  }
  ggsave(file = paste(loc, "/hourlyMonth.png", sep = ""), 
         hourlyPlotLocation(bike, loc))
  ggsave(file = paste(loc, "/dailyWeather.png", sep = ""),
         weatherPlotLocation(daily, loc))
})










# 
# #Plot daily totals by temperature, precipitation, Location
# #Need a 2-day running total
# daily <- arrange(daily, Long_Location, date)
# daily$precip2day <- filter(daily$precip, c(1, 1), sides = 1)
# daily <- do.call('rbind', by(daily, interaction(daily$Long_Location, daily$variable), function(x) {
#   x <- arrange(x, date)
#   x$precip2day <- as.numeric(filter(x$precip, c(1, 1), sides = 1))
#   x
# }))
# row.names(daily) <- NULL
# daily <- arrange(daily, Long_Location, variable, date)
# daily[1:20,]
# weatherPlot <- ggplot(daily) + theme_bw() + 
#   geom_point(aes(x = temp, y = value, col = weekday, size = precip)) +
#   facet_wrap(~Long_Location) +
#   geom_smooth(aes(x = temp, y = value, col = weekday), method = "lm", se = FALSE)
# weatherPlot
# ggsave(file = "weatherPlot.png", weatherPlot)
# 
# subset(daily, Location == "Elliot" & value > 5000)
# daily[daily$Location == "Elliot" & daily$variable == "Ped.Total" & daily$date %in% c("2014-08016")]
# bike[bike$Location == "Elliot" & bike$date == "2014-08-16" & bike$variable == "Bike.Total", ]
# subset(bike, Location == "BG70" & hour < 9 & value > 400 & variable == "Bike.Total")
# 
# bikenew$DST <- FALSE
# bikenew$DST[bikenew$date >= "2014-03-09"] <- TRUE
# hourly_elliot <- ggplot(bikenew[bikenew$variable == "Bike.Total" & bikenew$Location == "Elliot" & bikenew$weekday == TRUE, ]) + theme_bw() + 
#   geom_line(aes(x = hour, y = value_norm, col = as.numeric(date), group = date), alpha = .5) +
#   facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Total Trips") + 
#   ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips\nData Through July 2014") + 
#   scale_x_continuous(breaks = 0:23, labels = 0:23)
#   #scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
# hourly_elliot
# 
# bikenew[which(bikenew$Location == "Fremont" & bikenew$value_norm > .2), ]
# bikenew[which(bikenew$Location == "BG70" & bikenew$value_norm > .1 & bikenew$hour < 5), ]
# bikenew[which(bikenew$Location == "MTS90" & bikenew$value_norm > .1 & bikenew$hour > 21), ]
# 
# #What's going on with the Ballard greenway?
# table(bike$Location)
# hourly_NW58 <- ggplot(bike[bike$variable == "Bike.Total" & bike$Location == "NW58", ]) + theme_bw() + 
#   geom_line(aes(x = hour, y = value, col = weekday, group = date), alpha = .5) +
#   xlab("Hour of Day") + ylab("Total Trips") + 
#   ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips") + 
#   scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
# hourly_NW58
# 
# NW58 <- bike[bike$Location == "NW58", ]
# hourly_NW58 <- ggplot(NW58[NW58$variable != "Bike.Total",]) + theme_bw() + 
#   geom_line(aes(x = hour, y = value, col = variable, group = interaction(date, variable)), alpha = .5) +
#   xlab("Hour of Day") + ylab("Total Trips") + facet_wrap(~weekday) +
#   ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips")
# hourly_NW58
# 
# 
# #High volumes at sandpoint?
# bike[which(bike$Location == "BG70" & bike$value > 500),]
# 
# #Make a weekly thing
# weekly$sin1 <- sin(weekly$week * 2 * pi / 52)
# mod <- lm(value ~ sin1 + Location, data = weekly)
# weekly$fitted <- predict(mod, weekly)
# ggplot(subset(weekly, variable == "Bike.Total")) + theme_bw() + 
#   geom_point(aes(x = week, y = value, col = Location)) + 
#   geom_smooth(aes(x = week, y = value, col = Location), se = FALSE)
# 
# 
# #By counter, calculate commute shares...
# head(bike)
# table(bike$variable)
# 
# bikex <- bike[bike$date >= "2014-01-01" & bike$variable == "Bike.Total", ]
# commuteShares <- ddply(bikex[bikex$wday %in% c(1:5), ], 
#                        .(Long_Location, date), function(x) {
#                          print(x$Long_Location[1])
#                          print(x$date[1])
#                          x$commute <- 0
#                          x$commute[x$hour %in% c(6, 7, 8, 16, 17, 18)] <- 1
#                          if(sum(!is.na(x$value))) {
#                            aggregate(value ~ commute, data = x, FUN = sum)
#                          } else {
#                            return(NULL)
#                          }
#                        })
# head(commuteShares)
# ggplot(commuteShares) + theme_bw() + 
#   geom_histogram(aes(x = value, fill = factor(commute)), position = "dodge") + 
#   facet_wrap(~Long_Location) + 
#   scale_x_log10(breaks = c(1, 10, 100, 1000))
# #asdf
# 
# cFracs <- ddply(commuteShares, .(Long_Location), function(x) {
#   tmp <- aggregate(value ~ commute, data = x, FUN = sum)
#   frac <- tmp$value[tmp$commute == 1] / sum(tmp$value)
#   data.frame("frac" = frac)
# })
# cFracs <- arrange(cFracs, -frac)
# cFracs$frac <- paste(round(cFracs$frac * 100, 0), "%", sep = "")
# cFracs
# 
# 
# wFracs <- do.call("rbind", by(bikex, bikex$Long_Location, function(x) {
#   tmp <- aggregate(value ~ weekday, data = x, FUN = sum)
#   frac <- tmp$value[tmp$weekday == FALSE] / sum(tmp$value)
#   data.frame("Long_Location" = x$Long_Location[1], "frac" = frac)
# }))
# wFracs <- arrange(wFracs, -frac)
# cFracs$frac <- paste(round(cFracs$frac * 100, 0), "%", sep = "")
# 
# 
# 
# 
# #Look for hourly oddities w/ a log-linear model???
# bikex <- bike[bike$date >= "2014-01-01" & bike$variable == "Bike.Total" & 
#                 bike$date < "2015-01-01", ]
# bikex <- bikex[which(!is.na(bikex$value)), ]
# fremont <- bikex[bikex$Long_Location == "Fremont Bridge", ]
# dset <- bikex[bikex$Location == "Spokane", ]
# hourlyMod <- function(dset) {
#   dset$sin1 <- sin(as.integer(dset$date) * 2 * pi / 365)
#   dset$cos1 <- cos(as.integer(dset$date) * 2 * pi / 365)
#   dset$sin2 <- sin(as.numeric(dset$Date) * 2 * pi / 3600 / 24)
#   dset$cos2 <- cos(as.numeric(dset$Date) * 2 * pi / 3600 / 24)
#   dset$lValue <- log(1 + dset$value)
#   mod <- glm(value ~ factor(wday) * sin1 * factor(hour) * weekday2 + 
#               factor(wday) * cos1 * factor(hour) * weekday2,
#             data = dset[which(!is.na(dset$value)), ], family = "poisson")
#   summary(mod)
#   dset$fitted <- exp(predict(mod, dset))
#   ggplot(dset) + theme_bw() + 
#     geom_point(aes(x = value, y = fitted), alpha = .1) + 
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
#     scale_x_log10(breaks = c(1, 10, 100, 500)) + 
#     scale_y_log10(breaks = c(1, 10, 100, 500))
#   
#   dset$resid <- residuals(mod)
#   dset <- subset(dset, select = c(Location, value, date, hour, weekday2, fitted, resid))
#   dset <- arrange(dset, -resid)
#   high <- dset[1:20, ]
#   high <- arrange(high, date, hour)
#   head(dset, 20)
#   dset <- arrange(dset, resid)
#   low <- dset[1:20, ]
#   low <- arrange(low, date, hour)
#   low
# }
# 
# oneDay(dset, "2014-03-01", ylim = c(0, 150))
# oneDay(dset, "2014-03-02", ylim = c(0, 150))
# oneDay(dset, "2014-03-03", ylim = c(0, 150))
# oneDay(dset, "2014-03-04", ylim = c(0, 150))
# oneDay(dset, "2014-03-05", ylim = c(0, 150))
# oneDay(dset, "2014-03-06", ylim = c(0, 150))
# oneDay(dset, "2014-03-07", ylim = c(0, 150))
# oneDay(dset, "2014-03-08", ylim = c(0, 150))
# oneDay <- function(dset, day, ylim = NULL) {
#   p <- ggplot(dset[dset$date == day, ]) + theme_bw() + 
#     geom_point(aes(x = hour, y = value)) + 
#     geom_line(aes(x = hour, y = fitted))
#   if(!is.null(ylim)) {
#     p <- p + ylim(ylim)
#   }
#   p
# }
# 
# View(bikex)
# 
# #Merge in the weather?
# #This code only worked at my Ecotope computer, where Ecotope had the weather data
# cdx("qclcd")
# weather <- read.dta("../data/WASeattle24234hrly.dta")
# weather$precip[is.na(weather$precip)] <- 0
# weather$time <- stata_time_to_R_time(weather$readTime)
# sec_per_hour <- 3600
# weather$time <- floor(as.numeric(weather$time) / sec_per_hour) * sec_per_hour
# weather$time <- as.POSIXct(weather$time, origin = "1970-01-01")
# weather_tmp <- aggregate(temp ~ time, data = weather, FUN = mean)
# weather <- merge(weather_tmp, aggregate(precip ~ time, data = weather, FUN = sum), all = TRUE)
# setwd("/storage/homes/michael/Seattle/")
# save(weather, file = "SeattleWeather.rda")
# weather$date <- as.Date(as.character(weather$time))
# head(weather)
# 
# daily_temp <- aggregate(temp ~ date, FUN = mean, data = weather)
# daily_precip <- aggregate(precip ~ date, FUN = sum, data = weather)
# daily_weather <- merge(daily_temp, daily_precip)
# head(daily_weather)
# daily <- merge(daily, daily_weather, by = "date")
# 
# #Make the temperature rider plot
# weather_plot <- ggplot(daily[daily$variable == "Bike.Total",]) + theme_bw() + 
#   geom_point(aes(x = temp, y = value, col = weekday)) + 
#   facet_wrap(~Long_Location) + xlab("Daily Average Temperature") + 
#   ylab("Daily Total Trips") + 
#   ggtitle("Seattle Bike Counters 2014 - Daily Total Trips by Temperature\nData Through July 2014") +
#   geom_smooth(aes(x = temp, y = value, col = weekday), method = "lm", se = FALSE) + 
#   scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
# weather_plot
# ggsave("weather_plot.png", weather_plot, width = 11, height = 8)
# 
# #That's all well and good, but it has the problem that you can't tell what's going on w/ the less used 
# #Try again w/ values scaled by total
# 
# 
# tmp <- by(daily[daily$variable == "Bike.Total", ], daily$Location[daily$variable == "Bike.Total"], function(x) {
#   x$value_norm <- x$value / max(x$value)
#   x
# })
# dailynew <- do.call("rbind", tmp)
# dim(dailynew)
# head(dailynew)
# 
# 
# weather_plot2 <- ggplot(dailynew[dailynew$variable == "Bike.Total",]) + theme_bw() + 
#   geom_point(aes(x = temp, y = value_norm, col = weekday)) + 
#   facet_wrap(~Long_Location) + xlab("Daily Average Temperature") + 
#   ylab("Daily Total Trips (Fraction of Max Trips by Location)") + 
#   ggtitle("Seattle Bike Counters 2014 - Daily Total Trips by Temperature\nData Through July 2014") +
#   geom_smooth(aes(x = temp, y = value_norm, col = weekday), method = "lm", se = FALSE) + 
#   scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
# weather_plot2
# ggsave("weather_plot2.png", weather_plot2, width = 11, height = 8)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# spokane <- read.socrata("https://data.seattle.gov/Transportation/Spokane-St-Bridge-Counter/upms-nr8w")
# names(spokane) <- c("Date", "Spokane", "West", "East")
# fremont <- read.socrata("https://data.seattle.gov/Transportation/Fremont-Bridge-Hourly-Bicycle-Counts-by-Month-Octo/65db-xm6k")
# names(fremont) <- c("Date", "North", "South")
# fremont$Fremont <- fremont$North + fremont$South
# fremont$time <- as.POSIXct(fremont$Date, format = "%m/%d/%Y %I:%M:%S %p")
# spokane$time <- as.POSIXct(spokane$Date, format = "%m/%d/%Y %I:%M:%S %p")
# All <- merge(subset(spokane, select = c(time, Spokane)), subset(fremont, select = c(time, Fremont)), by = "time")
# All <- All[All$Fremont < 1000,]
# Allw <- melt(All, id.vars = "time")
# Allw$date <- as.Date(as.character(Allw$time))
# ggplot(aggregate(value ~ date + variable, data = Allw, FUN = sum), aes(x = date, y = value, col = variable)) + geom_point()
# ggplot(All, aes(x = Fremont, y = Spokane)) + geom_point() + geom_smooth(method = "lm")
# summary(lm(Spokane ~ Fremont, data = All))
# 
# fremont$ltime <- as.POSIXlt(fremont$time)
# 
# fremont$wday <- fremont$ltime$wday > 0 & fremont$ltime$wday < 6
# fremont$Total <- fremont$North + fremont$South
# fremont$date <- as.Date(as.character(fremont$time))
# fremont$hour <- fremont$ltime$hour
# 
# #There may have been
# ggplot(fremont) + theme_bw() + geom_point(aes(x = date, y = Total))
# fremont[fremont$Total > 1000 & !is.na(fremont$Total),]
# fremont <- fremont[!(as.character(fremont$date) %in% c("2014-04-23", "2014-04-25", "2014-04-28", "2014-04-29")), ]
# 
# dx <- aggregate(cbind(Total, North, South) ~ date, data = fremont, FUN = sum)
# dx$Northfrac <- dx$North / dx$Total
# hist(dx$Northfrac)
# t.test(dx$Northfrac, mu = .5)
# ggplot(dx) + theme_bw() + geom_point(aes(x = date, y = Total))
# #fremont <- fremont[fremont$date <= "2014-04-22",]
# 
# 
# ptitle <- paste("Total Hourly Bicycle Trips Across Fremont Bridge\nData from",
#                 min(fremont$date),"to",max(fremont$date))
# 
# ph <- ggplot(fremont) + theme_bw() + ggtitle(ptitle) + 
#   geom_line(aes(x = hour, y = Total, group = date, col =wday), size = .3, alpha = .8) +
#   xlab("Hour of Day") + ylab("Trips (Northbound + Southbound)") + 
#   scale_colour_discrete(name = "Weekday", labels = c("No", "Yes")) + 
#   geom_vline(xintercept = 8, size = 1.5, linetype = "dashed") +
#   geom_vline(xintercept = 17, size = 1.5, linetype = "dashed") +
#   annotate("text", x = 5, y = 800, label ="Morning Commute:\n8am to 9am") +
#   annotate("text", x = 20, y = 800, label ="Evening Commute:\n5pm to 6pm") +
#   annotate("text", x = 12.5, y = 300, label ="Lazy Weekends")
# ph
# ggsave("fremont_hourly.pdf", ph)
# 
# d1 <- aggregate(Total ~ date, data = fremont, FUN = sum)
# ggplot(d1) + theme_bw() + geom_line(aes(x = date, y = Total))
# 
# #Merge in the weather?
# cdx("qclcd")
# weather <- read.dta("../data/WASeattle24234hrly.dta")
# weather$precip[is.na(weather$precip)] <- 0
# weather$time <- stata_time_to_R_time(weather$readTime)
# sec_per_hour <- 3600
# weather$time <- floor(as.numeric(weather$time) / sec_per_hour) * sec_per_hour
# weather$time <- as.POSIXct(weather$time, origin = "1970-01-01")
# weather_tmp <- aggregate(temp ~ time, data = weather, FUN = mean)
# weather <- merge(weather_tmp, aggregate(precip ~ time, data = weather, FUN = sum), all = TRUE)
# save(weather, file = "SeattleWeather.rda")
# head(weather)
# setwd("/storage/homes/michael/Seattle/")
# fremont <- merge(fremont, weather, by = "time")
# 
# 
# daily <- aggregate(cbind(Total, precip) ~ date, data = fremont, FUN = sum)
# daily <- merge(daily, aggregate(temp ~ date, data = fremont, FUN = mean ))
# daily$ltime <- as.POSIXlt(daily$date)
# daily$wday <- daily$ltime$wday > 0 & daily$ltime$wday < 6
# 
# 
# ptitle <- paste("Total Daily Bicycle Trips Across Fremont Bridge\nData from",
#                 min(fremont$date),"to",max(fremont$date))
# 
# pd <- ggplot(daily) + theme_bw() + ggtitle(ptitle) +
#   geom_point(aes(x = temp, y = Total, col = wday, size = precip)) + 
#   geom_smooth(aes(x = temp, y = Total, col = wday, size = precip), method = "lm", se = FALSE) +
#   xlab("Daily Average Outdoor Temperature (F)") + ylab("Total Bicycle Trips Across Fremont Bridge") +
#   scale_size_continuous(name = "Precipitation (in)") + 
#   scale_colour_discrete(name = "Weekday", labels = c("No", "Yes"))
# pd
# ggsave("fremont_daily.pdf", pd)
# 
# mod.stupid <- lm(Total ~ temp + precip, data = subset(daily, wday == TRUE))
# summary(mod.stupid)
# qqPlot(mod.stupid)
# 
# 
# ggplot(daily) + theme_bw() + 
#   geom_point(aes(x = precip, y = Total, col = wday, size = temp)) +
#   geom_smooth(aes(x = precip, y = Total, col = wday, size = precip), method = "lm", se = FALSE)
# 
# ggplot(daily) + theme_bw() + 
#   geom_point(aes(x = date, y = Total, col = wday))
# 
# 
# #Is there a trend?
# daily$sin1 <- sin(as.numeric(daily$date) * 2 * pi / 365)
# daily$cos1 <- cos(as.numeric(daily$date) * 2 * pi / 365)
# daily$window1 <- c(NA, daily$precip[-nrow(daily)])
# 
# dailywday <- subset(daily, wday == TRUE)
# #bike Month
# bm <- grep("201[234]-05-[0-9][0-9]", dailywday$date)
# dailywday$bike.month <- 0
# dailywday$bike.month[bm] <- 1
# 
# #School
# sc <- dailywday$ltime$mon %in% c(9, 10, 11, 0, 1, 2, 3, 4)
# dailywday$school <- 0
# dailywday$school[sc] <- 1
# 
# mod.season <- lm(Total ~ temp + precip + sin1 + cos1 + window1 + bike.month + school + date, data = dailywday)
# summary(mod.season)
# qqPlot(mod.season)
# plot(fitted(mod.season), residuals(mod.season))
# #mod.season <- lm(Total ~ sin1 + cos1, data = dailywday)
# summary(mod.season)
# dailywday$fitted <- predict(mod.season, dailywday)
# dailywday$resid <- dailywday$Total - dailywday$fitted
# plot(dailywday$date, dailywday$resid)
# 
# pt <- ggplot(dailywday) + theme_bw() + 
#   geom_point(aes(x = date, y = Total, col = school)) + geom_line(aes(x = date, y = fitted))
# pt
# ggsave("school.pdf", pt)
# 
# ptitle <- paste("Residual Daily Bicycle Trips Across Fremont Bridge\nData from",
#                 min(fremont$date),"to",max(fremont$date))
# resid_mod <- lm(resid ~ date, data = dailywday)
# growth_rate <- round(coef(resid_mod)[2] * 365, 0)
# rate_int <- round(confint(resid_mod)[2,] * 365)
# 
# rp <- ggplot(dailywday) + theme_bw() + ggtitle(ptitle) +
#   geom_point(aes(x = date, y = resid)) + 
#   geom_smooth(aes(x = date, y = resid), method = "lm") + xlab("Date") +
#   ylab("Residual Trips (Observed # - Expected #)") + 
#   geom_hline(yintercept = 0, linetype = "dashed", size = 1.5) + 
#   annotate("text", x = as.Date("2014-01-01"), y = 1800, label = paste("Estimated Growth Rate =\n",growth_rate,"riders/day/year")) +
#   annotate("text", x = as.Date("2014-01-01"), y = 1400, label = paste("95% Conf Int: (",rate_int[1],", ",rate_int[2],")", sep = ""))
# rp
# ggsave("residtrip.pdf", width = 11, height = 8)
# 
# summary(resid_mod)
# 
# 
# 
# 
# 
# 
# 
# #Make a weekly dataset
# daily$ltime <- NULL
# weekly <- ddply(daily, .(variable, week, Location), nrow)
# weekly <- subset(weekly, variable == "Bike.Total")