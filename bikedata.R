
#setwd("/storage/homes/michael/Seattle/")
setwd("~/Documents/Seattle/")

library(ggplot2)
library(reshape)
library(reshape2)
library(lubridate)
library(car)

library(RSocrata)

counter_URLs <- c("https://data.seattle.gov/Transportation/Spokane-St-Bridge-Counter/upms-nr8w",
                  "https://data.seattle.gov/Transportation/Fremont-Bridge-Hourly-Bicycle-Counts-by-Month-Octo/65db-xm6k",
                  "https://data.seattle.gov/Transportation/Burke-Gilman-Trail-north-of-NE-70th-St-Bike-and-Pe/2z5v-ecg8",
                  "https://data.seattle.gov/Transportation/NW-58th-St-Greenway-at-22nd-Ave-NW-Bike-Counter/47yq-6ugv",
                  "https://data.seattle.gov/Transportation/MTS-Trail-west-of-I-90-Bridge/u38e-ybnc",
                  "https://data.seattle.gov/Transportation/Elliott-Bay-Trail-in-Myrtle-Edwards-Park/4qej-qvrz",
                  "https://data.seattle.gov/Transportation/Chief-Sealth-Trail-North-of-Thistle/uh8h-bme7",
                  "https://data.seattle.gov/Transportation/39th-Ave-NE-Greenway-at-NE-62nd-St/3h7e-f49s",
                  "https://data.seattle.gov/Transportation/26th-Ave-SW-Greenway-at-SW-Oregon-St/mefu-7eau")

counter_locs <- c("Spokane", "Fremont", "BG70", "NW58", "MTS90", "Elliot", "Sealth", "NE62", "Oregon")
counter_full <- c("Spokane St", "Fremont Bridge", "Burke Gilman Sandpoint", "NW 58th & 22nd NW (Ballard)",
                  "I90 Bridge", "Elliot Bay Trail", "Sealth (South Seattle)", "NE 62nd & 39th NE", "Oregon St (West Seattle)")
counter_dset <- data.frame("Location" = counter_locs, "Long_Location" = counter_full)

dsets <- list()
dsets_long <- list()

NSEW <- c("North", "South", "East", "West")
bNSEW <- paste("Bike", NSEW, sep = ".")
for(i in seq_along(counter_URLs)) {
  dsets[[i]] <- read.socrata(counter_URLs[i])

}
lapply(dsets, head)

for(i in seq_along(counter_URLs)) {
  for(vname in NSEW) {
    tmp <- grep(paste("^", vname, "$", sep = ""), names(dsets[[i]]))
    if(length(tmp)) {
      names(dsets[[i]])[tmp] <- paste("Bike", vname, sep = ".")
    }
  }
  #stupid fremont special case
  tmp <- grep("Fremont.Bridge.NB", names(dsets[[i]]))
  if(length(tmp)) {
    names(dsets[[i]])[tmp] <- "Bike.North"
  }
  tmp <- grep("Fremont.Bridge.SB", names(dsets[[i]]))
  if(length(tmp)) {
    names(dsets[[i]])[tmp] <- "Bike.South"
  }
  
  #Figure out variables to keep...
  vtokeep <- grep("Ped|Bike|Date", names(dsets[[i]]))
  dsets[[i]] <- dsets[[i]][, vtokeep]
  dsets[[i]]["Location"] <- counter_locs[i]
  
  #Make a bicycle total variable
  bike_vars <- grep("Bike", names(dsets[[i]]))
  dsets[[i]]["Bike.Total"] <- apply(dsets[[i]][, bike_vars], 1, sum)
  
  dsets_long[[i]] <- reshape::melt(dsets[[i]], id.vars = c("Date", "Location"))
}
lapply(dsets, head)
bike <- do.call("rbind", dsets_long)
bike <- merge(bike, counter_dset)

bike$date <- as.Date(bike$Date)
bike$week <- week(bike$Date)
bike$hour <- hour(bike$Date)
bike$wday <- wday(bike$date)
bike$weekday <- FALSE
bike$weekday[bike$wday > 1 & bike$wday < 7] <- TRUE


#Need to remove bad data from Fremont bridge counter
ggplot(bike[bike$Location == "Fremont", ]) + theme_bw() + 
  geom_point(aes(x = Date, y = value))
bike[which(bike$Location == "Fremont" & bike$value > 1000), ]
#Need to take out April 23, 25, 28, and 29
badrows <- which(bike$Location == "Fremont" & bike$date %in% as.Date(c("2014-04-23", "2014-04-25", "2014-04-28", "2014-04-29", "2012-10-02")))
bike <- bike[-badrows,]
ggplot(bike[bike$Location == "Fremont", ]) + theme_bw() + 
  geom_point(aes(x = Date, y = value))


#Make a daily dataset
daily <- aggregate(value ~ variable + date + Location + Long_Location, data = bike, FUN = sum)
daily <- daily[daily$date >= "2014-01-01",]
daily$ltime <- as.POSIXlt(daily$date)
daily$wday <- daily$ltime$wday
daily$weekday <- daily$wday > 0 & daily$wday < 6
daily$Month <- as.character(daily$ltime, format = "%B")

#Make a weekly dataset
weekly <- bike[bike$date >= "2014-01-01", ]
weekly <- aggregate(value ~ variable + week + Location, data = weekly, FUN = sum)
weekly <- subset(weekly, variable == "Bike.Total")

#Get Means...
daily_means <- aggregate(value ~ Location + Long_Location, FUN = mean, data = daily[daily$variable == "Bike.Total", ])
daily_means$y <- 5500
daily_means$x <- as.Date("2014-03-01")
daily_means$lab <- paste("YTD Average\n", round(daily_means$value, 0), "trips per day")

#Plot all by daily data
daily_all <- ggplot(daily[daily$variable == "Bike.Total", ]) + theme_bw() + 
  geom_point(aes(x = date, y = value, col = Long_Location)) + 
  geom_smooth(aes(x = date, y = value, col = Long_Location), se = FALSE) + 
  facet_wrap(~Long_Location) + ggtitle("Seattle Bike Counters 2014 - Daily Total Trips\nData Through July 2014") + 
  xlab("Date") + scale_colour_discrete(name = "Location") + ylab("Total Daily Trips") + 
  geom_text(data = daily_means, aes(x = x, y = y, label = lab), size = 4)
daily_all
ggsave("daily_all.png", daily_all, width = 11, height = 8)

daily[which(daily$Location == "NW58" & daily$value > 1000), ]


#Now w/ log
daily_all_log <- ggplot(daily[daily$variable == "Bike.Total", ]) + theme_bw() + 
  geom_point(aes(x = date, y = value, col = Long_Location)) + 
  geom_smooth(aes(x = date, y = value, col = Long_Location), se = FALSE) + 
  facet_wrap(~Long_Location) + ggtitle("Seattle Bike Counters 2014 - Daily Total Trips") + 
  xlab("Date") + scale_colour_discrete(name = "Location") + ylab("Total Daily Trips") + 
  scale_y_log10(breaks = c(5, 10, 30, 50, 100, 300, 500, 1000, 3000, 5000))
daily_all_log
ggsave("daily_all_log.png", daily_all_log, width = 11, height = 8)

#Plot all by hour of day
#Need means by location
means <- aggregate(value ~ Long_Location + hour + weekday, FUN = mean, data = bike[bike$variable == "Bike.Total",])
hourly_all <- ggplot(bike[bike$variable == "Bike.Total", ]) + theme_bw() + 
  geom_line(aes(x = hour, y = value, col = weekday, group = date), alpha = .5) +
  facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Total Trips") + 
  ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips\nData Through July 2014") + 
  scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
hourly_all
ggsave("hourly_all.png", hourly_all, width = 11, height = 8)


#Try a bit more tricky of a plot... normalize trips by location by day as probability distributions over hours
bike$dateloc <- factor(paste(bike$date, bike$Location))
tmp <- by(bike[bike$variable == "Bike.Total", ], bike$dateloc[bike$variable == "Bike.Total"], function(x) {
  x$value_norm <- x$value / sum(x$value)
  x
})
bikenew <- do.call("rbind", tmp)
dim(bikenew)
head(bikenew)

means2 <- aggregate(value_norm ~ Long_Location + hour + weekday, FUN = mean, data = bikenew)
hourly_all2 <- ggplot(bikenew) + theme_bw() + 
  geom_line(aes(x = hour, y = value_norm, col = weekday, group = date), alpha = .5) +
  facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Proportion of Day's Trips in That Hour") + 
  ggtitle("Seattle Bike Counters 2014 - Hourly Distribution of Trips\nData Through July 2014") + 
  scale_colour_discrete(name = "", labels = c("Weekend", "Weekday")) + ylim(0, .3)
hourly_all2
ggsave("hourly_all2.png", hourly_all2, width = 11, height = 8)

subset(bike, Location == "BG70" & hour < 9 & value > 400 & variable == "Bike.Total")

bikenew$DST <- FALSE
bikenew$DST[bikenew$date >= "2014-03-09"] <- TRUE
hourly_elliot <- ggplot(bikenew[bikenew$variable == "Bike.Total" & bikenew$Location == "Elliot" & bikenew$weekday == TRUE, ]) + theme_bw() + 
  geom_line(aes(x = hour, y = value_norm, col = as.numeric(date), group = date), alpha = .5) +
  facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Total Trips") + 
  ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips\nData Through July 2014") + 
  scale_x_continuous(breaks = 0:23, labels = 0:23)
  #scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
hourly_elliot

bikenew[which(bikenew$Location == "Fremont" & bikenew$value_norm > .2), ]
bikenew[which(bikenew$Location == "BG70" & bikenew$value_norm > .1 & bikenew$hour < 5), ]
bikenew[which(bikenew$Location == "MTS90" & bikenew$value_norm > .1 & bikenew$hour > 21), ]

#What's going on with the Ballard greenway?
table(bike$Location)
hourly_NW58 <- ggplot(bike[bike$variable == "Bike.Total" & bike$Location == "NW58", ]) + theme_bw() + 
  geom_line(aes(x = hour, y = value, col = weekday, group = date), alpha = .5) +
  xlab("Hour of Day") + ylab("Total Trips") + 
  ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips") + 
  scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
hourly_NW58

NW58 <- bike[bike$Location == "NW58", ]
hourly_NW58 <- ggplot(NW58[NW58$variable != "Bike.Total",]) + theme_bw() + 
  geom_line(aes(x = hour, y = value, col = variable, group = interaction(date, variable)), alpha = .5) +
  xlab("Hour of Day") + ylab("Total Trips") + facet_wrap(~weekday) +
  ggtitle("Seattle Bike Counters 2014 - Hourly Total Trips")
hourly_NW58


#High volumes at sandpoint?
bike[which(bike$Location == "BG70" & bike$value > 500),]

#Make a weekly thing
weekly$sin1 <- sin(weekly$week * 2 * pi / 52)
mod <- lm(value ~ sin1 + Location, data = weekly)
weekly$fitted <- predict(mod, weekly)
ggplot(subset(weekly, variable == "Bike.Total")) + theme_bw() + 
  geom_point(aes(x = week, y = value, col = Location)) + 
  geom_smooth(aes(x = week, y = value, col = Location), se = FALSE)



#Merge in the weather?
#This code only worked at my Ecotope computer, where Ecotope had the weather data
cdx("qclcd")
weather <- read.dta("../data/WASeattle24234hrly.dta")
weather$precip[is.na(weather$precip)] <- 0
weather$time <- stata_time_to_R_time(weather$readTime)
sec_per_hour <- 3600
weather$time <- floor(as.numeric(weather$time) / sec_per_hour) * sec_per_hour
weather$time <- as.POSIXct(weather$time, origin = "1970-01-01")
weather_tmp <- aggregate(temp ~ time, data = weather, FUN = mean)
weather <- merge(weather_tmp, aggregate(precip ~ time, data = weather, FUN = sum), all = TRUE)
setwd("/storage/homes/michael/Seattle/")
save(weather, file = "SeattleWeather.rda")
weather$date <- as.Date(as.character(weather$time))
head(weather)

daily_temp <- aggregate(temp ~ date, FUN = mean, data = weather)
daily_precip <- aggregate(precip ~ date, FUN = sum, data = weather)
daily_weather <- merge(daily_temp, daily_precip)
head(daily_weather)
daily <- merge(daily, daily_weather, by = "date")

#Make the temperature rider plot
weather_plot <- ggplot(daily[daily$variable == "Bike.Total",]) + theme_bw() + 
  geom_point(aes(x = temp, y = value, col = weekday)) + 
  facet_wrap(~Long_Location) + xlab("Daily Average Temperature") + 
  ylab("Daily Total Trips") + 
  ggtitle("Seattle Bike Counters 2014 - Daily Total Trips by Temperature\nData Through July 2014") +
  geom_smooth(aes(x = temp, y = value, col = weekday), method = "lm", se = FALSE) + 
  scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
weather_plot
ggsave("weather_plot.png", weather_plot, width = 11, height = 8)

#That's all well and good, but it has the problem that you can't tell what's going on w/ the less used 
#Try again w/ values scaled by total


tmp <- by(daily[daily$variable == "Bike.Total", ], daily$Location[daily$variable == "Bike.Total"], function(x) {
  x$value_norm <- x$value / max(x$value)
  x
})
dailynew <- do.call("rbind", tmp)
dim(dailynew)
head(dailynew)


weather_plot2 <- ggplot(dailynew[dailynew$variable == "Bike.Total",]) + theme_bw() + 
  geom_point(aes(x = temp, y = value_norm, col = weekday)) + 
  facet_wrap(~Long_Location) + xlab("Daily Average Temperature") + 
  ylab("Daily Total Trips (Fraction of Max Trips by Location)") + 
  ggtitle("Seattle Bike Counters 2014 - Daily Total Trips by Temperature\nData Through July 2014") +
  geom_smooth(aes(x = temp, y = value_norm, col = weekday), method = "lm", se = FALSE) + 
  scale_colour_discrete(name = "", labels = c("Weekend", "Weekday"))
weather_plot2
ggsave("weather_plot2.png", weather_plot2, width = 11, height = 8)













spokane <- read.socrata("https://data.seattle.gov/Transportation/Spokane-St-Bridge-Counter/upms-nr8w")
names(spokane) <- c("Date", "Spokane", "West", "East")
fremont <- read.socrata("https://data.seattle.gov/Transportation/Fremont-Bridge-Hourly-Bicycle-Counts-by-Month-Octo/65db-xm6k")
names(fremont) <- c("Date", "North", "South")
fremont$Fremont <- fremont$North + fremont$South
fremont$time <- as.POSIXct(fremont$Date, format = "%m/%d/%Y %I:%M:%S %p")
spokane$time <- as.POSIXct(spokane$Date, format = "%m/%d/%Y %I:%M:%S %p")
All <- merge(subset(spokane, select = c(time, Spokane)), subset(fremont, select = c(time, Fremont)), by = "time")
All <- All[All$Fremont < 1000,]
Allw <- melt(All, id.vars = "time")
Allw$date <- as.Date(as.character(Allw$time))
ggplot(aggregate(value ~ date + variable, data = Allw, FUN = sum), aes(x = date, y = value, col = variable)) + geom_point()
ggplot(All, aes(x = Fremont, y = Spokane)) + geom_point() + geom_smooth(method = "lm")
summary(lm(Spokane ~ Fremont, data = All))

fremont$ltime <- as.POSIXlt(fremont$time)

fremont$wday <- fremont$ltime$wday > 0 & fremont$ltime$wday < 6
fremont$Total <- fremont$North + fremont$South
fremont$date <- as.Date(as.character(fremont$time))
fremont$hour <- fremont$ltime$hour

#There may have been
ggplot(fremont) + theme_bw() + geom_point(aes(x = date, y = Total))
fremont[fremont$Total > 1000 & !is.na(fremont$Total),]
fremont <- fremont[!(as.character(fremont$date) %in% c("2014-04-23", "2014-04-25", "2014-04-28", "2014-04-29")), ]

dx <- aggregate(cbind(Total, North, South) ~ date, data = fremont, FUN = sum)
dx$Northfrac <- dx$North / dx$Total
hist(dx$Northfrac)
t.test(dx$Northfrac, mu = .5)
ggplot(dx) + theme_bw() + geom_point(aes(x = date, y = Total))
#fremont <- fremont[fremont$date <= "2014-04-22",]


ptitle <- paste("Total Hourly Bicycle Trips Across Fremont Bridge\nData from",
                min(fremont$date),"to",max(fremont$date))

ph <- ggplot(fremont) + theme_bw() + ggtitle(ptitle) + 
  geom_line(aes(x = hour, y = Total, group = date, col =wday), size = .3, alpha = .8) +
  xlab("Hour of Day") + ylab("Trips (Northbound + Southbound)") + 
  scale_colour_discrete(name = "Weekday", labels = c("No", "Yes")) + 
  geom_vline(xintercept = 8, size = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 17, size = 1.5, linetype = "dashed") +
  annotate("text", x = 5, y = 800, label ="Morning Commute:\n8am to 9am") +
  annotate("text", x = 20, y = 800, label ="Evening Commute:\n5pm to 6pm") +
  annotate("text", x = 12.5, y = 300, label ="Lazy Weekends")
ph
ggsave("fremont_hourly.pdf", ph)

d1 <- aggregate(Total ~ date, data = fremont, FUN = sum)
ggplot(d1) + theme_bw() + geom_line(aes(x = date, y = Total))

#Merge in the weather?
cdx("qclcd")
weather <- read.dta("../data/WASeattle24234hrly.dta")
weather$precip[is.na(weather$precip)] <- 0
weather$time <- stata_time_to_R_time(weather$readTime)
sec_per_hour <- 3600
weather$time <- floor(as.numeric(weather$time) / sec_per_hour) * sec_per_hour
weather$time <- as.POSIXct(weather$time, origin = "1970-01-01")
weather_tmp <- aggregate(temp ~ time, data = weather, FUN = mean)
weather <- merge(weather_tmp, aggregate(precip ~ time, data = weather, FUN = sum), all = TRUE)
save(weather, file = "SeattleWeather.rda")
head(weather)
setwd("/storage/homes/michael/Seattle/")
fremont <- merge(fremont, weather, by = "time")


daily <- aggregate(cbind(Total, precip) ~ date, data = fremont, FUN = sum)
daily <- merge(daily, aggregate(temp ~ date, data = fremont, FUN = mean ))
daily$ltime <- as.POSIXlt(daily$date)
daily$wday <- daily$ltime$wday > 0 & daily$ltime$wday < 6


ptitle <- paste("Total Daily Bicycle Trips Across Fremont Bridge\nData from",
                min(fremont$date),"to",max(fremont$date))

pd <- ggplot(daily) + theme_bw() + ggtitle(ptitle) +
  geom_point(aes(x = temp, y = Total, col = wday, size = precip)) + 
  geom_smooth(aes(x = temp, y = Total, col = wday, size = precip), method = "lm", se = FALSE) +
  xlab("Daily Average Outdoor Temperature (F)") + ylab("Total Bicycle Trips Across Fremont Bridge") +
  scale_size_continuous(name = "Precipitation (in)") + 
  scale_colour_discrete(name = "Weekday", labels = c("No", "Yes"))
pd
ggsave("fremont_daily.pdf", pd)

mod.stupid <- lm(Total ~ temp + precip, data = subset(daily, wday == TRUE))
summary(mod.stupid)
qqPlot(mod.stupid)


ggplot(daily) + theme_bw() + 
  geom_point(aes(x = precip, y = Total, col = wday, size = temp)) +
  geom_smooth(aes(x = precip, y = Total, col = wday, size = precip), method = "lm", se = FALSE)

ggplot(daily) + theme_bw() + 
  geom_point(aes(x = date, y = Total, col = wday))


#Is there a trend?
daily$sin1 <- sin(as.numeric(daily$date) * 2 * pi / 365)
daily$cos1 <- cos(as.numeric(daily$date) * 2 * pi / 365)
daily$window1 <- c(NA, daily$precip[-nrow(daily)])

dailywday <- subset(daily, wday == TRUE)
#bike Month
bm <- grep("201[234]-05-[0-9][0-9]", dailywday$date)
dailywday$bike.month <- 0
dailywday$bike.month[bm] <- 1

#School
sc <- dailywday$ltime$mon %in% c(9, 10, 11, 0, 1, 2, 3, 4)
dailywday$school <- 0
dailywday$school[sc] <- 1

mod.season <- lm(Total ~ temp + precip + sin1 + cos1 + window1 + bike.month + school + date, data = dailywday)
summary(mod.season)
qqPlot(mod.season)
plot(fitted(mod.season), residuals(mod.season))
#mod.season <- lm(Total ~ sin1 + cos1, data = dailywday)
summary(mod.season)
dailywday$fitted <- predict(mod.season, dailywday)
dailywday$resid <- dailywday$Total - dailywday$fitted
plot(dailywday$date, dailywday$resid)

pt <- ggplot(dailywday) + theme_bw() + 
  geom_point(aes(x = date, y = Total, col = school)) + geom_line(aes(x = date, y = fitted))
pt
ggsave("school.pdf", pt)

ptitle <- paste("Residual Daily Bicycle Trips Across Fremont Bridge\nData from",
                min(fremont$date),"to",max(fremont$date))
resid_mod <- lm(resid ~ date, data = dailywday)
growth_rate <- round(coef(resid_mod)[2] * 365, 0)
rate_int <- round(confint(resid_mod)[2,] * 365)

rp <- ggplot(dailywday) + theme_bw() + ggtitle(ptitle) +
  geom_point(aes(x = date, y = resid)) + 
  geom_smooth(aes(x = date, y = resid), method = "lm") + xlab("Date") +
  ylab("Residual Trips (Observed # - Expected #)") + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.5) + 
  annotate("text", x = as.Date("2014-01-01"), y = 1800, label = paste("Estimated Growth Rate =\n",growth_rate,"riders/day/year")) +
  annotate("text", x = as.Date("2014-01-01"), y = 1400, label = paste("95% Conf Int: (",rate_int[1],", ",rate_int[2],")", sep = ""))
rp
ggsave("residtrip.pdf", width = 11, height = 8)

summary(resid_mod)




