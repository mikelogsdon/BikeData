library(RSocrata)
library(ggplot2)
library(plyr)
library(lubridate)
library(scales)
library(rterm)

setwd("~/Documents/Seattle/BikeData/")

dset <- read.socrata("https://data.seattle.gov/Transportation/Burke-Gilman-Trail-north-of-NE-70th-St-Bike-and-Pe/2z5v-ecg8")
head(dset)
dset$Bike <- dset$Bike.North + dset$Bike.South
dset$date <- as.Date(dset$Date)
dset$hour <- hour(dset$Date)
dset$wday <- wday(dset$date, label = TRUE, abbr = FALSE)
dset$weekend <- dset$wday %in% c("Saturday", "Sunday")
dset$month <- month(dset$date, label = TRUE, abbr = FALSE)
dset$year <- year(dset$date)


# Look at dist of missing data
dset$missing <- is.na(dset$Bike)
ggplot(dset) + theme_bw() + 
  geom_point(aes(date, missing))
# dset <- dset[!(dset$date %in% (as.Date("2015-05-14") + days(0:8))), ]


holidays <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26",
                      "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11",
                      "2014-11-27", "2014-12-25", "2015-01-01", "2015-01-19",
                      "2015-02-16", "2015-05-25", "2015-07-03", "2015-07-04"))
dset$weekend[dset$date %in% holidays] <- TRUE

daily <- aggregate(cbind(Bike, Bike.North, Bike.South) ~ date + wday + weekend + month + year, data = dset, FUN = sum)
daily$sin1 <- sin(as.numeric(daily$date) / 365 * 2 * pi)
daily$cos1 <- cos(as.numeric(daily$date) / 365 * 2 * pi)
mod <- lm(Bike ~ sin1 * weekend + cos1 * weekend, data = daily)
daily$fitted <- predict(mod, daily)

# By day of week
View(aggregate(Bike ~ wday, data = daily[daily$year == 2014, ], FUN = mean))

# By month
View(aggregate(Bike ~ month, data = daily[daily$year == 2014, ], FUN = mean))
View(dset[dset$date == "2015-05-17", ])


bgDaily <- ggplot(daily) + theme_bw() + 
  geom_point(aes(date, Bike, col = weekend)) +
  geom_line(aes(date, fitted, col = weekend)) +
  xlab("") + ylab("Total Daily Bike Crossings") +
  ggtitle("Burke-Gilman near Sand Point Daily Bicycle Crossings") +
  scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_discrete(name = "Day", labels = c("Weekday", "Weekend/Holiday"))
bgDaily
ggsave(file = "bgDaily.png", bgDaily, width = 7, height = 5)


dset$weekend2 <- NA
dset$weekend2[dset$weekend] <- "Weekend/Holiday"
dset$weekend2[!dset$weekend] <- "Weekday"
dset2 <- ddply(dset, .(hour, weekend2), function(x) {
  c("q025" = as.numeric(quantile(x$Bike, .025, na.rm = TRUE)),
    "q25" = as.numeric(quantile(x$Bike, .25, na.rm = TRUE)),
    "q50" = as.numeric(quantile(x$Bike, .50, na.rm = TRUE)),
    "q75" = as.numeric(quantile(x$Bike, .75, na.rm = TRUE)),
    "q975" = as.numeric(quantile(x$Bike, .975, na.rm = TRUE)))
})
dlong <- reshape2::melt(dset2, id.vars = c("hour", "weekend2"))

hourBreaks <- seq(0, 22, 2)
hourLabels <- c("Midnight", "2AM", "4AM", "6AM", "8AM", "10AM", "Noon", "2PM", "4PM", "6PM", "8PM", "10PM")
bgHourly <- ggplot(dset) + theme_bw() +
  geom_line(aes(hour, Bike, group = date), size = .2, alpha = .7) +
  geom_line(data = dlong, aes(hour, value, col = variable), size = 2) +
  scale_x_continuous(breaks = hourBreaks, labels = hourLabels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Hour of Day") + ylab("Bike Crossings") +
  ggtitle("All Burke-Gilman Sand Point Bike Crossings by Hour") +
  scale_colour_discrete(name = "Quantile", labels = c("2.5%", "25%", "Median", "75%", "97.5%")) +
  facet_wrap(~weekend2) +
  scale_y_continuous(breaks = seq(0, 800, 100))
bgHourly
ggsave(file = "bgHourly.png", bgHourly, width = 7, height = 5)

dset[which(dset$Bike > 500), ]

# Now without the high values
bgHourly2 <- ggplot(dset[which(dset$Bike < 420), ]) + theme_bw() +
  geom_line(aes(hour, Bike, group = date), size = .2, alpha = .7) +
  geom_line(data = dlong, aes(hour, value, col = variable), size = 2) +
  scale_x_continuous(breaks = hourBreaks, labels = hourLabels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Hour of Day") + ylab("Bike Crossings") +
  ggtitle("All Burke-Gilman Sand Point Bike Crossings by Hour") +
  scale_colour_discrete(name = "Quantile", labels = c("2.5%", "25%", "Median", "75%", "97.5%")) +
  facet_wrap(~weekend2) +
  scale_y_continuous(breaks = seq(0, 800, 100))
bgHourly2
ggsave(file = "bgHourly2.png", bgHourly2, width = 7, height = 5)

dset[which(dset$Bike > 20 & dset$hour < 3), ]



# Balance the north and south...
ggplot(daily) + theme_bw() +
  geom_point(aes(Bike.North, Bike.South)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  ggtitle("Balancing Northbound and Southbound Trips") +
  xlab("Northbound Daily Trips") + ylab("Southbound Daily Trips")
  
ggplot(daily[!(daily$date %in% (as.Date("2015-05-14") + days(0:8))), ]) + theme_bw() +
  geom_point(aes(Bike.North, Bike.South)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  ggtitle("Balancing Northbound and Southbound Trips - More Bad Data") +
  xlab("Northbound Daily Trips") + ylab("Southbound Daily Trips")


daily <- arrange(daily, date)
daily$balance <- abs(daily$Bike.North - daily$Bike.South)
daily[which(daily$balance > 300), ]




# Inference
noaa_key <- "UxfeoBgTSvwfvRHFNqetJElZhoVfVafy"
google_key <- "AIzaSyBYJbN25Bg_6UbL_FuoyMcv4GaH8nE7Yq4"
print(stations <- stationSearch("Seattle, WA"))

temps <- read.ghcn(stations$id[2], "2014-01-01", "2015-07-01")

# Need to find precip data...
read.noaa(table = "datasets")
read.noaa("datacategories", "datasetid=GHCND")
read.noaa("datatypes", "datasetid=GHCND&datacategoryid=PRCP")
read.noaa("datatypeid=PRCP")

precip1 <- read.noaa("data", paste("datasetid=GHCND",
                                "stationid=GHCND:USW00094290",
                                "datatypeid=PRCP",
                                "startdate=2014-01-01",
                                "enddate=2015-01-01",
                                "limit=1000",
                                sep = "&"))

precip2 <- read.noaa("data", paste("datasetid=GHCND",
                                   "stationid=GHCND:USW00094290",
                                   "datatypeid=PRCP",
                                   "startdate=2015-01-01",
                                   "enddate=2015-07-01",
                                   "limit=1000",
                                   sep = "&"))
precip <- rbind(precip1, precip2)
precip$date <- as.Date(precip$date)
precip$inches <- precip$value / 10 * 0.0393701
ggplot(precip) + theme_bw() + 
  geom_bar(aes(date, inches), stat = "identity")
precip$inchesYesterday <- c(NA, precip$inches[1:(nrow(precip) - 1)])

weather <- merge(temps, precip[, c("date", "inches", "inchesYesterday")])

dailyPlus <- merge(daily, weather)
dailyPlus$Bike2 <- log(dailyPlus$Bike)
dailyPlus$bikeMonth <- dailyPlus$month == "May"
dailyPlus$yday <- yday(dailyPlus$date)

daylight <- read.csv("daylight.csv")
daylight <- reshape2::melt(daylight, id.vars = "Day")
daylight$date <- as.Date(paste(daylight$Day, daylight$variable, "2014"), format = "%d %B %Y")
daylight$yday <- yday(daylight$date)
daylight$hours <- as.numeric(gsub("^([0-9][0-9]):([0-9][0-9])", "\\1", daylight$value))
daylight$minutes <- as.numeric(gsub("^([0-9][0-9]):([0-9][0-9])", "\\2", daylight$value))
daylight$daylight <- daylight$hours + daylight$minutes / 60
daylight <- daylight[!is.na(daylight$daylight), ]

dailyPlus <- merge(dailyPlus, daylight[, c("yday", "daylight")], all.x = TRUE)

mod1 <- lm(Bike ~ aveTemp + inches + inchesYesterday + daylight + bikeMonth,
           data = dailyPlus[!dailyPlus$weekend, ])
summary(mod1)
View(summary(mod1)$coefficients)
dailyPlus$fitted1 <- predict(mod1, dailyPlus)
dailyPlus$resids1 <- dailyPlus$Bike - dailyPlus$fitted1

mod2 <- lm(Bike ~ aveTemp + inches + inchesYesterday + sin + bikeMonth,
           data = dailyPlus[dailyPlus$weekend, ])
View(summary(mod2)$coefficients)
dailyPlus$fitted2 <- predict(mod2, dailyPlus)
dailyPlus$resids2 <- dailyPlus$Bike - dailyPlus$fitted2


dailyPlusLong <- reshape2::melt(dailyPlus[, c("date", "weekend", "resids1", "resids2")], id.vars = c("date", "weekend"))
dailyPlusLong <- dailyPlusLong[(dailyPlusLong$weekend & dailyPlusLong$variable == "resids2") | (!dailyPlusLong$weekend & dailyPlusLong$variable == "resids1"), ]
dailyPlusLong$weekend2 <- NA
dailyPlusLong$weekend2[dailyPlusLong$weekend] <- "Weekend/Holiday"
dailyPlusLong$weekend2[!dailyPlusLong$weekend] <- "Weekday"

residsbg <- ggplot(dailyPlusLong) + theme_bw() + 
  geom_point(aes(date, value)) +
  geom_smooth(aes(date, value)) +
  facet_wrap(~weekend2) +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Daily Crossings Net of Daylight and Weather") +
  xlab("Date") + ylab("Observed Crossings Minus Expected Crossings")
residsbg
ggsave(file = "residsbg.png", residsbg, width = 7, height = 5)


ggplot(dailyPlus[!dailyPlus$weekend, ]) + theme_bw() + 
  geom_point(aes(date, Bike)) +
  geom_line(aes(date, fitted1))

ggplot(dailyPlus[dailyPlus$weekend, ]) + theme_bw() + 
  geom_point(aes(date, Bike, col = bikeMonth)) +
  geom_smooth(aes(date, fitted2))


# Bike Month

mod1x <- lm(Bike ~ aveTemp + inches + inchesYesterday + sin1 + cos1 + bikeMonth,
           data = dailyPlus[!dailyPlus$weekend, ])
summary(mod1x)


dailyPlus$fitted1 <- predict(mod1, dailyPlus)
dailyPlus$resids1 <- dailyPlus$Bike - dailyPlus$fitted1

