#Bike Data Functions...

#Read the data from the website...
readBikeData <- function() {
  
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
  counter_full <- c("Spokane St Viaduct", "Fremont Bridge", "Burke Gilman at Sandpoint", "NW 58th & 22nd NW (Ballard)",
                    "MTS I-90 Trail", "Elliot Bay Trail", "Chief Sealth (South Seattle)", "NE 62nd & 39th NE", "Oregon St (West Seattle)")
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
    
    #Make a pedestrian total variable
    ped_vars <- grep("Ped", names(dsets[[i]]))
    dsets[[i]]["Ped.Total"] <- apply(dsets[[i]][, ped_vars], 1, sum)
    
    dsets_long[[i]] <- reshape::melt(dsets[[i]], id.vars = c("Date", "Location"))
  }
  lapply(dsets, head)
  bike <- do.call("rbind", dsets_long)
  bike <- merge(bike, counter_dset)
  
  bike$date <- as.Date(bike$Date)
  bike$week <- week(bike$Date)
  bike$hour <- hour(bike$Date)
  bike$wday <- wday(bike$date)
  bike$weekday <- "Weekend/Holiday"
  bike$weekday[bike$wday > 1 & bike$wday < 7] <- "Weekday"
  bike$weekday2 <- "Weekday"
  bike$weekday2[bike$wday %in% c(1, 7)] <- "Weekend"
  
  #Bring in the federal holidays...
  holidays2014 <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17", 
                            "2014-05-26", "2014-07-04", "2014-09-01",
                            "2014-10-13", "2014-11-11", "2014-11-27", 
                            "2014-11-28", "2014-12-25"), origin = "1970-01-01")
  bike$weekday[bike$date %in% holidays2014] <- "Weekend/Holiday"
  bike$weekday2[bike$date %in% holidays2014] <- "Holiday"
  
  bike
}



mergeWeather <- function(bike, wfile) {
  #Let's bring in the weather, if possible...
  weather <- read.dta(file = wfile)
  weather$time <-as.numeric(weather$readTime)
  weather$time <- round(weather$time / 3600, 0) * 3600
  weather <- ddply(weather, .(time), function(x) {
    c("temp" = mean(x$temp, na.rm = TRUE),
      "precip" = sum(x$precip, na.rm = TRUE))
  })
  weather$pTime <- as.POSIXct(weather$time, origin = "1970-01-01")
  
  bike$time <- as.numeric(bike$Date)
  bike$time <- round(bike$time / 3600, 0) * 3600
  
  merge(bike, weather, all.x = TRUE)
}


makeDaily <- function(bikeplus) {
  
  #Make a daily dataset
  aggFun <- function(x) {
    if(!nrow(x)) return(NULL)
    c("value" = sum(x$value, na.rm = TRUE),
      "temp" = mean(x$temp, na.rm = TRUE),
      "precip" = sum(x$precip, na.rm = TRUE))
  }
  daily <- ddply(bikeplus, .(variable, date, week, Location, Long_Location, weekday, weekday2), aggFun)
  daily <- daily[which(daily$date >= "2014-01-01" & daily$date < "2015-01-01"), ]
  daily$ltime <- as.POSIXlt(daily$date)
  daily$wday <- daily$ltime$wday
  #daily$weekday <- daily$wday > 0 & daily$wday < 6
  daily$Month <- as.character(daily$ltime, format = "%B")

  #Add a lagged precip
  daily <- do.call('rbind', by(daily, interaction(daily$Long_Location, daily$variable), function(x) {
    x <- arrange(x, date)
    x$precip2day <- as.numeric(filter(x$precip, c(1, 1), sides = 1))
    x
  }))
  row.names(daily) <- NULL
  daily <- arrange(daily, Long_Location, variable, date)
  daily
}

addFracByDay <- function(bike) {
  #Try a bit more tricky of a plot... normalize trips by location by day as probability distributions over hours
  bike$dateLocVar <- factor(paste(bike$date, bike$Location, bike$variable))
  tmp <- by(bike, bike$dateLocVar, function(x) {
    x$value_norm <- x$value / sum(x$value, na.rm = TRUE)
    x
  })
  bikeTmp <- do.call("rbind", tmp)
  row.names(bikeTmp) <- NULL
  bikeTmp
}


dailyPlot <- function(daily, variable, log = FALSE) {
  daily <- removeBlankLocs(daily, variable)
  
  #Get Means...
  daily_means <- aggregate(value ~ Location + Long_Location + weekday, FUN = mean, data = daily[daily$variable == variable, ])
  daily_means <- ddply(daily_means, .(Location, Long_Location), function(x) {
    data.frame("label" = paste(x$Long_Location[1], "\nWeekday Average:", round(x$value[x$weekday == "Weekday"], 0), 
                               "\nWeekend Average:", round(x$value[x$weekday != "Weekday"], 0)))
  })
  daily <- merge(daily, daily_means)
  
  #Plot all by daily data
  daily_all <- ggplot(daily[daily$variable == variable, ]) + theme_bw() + 
    geom_point(aes(x = date, y = value, col = weekday)) + 
    geom_smooth(aes(x = date, y = value, col = weekday), se = FALSE) + 
    facet_wrap(~label) + ggtitle("Seattle Bike Counters 2014 - Daily Total Trips\nData Through July 2014") + 
    xlab("Date") + scale_colour_discrete(name = "Weekday") + ylab("Total Daily Trips")
  if(log) daily_all <- daily_all + scale_y_log10()
  daily_all
  
  
}


hourlyPlot <- function(bike, variable, type = "count", means = FALSE) {
  #Only use applicable counters
  bike <- removeBlankLocs(bike, variable)
  
  ptitle <- paste("Seattle Pedestrian Counters 2014 - Hourly", gsub("\\.", " ", variable), "Trips")
  
  #If means, aggregate
  if(means) {
    bike <- ddply(bike, .(Long_Location, hour, weekday, variable), function(x) {
      c("value" = mean(x$value, na.rm = TRUE),
        "value_norm" = mean(x$value_norm, na.rm = TRUE))
    })
    bike$date <- bike$weekday
    ptitle <- paste("Seattle Pedestrian Counters 2014 - Mean Hourly", gsub("\\.", " ", variable), "Trips")
  }
  
  if(type == "count") {
    #Plot all by hour of day
    #Need means by location
    means <- aggregate(value ~ Long_Location + hour + weekday, FUN = mean, data = bike[bike$variable == variable,])
    hourly_all <- ggplot(bike[bike$variable == variable, ]) + theme_bw() + 
      geom_line(aes(x = hour, y = value, col = weekday, group = date), alpha = .5) +
      facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Total Trips") + 
      ggtitle(ptitle) + 
      scale_colour_discrete(name = "")
    return(hourly_all)
  } else if(type == "freq") {
    #means2 <- aggregate(value_norm ~ Long_Location + hour + weekday, FUN = mean, data = bike)
    hourly_all2 <- ggplot(bike[bike$variable == variable, ]) + theme_bw() + 
      geom_line(aes(x = hour, y = value_norm, col = weekday, group = date), alpha = .5) +
      facet_wrap(~Long_Location) + xlab("Hour of Day") + ylab("Proportion of Day's Trips in That Hour") + 
      ggtitle(ptitle) + 
      scale_colour_discrete(name = "") + ylim(0, .3)
    return(hourly_all2)
  }
  
}


weatherPlot <- function(daily, variable) {
  daily <- removeBlankLocs(daily, variable)
  
  #Scale counts by max
  daily <- do.call('rbind', by(daily, daily$Location, function(x) {
    x$value2 <- x$value / max(x$value[x$variable == variable])
    x
  }))
  row.names(daily) <- NULL
  
  maxes <- aggregate(value ~ Long_Location, data = daily[daily$variable == variable, ], FUN = max)
  maxes$label <- paste(maxes$Long_Location, "\nMax Daily Crossings =", maxes$value)
  daily <- merge(daily, subset(maxes, select = c(Long_Location, label)))
  
  ggplot(daily[daily$variable == variable, ]) + theme_bw() + 
    geom_point(aes(x = temp, y = value2, col = weekday)) + 
    geom_smooth(aes(x = temp, y = value2, col = weekday), method = "lm", se = FALSE) + 
    facet_wrap(~label) + 
    ggtitle(paste("Daily", gsub("\\.", " ", variable), "Crossings by Temperature\nScaled by Max Counts for each Counter, for Presentation")) + 
    xlab("Daily Average Temperature (F)") +
    ylab("Daily Crossings as a Fraction of Max Crossings (For Plot Scale)")
  
}

removeBlankLocs <- function(dset, variable) {
  #Remove any location for which there are no observations for this variable
  obs <- aggregate(value ~ Long_Location, data = dset[dset$variable == variable, ], FUN = sum)
  locsToUse <- obs$Long_Location[obs$value > 0]
  dset <- dset[dset$Long_Location %in% locsToUse, ]
  dset
}


weatherPlotLocation <- function(daily, variable, loc) {
  
  ggplot(daily[daily$variable == variable & daily$Location == loc, ]) + theme_bw() + 
    geom_point(aes(x = temp, y = value, col = weekday, size = precip)) + 
    geom_smooth(aes(x = temp, y = value, col = weekday), method = "lm", se = FALSE) + 
    facet_wrap(~Month) + 
    ggtitle("Daily Counts by Weather") + 
    xlab("Daily Average Temperature (F)") +
    ylab("Count")
  
}



hourlyPlotLocation <- function(bike, location, variable = "Bike.Total") {
  dset <- bike[bike$Location == location & bike$variable == variable, ]
  longLoc <- dset$Long_Location[1]
  ggplot(dset) + theme_bw() + 
    geom_line(aes(x = hour, y = value, group = date, col = weekday)) + 
    facet_wrap(~Month) +
    ggtitle(paste(gsub("\\.", " ", variable), "Trips By Hour of Day -", longLoc)) + 
    xlab("Hour of Day") + ylab("Total Crossings")
  
}

weatherPlotLocation <- function(daily, location, variable = "Bike.Total") {
  dset <- daily[daily$Location == location & daily$variable == variable, ]
  longLoc <- dset$Long_Location[1]
  ggplot(dset) + theme_bw() + 
    geom_point(aes(x = temp, y = value, col = weekday, size = precip2day)) + 
    ggtitle(paste(gsub("\\.", " ", variable), "Trips By Temperature -", longLoc)) + 
    xlab("Daily Average Temperature") + ylab("Total Crossings") + 
    geom_smooth(method = "lm", aes(x = temp, y = value, col = weekday, linetype = weekday), se = FALSE) +
    scale_size_continuous(range = c(1.5,6))
}


