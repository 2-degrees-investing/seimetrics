### INPUT:
# Data frame: GDmaster

### Output:
# Saved CSV: GDplants_noYears.csv
# Saved CSV: GDorig_byCountry_byFuel.csv
# totcapscheck
# GDctystat
# noyears


# Interpolate Year.Online at asset level using different statistics (median, round(mean), mode)
GDmaster$Year.Online.Orig <- GDmaster$Year.Online

# Create flag for lines where plant name = asset name
GDmaster$plantflag <- 0
GDmaster$plantflag[GDmaster$Power.Plant.Name == GDmaster$Subsidiary.Asset.Name] <- 1

# Sum to plant level, by status and year
GDplant <- ddply(GDmaster,.(Power.Plant.Name,Fuel,Technology,Status,Status2),summarize,totcap = sum(Total.Capacity..MW.,na.rm=TRUE),activecap = sum(Active.Capacity..MW.,na.rm=TRUE),pipelinecap = sum(Pipeline.Capacity..MW.,na.rm=TRUE),decomcap = sum(Discontinued.Capacity..MW.,na.rm = TRUE),avgyear = round(mean(Year.Online,na.rm=TRUE)),maxyear = max(Year.Online,na.rm=TRUE), minyear = min(Year.Online,na.rm=TRUE))
yeardata <- subset(GDplant,select=c("Power.Plant.Name","Fuel","Technology","Status","Status2","avgyear","maxyear","minyear"))

# Fix NAs
yeardata$avgyear[is.nan(yeardata$avgyear)] <- NA
yeardata$maxyear[is.infinite(yeardata$maxyear)] <- NA
yeardata$minyear[is.infinite(yeardata$minyear)] <- NA

# Merge plant-level year data with asset-level data
GDmaster <- merge(GDmaster,yeardata,by=c("Power.Plant.Name","Fuel","Technology","Status","Status2"), all.x=TRUE,all.y = FALSE)

# Create flag for bad year estimate where: (maxyear - minyear) > 5
GDmaster$badyearflag <- 0
GDmaster$badyearflag[GDmaster$maxyear - GDmaster$minyear > 5] <- 1

# Replace Year.Online with estimates where applicable
# For active plants replace unless flagged, 24536 NAs -> 22470 NAs
GDmaster$Year.Online[is.na(GDmaster$Year.Online.Orig) & GDmaster$Status2 == "Active" & GDmaster$badyearflag == 0] <- GDmaster$avgyear[is.na(GDmaster$Year.Online.Orig) & GDmaster$Status2 == "Active" & GDmaster$badyearflag == 0]
# For plants with Status2=Pipeline, first try avgyear if > 2015, then assume Year.Online = 2020, 22470 NAs -> 22315 -> 14933 NAs
GDmaster$avgyearinfuture <- 0
GDmaster$avgyearinfuture[GDmaster$avgyear>2015] <- 1
GDmaster$Year.Online[is.na(GDmaster$Year.Online.Orig) & GDmaster$Status2 == "Pipeline" & GDmaster$badyearflag == 0 & GDmaster$avgyearinfuture == 1] <- GDmaster$avgyear[is.na(GDmaster$Year.Online.Orig) & GDmaster$Status2 == "Pipeline" & GDmaster$badyearflag == 0 & GDmaster$avgyear > 2015 & GDmaster$avgyearinfuture == 1]

# Create technology/fuel combination
GDmaster$Technology[GDmaster$Technology %in% "Thermal"] <- GDmaster$Fuel[GDmaster$Technology %in% "Thermal"]
GDmaster$Tech2 <- GDmaster$Technology
GDmaster$Tech2[GDmaster$Tech2 %in% c("Wind","Solar","Biopower","Geothermal","Ocean Power","Ocean")] <- "Renewables"

# Replace dual fuel category based on primary fuel
coals <- c("Coal","Subbituminous","Hard Coal","Pulverized Lignite")
gases <- c("Natural Gas","Gas","Liquefied Natural Gas","Natural gas","Natural Gas,Diesel","Natural Gas,Oil","Natural Gas, Diesel","Natural Gas, Furnace Oil","Natural Gas/Diesel")
GDmaster$Tech2[GDmaster$Tech2 %in% "Dual-Fuel" & GDmaster$Primary.Fuel %in% coals] <- "Coal"
GDmaster$Tech2[GDmaster$Tech2 %in% "Dual-Fuel" & GDmaster$Primary.Fuel %in% gases] <- "Gas"
GDmaster$Tech2[GDmaster$Tech2 %in% "Dual-Fuel"] <- "Oil"  #all remaining fuel types

# Rename capacity variables
names(GDmaster)[names(GDmaster) %in% c("Total.Capacity..MW.","Active.Capacity..MW.","Pipeline.Capacity..MW.","Discontinued.Capacity..MW.")] <- c("totcap","activecap","pipelinecap","discontcap")

# Convert NAs to zeros for capacity columns. For year column set an arbitrary future year to avoid index problems of NA
GDmaster$totcap[is.na(GDmaster$totcap)] <- 0
GDmaster$activecap[is.na(GDmaster$activecap)] <- 0
GDmaster$pipelinecap[is.na(GDmaster$pipelinecap)] <- 0
GDmaster$discontcap[is.na(GDmaster$discontcap)] <- 0
GDmaster$Year.Online[is.na(GDmaster$Year.Online)] <- 2100

# Check totals
totcapscheck <- colSums(GDmaster[names(GDmaster) %in% c("totcap","activecap","pipelinecap","discontcap")])

# Select and output plants with Year.Online=NA 
noyears <- subset(GDmaster,Year.Online == 2100)
write.csv(noyears,"GDplants_noYears.csv",row.names = FALSE)

# Create first breakdown by country and status
GDctystat <- ddply(GDmaster,.(Country,Status2,Technology,Tech2),summarize,totcap=sum(totcap),activecap = sum(activecap),pipelinecap = sum(pipelinecap),discontcap = sum(discontcap))
write.csv(GDctystat,"GDorig_byCountry_byFuel.csv",row.names = FALSE)

# Remove objects from environment
rm(GDplant)
rm(coals)
rm(gases)
rm(yeardata)
