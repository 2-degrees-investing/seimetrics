### INPUT:
# GDmaster
# outputDir

### Output:
# Saved CSV: ownerstruct_Apr16-f.csv
# ownerstruct (ownerstruct_Apr16-f.csv)

# Trim white space and merge with fuel to deal with 37 plants with identical names and different fuels
GDmasterorig$Power.Plant.Name <- paste(str_trim(GDmasterorig$Power.Plant.Name),str_trim(GDmasterorig$Fuel))
GDmasterorig$Subsidiary.Asset.Name <- paste(str_trim(GDmasterorig$Subsidiary.Asset.Name),str_trim(GDmasterorig$Fuel))

# Pull unique ownership relevant columns
ownerstructpre <- GDmasterorig[,c("Power.Plant.Name","Subsidiary.Asset.Name","Owner","Owner.Stake...")]
ownerstruct <- unique(ownerstructpre)

# Subset original data into 2 dataframes:
# 1: Asset level ownership information (check stake)
#	assetsowndata
# 2. Plants with no asset-level ownership information others (assumed owner at plant level or no owner)
#	plantsowndata


############# Dataframe 1: Asset level ownership information

# Define flag for when ownership data exists at asset level
ownerstruct$ownerflag <- 0
ownerstruct$assetown <- 0
ownerstruct$ownerflag[ownerstruct$Owner != ""] <- 1  
ownerstruct$assetown[ownerstruct$Owner != "" & ownerstruct$Power.Plant.Name != ownerstruct$Subsidiary.Asset.Name] <- 1  

# Rename stake variables
ownerstruct$stake <- as.numeric(ownerstruct$Owner.Stake...)

# Construct dataframe
assetsowndata <- subset(ownerstruct, assetown == 1)  ##762 assets
assetsowndata$stake <- assetsowndata$Owner.Stake...
assetsowndata$Ownerorig <- assetsowndata$Owner

# Count number of owners for asset level ownership
# countownsasset <- ddply(assetsowndata,.(Power.Plant.Name,Subsidiary.Asset.Name),summarize,countowners = length(Owner),countdashes = sum(Owner == ""))

# Step 1: First calculate number of unique owners that are not blanks

# Set flag for condition of owner not "" and needs ownership share assigned (NA)
assetsowndata$needsshareflag <- 0
assetsowndata$needsshareflag[is.na(assetsowndata$stake)] <- 1
checkstakesasset <- ddply(assetsowndata,.(Power.Plant.Name,Subsidiary.Asset.Name),summarize,numdistown = sum(Owner != ""),totstake = sum(stake,na.rm = TRUE),numownersnoshare = sum(needsshareflag==1))
assetsowndata <- merge(assetsowndata,checkstakesasset,by = c("Power.Plant.Name","Subsidiary.Asset.Name"),all.x=TRUE,all.y=FALSE)

#Step 2: For missing shares, correct ownership share to 100 by calculating allocable ownership left and dividing between owners with no share equally

# Only apply where there are owners with no stake and the sum doesn't already match 100
assetsowndata$stake2 <- assetsowndata$stake
assetsowndata$stake2[assetsowndata$needsshareflag==1] <- abs(100-assetsowndata$totstake[assetsowndata$needsshareflag==1])/assetsowndata$numownersnoshare[assetsowndata$needsshareflag==1]

# Error checking
# Find plants where total stake > 100 or < 100 = 1478 assets
checkstakesasset2 <- ddply(assetsowndata,.(Power.Plant.Name,Subsidiary.Asset.Name),summarize,totstake2 = sum(stake2))
assetsowndata <-  merge(assetsowndata,checkstakesasset2,by = c("Power.Plant.Name","Subsidiary.Asset.Name"),all.x=TRUE,all.y=FALSE)
badassets <- subset(checkstakesasset2,abs(totstake2 - 100)>0.1) #find plants where stake is materially different from 100
badassetsgt100 <- subset(badassets,totstake2 > 100) #zero at asset level

# Step 3: For those plants where sum(stake) < 100, insert new lines of Unknown owner to balance
assetsowndataba3 <- subset(assetsowndata,abs(totstake2 - 100)>0.1)

# Create a balancing append marked as Owner = "OtherUnknownBalancing"
balancingrows <- assetsowndataba3
# Remove duplicated plants so only 1 balancign row exists for each plant
balancingrows <- balancingrows[!duplicated(balancingrows[,1:2]),]
balancingrows$stake2 <- 100 - balancingrows$totstake2
balancingrows$Owner <- "OtherUnknownBalancing"
# Bind to assetsowndata
assetsowndata <- rbind(assetsowndata,balancingrows)
checkstakesfinal <- ddply(assetsowndata,.(Power.Plant.Name,Subsidiary.Asset.Name),summarize,totstake2 = sum(stake2))
badassets2 <- subset(checkstakesfinal,abs(totstake2 - 100)>0.1)  #zero now
#x <- assetsowndata[assetsowndata$Subsidiary.Asset.Name %in% badassets2$Subsidiary.Asset.Name,]

# Final asset-level part of ownerstruct
ownerstructasset <- subset(assetsowndata,select = c("Power.Plant.Name","Subsidiary.Asset.Name","Owner","Ownerorig","Owner.Stake...","stake2"))
names(ownerstructasset)[names(ownerstructasset) %in% "stake2"] <- "stake"

############# Dataframe 1


#############  Dataframe 2: Plants with no asset level ownership information
plantsowndata <- subset(ownerstruct,assetown == 0)

### Treat one-liner plants

# Find plants with single line "Power.Plant.Name == Subsidiary.Asset.Name" and set aside
plantsowndata$Ownerorig <- plantsowndata$Owner
countowns <- ddply(plantsowndata,.(Power.Plant.Name),summarize,countowners = length(Owner),countdashes = sum(Owner == ""))
plantsowndata <- merge(plantsowndata,countowns,by=c("Power.Plant.Name"),all.x=TRUE,all.y=FALSE)
oneliners <- subset(plantsowndata,countowners == 1)

# Check to ensure Power.Plant.Name == Subsidiary.Asset.Name for all of these
weirdassets <- subset(oneliners,Power.Plant.Name != Subsidiary.Asset.Name) #104 assets, seem to be mostly spelling/coding errors
plantsweird <- subset(GDmasterorig,Power.Plant.Name %in% weirdassets$Power.Plant.Name) #108 assets

# Find Owner information and replace with "UnknownOwner"
oneliners$Owner[oneliners$Owner == ""] <- "UnknownOwner"

# If stake is unknown but only one owner, assume stake = 100
oneliners$stake[is.na(oneliners$stake)] <- 100

# For those plants where sum(stake) < 100, insert new lines of Unknown owner to balance
onelowndatabp <- subset(oneliners,abs(stake - 100)>0.1)

# Create a balancing append marked as Owner = "OtherUnknownBalancing"
balancingrows <- onelowndatabp

# Remove duplicated plants so only 1 balancign row exists for each plant
balancingrows <- balancingrows[!duplicated(balancingrows[,1]),]
balancingrows$stake <- 100 - balancingrows$stake
balancingrows$Owner <- "OtherUnknownBalancing"

# Bind to plantsowndata
oneliners <- rbind(oneliners,balancingrows)

### Treat multiowner plants

# Make multiowner plant list and its ownerstructure
plantsowndata <- subset(plantsowndata,countowners > 1)  #65567 lines

# Where number of lines = number of blanks and Owner is blank, Owner is not known
plantsowndata$Owner[plantsowndata$Owner %in% "" & plantsowndata$countowners == plantsowndata$countdashes] <- "UnknownOwner" 

# Step 1: First calculate number of unique owners for each plant that are not blanks/Unknown
# Set flag for condition of owner not "" and needs ownership share assigned (NA)
plantsowndata$needsshareflag <- 0
plantsowndata$needsshareflag[!(plantsowndata$Owner %in% "") &!(plantsowndata$Owner %in% "UnknownOwner") & is.na(plantsowndata$stake)] <- 1
checkstakesplant <- ddply(plantsowndata,.(Power.Plant.Name),summarize,numdistown = sum(Owner != "" & Owner != "UnknownOwner"),totstake = sum(stake,na.rm = TRUE),numownersnoshare = sum(needsshareflag==1))
plantsowndata <- merge(plantsowndata,checkstakesplant,by = c("Power.Plant.Name"),all.x=TRUE,all.y=FALSE)

# Step 2: For missing shares, correct ownership share to 100 by calculating allocable ownership left and dividing between owners with no share equally
# Only apply where there are owners with no stake and the sum doesn't already match 100
plantsowndata$stake2 <- plantsowndata$stake
plantsowndata$stake2[plantsowndata$needsshareflag==1] <- abs(100-plantsowndata$totstake[plantsowndata$needsshareflag==1])/plantsowndata$numownersnoshare[plantsowndata$needsshareflag==1]

# Add a stake = 100 for plants with unknown owners
plantsowndata$stake2[plantsowndata$Power.Plant.Name == plantsowndata$Subsidiary.Asset.Name & plantsowndata$Owner == "UnknownOwner"] <- 100
# Drop the lines representing blank owners, usually at subsidiary asset level
plantsowndata <- subset(plantsowndata,!is.na(stake2))

# Error checking
# Find plants where total stake > 100 or < 100 = 1478 assets
checkstakesasset2 <- ddply(plantsowndata,.(Power.Plant.Name),summarize,totstake2 = sum(stake2))
plantsowndata <-  merge(plantsowndata,checkstakesasset2,by = c("Power.Plant.Name"),all.x=TRUE,all.y=FALSE)
badplants <- subset(checkstakesasset2,abs(totstake2 - 100)>0.1) #find plants where stake is materially different from 100
badplantsgt100 <- subset(badplants,totstake2 > 100) #38 plants where sum > 100 due to poor naming practices (same asset different years)

# Step 3: For those (79) plants where sum(stake) > 100, scale all stakes down so sum(stake2) = 100 in stake3
# Also need to deal with Dogger Bank style where different years have no owner structures
# Correct 'bad assets' by scaling all stakes to 100/sum(stake). Later manual fix
plantsowndata$stake3 <- plantsowndata$stake2
plantsowndata$stake3[plantsowndata$Power.Plant.Name %in% badplantsgt100$Power.Plant.Name] <- plantsowndata$stake3[plantsowndata$Power.Plant.Name %in% badplantsgt100$Power.Plant.Name]*100/plantsowndata$totstake2[plantsowndata$Power.Plant.Name %in% badplantsgt100$Power.Plant.Name]
checkstakesasset3 <- ddply(plantsowndata,.(Power.Plant.Name),summarize,totstake3 = sum(stake3))
plantsowndata <- merge(plantsowndata,checkstakesasset3,by = c("Power.Plant.Name"),all.x=TRUE,all.y=FALSE)
badplants3 <- subset(checkstakesasset3,abs(totstake3 - 100)>0.1)  #all < 100 totstake3

# Step 4: For those plants where sum(stake) < 100, insert new lines of unknown owner to balance
plantsowndatabp <- subset(plantsowndata,abs(totstake3 - 100)>0.1)
# Create a balancing append marked as Owner = "OtherUnknownBalancing"
balancingrows <- plantsowndatabp
# Remove duplicated plants so only 1 balancign row exists for each plant
balancingrows <- balancingrows[!duplicated(balancingrows[,1]),]
balancingrows$stake3 <- 100 - balancingrows$totstake3
balancingrows$Owner <- "OtherUnknownBalancing"
# Bind to plantsowndata
plantsowndata <- rbind(plantsowndata,balancingrows)
checkstakesfinal <- ddply(plantsowndata,.(Power.Plant.Name),summarize,totstake3 = sum(stake3))
badplantsfinal <- subset(checkstakesfinal,abs(totstake3 - 100)>0.1)  #zero plants

# Finanlize
# Add asset level detail back in for plants so merging can be done on asset
plantownlist <- unique(plantsowndata$Power.Plant.Name)
# Get all assets associated with these plants
assetlistplantlevel <- unique(subset(ownerstructpre,select=c("Power.Plant.Name","Subsidiary.Asset.Name")))
assetlistplantlevel <- subset(assetlistplantlevel,Power.Plant.Name %in% plantownlist)
# Merge with plantsowndata to get to asset level ownership, assuming all assets associated with plant have same ownership structure as plant
# first have to drop asset name from plantsowndata
plantsowndata <- plantsowndata[,!names(plantsowndata) %in% "Subsidiary.Asset.Name"]
plantsowndataassets <- merge(plantsowndata,assetlistplantlevel,by = c("Power.Plant.Name"),all.x=TRUE,all.y=TRUE)
# Remove all assets where asset level ownership exists
plantsowndataassets <- subset(plantsowndataassets,!Subsidiary.Asset.Name %in% unique(ownerstructasset$Subsidiary.Asset.Name))

# Select needed columns
ownerstructonel <- subset(oneliners,select = names(ownerstructasset))
ownerstructplants <- subset(plantsowndataassets,select = c("Power.Plant.Name","Subsidiary.Asset.Name","Owner","Ownerorig","Owner.Stake...","stake3"))
names(ownerstructplants)[names(ownerstructplants) %in% "stake3"] <- "stake"

#############  Dataframe 2

# Put back together: Create new dataframe from the previously constructed two
ownerstruct <- rbind(ownerstructasset,ownerstructonel)
ownerstruct <- rbind(ownerstruct,ownerstructplants)

# Check final ownership at subsidiary asset level
checkstakesend <- ddply(ownerstruct,.(Power.Plant.Name,Subsidiary.Asset.Name),summarize,totstake = sum(stake))
badfinal <- subset(checkstakesend,abs(totstake- 100) > 0.1)  #zero assets

#final ownerstructure
write.csv(ownerstruct,paste(c(outputDir,"ownerstruct_Apr16-f.csv"), collapse=""),row.names = FALSE)
# NB: original CSV wrapps values in quotes, however, if you load and save with Excel/Libreoffice, only values 
# containing commas as text and not as delimiter will be wrapped in quotes. This is the original ownerstruct_Apr16.csv

# Remove objects from environment
rm(assetlistplantlevel)
rm(assetsowndata)
rm(assetsowndataba3)
rm(badassets)
rm(badassets2)
rm(badassetsgt100)
rm(badfinal)
rm(badplants)
rm(badplants3)
rm(badplantsfinal)
rm(badplantsgt100)
rm(balancingrows)
rm(checkstakesasset)
rm(checkstakesasset2)
rm(checkstakesasset3)
rm(checkstakesend)
rm(checkstakesfinal)
rm(checkstakesplant)
rm(countowns)
rm(oneliners)
rm(onelowndatabp)
rm(ownerstructasset)
rm(ownerstructonel)
rm(ownerstructplants)
rm(ownerstructpre)
rm(plantownlist)
rm(plantsowndata)
rm(plantsowndataassets)
rm(plantsowndatabp)
rm(plantsweird)
rm(weirdassets)
