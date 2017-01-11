### INPUT:
# Data frame: GDmaster

### Output:
# Saved CSV: GDataTotalsbyStatus.csv
# Saved CSV: GDmasterclean.csv

# Trim whitespace from start and end of string
# Merge with fuel to deal with the 37 plants with identical names but different fuels
GDmaster$Power.Plant.Name <- paste(str_trim(GDmaster$Power.Plant.Name),str_trim(GDmaster$Fuel))
GDmaster$Subsidiary.Asset.Name <- paste(str_trim(GDmaster$Subsidiary.Asset.Name),str_trim(GDmaster$Fuel))

# Save unaltered original dataset
GDmasterorig <- GDmaster

# Set variables to keep
keeps <- c("Power.Plant.Name","Subsidiary.Asset.Name","Fuel","Technology","Status","Primary.Fuel","Country","Region","Total.Capacity..MW.","Active.Capacity..MW.","Pipeline.Capacity..MW.","Discontinued.Capacity..MW.","Year.Online")
GDmaster <- subset(GDmaster,select = keeps)

# Clean status variable
GDmaster$Status <- str_trim(GDmaster$Status)
GDmaster$Status[GDmaster$Status %in% "ACTIVE"] <- "Active"
GDmaster$Status[GDmaster$Status %in% "Temporarily shutdown"] <- "Temporarily Shutdown"

# Create new Status2 aggregation
# Shorten data frame to only those with  active or coming status, 147997 -> 136482
# Get rid of spaces at the end and return string w/o trailing whitespace
GDmaster <- subset(GDmaster,Status %in% c("Active","Announced","Financed","Partially Active","Permitting","Temporarily Shutdown","Under Construction","Under Rehabilitation & Modernization"))
GDmaster$Status2 <- GDmaster$Status
GDmaster$Status2[GDmaster$Status %in% c("Financed","Under Construction","Under Rehabilitation & Modernization")] <- "Pipeline"

# Remove random blank lines
# First, lines with no information whatsoever on capacity or ownership, 136482 -> 121174
GDmaster <- subset(GDmaster,!(is.na(Total.Capacity..MW.) & is.na(Active.Capacity..MW.) & is.na(Pipeline.Capacity..MW.) & is.na(Discontinued.Capacity..MW.))) 
# Remove duplicates, 121724 -> 121720
GDmaster <- unique(GDmaster)

# Format capacity variables to numeric
GDmaster$Total.Capacity..MW. <- as.numeric(GDmaster$Total.Capacity..MW. )
GDmaster$Active.Capacity..MW. <- as.numeric(GDmaster$Active.Capacity..MW. )
GDmaster$Pipeline.Capacity..MW. <- as.numeric(GDmaster$Pipeline.Capacity..MW. )
GDmaster$Discontinued.Capacity..MW. <- as.numeric(GDmaster$Discontinued.Capacity..MW. )
GDmaster$Year.Online <- as.numeric(GDmaster$Year.Online)

# Check totals by status, derive using active and pipeline and then combine
actstat2 <- dcast(ddply(GDmaster,.(Status2,Year.Online),summarize,totcap = sum(Active.Capacity..MW.,na.rm=TRUE)),Year.Online ~ Status2, value.var="totcap")
pipestat2 <- dcast(ddply(GDmaster,.(Status2,Year.Online),summarize,totcap = sum(Pipeline.Capacity..MW.,na.rm=TRUE)),Year.Online ~ Status2, value.var="totcap")
totstat2 <- pipestat2
totstat2$Active <- actstat2$Active

# Save totals by status
write.csv(totstat2,"GDataTotalsbyStatus.csv",row.names = FALSE)

# Save cleaned version of GDmaster
write.csv(GDmaster,"GDmasterclean.csv",row.names = FALSE)

# Remove objects from environment
rm(actstat2)
rm(keeps)
rm(pipestat2)
rm(totstat2)