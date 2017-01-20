### INPUT:
# GDmaster
# ownerstruct (ownerstruct_Apr16-f.csv) -> GDmaster2
# outputDir

### Output:
# Saved CSV: badplantsmultiowner.csv
# Saved CSV: badplantsmultiownercheck.csv
# GDmaster2
# missingown
# captotsnew (check)
# captotsold (check)


# Add ownership structure to GDmaster
GDmaster2 <- merge(GDmaster,ownerstruct,by=c("Power.Plant.Name","Subsidiary.Asset.Name"),all.x=TRUE,all.y=FALSE)

# Check for missing ownership structure data
missingown <- subset(GDmaster2,is.na(Owner)) #41 assets, not sure why missing

# Calculate capacities for multi-owner plants
GDmaster2$totcapmo <- GDmaster2$totcap
GDmaster2$activecapmo <- GDmaster2$activecap
GDmaster2$pipelinecapmo <- GDmaster2$pipelinecap
GDmaster2$discontcapmo <- GDmaster2$discontcap
GDmaster2$totcapmo <- GDmaster2$totcapmo*GDmaster2$stake/100
GDmaster2$activecapmo <- GDmaster2$activecapmo*GDmaster2$stake/100
GDmaster2$pipelinecapmo <- GDmaster2$pipelinecapmo*GDmaster2$stake/100
GDmaster2$discontcapmo <- GDmaster2$discontcapmo*GDmaster2$stake/100

# Check totals by merging at plant/year level original data and multiowner data
captotsnew <- colSums(GDmaster2[,c("totcapmo","activecapmo","pipelinecapmo","discontcapmo")],na.rm=TRUE)
captotsold <- colSums(GDmaster[c("totcap","activecap","pipelinecap","discontcap")],na.rm=TRUE)

# Check plant level totals before and after multi-owner adjustment
plantstotold <- ddply(GDmaster,.(Power.Plant.Name,Year.Online),summarize,totcap = sum(totcap,na.rm=TRUE),activecap = sum(activecap,na.rm = TRUE),pipelinecap=sum(pipelinecap,na.rm = TRUE))
plantstotnew <- ddply(GDmaster2, .(Power.Plant.Name,Year.Online),summarize, totcapmo = sum(totcapmo,na.rm=TRUE),activecapmo = sum(activecapmo,na.rm = TRUE),pipelinecapmo=sum(pipelinecapmo,na.rm = TRUE))
plantscheck <- merge(plantstotold,plantstotnew,by=c("Power.Plant.Name","Year.Online"),all.x=TRUE,all.y=FALSE)

# Plants where multiowner logic did not work fully
badplantsmo <- subset(plantscheck,abs(totcap- totcapmo)>1 | abs(activecap- activecapmo) >1 | abs(pipelinecap - pipelinecapmo)>1)  #13 plants, mostly spelling errors
bpmoorig <- GDmaster[GDmaster$Power.Plant.Name %in% unique(badplantsmo$Power.Plant.Name),]
write.csv(badplantsmo,paste(c(outputDir,"badplantsmultiowner.csv"), collapse=""))
write.csv(bpmoorig,paste(c(outputDir,"badplantsmultiownercheck.csv"), collapse=""))

# Rename capacity variables and drop mos
GDmaster2$totcap <- GDmaster2$totcapmo
GDmaster2$activecap <- GDmaster2$activecapmo
GDmaster2$pipelinecap <- GDmaster2$pipelinecapmo
GDmaster2$discontcap <- GDmaster2$discontcapmo
GDmaster2 <- GDmaster2[,!names(GDmaster2) %in% c("totcapmo","pipelinecapmo","activecapmo","discontcapmo")]

# Remove objects from environment
rm(plantstotold)
rm(plantstotnew)
rm(plantscheck)
rm(badplantsmo)
rm(bpmoorig)