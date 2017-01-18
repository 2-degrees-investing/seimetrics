### INPUT:
# bbgOwnershipInputFile (ProductionDataCompanyList-f.csv as BBGData)
# GDmaster

### Output:
# Saved CSV: NeedBGGlookups.csv
# Saved CSV: CompaniesFinalwithBBG.csv
# Saved CSV: companiessmall.csv
# Saved CSV: MasterWrongagg.csv
# Saved CSV: totcapcheck.csv
# Saved CSV: GDmaster2.csv
# Saved CSV: GD_Unit_April.csv


# Load Bloomberg data in order to deal with public subsidiaries
BBGData <- read.csv(bbgOwnershipInputFile,stringsAsFactors=FALSE,strip.white=TRUE)

# Set the total outstanding shares (TotalSharesOut) by checking if there is a value for multiple classed shares (MultiSharesOut)
# otherwise use the total common shares out (EQY_SH_OUT)
BBGData$TotalSharesOut[BBGData$MultiSharesOut %in% c("#VALUE!", "#N/A", "NA","")] <- BBGData$EQY_SH_OUT[BBGData$MultiSharesOut %in% c("#VALUE!", "#N/A", "NA","")]
BBGData$TotalSharesOut[!BBGData$MultiSharesOut %in% c("#VALUE!", "#N/A", "NA","")] <- BBGData$MultiSharesOut[!BBGData$MultiSharesOut %in% c("#VALUE!", "#N/A", "NA","")]
BBGData$TotalSharesOut[BBGData$TotalSharesOut %in% c("#N/A Invalid Security", "#N/A N/A", "NA", "", "#N/A Invalid Field")] <- NA
BBGData$TotalSharesOut<-as.numeric(BBGData$TotalSharesOut)
BBGData$TotalSharesOut[!is.na(BBGData$TotalSharesOut)] <- BBGData$TotalSharesOut[!is.na(BBGData$TotalSharesOut)]*1e6

BBGData$TotalFFSharesOut[BBGData$MultiFFSharesOut %in% c("#VALUE!", "#N/A", "NA","")] <- BBGData$EQY_FLOAT[BBGData$MultiFFSharesOut %in% c("#VALUE!", "#N/A", "NA","")]
BBGData$TotalFFSharesOut[!BBGData$MultiFFSharesOut %in% c("#VALUE!", "#N/A", "NA","")] <- BBGData$MultiFFSharesOut[!BBGData$MultiFFSharesOut %in% c("#VALUE!", "#N/A", "NA","")]
BBGData$TotalFFSharesOut[BBGData$TotalFFSharesOut %in% c("#N/A Invalid Security", "#N/A N/A", "NA", "", "#N/A Invalid Field")] <- NA
BBGData$TotalFFSharesOut<-as.numeric(BBGData$TotalFFSharesOut)
BBGData$TotalFFSharesOut[!is.na(BBGData$TotalFFSharesOut)] <- BBGData$TotalFFSharesOut[!is.na(BBGData$TotalFFSharesOut)]*1e6

# Calculate free float percentage
BBGData$FFperc <- BBGData$TotalFFSharesOut/BBGData$TotalSharesOut

# Ceck for FFperc = NA
nasFF <- subset(BBGData,is.na(FFperc)& GDCompany.Type == "Public")
write.csv(nasFF,paste(c(outputDir,"NeedBGGlookups.csv"), collapse=""),row.names = FALSE)

# Remove duplicates (sometimes the duplicates do not have eqy_sh_out data or are ADR's, so only keep duplicates with data)
BBGData <- BBGData[order(-BBGData$TotalSharesOut,BBGData$ADR_ADR_PER_SH),] 
BBG.No.GD.ID <- subset(BBGData, is.na(BBGData$GDCompany.ID))
BBGData <- BBGData[!duplicated(BBGData[,c('GDCompany.ID')]),] #this keeps the first entry and deltes the others so if the data in the right order it deletes the right duplicates
BBGData<-rbind(BBGData, BBG.No.GD.ID)

# Drop tickers that are ADRs (If these exist need to find their parent tickers, at the moment there are just some Chinese companies)
BBGData$TotalSharesOut[BBGData$ADR_ADR_PER_SH != "#N/A Field Not Applicable" | BBGData$ADR_ADR_PER_SH == "1" ] <- NA
BBGData$FFperc[BBGData$ADR_ADR_PER_SH != "#N/A Field Not Applicable" | BBGData$ADR_ADR_PER_SH == "1" ] <- NA

# Get public subsidiaries and change their FF and turn them into Parents:
# give them children of their own, responsiblities, mortgage, white picket fence etc
publicsubsid <- subset(BBGData,GDParent.Subsidiary %in% "Subsidiary" & GDCompany.Type %in% "Public")
# Assume wherever FFperc is NA, free float = 0, which leads to a wholly owned subsidiary in rollup
publicsubsid$FFperc[is.na(publicsubsid$FFperc)] <- 0
# Create secondary entries for public subsidiaries, labeled as parents
publicsubsid$GDParent.Subsidiary <- "Parent"
publicsubsid$FFperc <- 1 - publicsubsid$FFperc
# Bind publicsubsid back to BBGData
BBGData2 <- rbind(BBGData,publicsubsid)
companies<-BBGData2

# Get relevant columns
keeps <- c("ID_BB_ULTIMATE_PARENT_CO_NAME","SECURITY_NAME", "EQY_FUND_TICKER","GDCompany.ID","GDCompany.Name","GDCompany.Type","GDParent.Subsidiary","GDParent.Name","COUNTRY_ISO","TotalSharesOut","TotalFFSharesOut","FFperc")
companies <- companies[,names(companies) %in% keeps]

# Set FFperc == 0 for all NAs (private companies, so 100% roll up to parent)
companies$FFperc[is.na(companies$FFperc)] <- 0
write.csv(companies,paste(c(outputDir,"CompaniesFinalwithBBG.csv"), collapse=""),row.names = FALSE)
companiessmall <- subset(companies, select=c("GDCompany.ID","GDCompany.Name","GDParent.Subsidiary","GDParent.Name","GDCompany.Type","FFperc"))
companiessmall <- companiessmall[!duplicated(companiessmall[,c('GDCompany.ID', "GDParent.Subsidiary")]),]
companiessmall <-subset(companiessmall, companiessmall$GDCompany.ID != "NA")
write.csv(companiessmall,paste(c(outputDir,"companiessmall.csv"), collapse=""),row.names = FALSE)
#t<-subset(companiessmall, companiessmall$GDCompany.Name == "SunPower Corporation")
GDmaster2save <- GDmaster2


#############  Aggregate ownership to final parent sequentially

# Merge company information to GDmaster
GDmaster2 <- merge(GDmaster2save,companiessmall,by.x = "Owner",by.y= "GDCompany.Name",all.x=TRUE,all.y=FALSE)
missedmerges <- subset(GDmaster2,is.na(GDCompany.Type))
missingcomps <- as.character(unique(missedmerges$Owner))
# For missing companies assume Parent.Subsidiary = Independent
GDmaster2$GDParent.Subsidiary[is.na(GDmaster2$GDParent.Subsidiary)] <- "Independent"

# Add FFperc = 0 to all companies with missing company information
GDmaster2$FFperc[GDmaster2$FFperc == "NA" ] <- 0

##### Begin roll-up process:
# at each stage merge in parent owner, free float production and the opposite (1 - free float; assumed to roll up)

# Step 1
GDmaster2$Ownerpar1 <- GDmaster2$Owner
GDmaster2$Ownerpar1[GDmaster2$GDParent.Subsidiary %in% "Subsidiary"] <- GDmaster2$GDParent.Name[GDmaster2$GDParent.Subsidiary %in% "Subsidiary"]
GDmaster2$totcap1 <-GDmaster2$totcap
GDmaster2$activecap1 <-GDmaster2$activecap
GDmaster2$pipelinecap1 <-GDmaster2$pipelinecap
GDmaster2$discontcap1 <-GDmaster2$discontcap
# Multiply by free float only if subsidiary company is a public subsidiary
GDmaster2$totcap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name] <- GDmaster2$totcap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name]*(1-GDmaster2$FFperc[GDmaster2$Owner %in% publicsubsid$GDCompany.Name])
GDmaster2$activecap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name] <- GDmaster2$activecap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name]*(1-GDmaster2$FFperc[GDmaster2$Owner %in% publicsubsid$GDCompany.Name])
GDmaster2$pipelinecap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name] <- GDmaster2$pipelinecap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name]*(1-GDmaster2$FFperc[GDmaster2$Owner %in% publicsubsid$GDCompany.Name])
GDmaster2$discontcap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name] <- GDmaster2$discontcap1[GDmaster2$Owner %in% publicsubsid$GDCompany.Name]*(1-GDmaster2$FFperc[GDmaster2$Owner %in% publicsubsid$GDCompany.Name])
totcapcheckstep1 <- colSums(GDmaster2[,names(GDmaster2) %in% c("totcap1","activecap1","pipelinecap1","discontcap1")],na.rm = TRUE)
# Rename to avoid trouble merging
companiessmall1 <- rename(companiessmall,c("GDCompany.ID" = "GDCompany.ID1","GDParent.Subsidiary"="GDParent.Subsidiary1","GDParent.Name"="GDParent.Name1","GDCompany.Type"="GDCompany.Type1","FFperc"="FFperc1"))
GDmaster2 <- merge(GDmaster2,companiessmall1,by.x="Ownerpar1",by.y="GDCompany.Name",all.x=TRUE,all.y=FALSE)
# Zero out multiple copies in capacity variables
GDmaster2 <- GDmaster2[!(GDmaster2$GDParent.Subsidiary1 %in% "Parent" & GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 == GDmaster2$Owner),]

# Step 2
GDmaster2$Ownerpar2 <- GDmaster2$Ownerpar1
# Only roll up where not already rolled up (Ownerpar1 == Owner and still a subsidiary)
GDmaster2$Ownerpar2[GDmaster2$GDParent.Subsidiary1 %in% "Subsidiary" & GDmaster2$Ownerpar1 != GDmaster2$Owner] <- GDmaster2$GDParent.Name1[GDmaster2$GDParent.Subsidiary1 %in% "Subsidiary" & GDmaster2$Ownerpar1 != GDmaster2$Owner]
GDmaster2$totcap2 <-GDmaster2$totcap1
GDmaster2$activecap2 <-GDmaster2$activecap1
GDmaster2$pipelinecap2 <-GDmaster2$pipelinecap1
GDmaster2$discontcap2 <-GDmaster2$discontcap1
# Correct any free float values lost in merge due to missing company data
GDmaster2$FFperc1[is.na(GDmaster2$FFperc1)] <- 0
# Multiply by free float only if subsidiary company is a public subsidiary and Ownerpar1 /= Owner, meaning the subsidiary has not rolled up yet
GDmaster2$totcap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name& GDmaster2$Ownerpar1 != GDmaster2$Owner] <- GDmaster2$totcap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner]*(1-GDmaster2$FFperc1[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner])
GDmaster2$activecap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner] <- GDmaster2$activecap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner]*(1-GDmaster2$FFperc1[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner])
GDmaster2$pipelinecap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner] <- GDmaster2$pipelinecap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner]*(1-GDmaster2$FFperc1[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner])
GDmaster2$discontcap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner] <- GDmaster2$discontcap2[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner]*(1-GDmaster2$FFperc1[GDmaster2$Ownerpar1 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar1 != GDmaster2$Owner])
totcapcheckstep2 <- colSums(GDmaster2[,names(GDmaster2) %in% c("totcap2","activecap2","pipelinecap2","discontcap2")],na.rm = TRUE)
# Rename to avoid trouble merging
companiessmall2 <- rename(companiessmall,c("GDCompany.ID" = "GDCompany.ID2","GDParent.Subsidiary"="GDParent.Subsidiary2","GDParent.Name"="GDParent.Name2","GDCompany.Type"="GDCompany.Type2","FFperc"="FFperc2"))
GDmaster2 <- merge(GDmaster2,companiessmall2,by.x="Ownerpar2",by.y="GDCompany.Name",all.x=TRUE,all.y=FALSE)
# Zero out multiple copies in production variable
GDmaster2 <- GDmaster2[!(GDmaster2$GDParent.Subsidiary2 %in% "Parent" & GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 == GDmaster2$Ownerpar1),]

# Step 3
GDmaster2$Ownerpar3 <- GDmaster2$Ownerpar2
# Only roll up where not already rolled up (Ownerpar1 == Owner and still a subsidiary)
GDmaster2$Ownerpar3[GDmaster2$GDParent.Subsidiary2 %in% "Subsidiary" & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1] <- GDmaster2$GDParent.Name2[GDmaster2$GDParent.Subsidiary2 %in% "Subsidiary" & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1]
GDmaster2$totcap3 <-GDmaster2$totcap2
GDmaster2$activecap3 <-GDmaster2$activecap2
GDmaster2$pipelinecap3 <-GDmaster2$pipelinecap2
GDmaster2$discontcap3 <-GDmaster2$discontcap2
# Correct any free float values lost in merge due to missing company data
GDmaster2$FFperc2[is.na(GDmaster2$FFperc2)] <- 0
# Multiply by free float only if subsidiary company is a public subsidiary and Ownerpar2 /= Ownerpar1, meaning the subsidiary has not rolled up yet
GDmaster2$totcap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1] <- GDmaster2$totcap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1]*(1-GDmaster2$FFperc2[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1])
GDmaster2$activecap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1] <- GDmaster2$activecap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1]*(1-GDmaster2$FFperc2[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1])
GDmaster2$pipelinecap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1] <- GDmaster2$pipelinecap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1]*(1-GDmaster2$FFperc2[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1])
GDmaster2$discontcap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1] <- GDmaster2$discontcap3[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1]*(1-GDmaster2$FFperc2[GDmaster2$Ownerpar2 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar2 != GDmaster2$Ownerpar1])
totcapcheckstep3 <- colSums(GDmaster2[,names(GDmaster2) %in% c("totcap3","activecap3","pipelinecap3","discontcap3")],na.rm = TRUE)
# Rename to avoid trouble merging
companiessmall3 <- rename(companiessmall,c("GDCompany.ID" = "GDCompany.ID3","GDParent.Subsidiary"="GDParent.Subsidiary3","GDParent.Name"="GDParent.Name3","GDCompany.Type"="GDCompany.Type3","FFperc"="FFperc3"))
GDmaster2 <- merge(GDmaster2,companiessmall3,by.x="Ownerpar3",by.y="GDCompany.Name",all.x=TRUE,all.y=FALSE)
# Zero out multiple copies in production variable
GDmaster2 <- GDmaster2[!(GDmaster2$GDParent.Subsidiary3 %in% "Parent" & GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 == GDmaster2$Ownerpar2),]

# Step 4
GDmaster2$Ownerpar4 <- GDmaster2$Ownerpar3
# Only roll up where not already rolled up (Ownerpar1 == Owner and still a subsidiary)
GDmaster2$Ownerpar4[GDmaster2$GDParent.Subsidiary3 %in% "Subsidiary" & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2] <- GDmaster2$GDParent.Name3[GDmaster2$GDParent.Subsidiary3 %in% "Subsidiary" & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2]
GDmaster2$totcap4 <-GDmaster2$totcap3
GDmaster2$activecap4 <-GDmaster2$activecap3
GDmaster2$pipelinecap4 <-GDmaster2$pipelinecap3
GDmaster2$discontcap4 <-GDmaster2$discontcap3
# Correct any free float values lost in merge due to missing company data
GDmaster2$FFperc3[is.na(GDmaster2$FFperc3)] <- 0
# Multiply by free float only if subsidiary company is a public subsidiary and Ownerpar2 /= Ownerpar1, meaning the subsidiary has not rolled up yet
GDmaster2$totcap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2] <- GDmaster2$totcap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2]*(1-GDmaster2$FFperc3[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2])
GDmaster2$activecap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2] <- GDmaster2$activecap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2]*(1-GDmaster2$FFperc3[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2])
GDmaster2$pipelinecap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2] <- GDmaster2$pipelinecap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2]*(1-GDmaster2$FFperc3[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2])
GDmaster2$discontcap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2] <- GDmaster2$discontcap4[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2]*(1-GDmaster2$FFperc3[GDmaster2$Ownerpar3 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar3 != GDmaster2$Ownerpar2])
totcapcheckstep4 <- colSums(GDmaster2[,names(GDmaster2) %in% c("totcap4","activecap4","pipelinecap4","discontcap4")],na.rm = TRUE)
# Rename to avoid trouble merging
companiessmall4 <- rename(companiessmall,c("GDCompany.ID" = "GDCompany.ID4","GDParent.Subsidiary"="GDParent.Subsidiary4","GDParent.Name"="GDParent.Name4","GDCompany.Type"="GDCompany.Type4","FFperc"="FFperc4"))
GDmaster2 <- merge(GDmaster2,companiessmall4,by.x="Ownerpar4",by.y="GDCompany.Name",all.x=TRUE,all.y=FALSE)
# Zero out multiple copies in production variable
GDmaster2 <- GDmaster2[!(GDmaster2$GDParent.Subsidiary4 %in% "Parent" & GDmaster2$Ownerpar4 %in% publicsubsid$GDCompany.Name & GDmaster2$Ownerpar4 == GDmaster2$Ownerpar3),]


# Check plants with original 
checkorig <- ddply(GDmasterorig,.(Power.Plant.Name),summarize,totalorig = sum(Total.Capacity..MW.),activeorig = sum(Active.Capacity..MW.))
checkfinal <- ddply(GDmaster2,.(Power.Plant.Name),summarize,totalfinal = sum(totcap4), activefinal = sum(activecap4))
checkcomb <- merge(checkorig,checkfinal)
wrongs <- subset(checkcomb,abs((totalfinal - totalorig)/totalorig) > 0.01)
masterwrongs <- GDmaster2[GDmaster2$Power.Plant.Name %in% wrongs$Power.Plant.Name,]
write.csv(masterwrongs,paste(c(outputDir,"MasterWrongagg.csv"), collapse=""),row.names = FALSE)

# Check total capacities at each level
totcapcheck <- cbind(totcapscheck,totcapcheckstep1,totcapcheckstep2,totcapcheckstep3)
write.csv(totcapcheck,paste(c(outputDir,"totcapcheck.csv"), collapse=""),row.names = FALSE)

# Save full GDmaster database before aggregating to owner
keeps <- c("Power.Plant.Name","Subsidiary.Asset.Name","Fuel","Technology","Tech2","Status","Status2","Primary.Fuel","plantflag","Country","Region","Region2","totcap","activecap","pipelinecap","discontcap","Year.Online","Year.Online.Orig","totcap4","activecap4","pipelinecap4","discontcap4","Ownerorig","stake","Owner","GDCompany.ID","Ownerpar1","GDCompany.ID1","Ownerpar2","GDCompany.ID2","Ownerpar3","GDCompany.ID3","Ownerpar4","GDCompany.ID4")
GDmaster3 <- subset(GDmaster2,select = keeps)

# Write to CSV
write.csv(GDmaster2,paste(c(outputDir,"GDmaster2.csv"), collapse=""),row.names = FALSE)
#t<-subset(GDmaster2, GDmaster2$Subsidiary.Asset.Name == "Boulder Solar PV Park Solar PV")
write.csv(GDmaster3,paste(c(outputDir,"GD_Unit_April.csv"), collapse=""),row.names = FALSE)