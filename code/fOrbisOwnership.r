### INPUT:
# lookupTableInputFile
# oShareholderInputFile
# GDmasterOut

### Output:

# Load LookUp table with 129 ACWI utilities linking:
# ACWI (ISIN), Orbis (BvDID), ProductionDataCompanyList (GDID, ISIN) and GDCompanyList (GDID)
lookup <- read.csv(lookupTableInputFile,stringsAsFactors=FALSE,sep="\t")
olookup <- lookup[order(lookup$GDCompanyID),]
lookup <- olookup
rm(olookup)

# Merge look up information to GDmaster
GDmasterT <- merge(GDmasterOut,lookup,by.x = "Owner",by.y= "GDCompanyNameCL",all.x=FALSE,all.y=TRUE)
# unique(GDmasterO$Owner)
# Found the 125 utilities found in ACWI, Orbis, ProductionDataCompanyList and GDCompanyList plus a ""

# Remove entries void owners
ind <- which(GDmasterT$Owner == "")
GDmasterO <- GDmasterO[-ind,]
rm(ind)
rm(GDmasterT)

# Load shareholders of 125 utilities
oSh <- read.csv(oShareholderInputFile,stringsAsFactors=FALSE,sep="\t")
# 6890 shareholdings with 1741 unique shareholders
#uniqueShBvDId <- unique(oSh$Shareholder_BvD_ID_number)

# Sum capacity to owners
gdidCap <- ddply(GDmasterO, "GDCompanyID", summarize, mycap=sum(totcap))

# Merge with shareholders
oSh2 <- merge(oSh,gdidCap,by.x = "GDCompanyID",by.y= "GDCompanyID",all.x=TRUE,all.y=FALSE)

# Capacity per shareholding 
oSh2Cap <- ddply(oSh2, "Shareholder_BvD_ID_number", transform, capSh=sum(mycap*mappedOwnership))

# Sum capacity to shareholders
capShTot <- ddply(oSh2Cap, "Shareholder_Name", summarize, capShTot=sum(capSh))


