#Create Utilities at Unit level, matching utility capacity data to securities and calculating installed capacity per share.
# 09/01/17 Michael Hayne, Chris Weber, James Glattfelder

# Clear workspace
rm(list=ls())

# Load packages
library(grid)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(stringi)

###############
### Configuration

## Input data
#inputDir <- "/Users/micha/Dropbox (2° Investing)/UZH_2Dii/Power aggregation/" # Michael
inputDir <- "/home/jbg/work2/SEIMetrics/fresh011216/github/seimetrics/code/input/" # James

# See Perl script in seimetrics/data/ GitHub directory for formatting issues
plantsFileName <- "AprilDatabase2-f.csv" 
productionFileName <- "ProductionDataCompanyList-f.csv"

## Store generated CSV files
#outputDir <- "/Users/micha/Dropbox (2° Investing)/UZH_2Dii/Power aggregation/2iiOutput/" # Michael
outputDir <- "/home/jbg/work2/SEIMetrics/fresh011216/github/seimetrics/code/output/" # James

## GitHub https://github.com/2-degrees-investing/seimetrics/
#githubCodeDir <- "/Users/micha/Dropbox (2° Investing)/UZH_2Dii/github/code/" # Michael
githubCodeDir <- "/home/jbg/work2/SEIMetrics/fresh011216/github/seimetrics/code/" # James

# Accuracy
options(digits=10)

# Testing
totcaptest1 <- 9539050.56
totcaptest2 <- 5606925.189
totcaptest3 <- 1885614.534
physassetownershtest <- 13329293.85

# Set the path of the working directory
setwd(githubCodeDir) 

# Load plant level GD data
# Expects correctly formatted AprilDatabase2.csv called AprilDatabase2-f.csv
# See Perl script in removeBadFormattingInPerl.txt in seimetrics/data/ GitHub directory
plantsInputFile <- paste(c(inputDir, plantsFileName),collapse="")
GDmaster <- read.csv(plantsInputFile,stringsAsFactors=FALSE,strip.white=TRUE)

# Save unaltered original dataset
GDmasterorig <- GDmaster

###############
### Part 1: Data cleaning

# INPUT:
#	GDmaster
#	outputDir
source("fGDPowerDataCleaning.r")
# Output:
#	Saved CSV: GDataTotalsbyStatus.csv (totals by status, derive using active and pipeline)
#	Saved CSV: GDmasterclean.csv (cleaned version of GDmaster)
#	totstat2 (GDataTotalsbyStatus.csv)

# Test
totcaptmp <- sum(GDmaster$Total.Capacity..MW.[!is.na(GDmaster$Total.Capacity..MW.)])
cat("Test 1:")
if (abs(totcaptest1-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	stop("Test 1")
}
rm(totcaptmp)

totcaptmp <- sum(totstat2$Active[!is.na(totstat2$Active)])
cat("Test 2:")
if (abs(totcaptest2-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	stop("Test 2")
}
rm(totcaptmp)
rm(totcaptest2)
rm(totstat2)

###############


###############
### Part 2: Data manipulation

# INPUT:
#	GDmaster
#	outputDir
source("fGDPowerDataManipulation.r")
# Output:
#	Saved CSV: GDplants_noYears.csv
#	Saved CSV: GDorig_byCountry_byFuel.csv
#	totcapscheck
#	GDctystat (GDorig_byCountry_byFuel.csv)
#	noyears (GDplants_noYears.csv)

# Test
totcaptmp <- sum(GDctystat$totcap)
cat("Test 3:")
if (abs(totcaptest1-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	stop("Test 3")
}
rm(totcaptest1)
rm(GDctystat)

totcaptmp <- sum(noyears$totcap)
cat("Test 4:")
if (abs(totcaptest3-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	stop("Test 4")
}
rm(totcaptmp)
rm(noyears)
rm(totcaptest3)

###############


###############
### Part 3: Assigning owners and ownership stakes at physical asset level

## Part 3.1: GD power ownership structure (physical asset ownership)
# INPUT:
#	GDmasterorig
#	outputDir
source("fGDPowerOwnershipStructure.r")
#str <- "/home/jbg/work2/SEIMetrics/arch/analysis/rwd/in/theirin/ownerstruct_Apr16.csv"
#ownerstruct <-  read.csv(str,stringsAsFactors = FALSE,strip.white = TRUE)
# Output:
#	Saved CSV: ownerstruct_Apr16-f.csv
#	ownerstruct (ownerstruct_Apr16-f.csv)

# Test
physassetownershtmp <- sum(ownerstruct$stake)
cat("Test 5:")
if (abs(physassetownershtest-physassetownershtmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	stop("Test 5")
}
rm(physassetownershtest)
rm(physassetownershtmp)

## Part 3.2: Check multi-owner plants
# INPUT:
#	ownerstruct (ownerstruct_Apr16-f.csv)
#	GDmaster
#	outputDir
source("fGDPowerOwnershipCheckMultiOwners.r")
# Output:
#	Saved CSV: badplantsmultiowner.csv
#	Saved CSV: badplantsmultiownercheck.csv
#	GDmaster2
#	missingown
#	captotsnew (check)
#	captotsold (check)

# Test

rm(GDmaster)
rm(captotsnew)
rm(captotsold)
rm(missingown)

###############


###############
### Part 4.1: Ownership trees (subsidiary ownership): Bloomberg

# INPUT:
#	bbgOwnershipInputFile (ProductionDataCompanyList-f.csv as BBGData)
#	GDmaster2
#	totcapscheck
bbgOwnershipInputFile <- paste(c(inputDir, productionFileName),collapse="")
source("fBBGOwnership.r")
# Output:
#	Saved CSV: NeedBGGlookups.csv
#	Saved CSV: CompaniesFinalwithBBG.csv
#	Saved CSV: companiessmall.csv
#	Saved CSV: MasterWrongagg.csv
#	Saved CSV: totcapcheck.csv
#	Saved CSV: GDmaster2.csv
#	Saved CSV: GD_Unit_April.csv

# Test
#totcaptmp <- sum(GDctystat$totcap)
cat("Test X:")
#if (abs(totcaptest1-totcaptmp) < .Machine$double.eps) { 
#	cat("OK")
#} else {
#	stop("Test 3")
#}

###############


###############
### Part 4.2: Ownership trees (subsidiary ownership): Orbis

# INPUT:
#bbgOwnershipInputFile <- paste(c(inputDir, productionFileName),collapse="")
#source("fOrbisOwnership.r")
# Output:


# Test
#totcaptmp <- sum(GDctystat$totcap)
cat("Test X:")
#if (abs(totcaptest1-totcaptmp) < .Machine$double.eps) { 
#	cat("OK")
#} else {
#	stop("Test 3")
#}

###############