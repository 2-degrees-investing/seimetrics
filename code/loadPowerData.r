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

# Accuracy
options(digits=10)

# Testing
totcaptest <- 9539050.56

# Set the path of the working directory
setwd("/home/jbg/work2/SEIMetrics/fresh011216/github/seimetrics/code/") 
# setwd("...") 

# Load plant level GD data
# Expects correctly formatted AprilDatabase2.csv called AprilDatabase2-f.csv: See seimetrics/data/ GitHub directory 
GDmaster <- read.csv("AprilDatabase2-f.csv",stringsAsFactors=FALSE,strip.white=TRUE)


###############
### Part 1: Data cleaning

# INPUT: Data frame: GDmaster
source("fGDPowerDataCleaning.r")
# Output: Saved CSV: GDataTotalsbyStatus.csv (totals by status, derive using active and pipeline),
# 		  Saved CSV: GDmasterclean.csv (cleaned version of GDmaster)
#		  totstat2

# Test
totcaptmp <- sum(GDmaster$Total.Capacity..MW.[!is.na(GDmaster$Total.Capacity..MW.)])
cat("Test 1:")
if (abs(totcaptest-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	cat("ERROR")
}
rm(totcaptmp)

totcaptmp <- sum(totstat2$Active[!is.na(totstat2$Active)])
cat("Test 2:")
if (abs(totcaptmp-5606925.189) < .Machine$double.eps) { 
	cat("OK")
} else {
	cat("ERROR")
}
rm(totcaptmp)
rm(totstat2)

###############


###############
### Part 2: Data manipulation

# INPUT: Data frame: GDmaster
source("fGDPowerDataManipulation.r")
# Output: Saved CSV: GDataTotalsbyStatus.csv (totals by status, derive using active and pipeline),
# 		  Saved CSV: GDmasterclean.csv (cleaned version of GDmaster)
#		  totcapscheck
# 		  GDctystat
# 	      noyears

# Test
totcaptmp <- sum(GDctystat$totcap)
cat("Test 3:")
if (abs(totcaptest-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	cat("ERROR")
}
rm(totcaptest)
rm(GDctystat)

totcaptmp <- sum(sum(noyears$totcap))
cat("Test 4:")
if (abs(1885614.534-totcaptmp) < .Machine$double.eps) { 
	cat("OK")
} else {
	cat("ERROR")
}
rm(noyears$totcap)

###############