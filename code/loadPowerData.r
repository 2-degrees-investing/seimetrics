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

# Set the path of the working directory
setwd("/home/jbg/work2/SEIMetrics/fresh011216/github/seimetrics/code/") 
# setwd("...") 

# Load plant level GD data
GDmaster <- read.csv("AprilDatabase2-f.csv",stringsAsFactors=FALSE,strip.white=TRUE)


###############
### Part 1: Data cleaning
# INPUT: Data frame: GDmaster
source("fGDPowerDatatClean.r")
# Output: Saved CSV: GDataTotalsbyStatus.csv (totals by status, derive using active and pipeline),
# 		  GDmasterclean.csv (cleaned version of GDmaster)

# Test
totcaptest <- sum(GDmaster$Total.Capacity..MW.[!is.na(GDmaster$Total.Capacity..MW.)])
cat("Test 1:")
if (abs(totcaptest-9539050.56) < .Machine$double.eps) { 
	### XXX WTF? totcaptest is 9539051
	cat("OK")
} else {
	cat("ERROR")
}
totcaptest2 <- sum(totstat2$Active[!is.na(totstat2$Active)])
cat("Test 2:")
if (abs(totcaptest2-5606925.0-0.189) < .Machine$double.eps) { 
	### XXX WTF? totcaptest is 5606925
	cat("OK")
} else {
	cat("ERROR")
}
###############