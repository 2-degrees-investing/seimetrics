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
GDmaster <- read.csv("AprilDatabase2.csv",stringsAsFactors=FALSE,strip.white=TRUE)
GDmaster2 <- GDmaster

### Part 1:
source("fGDPowerDatatClean.r")
