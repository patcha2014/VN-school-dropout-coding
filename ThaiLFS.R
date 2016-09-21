#----------------------------------------------
# School dropout project
# Fall 2016
# Patcha Mint Chaikitmongkol
#----------------------------------------------

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")

# Import cleaned LFS data
temp <- csv.get("/Users/Mint/Dropbox/Mint/Dissertation_Data/LFS/cleaned_lfs.txt",sep="\t")
colnames(temp)
