#----------------------------#
# VN school dropout project 
#----------------------------#
# Mint Chaikitmongkol 

rm(list=ls()) # clear workspace

library("foreign")
library("AER")
library("Hmisc") # get spss (.sav), csv files
library("plyr")

# Import VHLSS education data 
setwd("/Users/Mint/Desktop/vhlss/2012/data/hhold")
data <- read.dta("Muc1A.dta")
temp <- read.dta("muc2a1.dta") # education data frame
data <- merge(data, temp, by=c("tinh","huyen","xa","diaban","hoso","matv")) # merge two data frames by hh identifiers
colnames(data)

#----Create hh identifier number----#
data$hhid <- do.call(paste0,data[c("tinh","huyen","xa","diaban","hoso")])
length(unique(data$hhid)) #9399 households 
hhlist <- data$hhid

#----Group hhs by income quintiles----#

data$wage <- data$m4ac11 # wage for each indiv in the past 12 months
# Compute hh wage 
temp <- aggregate(data$wage, by=list(hhid=data$hhid), FUN=sum, na.rm=TRUE) # aggregate wage variable at the hh level. Treat NA as 0 otherwise R wouldn't compute income for hh with some missing values.  
temp[temp==0] <- NA # For hh with all missing wage values, change hhinc variable back to NA
describe(temp$x) # 4529 hhs (out of 9399) didn't report income data

# income quintiles
incquan <- quantile(temp$x,probs=seq(0,1,0.2),na.rm=TRUE)
A = incquan[[2]] # cutoff for bottom quintile 
B = incquan[[5]] # cutoff for top quintile

# function to identify hhs in top and bottom income quintiles
incgroup <- function(x) if(!is.na(x) & x>B) 'top' else if(!is.na(x) & x<A) 'bottom' else NA 
# create identifiers for hhs in top and bottom income quintiles 
temp$incgroup <- sapply(temp$x,incgroup) 

colnames(temp) <- c("hhid","hhinc","incgroup")

# merge data to main data frame
data <- merge(data,temp,by=c("hhid"))


#----Age structure----#

# age variable 
data$age <- data$m1ac5
plot(density(data$age))

# density plot comparing top and bottom quintile groups
ggplot(data[is.na(data$incgroup)==0,], aes(age, fill=incgroup)) + geom_density(alpha=0.5)
setwd("/Users/Mint/Desktop/")
dev.copy(png,'agehist.png')
dev.off()

# histogram comparing top and bottom quintile groups
# ggplot(data[is.na(data$incgroup)==0,], aes(age, fill=incgroup)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') 

#----Schooling----#

# in school dummy
str(data$m2ac4)
data$m2ac4 <- as.numeric(data$m2ac4)
table(data$m2ac4) # value 1 = Có yes, 2 = Nghỉ hè summer break, 3 = Không no
inschool <- function(x) if(!is.na(x) & x<3) 1 else 0 
data$inschool <- sapply(data$m2ac4, inschool) 
table(data$inschool) # 9222 indivs in school out of 10810 indiv below 18 (count(data$age <18))

# years of education -----

# Current grade
str(data$m2ac7)
data$curgrade <- data$m2ac7 # In what grade is the indiv in  

# Last grade finished 
str(data$m2ac1) # string variable
data$edyrs <- as.numeric(data$m2ac1) 
table(data$edyrs)

# plot fractions of indivs enrolled in school ---------

F <- 5
L <- 18
subdat <- data[data$age>=F & data$age<=L,] # Look at kids age 6 to 18
temp <- aggregate(subdat$inschool,by=list(subdat$age),FUN=mean,na.rm=TRUE) # fraction of indivs in school at diff age 
colnames(temp) <- c("Age","FractionEnrolled")
temp$IncomeGroup <- "all"

# By income group
temp1 <- aggregate(subdat$inschool,by=list(subdat$age, subdat$incgroup),FUN=mean,na.rm=TRUE) # fraction of indivs in school at diff age 
colnames(temp1) <- c("Age", "IncomeGroup", "FractionEnrolled") 

frac_enrolled <- rbind(temp,temp1) # combine all, top, bottom income groups
rm(temp,temp1)

# plot 
ggplot(frac_enrolled, aes(Age, FractionEnrolled, color=IncomeGroup)) + geom_line(aes(group=IncomeGroup)) + geom_point()
dev.copy(png,'frac_enrolled.png')
dev.off()


#-----------------------------------------------
# Fraction of kids enrolled by age-grade
#-----------------------------------------------
# Select income levels
temp <- subdat[!(is.na(subdat$incgroup)) & subdat$incgroup=="top",]
temp <- subdat[!(is.na(subdat$incgroup)) & subdat$incgroup=="bottom",] 
temp <- subdat 

# number of children in each age 
n <- as.data.frame(table(temp$age)); colnames(n) <- c("Age","N")

# number of dropouts by age (Didn't use for graph)
dropout <- aggregate(temp$inschool,by=list(temp$age),FUN=sum,na.rm=TRUE); colnames(dropout) <- c("Age","InSchool")
output <- merge(n,dropout,by=c("Age"))
output$Dropout <- 1- output$InSchool / output$N

#-------------------------------------------------
#---Fraction of children in each GRADE by age-----#
#-------------------------------------------------
age <- F:L 
for (i in 1:length(age)){
  j <- age[i]
  subdat <- subset(temp, age==j)
  if(i==1) {base <- as.data.frame(table(subdat$curgrade))
            base$age <- j}
  else {temp2 <- as.data.frame(table(subdat$curgrade))
        temp2$age <- j
        base <- rbind(base,temp2)}
}
colnames(base) <- c("Grade","Freq","Age")
frac_grade <- merge(base,n,by=c("Age"))
frac_grade$FracGrade <- frac_grade$Freq / frac_grade$N 

frac_grade.wide <- reshape(frac_grade, v.names="Freq", timevar="Grade", idvar="Age", direction="wide",add.missing=TRUE)

library(reshape)
frac_grade.wide <- cast(frac_grade, Age~Grade, value='FracGrade',add.missing=TRUE)
# Change column name
coltemp <- paste("Grade",1:12,sep="") 
colnames(frac_grade.wide) <- c("Age",coltemp)
frac_grade.wide[is.na(frac_grade.wide)] <- 0 # replace NA with 0 

frac_grade.long <- melt(frac_grade.wide, id.vars = "Age")
colnames(frac_grade.long) <- c("Age","Fraction","Grade")
ggplot(frac_grade.long, aes(x=Age, y=Fraction, fill=Grade)) +
  geom_bar(stat="identity") + 
  xlab("\nAge") +
  ylab("Fraction\n") + 
  guides(fill=FALSE) + 
  theme_bw() 

#-------------------------------------------------
#---Fraction of children in edu LEVEL by age-----#
#-------------------------------------------------

# Fucntion to generate education level indicators
edlev <- function(x) if(!is.na(x) & x>=1 & x<=5) "Pri" else  # Primary level = grade 1-5
                     if(!is.na(x) & x>=6 & x<=9) "Sec" else  # Secondary level = grade 6-9
                     if(!is.na(x) & x>=10 & x<=12) "HS" else NA   # High school level = Grade 10-12
temp$edlev <- sapply(temp$curgrade,edlev) # Create indicator

# Compute fraction of children in each edu range by age group
age <- F:L 
for (i in 1:length(age)){
  j <- age[i]
  subdat <- subset(temp, age==j)
  if(i==1) {base <- as.data.frame(table(subdat$edlev))
            base$age <- j}
  else {temp2 <- as.data.frame(table(subdat$edlev))
        temp2$age <- j
        base <- rbind(base,temp2)}
}
colnames(base) <- c("Level","Freq","Age")
frac_grade <- merge(base,n,by=c("Age")) # merge with total number of children by age 
frac_grade$FracGrade <- frac_grade$Freq / frac_grade$N # Compute net enrollment by age-education level

#------Create stacked bar graph-----#

# Use reshape (cast) process to add missing data (e.g. add number of children age 6 enrolled in sec and HS levels as 0)
frac_grade.wide <- cast(frac_grade, Age~Level, value='FracGrade',add.missing=TRUE)

frac_grade.wide[is.na(frac_grade.wide)] <- 0 # replace NA with 0 

# Now reshape back to long
frac_grade.long <- melt(frac_grade.wide, id.vars = "Age")
colnames(frac_grade.long) <- c("Age","Fraction","Level")
ggplot(frac_grade.long, aes(x=Age, y=Fraction, fill=Level),) +
  geom_bar(stat="identity") + 
  xlab("\nAge") +
  ylab("Fraction of children in the richest 20% hh enrolled (2012)\n") + 
  guides(fill=FALSE) + 
  theme_bw() +
  coord_flip()
dev.copy(png,'topfrac_grade_age.png')
dev.off()

