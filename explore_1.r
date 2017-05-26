# https://rpubs.com/Jovial/R
# https://www.r-bloggers.com/plot-matrix-with-the-r-package-ggally/
# https://stats.stackexchange.com/questions/125611/quick-exploratory-analysis-of-categorical-data

# http://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/

############################################################################
############################################################################

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Hmisc)
library(pastecs)
library(psych)
library(GGally)



############################################################################################
###  Questions to Ask
############################################################################################

# what to do when a patient did the same type of test multiple times with multiple results?
# 'EncID.new' == 11101955 has done ALB 2 times, ALP 2 times, ALT 2 times...

# planning to do:
# calc duration, i.e., hospital stay time
# histogram of duration
#
# how many NAs for each patient
# calc %25, %50, %75, mean, sd for each test
#
# for each test, how many people have done it?
#
# should i add number of the tests? say test_count and then test_count_total
# 
# see things in short-stay and long-stay

# multiple Reference.Range:
# "lab.creatinine.csv"  
# "lab.hgb.csv" 
# "lab.lactate.csv"

#
# any INVALID data values?
#
# total number of tests a patient has done
# 
# some files have Test.ID with more than 1 value --> 'albumin' and 'lactate'
    #xx <- read.csv("Data/Clinical/lab.albumin.csv", header=T, stringsAsFactors = F)
    # unique(xx$Test.ID)
    #[1] "ALB"   "ALBPE" "CALB" 

    # > xx <- read.csv("Data/Clinical/lab.lactate.csv", header=T, stringsAsFactors = F)
    # unique(xx$Test.ID)
    # [1] "VLACT" "ALACT"

# some file does not have a value in Test.ID:  --> 'sodium' 
    #xx <- read.csv("Data/Clinical/lab.sodium.csv", header=T, stringsAsFactors = F)
    # unique(xx$Test.ID)
    #[1] NA
#
# how to interprete medication_sameday.csv and radiology.csv


          ####################################################
          ##                                                ##
          ##              Clinical Data                     ##
          ##                                                ##
          ####################################################

#(1) lab.albumin.csv

ALB <- read.csv("Data/Clinical/lab.albumin.csv", header=T, stringsAsFactors=F)
#ALB$EncID.new <- as.character(ALB$EncID.new)

ALB <- ALB %>% select(-c(Site, Test.Item)) # 10750    11 (10750 observations)

length(unique(ALB$EncID.new))
#[1] 10166 unique patient IDs


#ALB %>% mutate(reading = case_when(
#                                 .$Result.Value >  50 ~ "ABOVE",
#                                 .$Result.Value <  35 ~ "UNDER",
#                                 TRUE ~ "NORMAL"))


# how many different types of 'Test.ID' in the table
testDF <- data.frame(table(ALB$Test.ID))
colnames(testDF) <- c("Test.ID", "Count")
testDF

#####################################

id_occurCount <- ALB %>% group_by(EncID.new) %>% 
  summarise(ALB_Count=n(), Mean=round(mean(Result.Value),1)) %>% 
  arrange(desc(ALB_Count))

ALB_joined <- left_join(ALB, id_occurCount, by='EncID.new')


albumin <- ALB_joined %>% select(EncID.new, Mean, ALB_Count, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time) %>%
                    rename(ALB = Mean) %>%
                    select(EncID.new, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time, ALB, ALB_Count) %>%
                    distinct()

combined <- albumin # 10166     7



###########################################################################################

calcLOS <- function(startDate, startTime, endDate, endTime){
  t1 <- paste(startDate, startTime)
  t2 <- paste(endDate, startTime)
  tt.interval <- t1 %--% t2
  time.duration <- as.duration(tt.interval)
  return (as.numeric(time.duration, "hours"))
}



getTestData <- function(fileName){
  filePath <- paste0("Data/Clinical/",fileName)
  target <- read.csv(filePath, header=T, stringsAsFactors=F)
  #target <- target %>% select(-c(Site, Test.Item))
  
  # how many different types of 'Test.ID' in the table
  testDF <- data.frame(table(target$Test.ID))
  colnames(testDF) <- c("Test.ID", "Count")
  testDF
  
  
  id_occurCount <- target %>% group_by(EncID.new) %>% 
    summarise(ID_Count=n(), Mean=round(mean(Result.Value),1)) %>% 
    arrange(desc(ID_Count))
  
  target_joined <- left_join(target, id_occurCount, by='EncID.new')
  
  result <- target_joined %>% select(EncID.new, Mean, ID_Count, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time) %>%
    rename(target.code = Mean) %>%
    select(EncID.new, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time, target.code, ID_Count) %>%
    distinct()
  
  return (result)
}  



doTest <- function(){
  thenames <- list.files("./Data/Clinical")
  thenames <- thenames[2:16]  # BE VERY very CAREFUL HERE
  codes <- c("ALP", "ALT", "AST", 
             "CA", "CR", "GLUF", "GLUR",
             "HGB", "VLACT", "MCV", "PLT",
             "k", "sodium", "TNI", "IWBCR"
  )
  
  
  #for (i in 1:length(thenames)){
  for (i in 1:14){
    thefile <- thenames[i]
    codeName <- codes[i]
    testCount <- paste0(codeName, "_Count") 
    
    tmp <- getTestData(thefile)
    
    colnames(tmp)[which(colnames(tmp) == "target.code")] <- codeName
    colnames(tmp)[which(colnames(tmp) == "ID_Count")] <- testCount
    
    combined <- full_join(combined, tmp, by = c("EncID.new", "Admit.Date", "Admit.Time", "Discharge.Date", "Discharge.Time"))
  }
  
  return (combined)
}


# testSet has the information from 17 individual test tables
testSet <- doTest()  # 15693    37

# LOS unit: hour
testSet$LOS <- calcLOS(testSet$Admit.Date, testSet$Admit.Time, testSet$Discharge.Date, testSet$Discharge.Time)
# 15693    38


smallSet <- testSet %>% select(EncID.new, ALB, ALP, ALT, AST, CA, CR, GLUF, 
                               GLUR, HGB, VLACT, MCV, PLT, k, sodium, TNI, IWBCR, LOS)


              ####################################################
              ##                                                ##
              ##          simple exploration with smallSet      ##
              ##                                                ##
              ##  smallSet contains 16 different tests          ##
              ####################################################

totalNA <- function(x){
  return (sum(is.na(x)))
}

# smallSet$NA_count <- apply(smallSet, 1, totalNA)
smallSet$NA_count <- rowSums(is.na(smallSet))

ggplot(data=smallSet, aes(x=NA_count)) + geom_histogram(bins=25, color="lightblue") +
  scale_x_continuous(breaks = seq(0,16,2)) +
    scale_y_continuous(breaks = seq(0,3000,500)) + 
    xlab("Number of missing tests") +
    ylab("Number of patients")


smallSetTestOnly <- smallSet %>% select(-c(EncID.new, LOS, NA_count))
# NA_count_PerTest <- colSums(is.na(smallSetTestOnly)) # same as below
  
NA_count_PerTest <- smallSetTestOnly %>% 
          summarise_all(funs(Total = sum(is.na(.))))

md <- melt(NA_count_PerTest)
md$percent <- round((md$value / nrow(smallSetTestOnly)) * 100, 1)
names(md) <- c("missing Test.ID", "missing Count", "Percent(%)")
md$"missing Test.ID" <- gsub("_Total", "", md$"missing Test.ID")
md



boxplot(smallSet$typeStay)

smallSet$typeStay <- if_else(smallSet$LOS <= 72, "short_stay", "long_stay")
smallSetTally <- smallSet %>% count(typeStay) %>% rename(Count = n)
ggplot(data=smallSetTally, aes(x=typeStay, y=Count)) + 
  geom_bar(stat="identity", fill='lightgrey', colour='darkgrey') + 
  scale_y_continuous(breaks = seq(0,10000,1000)) 
  
  



############################################################################################
# medication_sameday.csv (haven't prcessed this one yet)

medication <- read.csv("Data/Clinical/medication_sameday.csv", header=T, stringsAsFactors=F)
medication <- medication %>% select(c(EncID.new, din, route, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time)) 

# testSet now has the information from medication table
testSet <- full_join(testSet, medication, by = c("EncID.new", "Admit.Date", "Admit.Time", "Discharge.Date", "Discharge.Time"))





          ####################################################
          ##                                                ##
          ##              Administrative Data               ##
          ##                                                ##
          ####################################################

###############################################################################
# smh.adm.nophi.csv

adm <- read.csv("Data/CIHI/smh.adm.nophi.csv", header= T, stringsAsFactors=F)
dim(adm) #[1] 16953    16

idx <- which(LETTERS %in% c("A", "F", "H"))
selectedVar <- names(adm)[idx]  # "EncID.new", "Admitting.Service", "Language"
adm <- adm[selectedVar]

patientSet <- left_join(testSet, adm, by = 'EncID.new') # [1] 15693    40

###############################################################################
# smh.er.nophi.csv

er <- read.csv("Data/CIHI/smh.er.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("E", "J", "K", "N", "O", "P", "Q", "R", "S"))
selectedVar <- names(er)[idx]
selectedVar <- c(selectedVar, 'EncID.new')
er<- er[selectedVar]

patientSet <- left_join(patientSet, er, by = 'EncID.new') # [1] 15693    49

###############################################################################
# smh.er_consults.nophi.csv

er_consults <- read.csv("Data/CIHI/smh.er_consults.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("B", "C", "E"))
selectedVar <- names(er_consults)[idx]
er_consults<- er_consults[selectedVar]

patientSet <- left_join(patientSet, er_consults, by = 'EncID.new')  # something wrong with this one!!

###############################################################################
# smh.er_diag.nophi.csv

er_diag <- read.csv("Data/CIHI/smh.er_diag.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("B", "C", "D"))
selectedVar <- names(er_diag)[idx]
er_diag<- er_diag[selectedVar]

patientSet <- left_join(patientSet, er_diag, by = 'EncID.new')

###############################################################################
# smh.er_int.nophi.csv (ask Yishan for a better table)



###############################################################################
#smh.ip_cmg.nophi.csv 

cmg <- read.csv("Data/CIHI/smh.ip_cmg.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("B", "E", "H"))
selectedVar <- names(cmg)[idx]
cmg<- cmg[selectedVar]

patientSet <- left_join(patientSet, cmg, by = 'EncID.new')

###############################################################################
#smh.ip_dad.nophi.csv 

dad <- read.csv("Data/CIHI/smh.ip_dad.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("A", "B", "C", "D", "F", "N", "P", "Q", "R", "S"))
selectedVar <- names(dad)[idx]
selectedVar <- c(selectedVar, 'EncID.new')
dad<- dad[selectedVar]

patientSet <- left_join(patientSet, dad, by = c("EncID.new", "Admit.Date", "Admit.Time", "Discharge.Date", "Discharge.Time"))

##############################################################################

# somewhere duplicate records were introduced
testSet <- distinct(testSet)

###############################################################################
#smh.ip_diag.nophi.csv 

ip_diag <- read.csv("Data/CIHI/smh.ip_diag.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("A", "C", "D"))
selectedVar <- names(ip_diag)[idx]
ip_diag<- ip_diag[selectedVar]

testSet <- left_join(testSet, ip_diag, by = c("EncID.new")) # something wrong with this one!!

###############################################################################
#smh.ip_xfer.nophi.csv 

xfer <- read.csv("Data/CIHI/smh.ip_xfer.nophi.csv", header= T, stringsAsFactors=F)
idx <- which(LETTERS %in% c("A", "C", "D", "G"))
selectedVar <- names(xfer)[idx]
xfer<- xfer[selectedVar]

testSet <- left_join(testSet, xfer, by = c("EncID.new"))


##############################################################################

# somewhere duplicate records were introduced
testSet <- distinct(testSet)
#[1] 5688532      68
###############################################################################




###############################################################################
###############################################################################
# try stuff here



#id_occurCount_ALB <- data.frame(table(ALB$EncID.new))
# (550 2) 550 patient IDs with more than 1 record
#id_occurCount_ALB_multi <- id_occurCount_ALB %>% 
#  filter(Freq > 1) %>% 
#  arrange(desc(Freq)) %>% 
#  rename(EncID.new = Var1) 


# ID = 11769379

adm <- read.csv("Data/CIHI/smh.adm.nophi.csv", header= T, stringsAsFactors=F)
dim(adm) #[1] 16953    16

idx <- which(LETTERS %in% c("A", "F", "H"))
selectedVar <- names(adm)[idx]  # "EncID.new", "Admitting.Service", "Language"

adm <- adm[selectedVar]
summary(adm)

## for numerical variable

fivenum(dat$EncID.new)
#min, max, median, lower-hinge and upper-hinge 
#[1] 11100037 11331793 11550689 11779887 11999988

describe(adm$EncID.new)

##################################################################
## these two statements are exactly the same
adm %>% group_by(Admitting.Service) %>% summarise(total=n())
adm %>% count(Admitting.Service) 

##################################################################
dat2 <- read.csv("Data/CIHI/smh.er.nophi.csv", header=T)

selected <- c("E", "J", "K", "N", "O", "P", "Q", "R", "S")

##################################################################
##################################################################
clinicalData <- fread("Data/Clinical/lab_all.csv", header=T)
# [1] 5336765       9

tmp <- clinicalData %>% select(1,3, 5, 8)
tmp_WBC <- tmp %>% filter(Test.ID == 'IWBCR')
dim(tmp_WBC)
#[1] 106853      2
id_occurCount_tmp_WBC <- data.frame(table(tmp_WBC$EncID.new))
dim(id_occurCount_tmp_WBC)  #[1] 16296     2
id_occurCount_tmp_WBC <- id_occurCount_tmp_WBC %>% 
  filter(Freq > 1) %>% 
  arrange(desc(Freq)) %>% 
  rename(EncID.new = Var1)


WBC <- read.csv("Data/Clinical/lab.wbc.csv", header=T, stringsAsFactors=F)
WBC <- WBC %>% select(1,3)
dim(WBC) # [1] 16569  2
length(unique(WBC$EncID.new)) # 15607 unique ID

id_occurCount_WBC <- data.frame(table(WBC$EncID.new))
dim(id_occurCount_WBC) #[1] 15607     2
id_occurCount_WBC <- id_occurCount_WBC %>% 
  filter(Freq > 1) %>% 
  arrange(desc(Freq)) %>% 
  rename(EncID.new = Var1) 
# 902   2


tmp_WBC %>% filter(EncID.new == 11126955)



found <- WBC$EncID.new %in% tmp_WBC$EncID.new
which(!found)
#integer(0)
which(found == F)
#integer(0)

diff_WBC <- setdiff(tmp_WBC, WBC)
dim(diff_WBC)


############
low <- 35
high <- 125
filename <- "lab.alp.csv"
tmp <- data.frame(filename, low, high)

testReference <- rbind(testReference, tmp)
testReference

write.csv(testReference, 'testReference.csv', row.names = F)
