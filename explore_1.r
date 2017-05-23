# https://rpubs.com/Jovial/R
# https://www.r-bloggers.com/plot-matrix-with-the-r-package-ggally/
# https://stats.stackexchange.com/questions/125611/quick-exploratory-analysis-of-categorical-data


library(ggplot2)
library(Hmisc)
library(pastecs)
library(psych)
library(GGally)


dat <- read.csv("Data/CIHI/smh.adm.nophi.csv", header=T)
dim(dat)
#[1] 16953    16

selectedVar <- c("EncID.new", "Admitting.Service", "Language")
dat <- dat[selectedVar]
summary(dat)

## for numerical variable

fivenum(dat$EncID.new)
#min, max, median, lower-hinge and upper-hinge 
#[1] 11100037 11331793 11550689 11779887 11999988

describe(dat$EncID.new)

#################################
## these two are exactly the same
dat %>% group_by(Admitting.Service) %>% summarise(total=n())
dat %>% count(Admitting.Service) 

#################################
dat2 <- read.csv("Data/CIHI/smh.er.nophi.csv", header=T)

selected <- c("E", "J", "K", "N", "O", "P", "Q", "R", "S")

  