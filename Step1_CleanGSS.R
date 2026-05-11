setwd("Z:/jc3528/OilSpill/CultureNetwork_0204")
getwd()
rm(list=ls())

install.packages(c("haven", "psych", "tidyr", "dplyr", "foreign"))
library(haven)
require(psych)
library(tidyr)
library(dplyr)
library(foreign)

# read in stata file of GSS
d=read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo.dta")
d <- as.data.frame(lapply(d, as.vector))  # Strip all haven attributes
d <- data.frame(lapply(d, as.numeric), stringsAsFactors = FALSE) 


# Take log of realinc and size variables
d$realinc <- log(d$realinc)
summary(d$realinc)

# log size of place
d$size <- ifelse(d$size == 0, 0.5, d$size)
d$size <- d$size * 1000
d$size <- log(d$size)
summary(d$size)

ncol(d) #1366 columns

write.dta(d,file="GSS_Recoded2024_0204_withdemo_logtransformed.dta")