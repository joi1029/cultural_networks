setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
getwd()
rm(list=ls())

library(haven)
require(psych)
library(tidyr)
library(dplyr)
library(foreign)

# Read in stata file of GSS
d=read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo.dta") #Stata file with all GSS varables including demographics
d <- as.data.frame(lapply(d, as.vector))  # Strip all haven attributes
d <- data.frame(lapply(d, as.numeric), stringsAsFactors = FALSE) 


# Take log of realinc and size variables
d$realinc <- log(d$realinc)
summary(d$realinc)

# Take log of size of place
d$size <- ifelse(d$size == 0, 0.5, d$size)
d$size <- d$size * 1000
d$size <- log(d$size)
summary(d$size)

ncol(d) #1366 columns, including 'year'

write.dta(d,file="GSS_Recoded2024_0204_withdemo_logtransformed2.dta")
