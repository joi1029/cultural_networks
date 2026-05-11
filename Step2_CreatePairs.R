#Code to save a listing of all correlations that need to be collected from the GSS

setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
getwd()
ls()
rm(list=ls())

library(haven)
require(psych)
library(tidyr)
library(dplyr)

# read in stata file of GSS
d=read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo_logtransformed.dta")
d <- as.data.frame(lapply(d, as.vector))  # Strip all haven attributes
d <- data.frame(lapply(d, as.numeric), stringsAsFactors = FALSE) 
# Get unique years
years <- sort(unique(d$year))

#To preview how many issues are present per year
# year_issue_counts <- data.frame(year = numeric(), issue_count = numeric())
# for (y in years) {
#   d_year <- d[d$year == y, ]
#   col_counts <- sapply(d_year, function(x) sum(!is.na(x)))
#   issues_with_100plus <- sum(col_counts >= 1)
#   year_issue_counts <- rbind(year_issue_counts, 
#                              data.frame(year = y, issue_count = issues_with_100plus))
# }
# print(year_issue_counts)
# show_columns_for_year <- function(year_to_check) {
#   d_year <- d[d$year == year_to_check, ]
#   col_counts <- sapply(d_year, function(x) sum(!is.na(x)))
#   valid_cols <- names(col_counts[col_counts >= 100])
#   cat("Year", year_to_check, "has", length(valid_cols), "issues with >=100 responses:\n")
#   cat(paste(head(valid_cols, 10), collapse=", "), "\n")
#   if(length(valid_cols) > 10) cat("... and", length(valid_cols) - 10, "more\n")
# }
# show_columns_for_year(1972)

#+=======================================================
#Looping through and gathering correlations

#parameters and inputs
years = unique(d$year) #vector of GSS years 
years
thresh = 100 #minimum number of respondents for a correlation to be gathered 

#create empty vector of correlation-pairs 
pairs = vector()

for(ye in years) { #loop through each year individually
  print(ye)
  d1=subset(d, year==ye)   #reduce to focal data for each year 
  d1 = d1[, colSums(is.na(d1))<nrow(d1) & !(colnames(d1)=="year")] #reduce to available variables in a given year. remove the year variable, and any columns that are all NA.
  if(nrow(d1) > 0 && ncol(d1) > 0) { #check that data set is not empty
    #loop through each combination of columns (belief items) in d1
    for(y in 1:ncol(d1)) {
      for(x in 1:ncol(d1)) {
        if(y>x) { #go through each pair only once
          n = pairwiseCount(d1[,y], d1[,x]) #number of respondents who have valid(non-missing) responses for both variables
          if(n>thresh) { #if the number of respondents is greater than 100
            var1 = colnames(d1)[x]
            var2 = colnames(d1)[y]
            pairs = append(pairs, paste(var1, var2, ye, n, sep=",")) #add variable pairs to vector
          }
        }
      }
    }
  }
}
#result in a vector of valid belief pairs
save(pairs, file="all_pairs_0312.saved")