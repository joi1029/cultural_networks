#Code to model observed correlations and use model to impute missing correlations
rm(list=ls())
setwd("Z:/jc3528/OilSpill/CultureNetwork_0223_sesnodem")

library(doBy)
library(lme4)
library(plyr)

load("Z:/jc3528/OilSpill/CultureNetwork_0223_sesnodem/all_pairs_0312.saved") # called pairs
load("Z:/jc3528/OilSpill/CultureNetwork_0312/all_correlations_0312.saved") # called r

# Preview
length(pairs)
length(unique(pairs))  
class(pairs)


r = subset(r, !(is.na(r$abs_c)) & is.finite(r$abs_c))#reduce to those for which correlations could be computed (removes NA and NaN)nrow(r)
length(unique(r$j))


min = 5 #minimum number of appearances in editions of GSS for each correlation-pair 


####################################################################
#Mixed-effects model of observed correlations

r$j=as.factor(r$j)
r$year72_dec = r$year72/10 #year variable starting at 1972 and scaled to decades
tail(r)

#original method:
#reduce to correlation-pairs that appear enough times to meet threshold
r$count = 1 #column filled with 1 for counting
#group data by j (variable pairs)
#for each group, calculate
  #1) mean of year72_dec [(current-1972)/10], which is the mean decade from 1972 
  #2) the number of belief pairs (sum count column)
s = summaryBy(year72_dec+count ~ j, data=r, FUN=c(mean, sum))
r$num_years=s$count.sum[match(r$j, s$j)] #in r, add column showing how many years a pair occurred in
nrow(r)
r=subset(r, r$num_years>=min) #filter, for at least_ years something occur in
nrow(r)
head(r)
length(unique(r$j)) # unique pairs after filtering for 5 occurrences
#mean-center year variable 
r$yearcen = scale(r$year72_dec, center = T, scale = F)

save(r, file="modelinput_0312.saved")

load("Z:/jc3528/OilSpill/CultureNetwork_0312/modelinput_0312.saved")

#=============================================================================================
#save a list of years for convenient use later
yearlist = unique(r$year)
save(yearlist, file="yearlist.saved")

load(file="modelinput_0312.saved")
nrow(r)

m <- lmer(
  abs_c ~ yearcen + (1 + yearcen | j),
  data    = r,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl   = list(maxfun = 200000)
  )
)
summary(m)
save(m, file="bbqmodel1_0312.saved")
rm(m)



###########################################
#Use model to impute missing correlations and create yearly data sets
getwd()
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")

load(file="modelinput_0312.saved")
load(file="bbqmodel1_0312.saved")
summary(m)


master = summaryBy(count ~ j, data=r, FUN="sum") # master set of unique item-pairs
getwd()
for(ye in 1972:2024) {
  print(ye)
  sub=subset(r, year==ye) #subset of observed correlations for that year
  e = master #set up for predictions
  if(!(empty(sub))) {
    e$year=ye
    e$yearcen = sub$yearcen[1]

    # Convert both to character to ensure matching works
    # (factors with different levels can cause match() to fail)
    e$j <- as.character(e$j)
    sub$j <- as.character(sub$j)

    e$c_est = predict(m, newdata=e) #model-estimated correlation
    e$c_obs = sub$abs_c[match(e$j, sub$j)] #observed correlation (where available)

    #e$p_val_obs = sub$p_val[match(e$j, sub$j)] #observed p-value (where available)
    e$n_obs = sub$n_obs[match(e$j, sub$j)] #observed sample size (where available)

    #if original condition (no partial correlations):
    e = e[, c("j", "year", "c_est", "c_obs", "n_obs")]

    head(e)

    output = paste("corr_predictions_model_0312", ye, ".saved", sep="")
    save(e, file = output)
  }
}
