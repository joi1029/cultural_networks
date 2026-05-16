#Code to model and impute correlations in bootstrapped samples
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
getwd()
rm(list=ls())

library(lme4)
library(doBy)
library(parallel)
library(dplyr)
library(pbapply)

load(file="yearlist.saved")
reps = 500 #number of bootstrap replications

###############################################
#define function
getwd()

#original version
predict_correlations = function(i) {
    # Load required libraries on each worker
    library(lme4)
    library(doBy)
    tryCatch({
        cat("Starting bootstrap replication", i, "\n")
        preds = data.frame() #empty data frame to hold predicted correlations 
        bc = data.frame() #appended data frame containing bootstrapped correlations for all years for a given replication set 
        for(y in yearlist) {
            file_path <- paste("bootstrapped_corrs_500_0312", y, ".saved", sep="")
            if (!file.exists(file_path)) {
                stop(paste("File does not exist:", file_path))
            }
            load(file = file_path)
            
            # Handle both naming conventions (b_corrs or b_corrs_filtered)
            if (exists("b_corrs_filtered")) {
                b_corrs <- b_corrs_filtered
                rm(b_corrs_filtered)
            }
            
            b_corrs[[i]]$year = y
            bc = rbind(bc, b_corrs[[i]])
            rm(b_corrs)  # Clean up for next iteration
        }
        cat("Fitting models for replication", i, "\n")
        
        # Filter out NaN, NA, and infinite values before fitting models
        bc_clean = bc[is.finite(bc$abs_c) & !is.na(bc$abs_c), ]
        cat("Filtered", nrow(bc) - nrow(bc_clean), "problematic values from", nrow(bc), "observations\n")
        
        #statistical models
        m = lmer(abs_c ~ yearcen + (1 + yearcen | j), data = bc_clean, control = lmerControl(optimizer = "bobyqa"))

        bc_clean$count = 1
        master = summaryBy(count ~ j, data = bc_clean, FUN = "sum") # master set of unique item-pairs to use later
        master$j <- factor(master$j, levels = levels(bc_clean$j))

        cat("Completed bootstrap replication", i, "\n")
        for(y in yearlist) {
            sub = bc[bc$year == y, ] #subset of observed correlations for focal year
            e = master #set up for predictions
            
            if(nrow(sub) > 0) {
                e$year = y
                e$yearcen = sub$yearcen[1]
                e$c_est = predict(m, newdata = e)
                e$c_obs = sub$abs_c[match(e$j, sub$j)]
                e$pval_obs = sub$pval[match(e$j, sub$j)] 
                e$cpart_est = NA
                e$cpart_obs = NA
                e$n_obs = sub$n_obs[match(e$j, sub$j)]
                e = e[, c("j", "year", "c_est", "c_obs", "cpart_est", "cpart_obs", "pval_obs", "n_obs")]
                preds = rbind(preds, e)
            }
            #print(head(e))
        }
        return(preds)
    }, error = function(e) {
        cat("Error in replication", i, ":", e$message, "\n")
        return(NULL)
    })
}


# Quick test on one bootstrap
# result <- predict_correlations(1)
# head(result)
# tail(result)
# str(result)
# summary(result$c_obs)
# save(result, file="bootstrapped_pred_corrs_1_0312.saved")
# rm(result)



# Parallel Processing ====================================================
n.core <- 7
cl <- makeCluster(n.core)
print(n.core)

clusterEvalQ(cl, {
    library(lme4)
    library(data.table)
    library(doBy)
    library(plyr)
})

# Set working directory on each worker nodes
clusterEvalQ(cl, setwd("Z:/jc3528/OilSpill/CultureNetwork_0312"))
clusterExport(cl, varlist = c("yearlist", "reps"), envir = environment())

cat("Starting parallel bootstrap with", reps, "replications on", n.core, "cores\n")

results <- pblapply(1:reps, predict_correlations, cl = cl)

# apply predict_correlations function in parallel, over all bootstrap replications
# put everything in the results list

stopCluster(cl)

cat("Bootstrap completed with", length(results), "replications\n")

length(results)
save(results, file = "bootstrapped_pred_corrs_1-500_0312.saved")