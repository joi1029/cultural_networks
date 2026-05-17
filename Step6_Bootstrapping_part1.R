#Code to create bootstrapped GSS samples and gather correlations from them 
rm(list=ls())

setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
getwd()
ls()

library(igraph)
library(dplyr)
library(foreign)
library(sjstats)
library(psych)
library(janitor)
library(parallel)
library(haven)
library(DescTools)
library(pbapply)

set.seed(36)

#load data
load(file="Z:/jc3528/OilSpill/CultureNetwork_0312/modelinput_0312.saved") #model-input dataframe
load(file="yearlist.saved")

d=read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo_logtransformed.dta")
d <- d %>% 
  mutate(across(where(haven::is.labelled), ~ as.numeric(.x)))
d[] <- lapply(d, function(x) as.numeric(as.character(x))) #check type
class(d)

#only keep GSS variables we need
load(file="Z:/jc3528/OilSpill/CultureNetwork_0312/full_network_03122024.saved")

# check nodes
V(g)
nodes = V(g)$name
length(nodes)
print(nodes)
class(nodes)

d = d[, colnames(d)=="year"|colnames(d) %in% nodes] #keep only relevant nodes
#d <- data.frame(lapply(d, as.numeric))
str(d)
length(d)


set.seed(36)
reps = 500 #number of bootstrap replications


###############################################
#define function
bootstrap_GSS_samples = function(y) {
    print(y)
    
    d1 <- subset(d, year == y)
    print(nrow(d1))
    d1 <- remove_empty(d1, which = c("cols"))
    
    # Capture original factor levels for nominal variables BEFORE bootstrapping
    # This ensures CramerV/Lambda use consistent category counts across all bootstrap samples
    original_levels <- list()
    for (v in nominal_vars) {
        if (v %in% names(d1)) {
            original_levels[[v]] <- sort(unique(d1[[v]][!is.na(d1[[v]])]))
        }
    }
    b_corrs = list()
    
    for(j in 1:reps) {
        # Bootstrap: resample with replacement, same number of rows as original
        # with bootstrapping with replacement
        n_rows <- nrow(d1)
        boot_indices <- sample(1:n_rows, size = n_rows, replace = TRUE)
        bs <- d1[boot_indices, ]

        if (j %% 50 == 0) print(paste("Bootstrap replication", j, "of", reps))

        year_pairs = r[r$year==y, c("x", "y", "year", "j", "year72", "year72_dec", "yearcen")]
        print("hi")
        b_corrs[[j]] = year_pairs
        b_corrs[[j]]$abs_c = NA
        b_corrs[[j]]$abs_cpart = NA
        b_corrs[[j]]$pval = NA
        b_corrs[[j]]$n_obs = NA

        for(idx in 1:nrow(b_corrs[[j]])) {
            var1 = as.character(b_corrs[[j]]$x[idx])
            var2 = as.character(b_corrs[[j]]$y[idx])

            if (!(var1 %in% names(bs)) || !(var2 %in% names(bs))) {
                b_corrs[[j]]$abs_c[idx] <- NA
                b_corrs[[j]]$abs_cpart[idx] <- NA
                b_corrs[[j]]$pval[idx] <- NA
                b_corrs[[j]]$n_obs[idx] <- 0
                next
            }
            
            # Determine variable types
            var1_nominal <- var1 %in% nominal_vars
            var2_nominal <- var2 %in% nominal_vars
            
            # Create a new copy for this pair
            bs_temp <- bs
            
            # Convert to appropriate types, using original factor levels for nominal vars
            if (var1_nominal) {
                if (var1 %in% names(original_levels)) {
                    bs_temp[[var1]] <- factor(bs_temp[[var1]], levels = original_levels[[var1]])
                } else {
                    bs_temp[[var1]] <- as.factor(bs_temp[[var1]])
                }
            } else {
                bs_temp[[var1]] <- suppressWarnings(as.numeric(as.character(bs_temp[[var1]])))
            }
            if (var2_nominal) {
                if (var2 %in% names(original_levels)) {
                    bs_temp[[var2]] <- factor(bs_temp[[var2]], levels = original_levels[[var2]])
                } else {
                    bs_temp[[var2]] <- as.factor(bs_temp[[var2]])
                }
            } else {
                bs_temp[[var2]] <- suppressWarnings(as.numeric(as.character(bs_temp[[var2]])))
            }
            
            # Remove rows with NA in either variable
            valid_cases <- complete.cases(bs_temp[[var1]], bs_temp[[var2]])
            bs_valid <- bs_temp[valid_cases, ]
            b_corrs[[j]]$n_obs[idx] = sum(valid_cases)
            
            # Helper function: PRE = sqrt(R^2) (same as compute_correlations_mixed) ----
            pre_sqrt_r2 <- function(dv_numeric, iv) {
                ok <- complete.cases(dv_numeric, iv)
                if (sum(ok) < 5) return(NA_real_)
                
                y <- dv_numeric[ok]
                x <- iv[ok]
                
                # DV must vary
                if (!is.numeric(y) || sd(y, na.rm = TRUE) == 0) return(NA_real_)
                
                # IV must vary
                if (is.factor(x) || is.character(x)) x <- as.factor(x)
                if (length(unique(x)) < 2) return(0)
                
                fit <- try(lm(y ~ x), silent = TRUE)
                if (inherits(fit, "try-error")) return(NA_real_)
                
                r2 <- summary(fit)$r.squared
                if (!is.finite(r2) || r2 < 0) return(NA_real_)
                
                out <- sqrt(r2)
                if (!is.finite(out)) return(NA_real_)
                out
            }
            
            # Dichotomize nominal variable based on variable name (same as compute_correlations_mixed)
            # race: White (1) vs. Black/Other (2,3)
            # region: South (5,6,7) vs. Non-South (others)
            # relig: modal category vs. others
            dichotomize_nominal <- function(f, varname) {
                f_numeric <- as.numeric(as.character(f))
                if (varname == "race") {
                    # White (1) vs. Black/Other (2,3)
                    return(as.numeric(f_numeric == 1))
                } else if (varname == "region") {
                    # South (5,6,7) vs. Non-South
                    return(as.numeric(f_numeric %in% c(5, 6, 7)))
                } else {
                    # relig and others: modal category vs. others
                    tab <- table(f, useNA = "no")
                    if (length(tab) == 0) return(rep(NA, length(f)))
                    modal_cat <- names(tab)[which.max(tab)]
                    return(as.numeric(as.character(f) == modal_cat))
                }
            }
            
            # PRE calculation (same logic as compute_correlations_mixed)
            if (!var1_nominal && !var2_nominal) {
                # ordinal-ordinal: PRE = |Pearson|
                cor_result <- suppressWarnings(cor.test(bs_valid[[var1]], bs_valid[[var2]], method = "pearson"))
                c_val <- as.numeric(abs(cor_result$estimate))
                p_val <- cor_result$p.value
                
                if (!is.finite(c_val)) c_val <- NA_real_
                if (is.finite(c_val)) c_val <- max(0, min(1, c_val))
                
                b_corrs[[j]]$abs_c[idx] <- c_val
                b_corrs[[j]]$pval[idx] <- p_val
                
            } else {
                # any edge with >=1 nominal: mean of directional PREs, dichotomize nominal DV when needed
                p_val <- NA_real_
                
                # Direction A: DV = var2, IV = var1
                if (var2_nominal) {
                    y_bin <- dichotomize_nominal(bs_valid[[var2]], var2)
                    x_iv  <- if (var1_nominal) factor(bs_valid[[var1]]) else bs_valid[[var1]]
                    pre_A <- if (all(is.na(y_bin))) NA_real_ else pre_sqrt_r2(y_bin, x_iv)
                } else {
                    y_num <- bs_valid[[var2]]
                    x_iv  <- if (var1_nominal) factor(bs_valid[[var1]]) else bs_valid[[var1]]
                    pre_A <- pre_sqrt_r2(y_num, x_iv)
                }
                
                # Direction B: DV = var1, IV = var2
                if (var1_nominal) {
                    y_bin <- dichotomize_nominal(bs_valid[[var1]], var1)
                    x_iv  <- if (var2_nominal) factor(bs_valid[[var2]]) else bs_valid[[var2]]
                    pre_B <- if (all(is.na(y_bin))) NA_real_ else pre_sqrt_r2(y_bin, x_iv)
                } else {
                    y_num <- bs_valid[[var1]]
                    x_iv  <- if (var2_nominal) factor(bs_valid[[var2]]) else bs_valid[[var2]]
                    pre_B <- pre_sqrt_r2(y_num, x_iv)
                }
                
                c_val <- mean(c(pre_A, pre_B), na.rm = TRUE)
                if (!is.finite(c_val)) c_val <- NA_real_
                if (is.finite(c_val)) c_val <- max(0, min(1, c_val))
                
                b_corrs[[j]]$abs_c[idx] <- c_val
                b_corrs[[j]]$pval[idx] <- p_val
            }
            
            b_corrs[[j]]$abs_cpart[idx] <- NA
        }
    }
    
    save(b_corrs, file=paste("bootstrapped_corrs_", reps, "_0312", y, ".saved", sep=""))
    return(b_corrs)
}



colnames(r)
#==================================================================
load("yearlist.saved")
nominal_vars <- c("region", "relig", "race")
reps = 500
yearlist

#no parallel processing, for a quick test with one year
# all_bcorrs <- lapply(c(1972), bootstrap_GSS_samples)
# load("bootstrapped_corrs_5003121972.saved")
# head(b_corrs)
# rm(b_corrs)

#with parallel
n.core <- 10
cl <- makeCluster(n.core)

clusterEvalQ(cl, {
  library(igraph)
  library(foreign)
  library(sjstats)
  library(psych)
  library(janitor)
  library(haven)
  library(DescTools)
})

clusterExport(cl, varlist = c("d", "r", "yearlist", "reps", "nominal_vars", "bootstrap_GSS_samples", "remove_empty"), envir = environment())

print("Starting parallel bootstrap ...")
all_bcorrs <- pblapply(yearlist, bootstrap_GSS_samples, cl = cl)

# Stop cluster
stopCluster(cl)