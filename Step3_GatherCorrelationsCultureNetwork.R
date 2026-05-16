library(haven)
library(psych)
library(parallel)
library(plyr)
library(janitor)
library(stringr)
library(pbapply)
library(infotheo)
library(ppcor)
library(DescTools)


# Set working directory
setwd("Z:/jc3528/OilSpill/CultureNetwork_0211")

# clear workspaec
rm(list = ls())

# Load required data
load(file = "Z:/jc3528/OilSpill/CultureNetwork_0211/all_pairs_0312.saved") #vector of valid belief pairs
head(pairs)
str(pairs)
length(pairs)

# Load in processed GSS dataset
d=read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo_logtransformed.dta")
d <- d %>% 
  mutate(across(where(haven::is.labelled), ~ as.numeric(.x)))
d[] <- lapply(d, function(x) as.numeric(as.character(x))) #check type
class(d)
getwd()


# name nominal variables for later correlation calculation
nominal_vars <- c("relig", "region", "race")


compute_correlations_mixed <- function(index, d, pairs) {
  pair <- pairs[index]
  var1 <- str_split(pair, ",")[[1]][1]
  var2 <- str_split(pair, ",")[[1]][2]
  ye   <- as.numeric(str_split(pair, ",")[[1]][3])

  # PRE = sqrt(R^2), and dichotomize variables as needed
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

  # Dichotomize nominal variable based on variable name
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

  # aubset data for year
  d1 <- d[d$year == ye, , drop = FALSE]
  d1 <- remove_empty(d1, which = c("cols"))

  nominal_vars <- c("relig", "region", "race")
  var1_nominal <- var1 %in% nominal_vars
  var2_nominal <- var2 %in% nominal_vars

  # Check if variables exist in dataset
  if (!(var1 %in% names(d1)) || !(var2 %in% names(d1))) {
    return(paste(var1, var2, ye, NA, NA, NA, NA, 0, sep = ","))
  }

  if (var1_nominal) d1[[var1]] <- as.factor(d1[[var1]]) else d1[[var1]] <- suppressWarnings(as.numeric(as.character(d1[[var1]])))
  if (var2_nominal) d1[[var2]] <- as.factor(d1[[var2]]) else d1[[var2]] <- suppressWarnings(as.numeric(as.character(d1[[var2]])))

  # Remove rows with NA in either variable
  valid_cases <- complete.cases(d1[[var1]], d1[[var2]])
  d1_valid <- d1[valid_cases, , drop = FALSE]
  n_obs <- sum(valid_cases)
  
  # Early return if insufficient observations
  if (n_obs < 2) {
    return(paste(var1, var2, ye, NA, NA, NA, NA, n_obs, sep = ","))
  }
  
  # Check for variance in non-nominal variables
  if (!var1_nominal && !var2_nominal) {
    var1_vals <- d1_valid[[var1]]
    var2_vals <- d1_valid[[var2]]
    if (length(unique(var1_vals[!is.na(var1_vals)])) < 2 || length(unique(var2_vals[!is.na(var2_vals)])) < 2) {
      return(paste(var1, var2, ye, 0, NA, NA, NA, n_obs, sep = ","))
    }
  }

  # PRE calculation
  if (!var1_nominal && !var2_nominal) {
    # ordinal–ordinal: PRE = |Pearson|
    cor_result <- suppressWarnings(cor.test(d1_valid[[var1]], d1_valid[[var2]], method = "pearson"))
    c <- as.numeric(abs(cor_result$estimate)) # absolute pearson correlation
    p_val <- cor_result$p.value

    if (!is.finite(c)) c <- NA_real_
    if (is.finite(c)) c <- max(0, min(1, c))

  } else {
    # any edge with >=1 nominal: mean of directional PREs, dichotomize nominal DV when needed
    p_val <- NA_real_

    # Direction A: DV = var2, IV = var1
    if (var2_nominal) {
      y_bin <- dichotomize_nominal(d1_valid[[var2]], var2)
      x_iv  <- if (var1_nominal) factor(d1_valid[[var1]]) else d1_valid[[var1]]
      pre_A <- if (all(is.na(y_bin))) NA_real_ else pre_sqrt_r2(y_bin, x_iv)
    } else {
      y_num <- d1_valid[[var2]]
      x_iv  <- if (var1_nominal) factor(d1_valid[[var1]]) else d1_valid[[var1]]
      pre_A <- pre_sqrt_r2(y_num, x_iv)
    }

    # Direction B: DV = var1, IV = var2
    if (var1_nominal) {
      y_bin <- dichotomize_nominal(d1_valid[[var1]], var1)
      x_iv  <- if (var2_nominal) factor(d1_valid[[var2]]) else d1_valid[[var2]]
      pre_B <- if (all(is.na(y_bin))) NA_real_ else pre_sqrt_r2(y_bin, x_iv)
    } else {
      y_num <- d1_valid[[var1]]
      x_iv  <- if (var2_nominal) factor(d1_valid[[var2]]) else d1_valid[[var2]]
      pre_B <- pre_sqrt_r2(y_num, x_iv)
    }

    c <- mean(c(pre_A, pre_B), na.rm = TRUE)
    if (!is.finite(c)) c <- NA_real_
    if (is.finite(c)) c <- max(0, min(1, c))
  }

  cpart_demo <- NA
  cpart <- NA
  paste(var1, var2, ye, c, cpart, cpart_demo, p_val, n_obs, sep = ",")
}


compute_correlations_mixed(2, d, pairs) #test function



#=================================================
options(warn = -1) 
# Set up the parallel backend to use multiple cores

n.core <- 14
cl <- makeCluster(n.core)
clusterEvalQ(cl, {
  library(ppcor)
  library(psych)
  library(infotheo)
  library(aricode)
  library(DescTools)
})

clusterExport(cl, varlist = c("d", "pairs", "str_split", "remove_empty", "cor.test", "partial.r", "subset"), envir = environment())
# execute compute_correlations in parallel
cat("Processing", length(pairs), "correlation pairs...\n")
results <- pblapply(cl = cl, X = seq_along(pairs), FUN = compute_correlations_mixed, d = d, pairs = pairs)

stopCluster(cl)

head(results)
tail(results[[1]])
str(results)
class(results)
length(results)

# Pre-allocate vectors for better performance  
n_results <- length(results)
x <- character(n_results)
y <- character(n_results) 
year <- numeric(n_results)
c <- numeric(n_results)
cpart <- numeric(n_results)
cpart_demo <- numeric(n_results)
p_val <- numeric(n_results)
n_obs <- numeric(n_results)

batch_size <- 10000
n_batches <- ceiling(n_results / batch_size)

cat("Processing", length(results), "correlation results...\n")

valid_results <- results[!sapply(results, is.null)]
valid_results <- valid_results[sapply(valid_results, is.character)]
valid_results <- valid_results[sapply(valid_results, function(x) length(x) > 0)]

cat("Found", length(valid_results), "valid results out of", length(results), "total results.\n")

for (batch in 1:n_batches) {
  start_idx <- (batch - 1) * batch_size + 1
  end_idx <- min(batch * batch_size, n_results)
  cat("Processing batch", batch, "of", n_batches, "(", start_idx, "to", end_idx, ")\\n")
  for (j in start_idx:end_idx) {
    parts <- unlist(strsplit(results[[j]], ",", fixed = TRUE))
    x[j] <- parts[1]
    y[j] <- parts[2]
    year[j] <- as.numeric(parts[3])
    c[j] <- as.numeric(parts[4])
    cpart[j] <- as.numeric(parts[5])
    cpart_demo[j] <- as.numeric(parts[6])
    p_val[j] <- as.numeric(parts[7])
    n_obs[j] <- as.numeric(parts[8])
  }
}

# create dataframe
r <- data.frame(
  x = x,
  y = y, 
  year = year,
  c = c,
  cpart = cpart,
  cpart_demo = cpart_demo,
  p_val = p_val,
  n_obs = n_obs,
  stringsAsFactors = FALSE
)

summary(r$c)
# # Construct the final dataframe
head(r)
r <- data.frame(x = as.character(x), y = as.character(y), year = as.numeric(year), 
                c = as.numeric(c), cpart = as.numeric(cpart), cpartdemo = as.numeric(cpart_demo),
                p_val = as.numeric(p_val), n_obs = as.numeric(n_obs), stringsAsFactors = FALSE)
r$j <- paste(r$y, "_", r$x, sep = "")
r$abs_c <- abs(r$c)
r$abs_cpart <- abs(r$cpart)
r$abs_cpartdemo <- abs(r$cpart_demo)
r$year72 <- r$year - 1972
nrow(r) #579422 0204version
head(r)
tail(r)


# save dataframe
head(r)
save(r, file = "all_correlations_0312.saved")