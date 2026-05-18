# Culture Network Analysis - GSS Data

Analysis of cultural belief networks using General Social Survey (GSS) data from 1972-2024.

## Required Packages
```r
igraph, dplyr, tidyr, ggplot2, lme4, haven, psych, parallel, pbapply, 
showtext, sysfonts, ineq, doBy, plyr, foreign, sjstats, janitor, DescTools
```

## Analysis Pipeline
**Step 0: Variable Recodes** (`Step0_Variable_Recodes.do`)  
Stata script to recode and select GSS variables to include.

**Step 1: Clean GSS** (`Step1_CleanGSS.R`)  
Import recoded GSS data, strip labels, convert to numeric format. Log-transform real-income and population density variables.

**Step 2: Create Pairs** (`Step2_CreatePairs.R`)  
Generate all variable-pair combinations (correlations) to calculate for each year, filtered by minimum N=100 respondents.

**Step 3: Gather Correlations** (`Step3_GatherCorrelationsCultureNetwork.R`)  
Calculate observed Pearson correlations for all variable pairs across years. Save with sample sizes and standard errors.

**Step 4: Model Correlations** (`Step4_ModelCorrelations.R`)  
Fit mixed-effects model to predict missing correlations using year trends and random effects.

**Step 5: Observed Networks** (`Step5_ObservedNetworks.R`)  
Construct yearly networks, primarily for visualization purpose.

**Step 6: Bootstrapping (Parts 1-3)**  
- `part1`: Generate 500 bootstrap GSS samples and calculate correlations  
- `part2`: Model bootstrap correlations to predict missing edges  
- `part2.5`: Filter bootstrap predictions by SE threshold using the bootstrap distribution 
- `part3`: Calculate network metrics (density, k-core) for each bootstrap, excluding demographic edges from density

**Step 7: Bootstrap Node-Level** (`Step7_Bootstrap_nodelevel.R`)  
Compute node-level centrality metrics (betweenness, degree) across bootstrap samples.

**Step 8: Figures** (`Step8_Figures.R`)  
Generate main paper figures: Fig. 2 density, k-core, Fig. 3 node centralities, Fig. 4 alignment over time, Fig. 5 PC1 Loadings over time.

**Supplement: AIC Test** (`Supplement_AIC_Test.R`)  
Compare model fit statistics for correlation prediction models (Table S2).

**Supplement: Figures** (`Supplement_Figures.R`)  
Generate supplementary figures: Fig. S1 extremism (SD by domain), Fig. S2 PCA eigenvalues, Fig. S3 node centralities with all demographics.

## Key Datafiles/Outputs
- `bootstrapped_pred_corrs_500_0312_filtered_by_se.saved` - list of 500 dataframes for the 500 bootstraps, each containing observed/predicted item-pair correlations used to construct networks
