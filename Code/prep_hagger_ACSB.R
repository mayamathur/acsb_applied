
# Hagger's OSF repo:  https://osf.io/jymhe/

# Manipulation check datasets come from Supplementary Analyses / Data and results files for analysis including all 24 laboratories


# PRELIMINARIES ---------------------------------------------------------------

# rm(list=ls())

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("data.table",
            "purrr",
            "dplyr",
            "tidyverse",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here",
            "MetaUtility",
            "metafor")

# load within installation if needed
for (pkg in to.load) {
  
  cat( paste("\nAbout to try loading package", pkg) )
  
  tryCatch({
    # eval below needed because library() will otherwise be confused
    # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
    eval( bquote( library( .(pkg) ) ) )
  }, error = function(err) {
    install.packages(pkg)
  })
  
}

# run this only if you want to update the R environment specs
# renv::snapshot()


# set working directories
code.dir = here("Code")
# check that it's set correctly
setwd(code.dir)

# if you need to go up a level in parent directory
data.dir = here("Data")
# check that it's set correctly
setwd(data.dir)

results.dir = here("Results") 
# check that it's set correctly
setwd(results.dir)


overleaf.dir = "/Users/mmathur/Dropbox/Apps/Overleaf/Attention checks selection bias (ACSB) Overleaf/R_objects"
setwd(overleaf.dir)


# get helper fns
setwd(code.dir)
source("helper_applied_ACSB.R")

# no sci notation
options(scipen=999)

# SANITY CHECK: REPRODUCE THEIR ANALYSIS  -------------------------------------------------

setwd(data.dir)
setwd("Hagger/Dependent variable")

# they must have used this file rather than RTV_incl for main analyses, because 
# RTV.csv agrees with the study estimates in Fig 1
d1 = fread("RTV.csv")

# remove the original study 
d1 = d1 %>% filter(`Study name` != "Sripada et al. (basis of protocol)")
expect_equal(nrow(d1), 23)

# their code; I updated variable names:
# RTVdat<- d
# RTVdat<- RTVdat[order(RTVdat$study),]
# ### random-effects model meta-analysis 
# # modeled after http://www.metafor-project.org/doku.php/tips:assembling_data_smd
# effectSizesAll<- escalc(measure="SMD", #standardized mean difference
#                         m1i= `Ego Depletion Mean`, m2i= `Control Mean`,
#                         sd1i=`Ego Depletion Std Dev`, sd2i= `Control Std-Dev`,
#                         n1i=`Ego Depletion Sample size`, n2i=`Control Sample size`,
#                         data= RTVdat)
# res <- rma(data=effectSizesAll, yi,vi,  method="REML", slab=paste(Study.name))


# using their yi, vi
rma.uni(yi = `Hedges's g`,
        sei = `Std Err g`,
        data = d1,
        method = "REML",
        knha = FALSE )
# yes, agrees :)

# KNHA: slightly wider, but not that much so
rma.uni(yi = `Hedges's g`,
        sei = `Std Err g`,
        data = d1,
        method = "REML",
        knha = TRUE )



# PREP X-Y DATA ---------------------------------------------------------------

d1 = d1 %>% rename( yi_XY = `Hedges's g`,
                    vi_XY = `Std Err g` )

# make a short ID string for later merging joy
d1$name = sapply(strsplit(d1$`Study name`, "\\s+|,"),
                   function(x) tolower(gsub(",", "", x[1])))

d1 = d1 %>% select(name, yi_XY, vi_XY)


# WRANGLE MANIPULATION CHECK DATA ---------------------------------------------------------------

# ~ Make long dataset of all the manipulation checks  -------------------------------------------------

setwd(data.dir)
setwd("Hagger/Manipulation check items")


# the manuscript seems to describe only these four self-reported measures as manipulation checks (pg 550),
#  not "LetterE" (task accuracy)
check_names = c("difficulty", "effort", "fatigue", "frustration")

# read in and rbind the 4 datasets
for ( check_name in check_names ) {
  
  .dat = fread( paste(check_name, ".csv", sep = "") )
  nrow(.dat)
  
  .dat = .dat %>% add_column(check_name = check_name)
  
  # has a blank row
  .dat = .dat %>% filter(!is.na(`Study name`))
  
  # make a short ID string comparable to that in dataset d1
  .dat$name = sapply(strsplit(.dat$`Study name`, "\\s+|,"),
                     function(x) tolower(gsub(",", "", x[1])))
  
  if ( check_name == check_names[1] ) {
    d2 = .dat
  } else {
    d2 = rbind(d2, .dat)
  }
}


d2 = d2 %>% rename(yi_XR = `Hedges's g`,
                   sei_XR = `Std Err g`)

# has one more name than dataset d1 because Tinghog isn't in Fig 1 forest plot
length(unique(d2$name))


# ~ Make composite scale of the 4 manipulation checks  -------------------------------------------------

# Approach: within each study, make composite using fixed-effects meta-analysis,
# but adjusting the usual inference to account for the (approximate) correlations between
#  the estimates for different checks

### Estimate the pairwise correlations

# get IPD data to estimate pairwise correlations between the manipulation checks themselves
# will use this as a proxy for pairwise correlations between the X-R *effects* for each of these
setwd(data.dir)
setwd("Hagger/Manipulation check items")
ipd = fread("RRR-MergedSubjectData.csv")

ipd = ipd %>% select(Effort, Difficulty, Tiredness, Frustration)
( cormat = cor(ipd, use = "pairwise.complete.obs") )

# unique correlations only
pairwise_corrs = unique( as.vector(cormat) )
pairwise_corrs = pairwise_corrs[ !pairwise_corrs == 1 ]
expect_equal( length(pairwise_corrs), 6 )  # 6 = (4 variables choose 2)

### Make composite manipulation check (R) for each study

# interpretation of R: the inverse-variance-weighted average of standardized effects of X on each check item

d2$yi_XR_agg = NA
d2$sei_XR_agg = NA
d2$sei_XR_agg_usualFE = NA

# loop over replication studies
for ( .name in d2$name ){
  temp = d2 %>% filter(name == .name)
  
  # FE point estimate by hand
  wi = 1 / temp$sei_XR^2
  ( FE_est = (1 / sum(wi) ) * sum( wi * temp$yi_XR ) )
  
  # sanity check: c.f. simple mean
  # mean(temp$yi_XR)
  
  d2$yi_XR_agg[ d2$name == .name ] = FE_est
  
  # following my derivation for conservative internal meta-analysis ("Meta-analysis cheat sheet")
  #
  # "term" is the term inside second summation, which would be 0 if everything were uncorrelated:
  #   sum_{i<j}{ Corr(thetahat_i, thetahat_j) * 1/SE_i * 1/SE_j }
  #
  # note that corr = covariance because R is standardized
  term = (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "frustration", "sei_XR"]) * cormat["Effort", "Frustration"] +
    (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) * cormat["Effort", "Difficulty"] +
  (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) * cormat["Effort", "Tiredness"] + 
    
    (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) * cormat["Effort", "Difficulty"] +
  (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) * cormat["Effort", "Tiredness"] + 
    
    (1/temp[check_name == "difficulty", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) * cormat["Difficulty", "Tiredness"] 
     
  term = as.numeric(term) 
  
  # FE variance estimate, but accounting for correlations
  FE_var_adjusted = (1 / sum(wi) ) + (1 / sum(wi) )^2 * 2 * term
  
  # c.f. the usual FE SE:
  sqrt(1 / sum(wi) ); sqrt(FE_var_adjusted)
  
  d2$sei_XR_agg[ d2$name == .name ] = sqrt(FE_var_adjusted)
  # just for comparison, also save the usual FE variance that assumes ||:
  d2$sei_XR_agg_usualFE[ d2$name == .name ] = sqrt(1 / sum(wi) )
  
  # sanity checks:
  if ( FALSE ) {
    
    # c.f. upper bound on variance, where all corrs are 1
    term2 = (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "frustration", "sei_XR"]) +
      (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) +
      (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) + 
      
      (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) +
      (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) + 
      
      (1/temp[check_name == "difficulty", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"])
    
    # should be > FE_var_adjusted
    sqrt( (1 / sum(wi) ) + (1 / sum(wi) )^2 * 2 * term2 )
    
    
    # reproduce FE meta-analysis assuming independence:
    rma.uni(yi = temp$yi_XR,
            vi = temp$sei_XR^2,
            method = "FE")
    sqrt((1 / sum(wi) ))  # will equal the SE from metafor
  }
}

# sanity check
View(d2 %>% arrange(name))


# remove unnecessary rows
# filtering on fatigue because for the main analysis using yi_XR_agg, we only need 1 row per study anyway,
#  and specifically we'll filter on fatigue for the fatigue-only analysis
d3 = d2 %>% filter( check_name == "fatigue" ) %>%
  select(name, yi_XR, sei_XR, yi_XR_agg, sei_XR_agg, sei_XR_agg_usualFE)


d3 = d3 %>% rename(yi_XR_fatigue = yi_XR,
                   sei_XR_fatigue = sei_XR)


# CALCULATE IV ESTIMATOR -------------------------------------------------

# merge with X-Y effect data
d = d1 %>% left_join(y = d3,
                     by = "name")

expect_equal(nrow(d), 23)

d$vi_XR_agg = d$sei_XR_agg^2

# calculate IV Wald estimator (composite manipulation check)
d = d %>% rowwise() %>%
  mutate( IV_est = yi_XY / yi_XR_agg,
          IV_var = ratio_var( num_est = yi_XY,
                              denom_est = yi_XR_agg,
                              num_var = vi_XY,
                              denom_var = sei_XR_agg^2) )


# calculate IV Wald estimator (fatigue only)
d = d %>% rowwise() %>%
mutate( IV_est_fatigue = yi_XY / yi_XR_fatigue,
        IV_var_fatigue = ratio_var( num_est = yi_XY,
                                    denom_est = yi_XR_fatigue,
                                    num_var = vi_XY,
                                    denom_var = sei_XR_fatigue^2) )

# sanity check: 
# reproduce their reported effect of X on fatigue: 0.09 [-0.03, 0.20] (pg 555)
# not using KNHA here to match their code
# yes :)
rma.uni( yi = yi_XR_fatigue,
         vi = sei_XR_fatigue^2,
         method = "REML",
         data = d,
         knha = FALSE)

# SAVE DATASET -------------------------------------------------

setwd(data.dir)
setwd("Hagger")

fwrite(d, "prepped_hagger_data_ACSB.csv")


