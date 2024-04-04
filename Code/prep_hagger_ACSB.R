
# Hagger's repo:  https://osf.io/jymhe/

# Manipulation check datasets come from Supplementary Analyses / Data and results files for analysis including all 24 laboratories



# think about: is it okay to use standardized effects here? 
# A: Yes. since numerator is effect of X on standardized-Y and denom is effect of X on standardized-R,
# resulting ATE is effect of 1-SD increase in R on standardized-Y




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

# TRY TO REPRODUCE THEIR ANALYSIS  -------------------------------------------------

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


d1 = d1 %>% rename( yi_XY = `Hedges's g`,
                    vi_XY = `Std Err g` )


# make a short ID string comparable to that in dataset d
d1$name = sapply(strsplit(d1$`Study name`, "\\s+|,"),
                   function(x) tolower(gsub(",", "", x[1])))

d1 = d1 %>% select(name, yi_XY, vi_XY)


# ALL MANIPULATION CHECKS ---------------------------------------------------------------

# ~ Make long dataset of all the manipulation checks  -------------------------------------------------

setwd(data.dir)
setwd("Hagger/Manipulation check items")


# the manuscript seems to describe only these as manipulation checks (pg 550),
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

# one more name than dataset d1 because Tinghog isn't in Fig 1 forest plot
length(unique(d2$name))


# ~ Make composite scale of the 4 manipulation checks  -------------------------------------------------

# get IPD data to estimate correlations between the manipulation checks
# will use this as a proxy for correlation between the XR effects on each of these
setwd(data.dir)
setwd("Hagger/Manipulation check items")
ipd = fread("RRR-MergedSubjectData.csv")

ipd = ipd %>% select(Effort, Difficulty, Tiredness, Frustration)
( cormat = cor(ipd, use = "pairwise.complete.obs") )

# unique correlations only
pairwise_corrs = unique( as.vector(cormat) )
pairwise_corrs = pairwise_corrs[ !pairwise_corrs == 1 ]
expect_equal( length(pairwise_corrs), 6 )  # 6 = (4 variables choose 2)

# make composite variable using FE meta-analysis, but 
#  adjusting the usual inference to account for the
#  (approximate) correlations between the measures
d2$yi_XR_agg = NA
d2$sei_XR_agg = NA
d2$sei_XR_agg_usualFE = NA

for ( .name in d2$name ){
  temp = d2 %>% filter(name == .name)
  
  # FE estimate by hand
  wi = 1 / temp$sei_XR^2
  ( FE_est = (1 / sum(wi) ) * sum( wi * temp$yi_XR ) )
  
  d2$yi_XR_agg[ d2$name == .name ] = FE_est
  
  # term inside second summation, which would be 0 if no correlations:
  # sum_{i<j}{ Corr(thetahat_i, thetahat_j) * 1/SE_i * 1/SE_j }
  # corr = covariance because R is standardized
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
  # just for comparison:
  d2$sei_XR_agg_usualFE[ d2$name == .name ] = sqrt(1 / sum(wi) )
  
  # sanity checks:
  if ( FALSE ) {
    
    # c.f. upper bound where all corrs are 1
    term2 = (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "frustration", "sei_XR"]) +
      (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) +
      (1/temp[check_name == "effort", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) + 
      
      (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "difficulty", "sei_XR"]) +
      (1/temp[check_name == "frustration", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"]) + 
      
      (1/temp[check_name == "difficulty", "sei_XR"]) * (1/temp[check_name == "fatigue", "sei_XR"])
    
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
d3 = d2 %>% filter( !duplicated(name) ) %>%
  select(name, yi_XR_agg, sei_XR_agg, sei_XR_agg_usualFE)





# ~ Merge with X-Y effect data  -------------------------------------------------

# merge with X-Y effect data
d = d1 %>% left_join(y = d3,
                     by = "name")

d$vi_XR_agg = d$sei_XR_agg^2


# ~ Calculate IV Wald estimator  -------------------------------------------------

d = d %>% rowwise() %>%
  mutate( IV_est = yi_XY / yi_XR_agg,
          IV_var = ratio_var( num_est = yi_XY,
                              denom_est = yi_XR_agg,
                              num_var = vi_XY,
                              denom_var = sei_XR_agg^2) )

# meta-analyze the IV estimates
mod = rma.uni(yi = IV_est,
        vi = IV_var,
        method = "REML",
        data = d,
        knha = TRUE)

# wow, super similar to original estimate!! very interesting
mean(d$yi_XY)
mean(d$IV_est)


# ~ What if we assume fatigue (the one that didn't change) is the only one that matters? -------------------------------------------------

# maximally charitable toward Baumeister's critique

temp = d %>% left_join(d2 %>% filter(check_name == "fatigue"),
                       by = "name")
  
temp = temp %>% rowwise() %>%
  mutate( IV_est = yi_XY / yi_XR,
          IV_var = ratio_var( num_est = yi_XY,
                              denom_est = yi_XR,
                              num_var = vi_XY,
                              denom_var = sei_XR^2) )

# sanity check: should reproduce their reported 0.09 [-0.03, 0.20] 
# for effect of X on fatigue (pg 555)
# since I'm not using KNHA
# yes :)
rma.uni( yi = yi_XR,
         vi = sei_XR^2,
         method = "REML",
         data = temp,
         knha = FALSE)

# meta-analyze the IV estimates
( modf = rma.uni(yi = IV_est,
              vi = IV_var,
              method = "REML",
              data = temp,
              knha = TRUE) )

# c.f. simple mean - very different!
# however, this makes sense given the plots below, since the 
mean(temp$IV_est)


# ~ Plots (sanity checks)  -------------------------------------------------

# ITT estimate vs. various IV estimates

# aggregated IV measure
# some increase as a result of IV estimate, but others decrease (become more negative)
ggplot( data = d, 
        aes(x = yi_XY,
            y = yi_XY / yi_XR_agg,
            size = 1/vi_XR_agg) ) +
  geom_abline(intercept = 0, slope = 1) +
  
  # line for IV estimate = 0
  geom_hline(yintercept = 0, lty = 2) +
  geom_point() 

mean(d$yi_XY / d$yi_XR_agg)


# only fatigue (the one with weakest XR relationship)
temp = d %>% left_join(d2, by = "name") %>%
  filter(check_name == "fatigue")

ggplot( data = temp, 
        aes(x = yi_XY,
            y = yi_XY / yi_XR,
            size = 1/sei_XR^2) ) +
  # line for IV estimate = 0
  geom_hline(yintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() 

mean(temp$yi_XY / temp$yi_XR)

