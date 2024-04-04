
# Hagger's repo:  https://osf.io/jymhe/

# Manipulation check datasets come from Supplementary Analyses / Data and results files for analysis including all 24 laboratories

# PRELIMINARIES ---------------------------------------------------------------

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
d = fread("RTV.csv")

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
        data = d,
        method = "REML",
        knha = FALSE )
# yes, agrees :)

# slightly wider with KNHA, but not that much so
rma.uni(yi = `Hedges's g`,
        sei = `Std Err g`,
        data = d,
        method = "REML",
        knha = TRUE )






names(d1) = c("yi_XY", "vi_XY")

d1 = d1 %>% add_column(.before = 1, name = name)



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
  
  # make a short ID string comparable to that in dataset d
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

# one more name than dataset d because Tinghog isn't in Fig 1 forest plot
length(unique(d2$name))


# ~ Make composite scale of the manipulation checks  -------------------------------------------------


d2$yi_XR_agg = NA
d2$sei_XR_agg = NA

for ( .name in d2$name ){
  mod = rma.uni(yi = yi_XR,
                sei = sei_XR,
                data = d2 %>% filter(name == .name),
                method = "FE")
  
  d2$yi_XR_agg[ d2$name == .name ] = mod$b
  d2$sei_XR_agg[ d2$name == .name ] = mod$se
}


# remove unnecessary rows
d3 = d2 %>% filter( !duplicated(name) ) %>%
  select(name, yi_XR_agg, sei_XR_agg)





# ~ Merge with X-Y effect data  -------------------------------------------------

# merge with X-Y effect data
d = d1 %>% left_join(y = d3,
                     by = "name")

d$vi_XR_effort = d$sei_XR_effort^2


# ~ Calculate IV Wald estimator  -------------------------------------------------

d = d %>% rowwise() %>%
  mutate( IV_est = yi_XY / yi_XR_agg,
          IV_var = ratio_var( num_est = yi_XY,
                              denom_est = yi_XR_agg,
                              num_var = vi_XY,
                              denom_var = sei_XR_agg^2) )


# meta-analyze the IV estimates
rma.uni(yi = IV_est,
        vi = IV_var,
        method = "REML",
        data = d,
        knha = TRUE)

# wow, super similar to original estimate!! very interesting

mean(d$yi_XY)
mean(d$IV_est)


# think about: is it okay to use standardized effects here? 
# A: Yes. since numerator is effect of X on standardized-Y and denom is effect of X on standardized-R,
# resulting ATE is effect of 1-SD increase in R on standardized-Y

#bm: check all of this! also look into the disagreement between my replication of their meta-analysis. 
#  Is it just rounding error?



