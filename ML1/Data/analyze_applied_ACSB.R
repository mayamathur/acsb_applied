
# ML1 data

# PRELIMINARIES ---------------------------------------------------------------

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("dplyr",
            "data.table",
            "purrr",
            "tidyr",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here",
            "metafor",
            "sandwich",
            "EValue",
            "xtable")

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


# get helper fns
setwd(code.dir)
source("helper_applied_ACSB.R")

# no sci notation
options(scipen=999)



# LOAD DATA ---------------------------------------------------------------

# Study 1 dataset
# just doing complete-case analysis here for simplicity
setwd(data.dir)
d = read.csv("prepped_merged_data.csv")

# I think no one is missing baseline demographics because we required answers:
any(is.na(d$hisp))
any(is.na(d$sex))
# so conducting CC analyses with covariates vs. without shouldn't cause more observations to be dropped



# EXPLORE ML1 DATA ---------------------------------------------------------------

# https://osf.io/wx7ck/

setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/2024/ACSB (attention checks and selection bias)/Linked to OSF (ACSB)/Applied examples/ML1/Data")

library(haven)
d = read_sav('Full_Dataset_De-Identified.sav')

d = read_sav('CleanedDataset.sav')

