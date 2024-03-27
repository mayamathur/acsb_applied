
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
            "haven",
            "xtable",
            "tableone",
            "effsize",
            "broom")

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


# GET ML1 DATA ---------------------------------------------------------------

# ML1 repo:
# https://osf.io/wx7ck/

setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/2024/ACSB (attention checks and selection bias)/Linked to OSF (ACSB)/Applied examples/ML1/Data")

library(haven)
#d = read_sav('Full_Dataset_De-Identified.sav')
d = read_sav('CleanedDataset.sav')


# per codebook, this is passing the check:
d$R = as.numeric( d$omdimc3 == 0 )
sum(is.na(d$R))
mean(d$R, na.rm = TRUE) # 79% overall

# sanity check: attention check success by site
# prop: what proportion of the entire sample is this site?
t = d %>% group_by(referrer) %>%
  summarise(mean = mean(R),
            n = n(),
            prop = n()/nrow(d)) %>%
  arrange(mean)
data.frame(t)

# demographics
demo_vars = c("referrer",  # site
              "age",
              "sex",
              "us_or_international",
              "lab_or_online") 
# also has citizenship, but too many levels

# entire dataset
CreateTableOne(data = d, vars = demo_vars)




# # ALLOWED/FORBIDDEN (RUGG) ---------------------------------------------------------------
# # after fixing the coding error, basically same results for attentive vs. not
# 
# 
# # exclude people who are NA
# mean(is.na(d$allowedforbiddenGroup) | is.na(d$allowedforbidden))
# d = d %>% filter( !is.na(allowedforbiddenGroup) & !is.na(allowedforbidden) )
# 
# 
# 

# # coding: 1 = YES; 0 = NO
# # does this need to be recoded since some 
# d$allowedforbidden
# 
# # "Do you think the United States should forbid public speeches against democracy?"
# d$allowedforbiddena
# 
# # "Do you think the United States should allow public speeches against democracy?"
# d$allowedforbiddenb
# 
# # recode
# # X=1 := "allowed" condition
# d$X = d$allowedforbiddenGroup == 1
# d$X = as.numeric(d$X)
# 
# # Y=1 := thinking speeches against democracy should NOT be allowed
# d$Y = NA
# # d$allowedforbidden == 1 is responding "yes"
# d$Y[ d$X == 1 ] = (d$allowedforbidden[d$X == 1 ] == 0)
# d$Y[ d$X == 0 ] = (d$allowedforbidden[ d$X == 0 ] == 1)
# d$Y = as.numeric(d$Y)
# 
# d %>% group_by(X) %>%
#   summarise(mean(Y))
# 
# # did everyone do the allowed/forbidden experiment?
# sum(is.na(d$allowedforbidden))
# 
# 
# 
# # try to replicate their SMD
# # **important! The SMD = 1.96 in their original paper is wrong; the correct one is 
# #  in this forest plot: https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000373
# cohen.d(d$Y[d$X==1], d$Y[d$X==0])
# 
# 
# # ****exclude MTurk since they were highly attentive
# d2 = d %>% filter( referrer != "mturk")
# nrow(d2)
# 
# 
# m = lm(Y ~ X*R, data = d2)
# tidy(m)
# summary(m)
# 
# 
# # without covariates
# summary( lm(Y ~ X, data = d2 ) )
# summary( lm(Y ~ X, data = d2 %>% filter(R == 1) ) )
# 
# # with covariates
# ( string = paste( "Y ~ X +", paste(demo_vars, collapse = " + ") ) )
# 
# summary( lm( eval( parse(text = "string") ), data = d2 ) )
# summary( lm( eval( parse(text = "string") ), data = d2 %>% filter(R == 1) ) )
# 
# # will need HC0 standard errors
# 
# 

# ANCHORING - NYC ---------------------------------------------------------------


# **not sure why so many people are NA
table(is.na(d$anchoring1a) & is.na(d$anchoring1b) )
table(is.na(d$anchoring1))
#table(is.na(d$anchoring1a) & is.na(d$anchoring1akm) & is.na(d$anchoring1b) & is.na(d$anchoring1bkm))

# exclude people who didn't do this study
# and people missing attention check measure
d = d %>% filter( !is.na(anch1group) & !is.na(d$anchoring1) & !is.na(R) )


# recode
# X=1 := high anchor
d$X = d$anch1group
d$X = as.numeric(d$X)

# Y=1 := thinking speeches against democracy should NOT be allowed
d$Y = d$anchoring1

d %>% group_by(X) %>%
  summarise(mean(Y))


# try to replicate their SMD
# reported: 1.17 [1.09, 1.25]
cohen.d(d$Y[d$X==1], d$Y[d$X==0])

# among attentive: SMALLER effect
cohen.d(d$Y[d$X==1 & d$R==1], d$Y[d$X==0 & d$R==1] )

m = lm(Y ~ X*R, data = d)
tidy(m)
summary(m)


# ****exclude MTurk since they were highly attentive
#d2 = d
d2 = d %>% filter( referrer != "mturk")
nrow(d2)


# without covariates
summary( lm(Y ~ X, data = d2 ) )
summary( lm(Y ~ X, data = d2 %>% filter(R == 1) ) )

# with covariates
( string = paste( "Y ~ X +", paste(demo_vars, collapse = " + ") ) )

summary( lm( eval( parse(text = "string") ), data = d2 ) )
summary( lm( eval( parse(text = "string") ), data = d2 %>% filter(R == 1) ) )


# hmmm...not a very big difference


### what if we subset to the site with lowest attention (52%)?
cohen.d(d$Y[d$X==1 & d$referrer == "osu"], d$Y[d$X==0 & d$referrer == "osu"])

# among attentive: SMALLER effect
cohen.d(d$Y[d$X==1 & d$R==1 & d$referrer == "osu"], d$Y[d$X==0 & d$R==1 & d$referrer == "osu"] )

# without covariates
summary( lm(Y ~ X, data = d2 %>% filter(referrer == "osu") ) )
summary( lm(Y ~ X, data = d2 %>% filter(referrer == "osu" & R == 1) ) )

# with covariates
# remove referrer from list of demographics
string = "Y ~ X + age + sex"

summary( lm( eval( parse(text = "string") ), data = d2 %>% filter(referrer == "osu") ) )
summary( lm( eval( parse(text = "string") ), data = d2 %>% filter(referrer == "osu" & R == 1) ) )


