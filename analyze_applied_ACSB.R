

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
            "EValue")

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

# ~ Lists of variables  -------------------------------------------------

# global var for helper fns
study = 1

# from EatingVeg helper_analysis.R
meats <<- c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds <<- c("dairy", "eggs")
decoy <<- c("refined", "beverages")
goodPlant <<- c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods <<- c(meats, animProds, decoy, goodPlant)

foodVars <<- c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
                names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

# exploratory psych variables
psychY <<- c("importHealth",
             "importEnviro",
             "importAnimals",
             "activ",
             "spec",
             "dom")

# secondary food outcomes
secFoodY <<- c("totalMeat",
               "totalAnimProd",
               meats,
               animProds,
               "totalGood")

# raw demographics, prior to collapsing categories for effect modification analyses
demo.raw <<- c("sex",
               "age",
               "educ",
               "cauc",
               "hisp",
               "black",
               "midEast",
               "pacIsl",
               "natAm",
               "SAsian",
               "EAsian",
               "SEAsian",
               "party",
               "pDem")




# ANALYZE - RETAINING INATTENTIVE SUBJECTS ---------------------------------------------------------------

# reported in Supplementary Table S1 for mainY:
# raw difference -0.37 oz per week [-6.38, 5.64]
# very similar to primary multiple imputation analysis

# ### just mainY
# # reproduce analysis in paper - done :)
# my_ttest(yName = "mainY", dat = d)
# 
# # now control for baseline demographics
# ( string = paste( c("mainY ~ treat", demo.raw), collapse = " + ") )
# ols = lm( eval( parse(text = string) ), data = d )
# summary(ols)
# 
# my_ols_hc0(dat = d, coefName = "treat", yName = "mainY", ols = ols )



### run all outcomes

# reproduce results in manuscript (don't drop inattentives; don't control covars)
x = analyze_all_outcomes(missMethod = "CC")
View(x$res.nice)

# control for demographics, but still don't drop inattentives
x = analyze_all_outcomes(missMethod = "CC",
                         control_covars = TRUE)
View(x$res.nice)





# ANALYZE - DROPPING INATTENTIVE SUBJECTS ---------------------------------------------------------------

# make new attention check variable
# for the present analysis, "attentive" depends on which treatment group you are in
d$passCheck = 0
d$passCheck[ d$treat == 1 & d$videoContent == "The ways we raise animals for human consumption causes the animals to suffer."] = 1
d$passCheck[ d$treat == 0 ] = 1  # ***doing this because no one checked zero answers, which would have been correct for the control group
mean(d$passCheck) # 87% by this definition


# drop inattentives; don't control covariates
x1 = analyze_all_outcomes(missMethod = "CC", drop_inattentives = TRUE)
View(x1$res.nice)


# drop inattentives; control covariates
x2 = analyze_all_outcomes(missMethod = "CC", drop_inattentives = TRUE, control_covars = TRUE)
View(x2$res.nice)



# ### what if we only control gender?
# # similar to controlling all covariates
# demo.raw = "sex"
# x2 = analyze_all_outcomes(missMethod = "CC", drop_inattentives = TRUE, control_covars = TRUE)
# View(x2$res.nice)




# PREDICTORS OF ATTENTION ---------------------------------------------------------------

# X ~ C
# fit model to treatment group only since control group were all set to attention = 1
# as in previous studies, females and older people were more attentive
( string = paste( c("passCheck ~ 1", demo.raw), collapse = " + " ) )
m = lm( eval( parse(text = string) ), data = d %>% filter(treat == 1) )
summary(m)

# Y ~ C
# ** great example because male also strongly predicts meat consumption
( string = paste( c("mainY ~ 1", demo.raw), collapse = " + " ) )
m = lm( eval( parse(text = string) ), data = d %>% filter(treat == 1) )
summary(m)



# SENSIIVITY ANALYSES ---------------------------------------------------------------

# need to dichotomize Y for this
# above vs. below the control group median
d$mainYbin = d$mainY > median( d$mainY[d$treat == 0], na.rm = TRUE )
d %>% group_by(treat) %>% summarise(mean(mainYbin, na.rm = TRUE) )


# ~ M-value -------------------------------------------------

# attentives only
da = d %>% filter(passCheck == TRUE)

# single outcome
( string = paste( c("mainYbin ~ treat", demo.raw), collapse = " + ") )
ols = lm( eval( parse(text = string) ), data = d )
summary(ols)

#m = my_ols_hc0(dat = d, coefName = "treat", yName = "mainYbin", ols = ols )

( n1.retained = sum(da$treat == 1) )
( n0.retained = sum(da$treat == 0) )
( pr = mean(d$passCheck) )


#bm: set this up for a single outcome to test it out 
# :)
# P(Y=1 | A=1, R=1)
( p1 = ols$coefficients[ "(Intercept)" ] + ols$coefficients[ "treat" ] )

# P(Y=1 | A=0, R=1)
( p0 = ols$coefficients[ "(Intercept)" ] )


# from NMAR:
# get E-value for each of several values of the sens parameter RD_0
rd_0_vec = c(0, -0.10)

#bm: use the lambda thing in eq (2) instead
# you got this! :)

for ( .rd_0 in rd_0_vec ) {
  
  alpha = get_alpha(pr = pr,
                    rd_0 = .rd_0,
                    true = 0)
  
  evalue0 = evalues.RD( n11 = n1.retained*p1,
                        n10 = n1.retained*(1-p1),
                        n01 = n0.retained*p0,
                        n00 = n0.retained*(1-p0),
                        #*note that equivalence arises when we set true = alpha:
                        true = alpha )
  
  update_result_csv( name = paste( analysis, " Evalue est rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$est.Evalue, 2) )
  
  update_result_csv( name = paste( analysis, " Evalue lo rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$lower.Evalue, 2) )
  
}


# ~ Louisa's expression -------------------------------------------------






