
#bm: just re-ran this. does overleaf update?

# Can use wr() to wipe results file and vr() to view it

# PRELIMINARIES ---------------------------------------------------------------

# rm(list=ls())

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
            "xtable",
            "tidymodels")

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
# setwd(here())
# renv::snapshot()


# set working directories
code.dir = here("Code")
# check that it's set correctly
setwd(code.dir)

# if you need to go up a level in parent directory
data.dir = here("Data/Mathur documentary")
# check that it's set correctly
setwd(data.dir)

results.dir = here("Results/Mathur documentary") 
# check that it's set correctly
setwd(results.dir)


overleaf.dir = "/Users/mmathur/Dropbox/Apps/Overleaf/Attention checks selection bias (ACSB) Overleaf/R_objects/meat_stats"
setwd(overleaf.dir)


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

# complete-case dataset
# attrition is the only reason for missing data
d = d %>% filter(!is.na(mainY))
expect_equal( nrow(d), 574 )

# I think no one is missing baseline demographics because we required answers:
any(is.na(d$hisp))
any(is.na(d$sex))
# so conducting CC analyses with covariates vs. without shouldn't cause more observations to be dropped

# rescale or recode for interpretability
d$age10y = d$age/10
d$pDem10 = d$pDem/.10
d$female = d$sex == "a.Female"

# ~ Lists of variables  -------------------------------------------------

# global var for helper fns
study = 1

# from EatingVeg helper_analysis.R
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
                names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

# exploratory psych variables
psychY = c("importHealth",
             "importEnviro",
             "importAnimals",
             "activ",
             "spec",
             "dom")

# secondary food outcomes
secFoodY = c("totalMeat",
               "totalAnimProd",
               meats,
               animProds,
               "totalGood")


demo.raw = c("female",
               "age10y",
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
               "pDem10")





# FOUR ANALYSES: [RETAIN INATTENTIVE] X [CONTROL COVARIATES] ---------------------------------------------------------------


# ~ Retaining inattentive subjects  -------------------------------------------------

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

# reproduce results in manuscript (retain all participants; don't control covars)
x1 = analyze_all_outcomes_meat(missMethod = "CC",
                          control_covars = FALSE)
View(x1$res.nice)

# control for demographics, but still retain all participants
x2 = analyze_all_outcomes_meat(missMethod = "CC",
                          control_covars = TRUE)
View(x2$res.nice)


# ~ Dropping inattentive subjects  -------------------------------------------------

# make new attention check variable
# for the present analysis, "attentive" depends on which treatment group you are in
d$passCheck = 0
d$passCheck[ d$treat == 1 & d$videoContent == "The ways we raise animals for human consumption causes the animals to suffer."] = 1
d$passCheck[ d$treat == 0 ] = 1  # control group is attentive by definition
mean(d$passCheck) # 87% by this definition

# ~ Tables for paper -------------------------------------------------

# three outcomes to display in paper
keepers = c("mainY CC", "totalMeat CC", "totalAnimProd CC")


for ( .drop in c(FALSE, TRUE) ) {
  for ( .covars in c(FALSE, TRUE) ) {
    
    
    x = analyze_all_outcomes_meat(missMethod = "CC",
                             drop_inattentives = .drop,
                             control_covars = .covars)
    
    
    
    x2 = x$res.nice %>%
      filter(analysis %in% keepers) %>%
      select(analysis,
             est,
             g.est,
             pval) 
    
    # add info about analysis
    if ( .drop == FALSE & .covars == FALSE ) string = "No exclusion; unadjusted"
    if ( .drop == FALSE & .covars == TRUE ) string = "No exclusion; adjusted"
    if ( .drop == TRUE & .covars == FALSE ) string = "Excluding inattentive; unadjusted"
    if ( .drop == TRUE & .covars == TRUE ) string = "Excluding inattentive; adjusted"
    
    cat( paste( "\n\n******* Analysis:", string ) )
    cat("Analyzed n = ", x$res.nice$nobs[1])
    
    # save the sample size separately
    update_result_csv(name = paste( "Applied analyzed n", string ),
                      value = x$res.nice$nobs[1],
                      .results.dir = results.dir,
                      .overleaf.dir = overleaf.dir)
    
    x2 = x2 %>% add_row(.before = 1,
                        analysis = string )
    
    # add spacer row for prettiness
    x2 = x2 %>% add_row(.after = 4)
    
    if (.drop == FALSE & .covars == FALSE) rs = x2 else rs = bind_rows(rs, x2)
    
  }
}


# prettify
rs[rs == "mainY CC"] = "Meat and animal products"
rs[rs == "totalMeat CC"] = "Meat"
rs[rs == "totalAnimProd CC"] = "Animal products"


rs

print( xtable(rs),
       include.rownames = FALSE )


# xtable of results, for pasting into manuscript
setwd(results.dir)

write.table( print( xtable( rs,
                            include.rownames = FALSE ) ),
             file = "xtable_ate_estimates.txt"
)

### Sanity checks
# simple exclusion
# drop inattentives; don't control covariates
x3 = analyze_all_outcomes_meat(missMethod = "CC",
                          drop_inattentives = TRUE)
View(x3$res.nice)


# drop inattentives; control covariates
x4 = analyze_all_outcomes_meat(missMethod = "CC",
                          drop_inattentives = TRUE,
                          control_covars = TRUE)
View(x4$res.nice)



# ~ One-off stats for paper  -------------------------------------------------

# sensitivity analysis on raw mean difference scale
# note that estimate for AP consumption was already in the "wrong" direction (positive) 
x5 = x4$res.raw %>% filter(analysis %in% keepers) %>% select(analysis, est)

pR = mean(d$passCheck)

maxRU = .3

update_result_csv(name = paste( "maxUY to explain away", x5$analysis ),
                  value = round( abs(x5$est)/(2 * (1-pR) * maxRU), 2),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)



# ~ One-off stats for paper  -------------------------------------------------

# number inattentive
update_result_csv(name = paste( "Applied n inattentive" ),
                  value = sum(d$passCheck == 0),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

# number inattentive
update_result_csv(name = paste( "Perc n inattentive" ),
                  value = 100*mean(d$passCheck == 0, na.rm = TRUE),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)





# PREDICTORS OF R AND OF Y ---------------------------------------------------------------

# ~ R ~ C -------------------------------------------------
# fit model to treatment group only since control group were all set to attention = 1
# as in previous studies, females and older people were more attentive
( string = paste( c("passCheck ~ 1", demo.raw), collapse = " + " ) )
m = glm( eval( parse(text = string) ),
         data = d %>% filter(treat == 1),
         family = "binomial"(link = "logit") )
summary(m)

# save results
x = tidy(m)
x$OR = exp(x$estimate)

CIs = as.data.frame( confint(m) )
CIs = exp(CIs)
names(CIs) = c("lo", "hi")

# for a merged table
col1 = stat_CI( round(x$OR, 2), round(CIs$lo, 2), round(CIs$hi, 2) )

update_result_csv( name = paste("OR for attentiveness ", x$term, sep = ""),
                   value = x$OR,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = TRUE)

update_result_csv( name = paste("OR pval for attentiveness ", x$term, sep = ""),
                   value = x$p.value,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = FALSE)


update_result_csv( name = paste("OR lo for attentiveness ", x$term, sep = ""),
                   value = CIs$lo,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = TRUE)


update_result_csv( name = paste("OR hi for attentiveness ", x$term, sep = ""),
                   value = CIs$hi,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = TRUE)




# ~ Y ~ C -------------------------------------------------

# ** great example because male also strongly predicts meat consumption
( string = paste( c("mainY ~ 1", demo.raw), collapse = " + " ) )
m = lm( eval( parse(text = string) ), data = d %>% filter(treat == 1) )
summary(m)

# get HC0 inference for all coefficients
for ( .coefName in names(m$coefficients) ){
  row = my_ols_hc0(coefName = .coefName,
                   ols = m,
                   show_SMD = FALSE)
  print(row)
  if ( .coefName == names(m$coefficients)[1] ) {
    rs = row
  } else {
    rs = bind_rows(rs, row)
  }
}

rs

# for a merged table
col2 = stat_CI( round(rs$est, 2), round(rs$lo, 2), round(rs$hi, 2) )

# one-off stats
update_result_csv( name = paste("Mean diff for mainY ", row.names(rs), sep = ""),
                   value = rs$est,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = FALSE)

update_result_csv( name = paste("Mean diff lo for mainY ", row.names(rs), sep = ""),
                   value = rs$lo,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = FALSE)


update_result_csv( name = paste("Mean diff hi for mainY ", row.names(rs), sep = ""),
                   value = rs$hi,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = FALSE)

# ~ Merged table: covariate associations with both R and Y  -------------------------------------------------

rs2 = dplyr::bind_cols(col1, col2)

rs2 = rs2 %>%
  add_column(.before = 1, row.names(rs))

names(rs2) = c("Covariate", "Association with attentiveness (OR)", "Association with MAP consumption (mean diff.)")

print( xtable(rs2),
       include.rownames = FALSE )

# xtable of results, for pasting into manuscript
setwd(results.dir)

write.table( print( xtable( rs2,
                            include.rownames = FALSE ) ),
             file = "xtable_predictors_attentiveness.txt"
)

