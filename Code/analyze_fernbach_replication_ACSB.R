
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
            "broom.mixed",
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
data.dir = here("Data/Fernbach replication")
# check that it's set correctly
setwd(data.dir)

results.dir = here("Results/Fernbach replication") 
# check that it's set correctly
setwd(results.dir)


overleaf.dir = "/Users/mmathur/Dropbox/Apps/Overleaf/Attention checks selection bias (ACSB) Overleaf/R_objects/fernbach_stats"
setwd(overleaf.dir)


# get helper fns
setwd(code.dir)
source("helper_applied_ACSB.R")

# no sci notation
options(scipen=999)



# LOAD DATA ---------------------------------------------------------------

setwd(data.dir)
d = read.csv("fernbach_prepped_wide.csv")
dl = read.csv("fernbach_prepped_long.csv")


# ~ Lists of variables  -------------------------------------------------

demo_vars_raw = c("female",
                  "age",
                  "educ", 
                  "party")

interest_vars_raw = c("interest_politics",
                      "interest_voted_2020",
                      "interest_news")


demo_vars_analysis = c("female",
                       "age10y",
                       "educ2", 
                       "party")

interest_vars_analysis = c("interest_politics2",
                           "interest_voted_2020_2",
                           "interest_news2") 

R_vars = c("R_one", "R_all")




# SIX ANALYSES: [EXCLUSION STRATEGY [3 levels]] X [CONTROL COVARIATES] ---------------------------------------------------------------

# ~ Tables for paper -------------------------------------------------

for ( .drop in c("no", "passed_one", "passed_all") ) {
  for ( .covars in c(FALSE, TRUE) ) {
    
    cat( paste("\n\nStarting this analysis: .drop =", .drop, "; .covars =", .covars) )
    
    
    x = analyze_all_outcomes_fernbach(drop_inattentives = .drop,
                                      control_covars = .covars,
                                      .dl = dl)
    
    # save the raw version for sanity checks
    string = paste("raw_ests_drop=", .drop, "_covars=", .covars, ".csv", sep = "" )
    setwd(results.dir)
    fwrite(x$res.raw, string)
    
    
    x2 = x$res.nice %>%
      select(outcome,
             est,
             pval) 
    
    # add info about analysis
    if ( .drop == "no" ) string1 = "No exclusion"
    if ( .drop == "passed_one" ) string1 = "Excluding inattentive (passed one)"
    if ( .drop == "passed_all" ) string1 = "Excluding inattentive (passed all)"
    
    if (.covars == FALSE) string2 = "unadjusted"
    if (.covars == TRUE) string2 = "adjusted"
    
    string = paste( string1, string2, sep = "; ")

    cat( paste( "\n\n******* Analysis:", string ) )
    cat("Analyzed n = ", x$res.nice$nobs[1])
    
    # save the sample size separately
    update_result_csv(name = paste( "Applied analyzed n", string ),
                      value = x$res.nice$nobs[1],
                      .results.dir = results.dir,
                      .overleaf.dir = overleaf.dir)
    
    x2 = x2 %>% add_row(.before = 1,
                        outcome = string )
    
    # add spacer row for prettiness
    x2 = x2 %>% add_row(.after = 4)
    
    # first iteration combo only
    if (.drop == "no" & .covars == FALSE) rs = x2 else rs = bind_rows(rs, x2)
    
  }
}



print( xtable(rs),
       include.rownames = FALSE )


# xtable of results, for pasting into manuscript
setwd(results.dir)

write.table( print( xtable( rs,
                            include.rownames = FALSE ) ),
             file = "xtable_estimates.txt"
)

### Sanity checks (also used in sensitivity analyses)
# simple exclusion
# drop inattentives (passed one); don't control covariates
x_passed_one_unadj = analyze_all_outcomes_fernbach(drop_inattentives = "passed_one",
                                                 control_covars = FALSE,
                                                 .dl = dl)
View(x_passed_one_unadj$res.nice)


# drop inattentives (passed one); control covariates
x_passed_one_adj = analyze_all_outcomes_fernbach(drop_inattentives = "passed_one",
                                                 control_covars = TRUE,
                                                 .dl = dl)
View(x_passed_one_adj$res.nice)


# drop inattentives (passed all); control covariates
x_passed_all_adj = analyze_all_outcomes_fernbach(drop_inattentives = "passed_all",
                                                 control_covars = TRUE,
                                                 .dl = dl)
View(x_passed_all_adj$res.nice)



# ~ One-off stats for paper  -------------------------------------------------

# sensitivity analysis on raw mean difference scale

# first for R_all exclusion criterion
pR = mean(d$R_all)

maxRU = .3

temp = x_passed_all_adj$res.raw %>% filter(coef_name == "condition_expl:time_postTRUE")

update_result_csv(name = paste( "R_all - maxUY to explain away", temp$outcome ),
                  value = round( abs(temp$est)/(2 * (1-pR) * maxRU), 2),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)



# now for R_one exclusion criterion
pR = mean(d$R_one)

maxRU = .3

temp = x_passed_all_adj$res.raw %>% filter(coef_name == "condition_expl:time_postTRUE")

update_result_csv(name = paste( "R_one - maxUY to explain away", temp$outcome ),
                  value = round( abs(temp$est)/(2 * (1-pR) * maxRU), 2),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

# note that the difference between the R_all and R_one sensitivity analyses is entirely about pR 



# ~ One-off stats for paper  -------------------------------------------------

# number inattentive
update_result_csv(name = paste( "Applied n inattentive (R_all)" ),
                  value = sum(d$R_all == 0),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

# percent inattentive
update_result_csv(name = paste( "Perc n inattentive (R_all)" ),
                  value = 100*mean(d$R_all == 0, na.rm = TRUE),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

# number inattentive
update_result_csv(name = paste( "Applied n inattentive (R_one)" ),
                  value = sum(d$R_one == 0),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

# percent inattentive
update_result_csv(name = paste( "Perc n inattentive (R_one)" ),
                  value = 100*mean(d$R_one == 0, na.rm = TRUE),
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)





# PREDICTORS OF R AND OF Y ---------------------------------------------------------------

# ~ R_all ~ C -------------------------------------------------

( string = paste( c("R_all ~ 1", demo_vars_analysis, interest_vars_analysis), collapse = " + " ) )
m = glm( eval( parse(text = string) ),
         data = d,
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
                   print = FALSE)


update_result_csv( name = paste("OR hi for attentiveness ", x$term, sep = ""),
                   value = CIs$hi,
                   .results.dir = results.dir,
                   .overleaf.dir = overleaf.dir,
                   print = FALSE)




# ~ Extremity ~ C -------------------------------------------------

string = paste( c("extr ~ (1|id)", demo_vars_analysis, interest_vars_analysis),
                collapse = " + ")

m = lmer( eval(parse(text = string)), data = dl)

# check for singular fit
messages = attr(m, "optinfo")$conv$lme4$messages

x2 = tidy(m, conf.int = TRUE)

# remove the random effect estimatse
x2 = x2 %>% filter( !(term %in% c("sd__(Intercept)", "sd__Observation")) )

# for a merged table
col2 = stat_CI( round(x2$estimate, 2), round(x2$conf.low, 2), round(x2$conf.high, 2) )




# ~ Merged table: covariate associations with both R and Y  -------------------------------------------------

# sanity check before binding 
expect_equal(x$term, x2$term)

rs2 = dplyr::bind_cols(x$term, col1, col2)


names(rs2) = c("Covariate", "Association with attentiveness (OR)", "Association with position extremity (mean diff.)")

print( xtable(rs2),
       include.rownames = FALSE )

# xtable of results, for pasting into manuscript
setwd(results.dir)

write.table( print( xtable( rs2,
                            include.rownames = FALSE ) ),
             file = "xtable_predictors_attentiveness.txt"
)

