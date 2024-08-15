

# Using Qualtrics test responses


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
            "lme4",
            "lmerTest",
            "rstatix",
            "geesmv",
            "gee",
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



# LOAD DATA ---------------------------------------------------------------

setwd(data.dir)
dr = read.csv("fake_raw_data.csv", header = TRUE)

# remove extra Qualtrics header rows if they are there
if ( dr$StartDate[1] == "Start Date" ) dr = dr[-c(1:2),]
dr = droplevels(dr)

#expect_equal(nrow(dr), 110)



# RECODE VARIABLES  -------------------------------------------------

# init new dataset
d = dr


# recode NAs throughout dataset
d[d==""] = NA

# subject ID
d$id = 1:nrow(d)


# rescale or recode for interpretability

#***only for Qualtrics test data!
d$age = round(runif(n= nrow(d), min = 18, max = 50))
d$age10y = d$age/10

# convert variables to numeric
to_num = c("attention_birth_year")
d = d %>% mutate_at(.vars = to_num, .funs  = as.numeric) 

# ~ Make attention_pass variables  -------------------------------------------------

d$expected_birth_year = 2024 - d$age
d$attention_pass_birth_year = abs(d$attention_birth_year - d$expected_birth_year) <= 1

d$attention_pass_ideology = (d$attention_ideology == "Somewhat Liberal")

#@@check this one; make sure I wrote the string correctly
d$attention_pass_news = (d$attention_news == "The Drudge Report,ABC News website")

# indicator for passing all checks
d$R_all = d$attention_pass_birth_year * d$attention_pass_ideology * d$attention_pass_news

# indicator for passing at least one
d$R_one = ( (d$attention_pass_birth_year + d$attention_pass_ideology + d$attention_pass_news) > 0 )


# ~ Make extremity variables  -------------------------------------------------

# Fernbach, pg 941:
# "We transformed raw ratings of
# positions on policies into a measure of position extremity
# by subtracting the midpoint of the scale (4) and taking
# the absolute value."

### Merge post-position and post-understanding vars

# for each of these, there are separate variables for the two conditions

# name_string = the part of string to search for in variable name
post_pos_vars = c("post_pos_health", "post_pos_ss", "post_pos_nuc",
                  "post_pos_teach", "post_pos_cap", "post_pos_tax")

post_underst_vars = c("post_underst_health", "post_underst_ss", "post_underst_nuc", 
                      "post_underst_teach", "post_underst_cap", "post_underst_tax")


#@@underying fn definitely needs sanity checks!!
for (.varname in post_pos_vars) d = make_merged_var(dat = d, name_string = .varname)

for (.varname in post_underst_vars) d = make_merged_var(dat = d, name_string = .varname)


### Make extremity variables
( pre_pos_vars = names_with(pattern = "pre_pos", dat = d) )

for (.varname in pre_pos_vars) d = make_extremity_var(dat = d, varname = .varname)

for (.varname in post_pos_vars) d = make_extremity_var(dat = d, varname = .varname)


# #***shouldn't need this for mixed model, but think RANOVA might need it?
# ### IMPORTANT: Remove pre-variables for which there is no corresponding post-variable 
# #  since subjects answered only a random subset of questions
# # only doing this for pos variables for now
# d$pre_pos_nuc[ is.na(d$post_pos_nuc) ] = NA
# d$pre_pos_ss[ is.na(d$post_pos_ss) ] = NA
# d$pre_pos_health[ is.na(d$post_pos_health) ] = NA
# d$pre_pos_cap[ is.na(d$post_pos_cap) ] = NA
# d$pre_pos_tax[ is.na(d$post_pos_tax) ] = NA
# d$pre_pos_teach[ is.na(d$post_pos_teach) ] = NA



# RESHAPE WIDE -> LONG  -------------------------------------------------

# all "pos" ratings should be a single variable
#  and there should also be two new vars: pos_issue (string after the _), pos_issue_number (1 or 2), and time ("pre" or "post" string before the first _)

# smaller toy dataset
s = d %>% select( c("id", "condition_expl", names_with("pos_", d) ) )

# https://stackoverflow.com/questions/57533341/reshape-wide-to-long-based-on-part-of-column-name
s2 = s %>%
  gather(key, value, -c(id, condition_expl)) %>%
  separate(key, into = c("time", "var", "issue_name"), sep = "_")

#**Save this reshape code!

# expect 1 row per subject, issue, and time combination
expect_equal( nrow(s2), nrow(d)*6*2 )

# sanity check: look at one participant
# all "pre_pos" variables will be observed (it's a matrix question with all 6 issues),
#   but only 2 of 6 "post_pos" variables will be observed
View(s2 %>% filter(id == 1))

# add issue number variable
# within each 2-issue set, the order is held constant
s2$issue_num = NA
s2$issue_num[s2$issue_name %in% c("health", "nuc", "cap")] = 1
s2$issue_num[s2$issue_name %in% c("ss", "teach", "tax")] = 2
expect_equal( any(is.na(s2$issue_num) ), FALSE )

s2 = s2 %>% rename(rating = value) 
s2$time_post = ( s2$time == "post" )

dl = s2 %>% filter(!is.na(rating)) %>%
  arrange(id)
# this test will FAIL if you used the "IMPORTANT" block above:
expect_equal(nrow(dl), nrow(d)*(6+2)) # they only rate 6 issues (pre) + 2 issues (post)

# need similar datasets for understanding and extremity vars



# PRACTICE ANALYSIS  -------------------------------------------------


# RANOVA with timing (pre/post) and issue number as within-S; condition is between-S; interaction of condition with timing is effect of interest 
# Fixed: condition x timing, issue number 

# with fake data, model doesn't fit due to 0 random effect variance
# F-tests for lmer in R: https://www.rdocumentation.org/packages/lmerTest/versions/3.1-3/topics/ranova

dat = dl

# simulate data because models below don't fit with Qualtrics fake data (b/c no variation in random intercepts)
if ( FALSE ){
  n=100
  condition = rbinom(n = n, size = 1, prob = 0.5)
  # generate subject random intercepts
  # **THIS IS THE KEY. IF SD=0 (I.E., NO RANDOM EFFECTS), THEN LMM IS SINGULAR.
  gamma_wide = rnorm(n = n, sd = .5)
  dat = data.frame( id = rep( c(1:n), each = 4 ),
                    condition_expl = rep(condition, each = 4),
                    gamma = rep(gamma_wide, each = 4),
                    time_post = rep( c(1:2), each = 2),
                    issue_num = rep( c(1:2), 2 ) )
  dat$rating = rnorm(n = nrow(dat), mean = dat$gamma)
  ## END OF SIMULATING DATA
}


# ~ Model 1: LMM :/ -------------------------------------------------
m1 = lmer(rating ~ condition_expl * time_post * issue_num + (1|id), data = dat) 
summary(m1)

# sanity check: OLS
lm(rating ~ condition_expl * time_post * issue_num, data = dat)


# ~ Model 2: ANOVA :(  -------------------------------------------------

# try using rstatix::anova_test
# https://www.r-bloggers.com/2021/04/repeated-measures-of-anova-in-r-complete-tutorial/
# the model I think we actually want
m2 = anova_test(dv = rating,
           wid = c(id),
           within = c(time_post, issue_num),
           between = condition_expl,
           data = dat)
# quite different from LMM due to the "annoying and dangerous" decomposition property of ANOVA



# ~ Model 3: GEE :) -------------------------------------------------

# other reasons to prefer GEE:
# "annoying and dangerous thing about ANOVA" is very relevant due to interactions
# outcome is a 7-point scale, so normality is questionable
m3 = report_gee_table(dat = dat,
                 formulaString = "rating ~ condition_expl * time_post * issue_num",
                 idString = "as.factor(id)",
                 subsetString = NA,  # should we subset the data?
                 analysisVarNames = c("rating", "condition_expl", "time_post", "issue_num"),  # for excluding missing data
                 analysisLabel = "myAnalysis",  # will become an identifer column in dataset
                 corstr = "exchangeable",
                 se.type = "mancl",  # "model" or "mancl"
                 
                 return.gee.model = TRUE,
                 write.dir = NA)


View(m3$res)
# makes perfect sense




# # FOUR ANALYSES: [RETAIN INATTENTIVE] X [CONTROL COVARIATES] ---------------------------------------------------------------
# 
# 
# # ~ Retaining inattentive subjects  -------------------------------------------------
# 
# # reported in Supplementary Table S1 for mainY:
# # raw difference -0.37 oz per week [-6.38, 5.64]
# # very similar to primary multiple imputation analysis
# 
# # ### just mainY
# # # reproduce analysis in paper - done :)
# # my_ttest(yName = "mainY", dat = d)
# # 
# # # now control for baseline demographics
# # ( string = paste( c("mainY ~ treat", demo.raw), collapse = " + ") )
# # ols = lm( eval( parse(text = string) ), data = d )
# # summary(ols)
# # 
# # my_ols_hc0(dat = d, coefName = "treat", yName = "mainY", ols = ols )
# 
# 
# 
# ### run all outcomes
# 
# # reproduce results in manuscript (retain all participants; don't control covars)
# x1 = analyze_all_outcomes(missMethod = "CC",
#                           control_covars = FALSE)
# View(x1$res.nice)
# 
# # control for demographics, but still retain all participants
# x2 = analyze_all_outcomes(missMethod = "CC",
#                           control_covars = TRUE)
# View(x2$res.nice)
# 
# 
# # ~ Dropping inattentive subjects  -------------------------------------------------
# 
# # make new attention check variable
# # for the present analysis, "attentive" depends on which treatment group you are in
# d$passCheck = 0
# d$passCheck[ d$treat == 1 & d$videoContent == "The ways we raise animals for human consumption causes the animals to suffer."] = 1
# d$passCheck[ d$treat == 0 ] = 1  # control group is attentive by definition
# mean(d$passCheck) # 87% by this definition
# 
# # ~ Tables for paper -------------------------------------------------
# 
# # three outcomes to display in paper
# keepers = c("mainY CC", "totalMeat CC", "totalAnimProd CC")
# 
# 
# for ( .drop in c(FALSE, TRUE) ) {
#   for ( .covars in c(FALSE, TRUE) ) {
#     
#     
#     x = analyze_all_outcomes(missMethod = "CC",
#                              drop_inattentives = .drop,
#                              control_covars = .covars)
#     
#     
#     
#     x2 = x$res.nice %>%
#       filter(analysis %in% keepers) %>%
#       select(analysis,
#              est,
#              g.est,
#              pval) 
#     
#     # add info about analysis
#     if ( .drop == FALSE & .covars == FALSE ) string = "No exclusion; unadjusted"
#     if ( .drop == FALSE & .covars == TRUE ) string = "No exclusion; adjusted"
#     if ( .drop == TRUE & .covars == FALSE ) string = "Excluding inattentive; unadjusted"
#     if ( .drop == TRUE & .covars == TRUE ) string = "Excluding inattentive; adjusted"
#     
#     cat( paste( "\n\n******* Analysis:", string ) )
#     cat("Analyzed n = ", x$res.nice$nobs[1])
#     
#     # save the sample size separately
#     update_result_csv(name = paste( "Applied analyzed n", string ),
#                       value = x$res.nice$nobs[1],
#                       .results.dir = results.dir,
#                       .overleaf.dir = overleaf.dir)
#     
#     x2 = x2 %>% add_row(.before = 1,
#                         analysis = string )
#     
#     # add spacer row for prettiness
#     x2 = x2 %>% add_row(.after = 4)
#     
#     if (.drop == FALSE & .covars == FALSE) rs = x2 else rs = bind_rows(rs, x2)
#     
#   }
# }
# 
# 
# # prettify
# rs[rs == "mainY CC"] = "Meat and animal products"
# rs[rs == "totalMeat CC"] = "Meat"
# rs[rs == "totalAnimProd CC"] = "Animal products"
# 
# 
# rs
# 
# print( xtable(rs),
#        include.rownames = FALSE )
# 
# 
# # xtable of results, for pasting into manuscript
# setwd(results.dir)
# 
# write.table( print( xtable( rs,
#                             include.rownames = FALSE ) ),
#              file = "xtable_ate_estimates.txt"
# )
# 
# ### Sanity checks
# # simple exclusion
# # drop inattentives; don't control covariates
# x3 = analyze_all_outcomes(missMethod = "CC",
#                           drop_inattentives = TRUE)
# View(x3$res.nice)
# 
# 
# # drop inattentives; control covariates
# x4 = analyze_all_outcomes(missMethod = "CC",
#                           drop_inattentives = TRUE,
#                           control_covars = TRUE)
# View(x4$res.nice)
# 
# 
# 
# # ~ One-off stats for paper  -------------------------------------------------
# 
# # sensitivity analysis on raw mean difference scale
# # note that estimate for AP consumption was already in the "wrong" direction (positive) 
# x5 = x4$res.raw %>% filter(analysis %in% keepers) %>% select(analysis, est)
# 
# pR = mean(d$passCheck)
# 
# maxRU = .3
# 
# update_result_csv(name = paste( "maxUY to explain away", x5$analysis ),
#                   value = round( abs(x5$est)/(2 * (1-pR) * maxRU), 2),
#                   .results.dir = results.dir,
#                   .overleaf.dir = overleaf.dir)
# 
# 
# 
# # ~ One-off stats for paper  -------------------------------------------------
# 
# # number inattentive
# update_result_csv(name = paste( "Applied n inattentive" ),
#                   value = sum(d$passCheck == 0),
#                   .results.dir = results.dir,
#                   .overleaf.dir = overleaf.dir)
# 
# # number inattentive
# update_result_csv(name = paste( "Perc n inattentive" ),
#                   value = mean(d$passCheck == 0, na.rm = TRUE),
#                   .results.dir = results.dir,
#                   .overleaf.dir = overleaf.dir)
# 
# 
# 
# 
# 
# # PREDICTORS OF R AND OF Y ---------------------------------------------------------------
# 
# # ~ R ~ C -------------------------------------------------
# # fit model to treatment group only since control group were all set to attention = 1
# # as in previous studies, females and older people were more attentive
# ( string = paste( c("passCheck ~ 1", demo.raw), collapse = " + " ) )
# m = glm( eval( parse(text = string) ),
#          data = d %>% filter(treat == 1),
#          family = "binomial"(link = "logit") )
# summary(m)
# 
# # save results
# x = tidy(m)
# x$OR = exp(x$estimate)
# 
# CIs = as.data.frame( confint(m) )
# CIs = exp(CIs)
# names(CIs) = c("lo", "hi")
# 
# # for a merged table
# col1 = stat_CI( round(x$OR, 2), round(CIs$lo, 2), round(CIs$hi, 2) )
# 
# update_result_csv( name = paste("OR for attentiveness ", x$term, sep = ""),
#                    value = x$OR,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = TRUE)
# 
# update_result_csv( name = paste("OR pval for attentiveness ", x$term, sep = ""),
#                    value = x$p.value,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = FALSE)
# 
# 
# update_result_csv( name = paste("OR lo for attentiveness ", x$term, sep = ""),
#                    value = CIs$lo,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = TRUE)
# 
# 
# update_result_csv( name = paste("OR hi for attentiveness ", x$term, sep = ""),
#                    value = CIs$hi,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = TRUE)
# 
# 
# 
# 
# # ~ Y ~ C -------------------------------------------------
# 
# # ** great example because male also strongly predicts meat consumption
# ( string = paste( c("mainY ~ 1", demo.raw), collapse = " + " ) )
# m = lm( eval( parse(text = string) ), data = d %>% filter(treat == 1) )
# summary(m)
# 
# # get HC0 inference for all coefficients
# for ( .coefName in names(m$coefficients) ){
#   row = my_ols_hc0(coefName = .coefName,
#                    ols = m,
#                    show_SMD = FALSE)
#   print(row)
#   if ( .coefName == names(m$coefficients)[1] ) {
#     rs = row
#   } else {
#     rs = bind_rows(rs, row)
#   }
# }
# 
# rs
# 
# # for a merged table
# col2 = stat_CI( round(rs$est, 2), round(rs$lo, 2), round(rs$hi, 2) )
# 
# # one-off stats
# update_result_csv( name = paste("Mean diff for mainY ", row.names(rs), sep = ""),
#                    value = rs$est,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = FALSE)
# 
# update_result_csv( name = paste("Mean diff lo for mainY ", row.names(rs), sep = ""),
#                    value = rs$lo,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = FALSE)
# 
# 
# update_result_csv( name = paste("Mean diff hi for mainY ", row.names(rs), sep = ""),
#                    value = rs$hi,
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir,
#                    print = FALSE)
# 
# # ~ Merged table: covariate associations with both R and Y  -------------------------------------------------
# 
# rs2 = dplyr::bind_cols(col1, col2)
# 
# rs2 = rs2 %>%
#   add_column(.before = 1, row.names(rs))
# 
# names(rs2) = c("Covariate", "Association with attentiveness (OR)", "Association with MAP consumption (mean diff.)")
# 
# print( xtable(rs2),
#        include.rownames = FALSE )
# 
# # xtable of results, for pasting into manuscript
# setwd(results.dir)
# 
# write.table( print( xtable( rs2,
#                             include.rownames = FALSE ) ),
#              file = "xtable_predictors_attentiveness.txt"
# )
# 
