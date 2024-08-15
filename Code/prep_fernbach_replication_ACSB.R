

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
            "tableone",
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

# should we use fake Qualtrics data to test the code?
# doing so requires a few variables to be regenerated
use_fake_data = TRUE



# LOAD DATA ---------------------------------------------------------------

setwd(data.dir)

if ( use_fake_data ) dr = read.csv("fake_raw_data.csv", header = TRUE)


# remove extra Qualtrics header rows if they are there
if ( dr$StartDate[1] == "Start Date" ) dr = dr[-c(1:2),]
dr = droplevels(dr)

nrow(dr)

#expect_equal(nrow(dr), 110)





# RECODE VARIABLES  -------------------------------------------------

# init new dataset
d = dr

# recode NAs throughout dataset
d[d==""] = NA

# subject ID
d$id = 1:nrow(d)



# only for fake Qualtrics data: regenerate age to be reasonable
if ( use_fake_data ) d$age = round(runif(n= nrow(d), min = 18, max = 50))

d$age10y = d$age/10


### Simplify categories of education for analysis
# new categories are like EatingVeg
d$educ2 = NA
d$educ2[ d$educ %in% c("a.Some high school, no diploma",
                          "b.High school graduate, or equivalent",
                          "c.Some college") ] = "a.High school or less"
d$educ2[ d$educ %in% c("d.Associate's degree", "g.Professional degree") ] = "b.Associate's or professional degree"
d$educ2[ d$educ %in% c("e.Bachelor's degree") ] = "c.Bachelor's degree"
d$educ2[ d$educ %in% c("e.Doctoral degree",
                       "f.Master's degree") ] = "c.Master's or doctoral degree"
table(d$educ, d$educ2)


### Standardize the interest variables
#@@@ will need to as "a.", "b.", etc to var names in the future
d$interest_politics2 = NA
d$interest_politics2[d$interest_politics == "a.Far less than average"] = 1
d$interest_politics2[d$interest_politics == "b.Somewhat less than average"] = 2
d$interest_politics2[d$interest_politics == "c.Average"] = 3
d$interest_politics2[d$interest_politics == "d.Somewhat more than average"] = 4
d$interest_politics2[d$interest_politics == "e.Far more than average"] = 5
table(d$interest_politics, d$interest_politics2, useNA = "ifany")
# standardize it
d$interest_politics2 = ( d$interest_politics2 - mean(d$interest_politics2) ) / sd(d$interest_politics2)
summary(d$interest_politics2)

d$interest_voted_2020_2 = "b.No or don't remember"
d$interest_voted_2020_2[ d$interest_voted_2020 == "c.Yes" ] = "a.Yes"
table(d$interest_voted_2020, d$interest_voted_2020_2)
  
#@@@ will need to as "a.", "b.", etc to var names in the future
d$interest_news2 = NA
d$interest_news2[d$interest_news == "a.Less than once per week"] = 1
d$interest_news2[d$interest_news == "b.1-3 times per week"] = 2
d$interest_news2[d$interest_news == "c.More than 3 times per week"] = 3
table(d$interest_news, d$interest_news2)
# standardize it
d$interest_news2 = ( d$interest_news2 - mean(d$interest_news2) ) / sd(d$interest_news2)


# convert variables to numeric
to_num = c("attention_birth_year")
d = d %>% mutate_at(.vars = to_num, .funs  = as.numeric) 







# ~ Make attention_pass indicator variables  -------------------------------------------------

d$expected_birth_year = 2024 - d$age
d$attention_pass_birth_year = abs(d$attention_birth_year - d$expected_birth_year) <= 1

d$attention_pass_ideology = (d$attention_ideology == "Somewhat Liberal")

#@@with real data, check this one; make sure I wrote the string correctly
d$attention_pass_news = (d$attention_news == "The Drudge Report,ABC News website")

# indicator for passing all checks
d$R_all = d$attention_pass_birth_year * d$attention_pass_ideology * d$attention_pass_news

# indicator for passing at least one
d$R_one = ( (d$attention_pass_birth_year + d$attention_pass_ideology + d$attention_pass_news) > 0 )


# only for fake Qualtrics data: regenerate these indicators to be reasonable
if ( use_fake_data ) {
  d$R_one = rbinom(n = nrow(d), size = 1, prob = 0.9)
  d$R_all = rbinom(n = nrow(d), size = 1, prob = 0.6)
}



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




# LOOK AT VARIABLES  -------------------------------------------------

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


CreateTableOne(dat = d,
               vars = c(demo_vars_analysis, interest_vars_analysis),
               strata = "condition_expl")




# RESHAPE WIDE -> LONG  -------------------------------------------------

# all "pos" ratings should be a single variable
#  and there should also be two new vars: pos_issue (string after the _), pos_issue_number (1 or 2), and time ("pre" or "post" string before the first _)


# list of variables to keep in the long dataset
static_vars = c("id",
                "condition_expl", 
                demo_vars_analysis, 
                interest_vars_analysis)


# smaller dataset with just the pos_ outcome variables
s = d %>% select( c(static_vars, names_with("pos_", d) ) )

# https://stackoverflow.com/questions/57533341/reshape-wide-to-long-based-on-part-of-column-name
s2 = s %>%
  gather(key = "key", value = "rating", -static_vars) %>%
  separate(key, into = c("time", "var", "issue_name"), sep = "_")

head(s2)

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

s2$time_post = ( s2$time == "post" )

dl = s2 %>% filter(!is.na(rating)) %>%
  arrange(id)
# this test will FAIL if you used the "IMPORTANT" block above:
expect_equal(nrow(dl), nrow(d)*(6+2)) # they only rate 6 issues (pre) + 2 issues (post)

# need similar datasets for understanding and extremity vars


# WRITE PREPPED DATA -------------------------------------------------

setwd(data.dir)
fwrite(d, "fernbach_prepped_wide.csv")

fwrite(dl, "fernbach_prepped_long_position_vars.csv")






# PRACTICE ANALYSIS WITH SINGLE OUTCOME -------------------------------------------------


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


