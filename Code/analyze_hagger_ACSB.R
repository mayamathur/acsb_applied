

# Hagger's repo:  https://osf.io/jymhe/

# Q: Is it okay to use standardized effects when aggregate the R measures? 
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

overleaf.dir.figs = "/Users/mmathur/Dropbox/Apps/Overleaf/Attention checks selection bias (ACSB) Overleaf/figures"


# get helper fns
setwd(code.dir)
source("helper_applied_ACSB.R")

# no sci notation
options(scipen=999)


# get data
setwd(data.dir)
setwd("Hagger")

d = fread("prepped_hagger_data_ACSB.csv")
expect_equal(nrow(d), 23)


# ITT ESTIMATE  -------------------------------------------------

# as in original analysis, except I'm using KNHA
mod_ITT = rma.uni(yi = yi_XY,
                  vi = vi_XY,
                  data = d,
                  method = "REML",
                  knha = TRUE )


# one-off stats for paper
update_result_csv(name = "ITT est Hagger",
                  value = mod_ITT$b,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "ITT lo Hagger",
                  value = mod_ITT$ci.lb,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "ITT hi Hagger",
                  value = mod_ITT$ci.ub,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)


# IV ANALYSIS USING COMPOSITE MANIPULATION CHECK -------------------------------------------------

# look at the estimates and variances for reasonableness and weak instrument issues
# also see the plots in next section
d %>% select(IV_est, IV_var, yi_XR_agg, yi_XY) %>%
  arrange(IV_est)

# meta-analyze the IV estimates
mod_IV = rma.uni(yi = IV_est,
                 vi = IV_var,
                 method = "REML",
                 data = d,
                 knha = TRUE)

# wow, super similar to original estimate!! very interesting

# c.f. simple mean:
mean(d$IV_est)

# one-off stats for paper
update_result_csv(name = "IV est Hagger",
                  value = mod_IV$b,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "IV lo Hagger",
                  value = mod_IV$ci.lb,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "IV hi Hagger",
                  value = mod_IV$ci.ub,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)


# ~ Forest plot of ITT estimates vs. IV estimates -------------------------------------------------

# plotting df
dp = d %>% select( name, yi_XY, vi_XY, IV_est, IV_var)

# capitalize author names
dp$name = firstup( as.character(dp$name) )

# for later plot ordering
dp = dp %>% arrange(yi_XY)
correct.study.order = rev(dp$name)
dp$name = factor(dp$name, correct.study.order)
levels(dp$name)

# long form
l = reshape( dp, varying = list(est = c("yi_XY", "IV_est"),
                                var = c("vi_XY", "IV_var") ),
             v.names = c("est", "var"),
             idvar="name",
             direction="long" )

l = l %>% arrange(name) %>%
  rename(method = "time")
l$method[l$method == 1] = "ITT"
l$method[l$method == 2] = "IV"

# Wald-type CIs
l = l %>% rowwise() %>%
  mutate(lo = quick_ci(est = est, var = var)[1],
         hi = quick_ci(est = est, var = var)[2] )

# for later plot shape
l$is_pooled = "a.no"


# add the pooled points
l = l %>% add_row(method = c("ITT", "IV"),
                  est = c(mod_ITT$b, mod_IV$b),
                  lo = c(mod_ITT$ci.lb, mod_IV$ci.lb),
                  hi = c(mod_ITT$ci.ub, mod_IV$ci.ub),
                  name = c("Pooled ITT", "Pooled IV"),
                  is_pooled = c("b.yes", "b.yes"),
                  .before = 1)

# # order studies by ITT estimate
# l$name = factor(l$name, levels = correct.order)
# levels(l$name)

# reorder names again
correct.study.order2 = c("Pooled IV", "Pooled ITT", correct.study.order)
l$name = factor(l$name, correct.study.order2)
levels(l$name)

# factor levels
correct.order = rev( c("ITT", "IV") )
l$method = factor(l$method, levels = correct.order)
levels(l$method)


.colors = c("orange",
            "black")

#.shapes = c(1, 5)


# find good x-axis limits
summary(l$lo)
summary(l$hi)
xmin = -3
xmax = 2.5


p = ggplot( data = l,
            aes( y = name,
                 x = est, 
                 xmin = lo, 
                 xmax = hi,
                 color = method,
                 shape = is_pooled) ) +
  
  # reference line at null
  geom_vline(xintercept = 0,
             lwd = .8,
             color = "gray") +
  
  
  geom_errorbarh( aes(xmax = lo,
                      xmin = hi),
                  height = 0,
                  lwd = 0.8,
                  position = position_dodge(width = 0.5) ) +
  
  geom_point(size=3,
             position=position_dodge(width = 0.5) ) +
  
  # manually provided colors
  scale_colour_manual(values = .colors,
                      guide = guide_legend(reverse = TRUE)) +
  
  
  scale_y_discrete( name = "Replication lead author" ) +
  # scale_x_continuous( limits = c(xmin, xmax),
  #                     breaks = seq(xmin, xmax, 0.05) ) +
  
  # scale_x_log10(breaks = seq(0.1, 10, 1)) +
  # coord_cartesian( xlim = c(0.1, 10)) +
  
  coord_cartesian( xlim = c(xmin, xmax)) +
  scale_x_continuous(breaks = seq(xmin, xmax, 0.5)) +
  
  xlab( "Pooled SMD with 95% CI" ) +
  
  labs(color  = "Method") +
  
  scale_shape(guide = 'none') +
  
  theme_bw(base_size = 16) +
  
  theme( text = element_text(face = "bold"),
         axis.title = element_text(size=16),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.position = "bottom" )

p



my_ggsave(name = "hagger_forest.pdf",
          width = 13,
          height = 10,
          .results.dir = results.dir,
          .overleaf.dir = overleaf.dir.figs)


# IV ANALYSIS USING FATIGUE ONLY -------------------------------------------------

# What if we assume fatigue (the one that didn't change) is the only one that matters?
# This is maximally charitable toward Baumeister's critique
# We are now assuming (again, charitably) that excludability holds wrt fatigue, i.e., the effect
# of the manipulation isn't mediated by by effort, difficulty, frustration at all

# look at the estimates and variances for reasonableness and weak instrument issues
# also see the plots in next section
d %>% select(IV_est_fatigue, IV_var_fatigue, yi_XR_fatigue, yi_XY) %>%
  arrange(IV_est_fatigue)

# meta-analyze the IV estimates
( modf = rma.uni(yi = IV_est_fatigue,
                 vi = IV_var_fatigue,
                 method = "REML",
                 data = d,
                 knha = TRUE) )

# c.f. simple mean - very different!
# however, this makes sense because there were a couple of huge IV estimates that also had huge SEs 
mean(d$IV_est_fatigue)


# one-off stats for paper
update_result_csv(name = "IV est fatigue Hagger",
                  value = modf$b,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "IV lo fatigue Hagger",
                  value = modf$ci.lb,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)

update_result_csv(name = "IV hi fatigue Hagger",
                  value = modf$ci.ub,
                  .results.dir = results.dir,
                  .overleaf.dir = overleaf.dir)


# sensitivity analysis
# exclude weak instruments based on arbitrary criterion of |d < 0.10| since we don't have F-statistics
# meta-analyze the IV estimates
( modf2 = rma.uni(yi = IV_est_fatigue,
                  vi = IV_var_fatigue,
                  method = "REML",
                  data = d %>% filter( abs(yi_XR_fatigue) > 0.10 ),
                  knha = TRUE) )

# point estimate is almost identical since the excluded ones had huge SEs anyway :)




# ~ Plots (sanity checks)  -------------------------------------------------

#### ITT estimate vs. IV estimate

# composite manipulation check
# some increase as a result of IV estimate, but others decrease (become more negative)
ggplot( data = d, 
        aes(x = yi_XY,
            y = yi_XY / yi_XR_agg,
            size = 1/vi_XR_agg) ) +
  geom_abline(intercept = 0, slope = 1) +
  
  # line for IV estimate = 0
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(alpha = 0.4) + 
  theme_bw()


mean(d$yi_XY / d$yi_XR_agg)


# fatigue only
ggplot( data = d, 
        aes(x = yi_XY,
            y = yi_XY / yi_XR_fatigue,
            size = 1/sei_XR_fatigue^2) ) +
  # line for IV estimate = 0
  geom_hline(yintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.4) + 
  theme_bw()


#### IV estimate vs. its SE

# composite manipulation check
ggplot( data = d, 
        aes(x = IV_est,
            y = sqrt(IV_var) ) ) +
  
  geom_point(alpha = 0.4) + 
  theme_bw()


# fatigue
ggplot( data = d, 
        aes(x = IV_est_fatigue,
            y = sqrt(IV_var_fatigue) ) ) +
  
  geom_point(alpha = 0.4) + 
  theme_bw()


