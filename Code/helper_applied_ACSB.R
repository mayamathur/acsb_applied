
# Straight from EatingVeg / helper_analysis.R:

##### Major Fn: Analyze All Outcomes, Primary and Secondary #####

# This fn accounts for the following differences in analyses across the 3 studies:
#  - Studies 1 and 2 used marginal randomization, so each outcome is analyzed with a t-test. However, Study 2 used stratified randomization, so each outcome is analyzed with OLS + robust inference, controlling for variable, `targetDemographic`, used in randomization.
#  - For Study 3, we analyzed treatment effects on `mainY` in two ways: first among all subjects, and then only among those with `targetDemoSimple = TRUE`.
#  - Study 2 has a single binary outcome, `intentionReduce`, for which we used a log-linear model (with regular model-based inference) to estimate a risk ratio.

# uses a lot of global vars, like study
# missMethod: "MI" or "CC"
# control_covars: should we control for covariates listed in demo.raw (global var)?
analyze_all_outcomes = function(missMethod,
                                control_covars = FALSE,
                                drop_inattentives = FALSE ) {
  
  
  if ( drop_inattentives == TRUE ) d = d %>% filter(passCheck == TRUE)
  
  ##### Analyze Each Outcome (Including Primary) #####
  
  # for Bonferroni
  n.secY = sum( length(secFoodY), length(psychY) )
  ( alpha2 = 0.05 / n.secY ) # Bonferroni-adjusted alpha
  
  # outcomes to analyze
  toAnalyze = c("mainY", secFoodY, psychY )
  
  if ( exists("res.raw") ) rm(res.raw)
  
  # loop over outcomes
  for ( i in toAnalyze ) {
    
    # don't control for any covariates
    if ( control_covars == FALSE ){
      
      if ( missMethod == "CC" ) {
        mi.res = my_ttest(yName = i,
                          dat = d)
      }
      
      
    }
    
    # control covariates
    if ( control_covars == TRUE ) {
      
      if ( missMethod == "CC" ) {
        .d = d
        .d$Y = .d[[i]]
        
        ( string = paste( c("Y ~ treat", demo.raw), collapse = " + ") )
        ols = lm( eval( parse(text = string) ), data = .d )
        
        
        mi.res = my_ols_hc0( ols = ols,
                             coefName = "treat",
                             
                             # for SMDs:
                             dat = .d,
                             xName = "treat",
                             yName = i )
        
        
      }
      
    }
    
    # pool the imputations
    # might have only 1 row if we're doing CC analysis
    if ( missMethod == "MI" ) {
      mi.res = do.call(what = rbind, mi.res)
      part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
      part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
      names(part2) = paste( "g.", names(part2), sep = "" )
      names(part2)[ names(part2) == "g.est" ] = "g"
      new.row = cbind(part1, part2)
      
    } else if ( missMethod == "CC" ) {
      # no need to pool in this case
      new.row = mi.res
    }
    
    
    
    # Bonferroni-corrected p-value
    if( i %in% c(secFoodY, psychY) ) {
      new.row$pvalBonf = min( 1, new.row$pval * n.secY )
      new.row$group = "secY"
      
      if( i %in% secFoodY) new.row$group.specific = "secY food"
      if( i %in% psychY) new.row$group.specific = "secY psych"
      
    } else if (i %in% c("mainY") ) {
      # for primary outcome
      new.row$pvalBonf = NA
      new.row$group = i
      new.row$group.specific = i
    } else if ( i == "intentionCont" ) {
      new.row$pvalBonf = NA
      new.row$group = "intention"
      # because this outcome produces 2 rows
      new.row$group.specific = c("intentionCont", "intentionReduce")
    }
    
    # for CC analyses only, add raw means and medians
    # NOTE: FOR STUDY 3, DIFF IN MEANS WON'T EXACTLY MATCH EST BECAUSE EST CONTROLS
    #  FOR STRATIFICATION VARS
    # For Study 2, yName = "intentionCont", we have a row for intentionCont AND a row for intentionReduce,
    #   so this step will insert the same means and medians for both rows. This is overwritten in the ad hoc
    #   adjustments below. 
    if ( missMethod == "CC" ) {
      new.row = new.row %>% add_column( .before = 1,
                                        mn0 = mean( d[[i]][ d$treat == 0], na.rm = TRUE ),
                                        mn1 = mean( d[[i]][ d$treat == 1], na.rm = TRUE ),
                                        med0 = median( d[[i]][ d$treat == 0], na.rm = TRUE ),
                                        med1 = median( d[[i]][ d$treat == 1], na.rm = TRUE ) )
    }
    
    
    # add name of this analysis
    string = paste(i, missMethod, sep = " ")
    
    new.row = add_column(new.row, analysis = string, .before = 1)
    
    if ( !exists("res.raw") ) res.raw = new.row else res.raw = bind_rows(res.raw, new.row)
  }  # end loop over all outcomes to be analyzed
  
  #res.raw
  
  
  ##### Save Both Raw and Cleaned-Up Results Tables #####
  
  # in order to have the unrounded values
  setwd(results.dir)
  if ( missMethod == "MI") write.csv(res.raw, "raw_ate_estimates_all_outcomes_mi.csv")
  if ( missMethod == "CC") write.csv(res.raw, "raw_ate_estimates_all_outcomes_cc.csv")
  
  
  # cleaned-up version
  # round it
  res.raw = res.raw %>% mutate_at( names(res.raw)[ !names(res.raw) %in% c("analysis", "analysis.1", "group", "group.specific", "note" ) ], function(x) round(x,2) )
  
  if ( missMethod == "MI") {
    res.nice = data.frame( analysis = res.raw$analysis,
                           est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
                           g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
                           pval = res.raw$pval,
                           pvalBonf = res.raw$pvalBonf )
  }
  
  if ( missMethod == "CC") {
    res.nice = data.frame( analysis = res.raw$analysis,
                           mn0 = res.raw$mn0,
                           mn1 = res.raw$mn1,
                           med0 = res.raw$med0,
                           med1 = res.raw$med1,
                           est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
                           g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
                           nobs = res.raw$nobs,
                           pval = res.raw$pval,
                           pvalBonf = res.raw$pvalBonf )
  }
  
  
  setwd(results.dir)
  if ( missMethod == "MI") write.csv(res.nice, "pretty_ate_estimates_all_outcomes_mi_pretty.csv")
  if ( missMethod == "CC") write.csv(res.nice, "pretty_ate_estimates_all_outcomes_cc_pretty.csv")
  
  
  # ##### One-Off Stats for Paper: Main Estimates #####
  # analysis.string = paste( "mainY", missMethod, sep = " ")
  # 
  # update_result_csv( name = paste( "mainY diff", missMethod ),
  #                    value = round( res.raw$est[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # update_result_csv( name = paste( "mainY diff lo", missMethod ),
  #                    value = round( res.raw$lo[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # update_result_csv( name = paste( "mainY diff hi", missMethod ),
  #                    value = round( res.raw$hi[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # 
  # update_result_csv( name = paste( "mainY diff pval", missMethod ),
  #                    value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # update_result_csv( name = paste( "mainY diff g", missMethod ),
  #                    value = round( res.raw$g[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # update_result_csv( name = paste( "mainY diff g lo", missMethod ),
  #                    value = round( res.raw$g.lo[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # update_result_csv( name = paste( "mainY diff g hi", missMethod ),
  #                    value = round( res.raw$g.hi[ res.raw$analysis == analysis.string], 2 ) )
  # 
  # ##### One-Off Stats for Study 3 Only: Main Estimate Among Target Demographic #####
  # 
  # if ( study == 3 ) {
  #   analysis.string2 = paste( "mainY targetDemoSimple-subset", missMethod, sep = " ")
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff", missMethod ),
  #                      value = round( res.raw$est[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff lo", missMethod ),
  #                      value = round( res.raw$lo[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff hi", missMethod ),
  #                      value = round( res.raw$hi[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff pval", missMethod ),
  #                      value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g", missMethod ),
  #                      value = round( res.raw$g[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g lo", missMethod ),
  #                      value = round( res.raw$g.lo[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g hi", missMethod ),
  #                      value = round( res.raw$g.hi[ res.raw$analysis == analysis.string2], 2 ) )
  #   
  # }
  # 
  # ##### One-Off Stats for Paper: Various Multiple-Testing Metrics for Secondary Outcomes #####
  # update_result_csv( name = paste( "Bonferroni alpha secY", missMethod ),
  #                    value = round( alpha2, 4 ) )
  # 
  # update_result_csv( name = paste( "Bonferroni number secY", missMethod ),
  #                    value = n.secY )
  # 
  # update_result_csv( name = paste( "Number secY pass Bonf", missMethod ),
  #                    value = sum( res.raw$pvalBonf[ res.raw$group == "secY" ] < 0.05 ) )
  # 
  # # harmonic mean p-values by subsets of effect modifiers
  # update_result_csv( name = paste( "HMP all secY", missMethod ),
  #                    value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group == "secY" ],
  #                                                L = sum(res.raw$group == "secY") ), 2 ) )
  # 
  # update_result_csv( name = paste("HMP food secY", missMethod ),
  #                    value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY food" ],
  #                                                L = sum(res.raw$group.specific == "secY food") ), 2 ) )
  # 
  # update_result_csv( name = paste( "HMP psych secY", missMethod ),
  #                    value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY psych" ],
  #                                                L = sum(res.raw$group.specific == "secY psych") ), 2 ) )
  
  
  
  return( list(res.nice = res.nice,
               res.raw = res.raw) )
}






# nicely organize Welch t-test results
my_ttest = function( yName, dat ){
  
  tres = t.test( dat[[yName]] ~ treat,
                 data = dat,
                 var.equal = FALSE )
  
  dat$Y = dat[[yName]]
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(Y, na.rm = TRUE),
                                       sd = sd(Y, na.rm = TRUE),
                                       n = n() ) )
  
  # standardized mean difference (Hedges' g)
  es = escalc( m1i = tab[2,]$m,
               sd1i = tab[2,]$sd,
               n1i = tab[2,]$n,
               
               m2i = tab[1,]$m,
               sd2i = tab[1,]$sd,
               n2i = tab[1,]$n,
               
               # Hedges' g by default
               measure = "SMD")
  summ = summary(es)
  
  return( data.frame( # documentary - control
    est = tres$estimate[2] - tres$estimate[1],
    se = tres$stderr,
    # note the reversed order because t-test calculates 
    #  control - documentary:
    lo = -tres$conf.int[2],
    hi = -tres$conf.int[1],
    pval = tres$p.value,
    nobs = sum( !is.na( dat[[yName]] ) & !is.na( dat$treat ) ),
    
    # standardized mean difference (Hedges' g)
    g = es$yi,
    g.se = summ$sei,
    g.lo = summ$ci.lb,
    g.hi = summ$ci.ub ) )
}

# ols: the OLS model
# coefName: which coefficient to report

# dat: dataset (only needed to calculate Hedges' g)
# yName: outcome
# xName: names of the coefficient in d (may differ from coefName for categorical variables)
my_ols_hc0 = function( ols, coefName,
                       # these ones are only needed if you want Hedges' g
                       dat = NULL, yName = NULL, xName = NULL,
                       show_SMD = TRUE){
  
  ( se.ols = sqrt( vcov(ols)[coefName, coefName] ) )
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( vcovHC( ols, type="HC0")[coefName, coefName] ) )
  
  tcrit = qt(.975, df = ols$df.residual)
  t = as.numeric( abs(bhat.ols / se.hc0) )
  
  if ( show_SMD == FALSE ) {
    return( data.frame(
      est = bhat.ols,
      se = se.hc0,
      lo = bhat.ols - tcrit * se.hc0,
      hi = bhat.ols + tcrit * se.hc0,
      pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ),
      nobs = nobs(ols) ) ) # number of observations in model
  }
  
  if ( show_SMD == TRUE) {
    
    if ( is.null(dat) | is.null(yName) | is.null(xName) ) stop("Need to provide dat, yName, and xName if you want SMD")
    
    dat$Y = dat[[yName]]
    dat$X = dat[[xName]]
    
    # standardized mean difference
    # **note for paper: standardizing by SD(Y|X) rather than SD(Y|X,Z) where
    #  Z is the effect modifiers because former is more directly comparable
    #  to the effect sizes in main analysis
    # note also that we need to calculate sd.pooled for each MI dataset rather than 
    #  just transforming the final pooled estimate to an SMD, because SD(Y|X) differs 
    #  in each imputed dataset
    tab = suppressMessages( dat %>% group_by(X) %>%
                              summarise( m = mean(Y, na.rm = TRUE),
                                         sd = sd(Y, na.rm = TRUE),
                                         n = n() ) )
    num = (tab$n[1] - 1) * tab$sd[1]^2 + (tab$n[2] - 1) * tab$sd[2]^2
    denom = (tab$n[1] - 1) + (tab$n[2] - 1)
    sd.pooled = sqrt(num/denom)
    # adjustment factor for Hedges' g
    # https://www.statisticshowto.com/hedges-g/#:~:text=Hedges'%20g%20is%20a%20measure,of%20up%20to%20about%204%25.
    N = sum(tab$n)
    J = ( (N-3) / (N-2.25) ) * sqrt( (N-2) / N )
    # factor to multiply with the raw mean difference to get Hedges' g
    term = J / sd.pooled
    
    return( data.frame(
      est = bhat.ols,
      se = se.hc0,
      lo = bhat.ols - tcrit * se.hc0,
      hi = bhat.ols + tcrit * se.hc0,
      pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ),
      nobs = nobs(ols),  # number of observations in model
      
      # standardized mean difference (Hedges' g)
      g = bhat.ols * term,
      g.se = se.hc0 * term,
      g.lo = (bhat.ols - tcrit * se.hc0) * term,
      g.hi = (bhat.ols + tcrit * se.hc0) * term ) )
  }
  
  
}


# make a string for estimate and CI
stat_CI = function(est, lo, hi){
  paste( est, " [", lo, ", ", hi, "]", sep = "" )
}
# stat_CI( c(.5, -.1), c(.3, -.2), c(.7, .0) )



# MISC  -------------------------------------------------

my_ggsave = function(name,
                     width,
                     height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  setwd(.results.dir)
  ggsave( name,
          width = width, 
          height = height)
  
  setwd(.overleaf.dir)
  ggsave( name,
          width = width, 
          height = height)
}


# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
update_result_csv = function( name,
                              .section = NA,
                              .results.dir = results.dir,
                              .overleaf.dir = overleaf.dir.stats,
                              value = NA,
                              print = FALSE ) {
  
  # if either is NULL, it just won't be included in this vector
  dirs = c(.results.dir, .overleaf.dir)
  
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(.section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  for (.dir in dirs) {
    
    setwd(.dir)
    
    if ( "stats_for_paper.csv" %in% list.files() ) {
      res = read.csv( "stats_for_paper.csv",
                      stringsAsFactors = FALSE,
                      colClasses = rep("character", 3 ) )
      
      # if this entry is already in the results file, overwrite the
      #  old one
      if ( all(name %in% res$name) ) res[ res$name %in% name, ] = new.rows
      else res = rbind(res, new.rows)
    }
    
    if ( ! "stats_for_paper.csv" %in% list.files() ) {
      res = new.rows
    }
    
    write.csv( res, 
               "stats_for_paper.csv",
               row.names = FALSE,
               quote = FALSE )
    
  }  # end "for (.dir in dirs)"
  
  
  if ( print == TRUE ) {
    View(res)
  }
  
}

# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


quick_ci = function( est, var ) {
  c( est - qnorm(.975) * sqrt(var),
     est + qnorm(.975) * sqrt(var) )
}

quick_pval = function( est, var ) {
  2 * ( 1 - pnorm( abs( est / sqrt(var) ) ) )
}