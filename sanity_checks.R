
# load packages
library(dplyr)
library(ICC)
library(metafor)
library(robumeta)
library(MetaUtility)
library(weightr)
library(PublicationBias)
library(tableone)
library(readxl)
library(testthat)

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Analysis/awr_analysis_git"
results.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Analysis/Results from R"
# we write results straight to Overleaf-connected DropBox folder for error-proofing
overleaf.dir = "~/Dropbox/Apps/Overleaf/AWR (animal welfare interventions review)/R_objects"

# helper fns
setwd(code.dir); source("helper_analysis.R")

# for formatting stats
digits = 2  # rounding
pval.cutoff = 10^-4  # threshold for using "<"
options(scipen=999)  # disable scientific notation


##### Read In Datasets #####

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")
d = d %>% filter( !is.na(authoryear) )  # because that table has blank rows for prettiness



################################ CHECK EFFECT SIZE CONVERSIONS ################################

library(testthat)

# when original estimate was already a log-RR, so should be the analyzed one
expect_equal( d$logRR[ d$effect.measure == "logRR" ],
              d$yi[ d$effect.measure == "smd" ],
              tol = 0.001 )
expect_equal( d$varlogRR[ d$effect.measure == "logRR" ],
              d$vi[ d$effect.measure == "smd" ],
              tol = 0.001 )

# when original estimate was a SMD, check conversion
expect_equal( d$logRR[ d$effect.measure == "smd" ],
              log( sqrt( exp( d$yi[ d$effect.measure == "smd" ] * pi / sqrt(3) ) ) ), 
              tol = 0.001 )
# Wolfram Alpha derivative: derivative of log( sqrt( exp(d*pi/sqrt(3) ) ) )
expect_equal( d$varlogRR[ d$effect.measure == "smd" ],
              ( pi / (2*sqrt(3)) )^2 * d$vi[ d$effect.measure == "smd" ], 
              tol = 0.001 )

################################ CHECK TABLE 1 ################################

# check one of each variable type
# this is basically a sanity check for the table1_add_row function
# a categorical variable
d %>% group_by(country) %>%
  summarise( n(),
             n()/nrow(d) )

# a continuous variable
median( d$perc.male, na.rm=TRUE ) 
quantile( d$perc.male, c( 0.25, 0.75 ), na.rm=TRUE )
prop.table( table( is.na( d$perc.male) ) )

# a binary variable
d %>% group_by(x.has.text) %>%
  summarise( n(), 
             n()/nrow(d) )

# check behavior of perc_incl_NA 
#  used heavily in "Intervention Characteristics" section
# this is a variable that definitely has missing data
expect_equal( as.numeric( percTRUE_incl_NA(d$x.suffer) ),
              sum(d$x.suffer, na.rm = TRUE) / length(d$x.suffer) )

################################ CHECK NPPHAT VALUES IN PLOTTING DATAFRAME ################################

setwd(results.dir)
res = read.csv("npphat_results.csv")

# randomly choose a few estimates and CIs to reproduce
inds = sample( 1:nrow(res) )[1:5]

for ( i in inds ) {
  row = res[i,]
  Phat = prop_stronger( q = row$q,
                        tail = "above",
                        estimate.method = "calibrated",
                        ci.method = "calibrated",
                        dat = d,
                        yi.name = "logRR",
                        vi.name = "varlogRR" )
  
  # note: the CI limits might actually differ due to bootstrapping
  #  but obviously the estimate should agree exactly
  expect_equal( 100*Phat$Est, row$Est, tol = 0.001 )
  expect_equal( 100*Phat$lo, row$lo, tol = 0.001 )
  expect_equal( 100*Phat$hi, row$hi, tol = 0.001 )
}

################################ MANUAL SUBSET ANALYSIS ################################

# choose one variable and do subset analysis manually
dat = d %>% filter( !is.na(qual.y.prox2) & qual.y.prox2 == "b.Actual or self-reported" )

dim(dat)

library(robumeta)
( meta = robu( logRR ~ 1, 
               data = dat, 
               studynum = as.factor(authoryear),
               var.eff.size = varlogRR,
               modelweights = "HIER",
               small = TRUE) )

exp(meta$b.r)
sqrt(meta$mod_info$tau.sq)
exp(meta$reg_table$CI.L)
exp(meta$reg_table$CI.U)


prop_stronger( q = log(1), 
               tail = "above",
               estimate.method = "calibrated",
               ci.method = "calibrated",
               dat = dat,
               R = 2000 )

prop_stronger( q = log(1.1), 
               tail = "above",
               estimate.method = "calibrated",
               ci.method = "calibrated",
               dat = dat,
               R = 2000 )

prop_stronger( q = log(1.2), 
               tail = "above",
               estimate.method = "calibrated",
               ci.method = "calibrated",
               dat = dat,
               R = 2000 )

# oh yeaaahhhhhhhh :D






