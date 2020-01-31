
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


################################ MANUAL SUBSET ANALYSIS ################################

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






