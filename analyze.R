

# Impressions: I think all the top interventions don't make requests and are more informative or indirect

# to do: update analyze_one_meta to use the new Phat function :)

################################# PREP ################################# 

# remove existing results files
start.res.from.scratch = FALSE

if ( start.res.from.scratch == TRUE ) {
  setwd(results.dir)
  system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  system("rm stats_for_paper.csv")
}



data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Analysis/awr_analysis_git"
results.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Analysis/Results from R"
overleaf.dir = "~/Dropbox/Apps/Overleaf/AWR (animal welfare interventions review)/R_objects"

setwd(code.dir); source("helper_analysis.R")


# for formatting stats
digits = 2
pval.cutoff = 10^-4  # threshold for using "<"
options(scipen=999)


library(dplyr)
library(ICC)
library(metafor)
library(robumeta)
library(MetaUtility)
library(weightr)
library(PublicationBias)
library(tableone)
library(readxl)

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")
d = d %>% filter( !is.na(authoryear) )  # blank rows for visual appeal
# sanity check: should be none
d$unique[ is.na(d$logRR) & d$use.rr.analysis == 1]
# sanity check
table(!is.na(d$logRR), d$use.rr.analysis)

# make different datasets for different analyses
d.veg = d[ d$use.veg.analysis == 1,]
d.grams = d[ d$use.grams.analysis == 1,]

# main dataset without high-bias challenges
# ~~~ after coding of exclude.main is complete, remove the is.na call
d = d[ d$use.rr.analysis == 1 & !is.na(d$exclude.main) & d$exclude.main == 0,]  
d = droplevels(d)
d$n.paper = as.numeric( as.character(d$n.paper) )

# article-level dataset
d.arts = d[ !duplicated(d$authoryear), ]

# # study characteristics, including the ones that didn't contribute point estimates
# # only used to calculate the number of hopeless studies
# d2 = read_xlsx("Extracted qualitative data.xlsx", na = "NR")
# # remove missing rows
# d2 = d2 %>% filter(!is.na(`First author last name`))
# d2$authoryear = paste( d2$`First author last name`,
#                        as.character(d2$Year),
#                        sep = " " )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                             0. CHARACTERISTICS OF INCLUDED STUDIES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# BASICS #################################

# number of articles
update_result_csv( name = "k articles",
                   section = 0,
                   value = length(unique(d$authoryear)),
                   print = FALSE )

# number of point estimates
update_result_csv( name = "k ests",
                   section = 0,
                   value = nrow(d),
                   print = FALSE )

# total number of unique subjects
update_result_csv( name = "n total",
                   section = 0,
                   value = sum(d.arts$n.paper),
                   print = FALSE )

# sample size per paper
update_result_csv( name = "n per article median",
                   section = 0,
                   value = median(d.arts$n.paper),
                   print = FALSE )
update_result_csv( name = "n per article Q1",
                   section = 0,
                   value = round( quantile(d.arts$n.paper, 0.25) ),
                   print = FALSE )
update_result_csv( name = "n per article Q3",
                   section = 0,
                   value = round( quantile(d.arts$n.paper, 0.75) ),
                   print = FALSE )

# percent articles published
update_result_csv( name = "Perc articles published",
                   section = 0,
                   value = round( 100 * mean( d.arts$published ), 0 ),
                   print = FALSE )

# how we obtained point estimates
t = d %>% group_by(stats.source) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )
             
update_result_csv( name = paste( "Stats source", t$stats.source, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# number of "hopeless" articles and interventions (included but couldn't get data)
# ~~~ once d2 spreadsheet is cleaner, try to get this from there
# for now, just counted the 2 Norris studies and Dowsett
update_result_csv( name = "Number articles hopeless",
                   section = 0,
                   value = 3,
                   print = TRUE )
update_result_csv( name = "Number estimates hopeless",
                   section = 0,
                   value = 7,
                   print = TRUE )

# median number of point estimates contributed by each study
update_result_csv( name = "Perc articles multiple ests",
                   section = 0,
                   value = round( 100 * mean( table(d$authoryear) > 1 ) ),
                   print = TRUE )

# ICC of point estimates clustered in papers
update_result_csv( name = "ICC",
                   section = 0,
                   value = round( ICCest(authoryear, logRR, d)$ICC, digits ),
                   print = TRUE )



################################# TABLE 2 (INDIVIDUAL STUDY CHARACTERISTICS) #################################

# variables to include in table
analysis.vars = c(
  "country",
  "perc.male",
  
  "x.has.text",
  "x.has.visuals",
  "x.suffer",
  "x.pure.animals",
  "x.min.exposed",
  "x.tailored",
  "x.pushy",
  
  "y.cat",
  "y.lag.days",
  
  "stats.source")

##### Make Table 2 #####
# to force use of median
median.vars = c( "perc.male",
                "x.min.exposed",
                "y.lag.days" )

t = CreateTableOne(data=d[,analysis.vars],
                     includeNA = TRUE )
t = print(t, nonnormal = median.vars)
xtable( print(t, noSpaces = TRUE, printToggle = FALSE, nonnormal = median.vars) )

setwd(results.dir)
setwd("Tables to prettify")
write.csv(print(t), "study_char_table.csv")


################################# SUBJECT CHARACTERISTICS #################################

# interventions containing text
update_result_csv( name = "Median perc male",
                   section = 0,
                   value = round( median(d$perc.male, na.rm=TRUE), 0 ),
                   print = FALSE )




################################# INTERVENTION CHARACTERISTICS #################################

# interventions containing text
update_result_csv( name = "Perc interventions text",
                   section = 0,
                   value = round( 100 * mean( d$x.has.text == 1, na.rm = TRUE ) ),
                   print = FALSE )

# interventions containing visuals
update_result_csv( name = "Perc interventions visuals",
                   section = 0,
                   value = round( 100 * mean( d$x.has.visuals == 1, na.rm = TRUE ) ),
                   print = FALSE )

# interventions containing graphic suffering
update_result_csv( name = "Perc interventions graphic",
                   section = 0,
                   value = round( 100 * mean( d$x.suffer == 1, na.rm = TRUE ) ),
                   print = FALSE )

# interventions purely animal welfare
update_result_csv( name = "Perc interventions pure",
                   section = 0,
                   value = round( 100 * mean( d$x.pure.animals == 1, na.rm = TRUE ) ),
                   print = FALSE )

# intervention duration
update_result_csv( name = "Perc interventions short",
                   section = 0,
                   value = round( 100 * mean( d$x.long == 0, na.rm = TRUE ), 0 ),
                   print = FALSE )

# request type
t = d %>% filter( !is.na(x.pushy) ) %>%
  group_by(x.pushy) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Pushy", t$x.pushy, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# interventions not personally tailored
update_result_csv( name = "Perc interventions not tailored",
                   section = 0,
                   value = round( 100 * mean( d$x.tailored == 0, na.rm = TRUE ) ),
                   print = FALSE )


################################# OUTCOME CHARACTERISTICS #################################

# outcome category
t = d %>% filter( !is.na(y.cat) ) %>%
  group_by(y.cat) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Y cat", t$y.cat, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# outcome proximity
t = d %>% filter( !is.na(qual.y.prox) ) %>%
  group_by(qual.y.prox) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Y prox", t$qual.y.prox, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# time lag
update_result_csv( name = "Perc time lag 0",
                   section = 0,
                   value = round( 100 * mean( d$y.lag.days == 0, na.rm = TRUE ), 0 ),
                   print = FALSE )
update_result_csv( name = "Median time lag when >0",
                   section = 0,
                   value = round( median( d$y.lag.days[ !is.na(d$y.lag.days) & d$y.lag.days > 0 ] ), digits ),
                   print = TRUE )
update_result_csv( name = "Max time lag",
                   section = 0,
                   value = round( max( d$y.lag.days[ !is.na(d$y.lag.days) & d$y.lag.days > 0 ] ), digits ),
                   print = TRUE )

# interpretation of outcome
table( grepl("Reduce", d$interpretation) )
unique( d$interpretation[grepl("Reduce", d$interpretation)] )
unique( d$interpretation[!grepl("Reduce", d$interpretation)] )

update_result_csv( name = "Y interpret reduce",
                   section = 0,
                   value = round( 100 * mean( grepl("Reduce", d$interpretation), na.rm = TRUE ), 0 ),
                   print = FALSE )
update_result_csv( name = "Y interpret absolute",
                   section = 0,
                   value = round( 100 * mean( !grepl("Reduce", d$interpretation), na.rm = TRUE ), 0 ),
                   print = FALSE )


################################# TABLE 2 (RISKS OF BIAS AT ARTICLE LEVEL) #################################

##### Make High-Quality Variable #####
# how many meet bar of high quality?
d = d %>% mutate( hi.qual = grepl("RCT", design) == TRUE & 
                    #qual.y.prox %in% c("Self-reported", "Actual behavior") &
                    qual.exch %in% c("a.Low", "b.Medium") &
                    qual.sdb %in% c("a.Low", "b.Medium") &  # this is the killer
                    qual.gen %in% c("a.Low", "b.Medium") &
                    !is.na(qual.missing) & qual.missing < 15 )   # reducing this to 5 doesn't change number of studies

table(d$hi.qual)
unique( d$authoryear[ d$hi.qual == TRUE ] )
length( unique( d$authoryear[ d$hi.qual == TRUE ] ) )

# compare to my personal list of methodological favorites
#unique( d$authoryear[ d$mm.fave == 1 ] )
# good! seems to line up well :)

##### Make Table #####
quality.vars = c( "design", names(d)[ grepl("qual", names(d)) ] )

median.vars = c("qual.missing")

setwd(results.dir)
t = CreateTableOne(data=d[,quality.vars], includeNA = TRUE)
t = print(t, nonnormal = median.vars)

setwd(results.dir)
setwd("Tables to prettify")
write.csv(print(t), "study_quality_table.csv")




# percent randomized
update_result_csv( name = "Perc randomized",
                   section = 0,
                   value = round( 100 * mean(d$randomized, na.rm = TRUE), 0 ),
                   print = FALSE )

# percent with public data
update_result_csv( name = "Perc public data",
                   section = 0,
                   value = round( 100 * mean(d$qual.public.data == "Yes", na.rm = TRUE), 0 ),
                   print = TRUE )

# percent with public code
update_result_csv( name = "Perc public code",
                   section = 0,
                   value = round( 100 * mean(d$qual.public.code == "Yes", na.rm = TRUE), 0 ),
                   print = TRUE )

# percent preregistered
update_result_csv( name = "Perc prereg",
                   section = 0,
                   value = round( 100 * mean(d$qual.prereg == "Yes", na.rm = TRUE), 0 ),
                   print = TRUE )

# median missing data
update_result_csv( name = "Median missing",
                   section = 0,
                   value = round( median(d$qual.missing, na.rm = TRUE), digits ),
                   print = TRUE )
update_result_csv( name = "Perc missing NR",
                   section = 0,
                   value = round( 100*mean( is.na(d$qual.missing) ), 0 ),
                   print = FALSE )

# percent using intended or self-reported consumption/purchase
update_result_csv( name = "Perc self-reported or intended",
                   section = 0,
                   value = round( 100 * mean( d$qual.y.prox != "Actual", na.rm = TRUE ), 0 ),
                   print = FALSE )

# percent strong or medium on each subjective variable
update_result_csv( name = "Perc qual.exch okay",
                   section = 0,
                   value = round( 100 * mean(d$qual.exch %in% c("a.Low", "b.Medium"), na.rm = TRUE), 0 ),
                   print = FALSE )
update_result_csv( name = "Perc qual.sdb okay",
                   section = 0,
                   value = round( 100 * mean(d$qual.sdb %in% c("a.Low", "b.Medium"), na.rm = TRUE), 0 ),
                   print = FALSE )
update_result_csv( name = "Perc qual.gen okay",
                   section = 0,
                   value = round( 100 * mean(d$qual.gen %in% c("a.Low", "b.Medium"), na.rm = TRUE), 0 ),
                   print = TRUE )

# percent high-quality
update_result_csv( name = "Perc ests hi.qual",
                   section = 0,
                   value = round( 100 * mean(d$hi.qual, na.rm = TRUE), 0 ),
                   print = TRUE )

update_result_csv( name = "Unique articles hi.qual",
                   section = 0,
                   value = length( unique( d$authoryear[ d$hi.qual == 1 ] ) ),
                   print = TRUE )


# bm: separate the quality variables into study-level and intervention-level (and maybe separate the tables that was
#  as well?)
#  for example, public data/code should be at study level


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 1. MAIN ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# 1. OVERALL META-ANALYSIS #################################

##### Robust Meta-Analysis ######
# also reproduced in the subset analyses, but need to run here for 
#  forest plotting joy 
# allows for correlated point estimates within studies
# and has no distributional assumptions
( meta.rob = robu( logRR ~ 1, 
                   data = d, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )


mu = meta.rob$b.r
t2 = meta.rob$mod_info$tau.sq
mu.lo = meta.rob$reg_table$CI.L
mu.hi = meta.rob$reg_table$CI.U
mu.se = meta.rob$reg_table$SE
mu.pval = meta.rob$reg_table$prob


update_result_csv( name = "Overall muhat RR",
                   section = 1,
                   value = round( exp(mu), digits ),
                   print = FALSE )
update_result_csv( name = "Overall muhat RR-1",
                   section = 1,
                   value = round( 100 * (exp(mu) - 1), 0 ),
                   print = FALSE )
update_result_csv( name = "Overall muhat RR lo",
                   section = 1,
                   value = round( exp(mu.lo), digits ),
                   print = FALSE )
update_result_csv( name = "Overall muhat RR hi",
                   section = 1,
                   value = round( exp(mu.hi), digits ),
                   print = FALSE )
update_result_csv( name = "Overall muhat RR pval",
                   section = 1,
                   value = MetaUtility::format_stat( mu.pval, cutoffs = c(.01, pval.cutoff) ),
                   print = FALSE )
update_result_csv( name = "Overall tau logRR",
                   section = 1,
                   value = round( sqrt(t2), digits ),
                   print = TRUE )


##### Check Normality ######
# use only main estimates here
std = (d$logRR - c(mu)) / sqrt(c(t2) + d$varlogRR)
hist(std, breaks=20)
shapiro.test(std)
# reasonable


################################# CALIBRATED ENSEMBLE ESTIMATES #################################

##### Plot the Ensemble Estimates #####
d$ens = my_ens(yi = d$logRR, 
               sei = sqrt(d$varlogRR))

ggplot( data = d,
            aes(x = exp(d$ens))) +
  
  # null
  geom_vline(xintercept = 1,
             color = "black",
             lty = 2) +
  
  geom_density(lwd = 1) +
  
  # pooled point estimate
  geom_vline(xintercept = exp(mu),
             color = "red",
             lty = 2) +
  
  xlab("Risk ratio of low vs. high meat") +
  ylab("Kernel estimate of true effect density") +
  
  scale_x_continuous( limits = c(0.8, 1.8),
                      breaks = seq(0.8, 1.8, .1) ) +
  
  theme_classic() +
  theme( axis.text.y = element_blank(),
        axis.ticks.y = element_blank() )


setwd(results.dir)
ggsave( "ensemble_density.pdf",
        width = 8,
        height = 6 )


setwd(overleaf.dir)
ggsave( "ensemble_density.pdf",
        width = 8,
        height = 6 )




##### Studies with Best Ensemble Estimates #####
# handful of studies with best ensemble estimates
( best.ens = d$unique[ order(d$ens, decreasing = TRUE) ][1:8] )

# ...vs. handful with best point estimates
( best.est = d$unique[ order(d$logRR, decreasing = TRUE) ][1:8] )

# interesting...only 6/8 best point estimates are among 5 best ensemble estimates!
sum(best.ens %in% best.est)

# look at characteristics of best interventions
# interesting that all are no-request interventions
View(d[d$unique %in% best.ens,])


################################# FOREST PLOT #################################

# relative weight of each study in meta-analysis
d$rel.wt = NA
d$rel.wt = 100 * (1/d$varlogRR) / sum(1/d$varlogRR )


# lean plotting df
dp = d

# sort by ensemble estimate
dp = dp[ order(dp$ens, decreasing = FALSE), ]
#dp = dp[ order(dp$logRR, decreasing = FALSE), ]


# add pooled point estimates as first rows
# arbitrary relative weight
library(dplyr)
dp = add_row( dp,
              .before = 1,
              logRR = meta.rob$b.r,
              ens = NA,
              varlogRR = meta.rob$reg_table$SE^2,
              RR.lo = exp(mu.lo),
              RR.hi = exp(mu.hi),
              #X.cat = "pooled",
              #Y.cat = "pooled",
              unique = "POOLED",
              borderline = 0,
              rel.wt = 5 )


# make sure POOLED is the first level of the factor variable so it's displayed at bottom
correct.order = dp$unique
dp$unique = factor(dp$unique, levels = correct.order)
levels(dp$unique)


dp$is.pooled = as.factor( c( "Pooled estimate", rep("Individual study", nrow(dp) - 1) ) )


#colors = c("red", "black", "blue")

shapes = c(10, 19, 15)
#breaks = seq(0.4, 6.0, .2)

breaks = c( seq(0.4, 1.3, .1),
            1.5,
            seq(1.5, 4, .5),
            seq(4, 6, 1) )

#breaks = exp( seq( log(0.4), log(6), .2 )

# for borderline status
colors = c("black", "orange")

library(ggplot2)
base = ggplot( data = dp, aes( x = exp(logRR), 
                               y = unique,
                               size = rel.wt,
                               shape = is.pooled,
                               color = as.factor(borderline) ) ) +
  geom_point() +
  #color = X.intensiveness ) ) +
  geom_errorbarh( aes(xmin = RR.lo,
                      xmax = RR.hi ),
                  lwd = .5,
                  height = .001) +
  
  # calibrated estimates
  geom_point( data = dp, aes( x = exp(ens),
                              y = unique ),
              size = 3,
              shape = 124,
              color = "red") +
  
  xlab( "Estimated relative risk of low vs. high meat" ) +
  ylab("") +
  
  geom_vline(xintercept = 1, lty = 2) +
  
  guides(size = guide_legend("% weight in analysis") ) +
  
  scale_color_manual(values = colors,
                     name = "Borderline inclusion") +
  
  scale_shape_manual(values = shapes,
                     name = "") +
  #guide=FALSE) +
  
  # scale_x_continuous( breaks = breaks,
  #                     lim = c(breaks[1], breaks[length(breaks)] ),
  #                     trans = "log10") +
  
  scale_x_continuous( breaks = breaks, trans = "log10") +
  # https://michaelbach.de/2012/07/22/R-ggplot2-and-axis-limits-unexpected-behaviour-solution.html
  coord_cartesian( xlim = c(breaks[1], breaks[length(breaks)] ) ) +
  
  theme_bw()

#facet_wrap( ~Y.cat)  # to facet by outcome measure type

base

setwd(results.dir)
ggsave( "forest.pdf",
        width = 15,
        height = 10 )


setwd(overleaf.dir)
ggsave( "forest.pdf",
        width = 15,
        height = 10 )


################################# NPPHAT :D #################################

npphat.from.scratch = FALSE 


if (npphat.from.scratch == TRUE) {
  ##### Make Plotting Dataframe #####
  q.vec = seq( 0, log(2), 0.01 )
  ql = as.list(q.vec)
  
  # again pass threshold and calibrated estimates on log-RR scale
  Phat.above.vec = lapply( ql,
                           FUN = function(.q) mean( d$ens > .q, na.rm = TRUE ) )
  
  res = data.frame( q = q.vec,
                    Est = unlist(Phat.above.vec) )
  
  
  ##### Selective Bootstrapping #####
  
  # # look at just the values of q at which Phat jumps
  # #  this will not exceed the number of point estimates in the meta-analysis
  res.short = res[ diff(res$Est) != 0, ]
  
  library(dplyr)
  
  # # bootstrap a CI using only the values of q at which Phat jumps
  q.jump = res.short$q
  
  temp.l = lapply( q.jump, 
                   FUN = function(.q) prop_stronger( q = .q, 
                                                     tail = "above",
                                                     estimate.method = "calibrated",
                                                     ci.method = "calibrated",
                                                     dat = d,
                                                     R = 2000 ) )
  
  temp.df = do.call( rbind, temp.l )
  temp.df$q = q.jump
  
  
  # merge this with the full-length res dataframe, merging by Phat itself
  res = merge( res, temp.df, by.x = "q", by.y = "q")
  
  # turn into percentage
  res$Est = 100*res$Est.x # ~~~ why are there 2 different estimates that are different??
  res$lo = 100*res$lo
  res$hi = 100*res$hi
  
  setwd(results.dir)
  write.csv(res, "npphat_results.csv")
  
} else {
  setwd(results.dir)
  res = read.csv("npphat_results.csv")
}

# remove last row because CI is NA
res = res[ -nrow(res), ]

##### Make Plot #####
library(ggplot2)

ggplot( data = res,
        aes( x = exp(q),
             y = Est ) ) +
  theme_bw() +
  
  # pooled point estimate
  geom_vline( xintercept = exp(mu),
              lty = 2,
              color = "red" ) +
  
  scale_y_continuous(  breaks = seq(0, 100, 10) ) +
  # stop x-axis early because last CI is NA
  scale_x_continuous(  breaks = seq(1, exp(.56), .1) ) +
  
  geom_line(lwd=1.2) +
  
  xlab("Threshold (RR scale)") +
  ylab( paste( "Estimated percent of effects above threshold" ) ) +
  
  geom_ribbon( aes(ymin=res$lo, ymax=res$hi), alpha=0.15, fill = "black" ) 

# # parametric CI
# geom_ribbon( aes(ymin=res$CI.lo.param, ymax=res$CI.hi.param), alpha=0.15, fill = "blue" )   

# 8 x 6 works well

setwd(results.dir)
ggsave( "npphat.pdf",
        width = 8,
        height = 6 )


setwd(overleaf.dir)
ggsave( "npphat.pdf",
        width = 9,
        height = 6 )


##### Add to Results a Few Selected Values ####

#### Phat > 1 #####
update_result_csv( name = "Phat above 1",
                   section = 1,
                   value = round( res$Est[ res$q == log(1) ], 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1 lo",
                   section = 1,
                   value = round( res$lo[ res$q == log(1) ], 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1 hi",
                   section = 1,
                   value = round( res$hi[ res$q == log(1) ], 0 ),
                   print = TRUE )

#### Phat > 1.1 #####
# this exact value of q isn't in the dataframe above
Phat = prop_stronger( q = log(1.1), 
                      tail = "above",
                      estimate.method = "calibrated",
                      ci.method = "calibrated",
                      dat = d,
                      R = 2000 )
update_result_csv( name = "Phat above 1.1",
                   section = 1,
                   value = round( 100 * Phat$Est,  ),
                   print = FALSE )
update_result_csv( name = "Phat above 1.1 lo",
                   section = 1,
                   value = round( 100 * Phat$lo, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1.1 hi",
                   section = 1,
                   value = round( 100 * Phat$hi, 0 ),
                   print = FALSE )


#### Phat > 1.2 #####
Phat = prop_stronger( q = log(1.2), 
                            tail = "above",
                            estimate.method = "calibrated",
                            ci.method = "calibrated",
                            dat = d,
                            R = 2000 )
update_result_csv( name = "Phat above 1.2",
                   section = 1,
                   value = round( 100 * Phat$Est,  ),
                   print = FALSE )
update_result_csv( name = "Phat above 1.2 lo",
                   section = 1,
                   value = round( 100 * Phat$lo, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1.2 hi",
                   section = 1,
                   value = round( 100 * Phat$hi, 0 ),
                   print = FALSE )


#### Phat < 0.90 #####

Phat.below = prop_stronger( q = log(.9), 
                            tail = "below",
                            estimate.method = "calibrated",
                            ci.method = "calibrated",
                            dat = d,
                            R = 2000 )

update_result_csv( name = "Phat below 0.9",
                   section = 1,
                   value = round( 100*Phat.below$Est, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat below 0.9 lo",
                   section = 1,
                   value = round( 100*Phat.below$lo, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat below 0.9 hi",
                   section = 1,
                   value = round( 100*Phat.below$hi, 0 ),
                   print = TRUE )



#### Phat < 1 #####

Phat.below = prop_stronger( q = log(1), 
                            tail = "below",
                            estimate.method = "calibrated",
                            ci.method = "calibrated",
                            dat = d,
                            R = 2000 )

update_result_csv( name = "Phat below 1",
                   section = 1,
                   value = round( 100*Phat.below$Est, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat below 1 lo",
                   section = 1,
                   value = round( 100*Phat.below$lo, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat below 1 hi",
                   section = 1,
                   value = round( 100*Phat.below$hi, 0 ),
                   print = TRUE )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                SECONDARY EFFECT SIZE CODINGS          
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# my_robu( dat = d.veg,
#          # yi.name = "yi",
#          # vi.name = "vi",
#          take.exp = TRUE )
# 
# 
# my_robu( dat = d.grams,
#          take.exp = FALSE )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              2. SENSITIVITY ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################# PUBLICATION BIAS ################################# 

# ~~~ also do these analyses excluding the 2 extreme ones (non.extreme.logRR)
# to this end, put this script in a loop and make a nice table



##### Hedges Selection Model #####
# be careful about inference due to correlated point estimates
# can't fit model with 3 cutoffs because there are no significant negative studies
library(weightr)
( m1 = weightfunct( effect = d$logRR,
                    v = d$varlogRR,
                    steps = c(0.025, 1),
                    table = TRUE ) )
# actually makes the estimate larger
H = m1[[2]]$hessian
ses = sqrt( diag( solve(H) ) )

update_result_csv( name = "weightr mu",
                   section = 2,
                   value = round( exp(m1[[2]]$par[2]), digits ),
                   print = FALSE )
update_result_csv( name = "weightr mu lo",
                   section = 2,
                   value = round( exp(m1[[2]]$par[2] - qnorm(.975) * ses[2]), digits ),
                   print = FALSE )
update_result_csv( name = "weightr mu hi",
                   section = 2,
                   value = round( exp(m1[[2]]$par[2] + qnorm(.975) * ses[2]), digits ),
                   print = FALSE )
update_result_csv( name = "weightr mu pval",
                   section = 2,
                   value = format_stat( 2 * ( 1 - pnorm( abs(m1[[2]]$par[2]) / ses[2] ) ) ),
                   print = FALSE )


##### S-values ######
# s-values to reduce to null
( res = svalue( yi = d$logRR,
                vi = d$varlogRR,
                q = log(1), 
                clustervar = d$authoryear,
                model = "robust" ) )
# N.P. for both point estimate and CI
update_result_csv( name = "sval est to 1",
                   section = 2,
                   value = res$sval.est,
                   print = FALSE )
update_result_csv( name = "sval CI to 1",
                   section = 2,
                   value = res$sval.ci,
                   print = FALSE )


# s-values to reduce effect size to RR=1.1
( res = svalue( yi = d$logRR,
                vi = d$varlogRR,
                q = log(1.1), 
                clustervar = d$authoryear,
                model = "robust" ) )
# N.P. for estimate
update_result_csv( name = "sval est to 1.1",
                   section = 2,
                   value = res$sval.est,
                   print = FALSE )
update_result_csv( name = "sval CI to 1.1",
                   section = 2,
                   value = round( res$sval.ci, 2 ),
                   print = FALSE )


##### Worst-Case Meta-Analysis ######
# affirmative vs. non-affirmative
d$pval = 2 * ( 1 - pnorm( abs(d$logRR) / sqrt(d$varlogRR) ) )
d$affirm = d$pval < 0.05 & d$logRR > 0
table(d$affirm)

# meta-analyze only the nonaffirmatives
# 2-sided pval
( meta.worst = robu( yi ~ 1, 
                     data = d[ d$affirm == FALSE, ], 
                     studynum = authoryear,
                     var.eff.size = varlogRR,
                     modelweights = "HIER",
                     small = TRUE) )
mu.worst = meta.worst$b.r
t2.worst = meta.worst$mod_info$tau.sq
mu.lo.worst = meta.worst$reg_table$CI.L
mu.hi.worst = meta.worst$reg_table$CI.U
mu.se.worst = meta.worst$reg_table$SE
pval.worst = meta.worst$reg_table$prob

update_result_csv( name = "k affirm",
                   section = 2,
                   value = sum(d$affirm),
                   print = TRUE )
update_result_csv( name = "k nonaffirm",
                   section = 2,
                   value = sum(d$affirm == 0),
                   print = TRUE )
update_result_csv( name = "Worst mu",
                   section = 2,
                   value = round( exp(mu.worst), digits),
                   print = FALSE )
update_result_csv( name = "Worst mu lo",
                   section = 2,
                   value = round( exp(mu.lo.worst), digits),
                   print = FALSE )
update_result_csv( name = "Worst mu hi",
                   section = 2,
                   value = round( exp(mu.hi.worst), digits),
                   print = FALSE )
update_result_csv( name = "Worst mu pval",
                   section = 2,
                   value = round(pval.worst, 3),
                   print = TRUE )


##### Significance Funnel ######
significance_funnel2(yi = d$logRR,
                     vi = d$varlogRR,
                     est.N = mu.worst,
                    est.all = mu )

setwd(results.dir)
ggsave( "funnel.pdf",
        width = 8,
        height = 6 )

setwd(overleaf.dir)
ggsave( "funnel.pdf",
        width = 8,
        height = 6 )




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              3. SUBSET ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# RUN ALL MODERATOR AND SUBSET ANALYSES #################################

if( exists("resE") ) rm(resE)

# for Bonferroni
# don't penalize since these are sensitivity analyses
n.tests = 1

# for Phat
ql = c(log(1), log(1.1), log(1.2))
boot.reps = 500  # ~~ increase later

digits = 2


# two studies have really huge logRRs, as can be seen in significance funnel
d$non.extreme.logRR = d$logRR < 1.5


##### Overall and Subset Analyses #####


subsets = list( d,
                d %>% filter( !is.na(borderline) & borderline == 0 ),
                d %>% filter( !is.na(x.pure.animals) & x.pure.animals == 1 ),
                d %>% filter( !is.na(hi.qual) & hi.qual == 0 ),
                d %>% filter( !is.na(design) & grepl("RCT", design) == 1 ),
                d %>% filter( !is.na(reproducible) & reproducible == 1 ),
                d %>% filter( !is.na(non.extreme.logRR) & non.extreme.logRR == 1 ) )


subset.labels = c( "Overall",
                   "Non-borderline",
                   "Animal welfare only",
                   "High quality",
                   "Randomized",
                   "Preregistered with open data",
                   "Exclude two extreme estimates" )

for (i in 1:length(subsets)) {
  analyze_one_meta( dat = subsets[[i]],
                    meta.name = subset.labels[i],
                    yi.name = "logRR",
                    vi.name = "varlogRR",
                    digits = digits,
                    boot.reps = boot.reps, 
                    ql = ql,
                    take.exp = TRUE,
                    n.tests = n.tests)
}





# simplify and prettify
resE = resE %>% select( Meta, k, Est, Pval, Tau, `Percent above 1`, `Percent above 1.1`, `Percent above 1.2`)

# save results
setwd(results.dir)
setwd("Tables to prettify")
write.csv(resE, "subsets_table.csv", row.names = FALSE)


# note: for some subsets, tau = 0, hence 0 estimate for certain Phats

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              4. MODERATOR ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Correlation Among Numeric Moderators ####
vars = c(
  "x.has.text",
  "x.has.visuals",
  "x.suffer",
  "x.pushy",
  "x.long",
  "y.long.lag",
  "perc.male.10"
)
cor.mat = cor(d[,vars], use = "pairwise.complete.obs")
cor.mat = round( cor.mat, 2 )

setwd(results.dir)
setwd("Tables to prettify")
write.csv(cor.mat, "moderator_cormat.csv", row.names = FALSE)

##### Moderators in One Big Model #####
if( exists("resE") ) rm(resE)

# ~~~ maybe add country (U.S. vs. other) to this?
moderators = c(
              #"published",
               "x.has.text",
               "x.has.visuals",
               "x.suffer",
               "x.pushy",
               "x.long",
               "y.long.lag",
               "qual.y.prox2",
               "perc.male.10"
               )

linpred.string = paste( moderators, collapse=" + ")
string = paste( "logRR ~ ", linpred.string, collapse = "")

( meta = robu( eval( parse( text = string ) ), 
               data = d, 
               studynum = as.factor(authoryear),
               var.eff.size = varlogRR,
               modelweights = "HIER",
               small = TRUE) )

est = meta$b.r
t2 = meta$mod_info$tau.sq
mu.lo = meta$reg_table$CI.L
mu.hi = meta$reg_table$CI.U
mu.se = meta$reg_table$SE
pval = meta$reg_table$prob
V = meta$VR.r  # variance-covariance matrix

# report tau in this model
update_result_csv( name = "Meta-regression tau",
                   section = 4,
                   value = round( sqrt( meta$mod_info$tau.sq ), 2 ),
                   print = TRUE )


# estimates for text
ests = round( exp(est), 2 )
pvals2 = format_stat(pval)
pvals2[ pval < 0.01 ] = round(pval[ pval < 0.01 ], 3)
update_result_csv( name = paste( "Meta-regression est", meta$labels ),
                   section = 4,
                   value = ests,
                   print = TRUE )
update_result_csv( name = paste( "Meta-regression lo", meta$labels ),
                   section = 4,
                   value = format_stat( exp(mu.lo) ),
                   print = TRUE )
update_result_csv( name = paste( "Meta-regression hi", meta$labels ),
                   section = 4,
                   value = format_stat( exp(mu.hi) ),
                   print = TRUE )
update_result_csv( name = paste( "Meta-regression pval", meta$labels ),
                   section = 4,
                   value = pvals2,
                   print = TRUE )


##### Meta-Regression Table #####
CIs = format_CI( exp( mu.lo ),
                 exp( mu.hi ) )
temp = data.frame( Moderator = meta$labels, 
                   EstCI = paste( ests, CIs, sep = " " ),
                   Pval = pvals2 )

# save results
setwd(results.dir)
setwd("Tables to prettify")
write.csv(temp, "meta_regression_table.csv", row.names = FALSE)


# ##### Moderator Forest Plot Hell Yeah #####
# # these are the least-squares means for each group, NOT the coefficient estimates
# # calculate estimated RR for each characteristic using the intercept
# int = est[1]
# bhat = est[2: length(est)]
# log.RRs = int + bhat
# 
# # SE of RR itself rather than the model coefficient
# # ~~~ note that this does not use the small-sample correction but rather 
# #  asymptotic normality
# # ~~~ email Fisher and Tipton about this
# n.mods = length(est) - 1
# se.RR = vapply( X = 1:n.mods, 
#                 FUN = function(i) sqrt( V[1,1] + V[(i+1),(i+1)] + 2*V[1,(i+1)] ),
#                 FUN.VALUE = -99 )
# # i+1 because V's first entry is for the intercept itself
# 
# # sanity check for second moderator
# sqrt( 0.0330520616 + 0.0064163250 + 2*-0.0080392687 )
# 
# # make plotting dataframe
# dp = data.frame( mod = meta$labels[-1],
#                  RR = exp(log.RRs),
#                  lo = exp( log.RRs - qnorm(.975) * se.RR ),
#                  hi = exp( log.RRs + qnorm(.975) * se.RR ),
#                  pval = pval[-1] )
# 
# ggplot( data = dp,
#         aes( x = RR,
#              y = mod) ) +
#   # pooled point estimate
#   geom_vline( xintercept = exp(mu),
#               lty = 2, 
#               color = "red" ) +
#   
#   # null
#   geom_vline( xintercept = 1,
#               lty = 2, 
#               color = "black" ) +
#   
#   geom_point(size = 2) +
#   geom_errorbarh( aes(xmin = lo, 
#                       xmax = hi,
#                       height = 0.001 ) ) +
#   scale_x_continuous( breaks = seq(0.5, 3.5, .1)) +
#   theme_bw()
  



################################# CONTINUOUS MODERATOR PLOTS #################################

##### time lag from intervention to outcome measurement #####
ggplot( data = dp, aes( x = y.lag.days,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4,
             alpha = 1) + 
  geom_smooth() +
  xlab("Days elapsed between intervention and outcome") +
  ylab("True effect estimate (RR)") +
  geom_hline(yintercept = 1, lty = 2) +
  
  # point of dichotomization
  geom_vline(xintercept = 7, lty = 2, color = "red") +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_shape_manual(values = 1:50) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw()


##### time exposed to intervention
ggplot( data = dp, aes( x = x.min.exposed,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4) + 
  geom_smooth() +
  xlab("Total time exposed to intervention (min)") +
  ylab("True effect estimate (RR)") +
  
  # point of dichotomization
  geom_vline(xintercept = 5, lty = 2) +
  scale_shape_manual(values = 1:50) +
  # scale_x_continuous(limits = c(0,110),
  #                    breaks = seq(0,95,5)) +
  scale_x_log10( limits = c(1,100),
                 breaks = c(1,5,10,30,100)) +
  theme_bw()



##### percent male subjects


# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE
# mu.pval = meta.rob$reg_table$prob



