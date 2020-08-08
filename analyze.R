

################################# PREP ################################# 

# should we remove existing results file instead of overwriting individual entries? 
start.res.from.scratch = TRUE
# should we redo the time-consuming NPPhat bootstrapping?
npphat.from.scratch = FALSE 
# should we use the grateful package to scan and cite packages?
cite.packages.anew = FALSE

if ( start.res.from.scratch == TRUE ) {
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# for some reason, checkpoint generates an error for metafor,
#  so this one is exempt from the checkpoint installation
library(metafor)

# load packages
# this will reinstall the versions of all packages as they existed on 
#  the date MBM analyzed
# https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
library(checkpoint)
checkpoint("2020-02-12")


detach("package:plyr", unload=TRUE)  # is a PITA if using dplyr
library(dplyr)
library(ICC)

library(robumeta)
library(MetaUtility)
library(weightr)
library(PublicationBias)
library(tableone)
library(readxl)
library(testthat)
library(corrr)
library(ggplot2)
library(xtable)
library(RColorBrewer)
library(irr)
library(tidyverse)
library(readxl)
library(grateful)


data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Analysis/awr_analysis_git"
results.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Analysis/Results from R"
# we write results straight to Overleaf-connected DropBox folder for error-proofing
overleaf.dir = "~/Dropbox/Apps/Overleaf/AWR (animal welfare interventions review)/R_objects"
# directory with interrater reliability stats from Covidence
irr.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Literature search/Interrater reliability from Covidence"
# directory with Scherer's per-capita animal deaths data
scherer.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction/Scherer animal slaughter data"

# helper fns
setwd(code.dir); source("helper_analysis.R")

# for formatting stats
digits = 2  # rounding
pval.cutoff = 10^-4  # threshold for using "<"
options(scipen=999)  # disable scientific notation
boot.reps = 2000


##### Read In The Main Meta-Analytic Datasets #####

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")
d = d %>% filter( !is.na(authoryear) )  # because that table has blank rows for prettiness

# dataset that still includes the SSWS
d.chal = d[ d$use.rr.analysis == 1, ]
d.chal = droplevels(d.chal)

# main-analysis dataset without SSWS
expect_equal( sum( is.na(d$exclude.main) ), 0 )  # indicator for being a high-bias challenge should never be NA
d = d[ d$use.rr.analysis == 1 & d$exclude.main == 0, ]  
d = droplevels(d)
# fix variable type after read-in
d$n.paper = as.numeric( as.character(d$n.paper) )

# article-level dataset
# 1 row per article instead of per point estimate
d.arts = d[ !duplicated(d$authoryear), ]


##### Read In Other Datasets for Misc Purposes #####

# for counting hopeless studies
setwd(data.dir)
# NOTE: this step will break if cell values are hyphenated! 
d2 = read_xlsx("Extracted qualitative data.xlsx", na = "NR")
# remove missing rows
d2 = d2 %>% filter(!is.na(`First author last name`))

# subjective quality variables
# these are already in the dataset above, but the following allows assessment
#  of interrater reliability
setwd(data.dir)
setwd("Dual review of quality")
d3 = read.csv("subjective_data_full_prepped.csv")

# coders' datasets for intervention components
# these are already in the dataset above, but the following allows assessment
#  of interrater reliability
setwd(data.dir)
setwd("Dual review of intervention components")
dd = read.csv("component_coding_dr_prepped.csv")
dm = read.csv("component_coding_mm_prepped.csv")

# interrater reliability stats from Covidence
setwd(irr.data.dir)
d4 = read.csv("irr_title_abstract_screening.csv") 
d5 = read.csv("irr_full_text_screening.csv") 

# Scherer data (just used for a statistic in the Intro)
setwd(scherer.data.dir)
ds = read_xlsx("scherer_data.xlsx", sheet = "welfare_AL")
names(ds)[1] = "country"

##### Generate Citations for R Packages #####

if ( cite.packages.anew == TRUE ) {
  setwd(code.dir)
  cite_packages()
  # need to manually move the citations.html file
  
  # also look through the data prep code for additional packages
  code.dir.prep = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction/awr_data_extraction_git"
  setwd(code.dir.prep)
  cite_packages()
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                             0. CHARACTERISTICS OF INCLUDED STUDIES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# BASICS #################################

# MM audited 2020-2-1

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
                   value = round(median(d.arts$n.paper)), # round in case it falls between two values
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

# earliest publication date
update_result_csv( name = "Earliest year",
                   section = 0,
                   value = sort(d$year)[1],  # it's a factor variable
                   print = FALSE )

# how we obtained point estimates
t = d %>% group_by(stats.source) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )
             
update_result_csv( name = paste( "Stats source", t$stats.source, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# presence of multiple eligible food outcomes
update_result_csv( name = "Perc articles multiple elig foods",
                   section = 0,
                   value = round( 100 * mean( d.arts$y.other.eligible.foods ) ),
                   print = TRUE )

update_result_csv( name = "Perc articles multiple elig foods formed composite",
                   section = 0,
                   value = round( 100 * mean( grepl(pattern = "composite", d.arts$prose.y.other.eligible[ d.arts$y.other.eligible.foods == 1 ] ) ) ),
                   print = TRUE )

# look at articles whose additional outcomes were ALL not contributing to the composite
d.arts %>% filter( !is.na(y.other.eligible.foods == 1) & 
                     y.other.eligible.foods == 1 &
                     grepl(pattern = "composite", prose.y.other.eligible ) == FALSE ) %>%
  select( authoryear,
          prose.y.other.eligible )
# only Caldwell

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

# number of high-bias challenge estimates analyzed (non-hopeless)
update_result_csv( name = "k ests SSWS",
                   section = 0,
                   value = sum( d.chal$exclude.main == 1 ),
                   print = TRUE )


##### Count Hopeless Articles/Studies in Various Categories #####
# sanity check: this should match the separate table in Appendix
( hopeless = d2 %>% filter( `Stats source (public data, data from author, paper, hopeless)` == "Hopeless" ) %>%
  group_by(`Excluded challenge`, `First author last name`, `Year`) %>%
  summarise( k = n() ) )


update_result_csv( name = "Number articles hopeless",
                   section = 0,
                   # this works because table has 1 row per article
                   value = length( hopeless$`First author last name`[ hopeless$`Excluded challenge` == 0 ] ),
                   print = TRUE )
update_result_csv( name = "Number estimates hopeless",
                   section = 0,
                   value = sum( hopeless$k[ hopeless$`Excluded challenge` == 0 ] ),
                   print = TRUE )



################################# TABLE 2 (INDIVIDUAL STUDY CHARACTERISTICS) #################################

# MM audited 2020-8-7

t = table1_add_row( x = d$country,
                    var.header = "Country",  # variable name to use in table
                    type = "cat",
                    countNA = TRUE,
                    .tab1 = NULL )

t = table1_add_row( x = d$perc.male,
                    var.header = "Percent male subjects",  # variable name to use in table
                    type = "cont",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$age,
                    var.header = "Average age",  # variable name to use in table
                    type = "cont",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$students,
                    var.header = "Subjects' student status",  # variable name to use in table
                    type = "cat",
                    countNA = TRUE,
                    .tab1 = t )


t = table1_add_row( x = d$x.has.text,
                    var.header = "Intervention had text",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.has.visuals,
                    var.header = "Intervention had visuals",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.suffer,
                    var.header = "Intervention had graphic content",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )


t = table1_add_row( x = d$x.mind.attr,
                    var.header = "Intervention used mind attribution",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.soc.norm,
                    var.header = "Intervention used social norms",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )


t = table1_add_row( x = d$x.id.victim,
                    var.header = "Intervention identified a named victim",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )


t = table1_add_row( x = d$x.pets,
                    var.header = "Intervention depicted pets",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.impl,
                    var.header = "Intervention gave implementation suggestions",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.pure.animals,
                    var.header = "Intervention was animal welfare only",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.tailored,
                    var.header = "Intervention was personally tailored",  # variable name to use in table
                    type = "bin01",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.rec,
                    var.header = "Intervention's recommendation",  # variable name to use in table
                    type = "cat",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$x.min.exposed,
                    var.header = "Intervention's duration (minutes)",  # variable name to use in table
                    type = "cont",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$y.cat,
                    var.header = "Outcome category",  # variable name to use in table
                    type = "cat",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$y.lag.days,
                    var.header = "Length of follow-up (days)",  # variable name to use in table
                    type = "cont",
                    countNA = TRUE,
                    .tab1 = t )

t = table1_add_row( x = d$stats.source,
                    var.header = "Source of statistics",  # variable name to use in table
                    type = "cat",
                    countNA = TRUE,
                    .tab1 = t )

setwd(results.dir)
setwd("Tables to prettify")
write.csv( t, "study_char_table.csv", row.names = FALSE )



################################# SUBJECT CHARACTERISTICS FOR IN-TEXT REPORTING #################################

# MM audited 2020-8-7

update_result_csv( name = "Median perc male",
                   section = 0,
                   value = round( median(d$perc.male, na.rm=TRUE), 0 ),
                   print = FALSE )

update_result_csv( name = "Median avg age",
                   section = 0,
                   value = round( median(d$age, na.rm=TRUE), 1 ),
                   print = FALSE )

# student status
update_result_csv( name = "Perc any undergrad",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$students %in% c("General undergraduates", "Social sciences undergraduates") ) ),
                   print = FALSE )

update_result_csv( name = "k social undergrad",
                   section = 0,
                   value = sum( d$students == "Social sciences undergraduates" ),
                   print = FALSE )

update_result_csv( name = "Perc social undergrad",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$students == "Social sciences undergraduates" ) ),
                   print = FALSE )

update_result_csv( name = "Perc not undergrad",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$students == "No" ) ),
                   print = TRUE )

update_result_csv( name = "Perc student mixed",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$students == "Mixed" ) ),
                   print = TRUE )



################################# INTERVENTION CHARACTERISTICS #################################

# MM audited 2020-2-1

# interventions containing text
update_result_csv( name = "Perc interventions text",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.has.text) ),
                   print = FALSE )

# interventions containing visuals
update_result_csv( name = "Perc interventions visuals",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.has.visuals) ),
                   print = FALSE )

# interventions containing graphic suffering
update_result_csv( name = "Perc interventions graphic",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.suffer) ),
                   print = FALSE )



# subjective intervention components
update_result_csv( name = "Perc interventions mind attr",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.mind.attr) ),
                   print = FALSE )

update_result_csv( name = "Perc interventions soc norms",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.soc.norm) ),
                   print = FALSE )

update_result_csv( name = "Perc interventions pets",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.pets) ),
                   print = FALSE )

update_result_csv( name = "Perc interventions impl",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.impl) ),
                   print = FALSE )

update_result_csv( name = "Perc interventions ID victim",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.id.victim) ),
                   print = TRUE )


# interventions purely animal welfare
update_result_csv( name = "Perc interventions pure",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$x.pure.animals) ),
                   print = FALSE )

# intervention duration
update_result_csv( name = "Perc interventions short",
                   section = 0,
                   value = round( 100 * (1 - percTRUE_incl_NA(d$x.long)), 0 ),
                   print = FALSE )

# request type
t = d %>%
  group_by(x.rec) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Pushy", t$x.rec, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

# interventions not personally tailored
update_result_csv( name = "Perc interventions not tailored",
                   section = 0,
                   value = round( 100 * (1 - percTRUE_incl_NA(d$x.tailored)), 0 ),
                   print = FALSE )


################################# OUTCOME CHARACTERISTICS #################################

# outcome category (percent and n)
t = d %>%
  group_by(y.cat) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Perc Y cat", t$y.cat, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )

update_result_csv( name = paste( "k Y cat", t$y.cat, sep = " " ),
                   section = 0,
                   value = t$k,
                   print = TRUE )

# outcome proximity
t = d %>% 
  group_by(qual.y.prox) %>%
  summarise( k = n() ) %>%
  mutate( perc = round( 100 * k / sum(k) ) )

update_result_csv( name = paste( "Perc Y prox", t$qual.y.prox, sep = " " ),
                   section = 0,
                   value = t$perc,
                   print = TRUE )


update_result_csv( name = paste( "k Y prox", t$qual.y.prox, sep = " " ),
                   section = 0,
                   value = t$k,
                   print = TRUE )

# time lag
lag0 = d$y.lag.days == 0
update_result_csv( name = "Perc time lag 0",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(lag0), 0 ),
                   print = FALSE )
update_result_csv( name = "k time lag 0",
                   section = 0,
                   value = sum( lag0[ !is.na(lag0) ] ),
                   print = FALSE )
update_result_csv( name = "Median time lag when >0",
                   section = 0,
                   value = round( median( d$y.lag.days[ !is.na(d$y.lag.days) & d$y.lag.days > 0 ] ), digits ),
                   print = TRUE )
update_result_csv( name = "Max time lag",
                   section = 0,
                   value = round( max( d$y.lag.days[ !is.na(d$y.lag.days) ] ), digits ),
                   print = TRUE )

# interpretation of outcome
table( grepl("Reduce", d$interpretation) )
# sanity check for sorting them based on the "Reduce" substring:
unique( d$interpretation[grepl("Reduce", d$interpretation)] )
unique( d$interpretation[!grepl("Reduce", d$interpretation)] )

reduce = grepl("Reduce", d$interpretation)

update_result_csv( name = "Perc Y interpret reduce",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(reduce), 0 ),
                   print = FALSE )
update_result_csv( name = "k Y interpret reduce",
                   section = 0,
                   value = sum( reduce[!is.na(reduce)] ),
                   print = FALSE )

update_result_csv( name = "Perc Y interpret absolute",
                   section = 0,
                   value = round( 100 * (1 - percTRUE_incl_NA(reduce)), 0 ),
                   print = FALSE )
update_result_csv( name = "k Y interpret absolute",
                   section = 0,
                   value = sum( (1-reduce)[!is.na(reduce)] ),
                   print = FALSE )

################################# TABLE 2 (RISKS OF BIAS AT ARTICLE LEVEL) #################################


##### Make High-Quality Variable #####
# how many meet bar of high quality?
d = d %>% mutate( hi.qual = (randomized == TRUE) & 
                    qual.exch %in% c("a.Low", "b.Medium") &
                    qual.sdb %in% c("a.Low", "b.Medium") &  # this is the killer
                    !is.na(qual.missing) & qual.missing < 15 )   # reducing this to 5 doesn't change number of studies
# which are high-quality?
table(d$hi.qual)
unique( d$authoryear[ d$hi.qual == TRUE ] )
length( unique( d$authoryear[ d$hi.qual == TRUE ] ) )
# compare to my personal list of methodological favorites
# unique( d$authoryear[ d$mm.fave == 1 ] )


##### Make Table 2 #####

# MM audited 2020-2-6

# for all studies and stratified by publication status
t = my_quality_table(d)
t.pub = my_quality_table(d[ d$published == 1, ])
t.unpub = my_quality_table(d[ d$published == 0, ])

t$Published = t.pub$Summary
t$Unpublished = t.unpub$Summary

setwd(results.dir)
setwd("Tables to prettify")
write.csv(print(t), "study_quality_table.csv")


##### Individual Stats on Quality Characteristics #####

# MM audited 2020-2-1

# percent randomized
update_result_csv( name = "Perc randomized",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$randomized), 0 ),
                   print = FALSE )

# percent with public data
update_result_csv( name = "Perc public data",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$qual.public.data == "Yes"), 0 ),
                   print = TRUE )

# percent with public code
update_result_csv( name = "Perc public code",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$qual.public.code == "Yes"), 0 ),
                   print = TRUE )

# percent preregistered
update_result_csv( name = "Perc prereg",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$qual.prereg == "Yes"), 0 ),
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
                   value = round( 100 * percTRUE_incl_NA(d$qual.y.prox != "a.Actual"), 0 ),
                   print = FALSE )

# percent with low or medium risk of bias on each subjective variable
#  rather than unclear or high risk of bias
update_result_csv( name = "Perc qual.exch okay",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA( d$qual.exch %in% c("a.Low", "b.Medium") ), 0 ),
                   print = FALSE )
update_result_csv( name = "Perc qual.sdb okay",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA( d$qual.sdb %in% c("a.Low", "b.Medium") ), 0 ),
                   print = FALSE )
update_result_csv( name = "Perc qual.gen okay",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA( d$qual.gen %in% c("a.Low", "b.Medium") ), 0 ),
                   print = TRUE )

# percent high-quality
update_result_csv( name = "Perc ests hi.qual",
                   section = 0,
                   value = round( 100 * percTRUE_incl_NA(d$hi.qual), 0 ),
                   print = TRUE )

update_result_csv( name = "k ests hi.qual",
                   section = 0,
                   value = sum(d$hi.qual, na.rm = TRUE),
                   print = TRUE )


update_result_csv( name = "Unique articles hi.qual",
                   section = 0,
                   value = length( unique( d$authoryear[ d$hi.qual == 1 ] ) ),
                   print = TRUE )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 1. MAIN ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# 1. OVERALL META-ANALYSIS #################################

# MM audited 2020-2-1

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
update_result_csv( name = "Overall muhat RR-1 lo",
                   section = 1,
                   value = round( 100 * (exp(mu.lo) - 1), 0 ),
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


################################# CALIBRATED ENSEMBLE ESTIMATES #################################

# MM audited 2020-2-2

##### Plot the Ensemble Estimates #####
# note these are on the log scale
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
( best.ens = d$unique[ order(d$ens, decreasing = TRUE) ][1:10] )

# look at characteristics of best interventions
# interesting that all are no-request interventions
View(d[d$unique %in% best.ens,])

# look at the outcomes they used
top.ens = d$ens %in% sort(d$ens)[91:100]
d$ens[top.ens]
d$qual.y.prox[top.ens]

d$authoryear[ top.ens & d$qual.y.prox == "b.Self-reported" ]

# write list of studies with best ensemble estimates
setwd(results.dir)
write.csv( data.frame(best.ens),
           "best_calibrated_estimates_studies.csv")



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
dp = add_row( dp,
              .before = 1,
              logRR = meta.rob$b.r,
              ens = NA,
              varlogRR = meta.rob$reg_table$SE^2,
              RR.lo = exp(mu.lo),
              RR.hi = exp(mu.hi),
              unique = "POOLED",
              borderline = 0,
              rel.wt = 5 )


# set POOLED as the first level of the factor variable so it's displayed at bottom
correct.order = dp$unique
dp$unique = factor(dp$unique, levels = correct.order)
levels(dp$unique)

dp$is.pooled = as.factor( c( "Pooled estimate", rep("Individual study", nrow(dp) - 1) ) )

dp$borderline.pretty = NA
dp$borderline.pretty[ dp$borderline == 0 ] = "Non-borderline inclusion"
dp$borderline.pretty[ dp$borderline == 1 ] = "Borderline inclusion"

shapes = c(10, 19, 15)

breaks = c( seq(0.4, 1.3, .1),
            1.5,
            seq(1.5, 4, .5),
            seq(4, 6, 1) )

# for borderline status
colors = c("orange", "black")


  
base = ggplot( data = dp, aes( x = exp(logRR), 
                               y = unique,
                               size = rel.wt,
                               shape = is.pooled,
                               color = borderline.pretty ) ) +
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
  
  # guides(size = guide_legend("% weight in analysis",
  #                            guide = FALSE) ) +
  scale_size(guide = FALSE) +
  
  scale_color_manual(values = colors,
                     name = "Study inclusion") +
  
  scale_shape_manual(values = shapes,
                     name = "",
                     guide = FALSE) +
  
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
        plot = base,
        width = 15,
        height = 12 )


setwd(overleaf.dir)
ggsave( "forest.pdf",
        plot = base,
        width = 15,
        height = 12 )


################################# PERCENTAGE MEANINGFULLY STRONG EFFECTS #################################

# MM audited 2020-1-2

if (npphat.from.scratch == TRUE) {
  ##### Make Plotting Dataframe #####
  q.vec = seq( log(0.85), log(2), 0.01 )
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
  
  # bootstrap a CI for each entry in res.short
  temp = res.short %>% rowwise() %>%
    do( prop_stronger( q = .$q, 
                       tail = "above",
                       estimate.method = "calibrated",
                       ci.method = "calibrated",
                       yi.name = "logRR",
                       vi.name = "varlogRR",
                       dat = d,
                       R = boot.reps ) )
  
  temp$q = res.short$q
  temp$Est = 100*temp$Est
  
  # merge this with the full-length res dataframe, merging by Phat itself
  res = merge( res, temp, by.x = "q", by.y = "q")
  
  # # NOT USED?
  # # turn into percentage
  res$Est = 100*res$est.x
  res$lo = 100*res$lo
  res$hi = 100*res$hi
  
  setwd(results.dir)
  write.csv(res, "npphat_results.csv")
  
} else {
  setwd(results.dir)
  res = read.csv("npphat_results.csv")
}

# remove last row because CI is NA
#res = res[ -nrow(res), ]

##### Make Plot #####
ggplot( data = res,
        aes( x = exp(q),
             y = Est ) ) +
  theme_bw() +
  
  # pooled point estimate
  geom_vline( xintercept = exp(mu),
              lty = 2,
              color = "red" ) +
  
  # null
  geom_vline( xintercept = 1,
              lty = 2,
              color = "black" ) +
  
  scale_y_continuous(  breaks = seq(0, 100, 10) ) +
  # stop x-axis early because last CI is NA
  scale_x_continuous(  breaks = seq(0.8, 2, .1) ) +
  
  geom_line(lwd=1.2) +
  
  xlab("Threshold (RR scale)") +
  ylab( paste( "Estimated percent of effects above threshold" ) ) +
  
  geom_ribbon( aes(ymin=res$lo, ymax=res$hi), alpha=0.15, fill = "black" ) 


setwd(results.dir)
ggsave( "npphat.pdf",
        width = 9,
        height = 6 )

setwd(overleaf.dir)
ggsave( "npphat.pdf",
        width = 9,
        height = 6 )


##### Add to Results a Few Selected Values ####

#### Phat > 1 #####
Phat = prop_stronger( q = log(1), 
                      tail = "above",
                      estimate.method = "calibrated",
                      ci.method = "calibrated",
                      dat = d,
                      yi.name = "logRR",
                      vi.name = "varlogRR",
                      R = boot.reps )
update_result_csv( name = "Phat above 1",
                   section = 1,
                   value = round( 100 * Phat$est, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1 lo",
                   section = 1,
                   value = round( 100 * Phat$lo, 0 ),
                   print = FALSE )
update_result_csv( name = "Phat above 1 hi",
                   section = 1,
                   value = round( 100 * Phat$hi, 0 ),
                   print = TRUE )

#### Phat > 1.1 #####
# this exact value of q isn't in the dataframe above
Phat = prop_stronger( q = log(1.1), 
                      tail = "above",
                      estimate.method = "calibrated",
                      ci.method = "calibrated",
                      yi.name = "logRR",
                      vi.name = "varlogRR",
                      dat = d,
                      R = boot.reps )
update_result_csv( name = "Phat above 1.1",
                   section = 1,
                   value = round( 100 * Phat$est, 0 ),
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
                      yi.name = "logRR",
                      vi.name = "varlogRR",
                      dat = d,
                      R = boot.reps )
update_result_csv( name = "Phat above 1.2",
                   section = 1,
                   value = round( 100 * Phat$est, 0 ),
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
                            yi.name = "logRR",
                            vi.name = "varlogRR",
                            dat = d,
                            R = boot.reps )

update_result_csv( name = "Phat below 0.9",
                   section = 1,
                   value = round( 100*Phat.below$est, 0 ),
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
                            yi.name = "logRR",
                            vi.name = "varlogRR",
                            dat = d,
                            R = boot.reps )

update_result_csv( name = "Phat below 1",
                   section = 1,
                   value = round( 100*Phat.below$est, 0 ),
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
#                              2. SENSITIVITY ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################# PUBLICATION BIAS ################################# 

# MM audited 2020-2-2

##### Hedges Selection Model #####
# be careful about inference due to correlated point estimates
# can't fit model with 3 cutoffs because there are no significant negative studies
( m1 = weightfunct( effect = d$logRR,
                    v = d$varlogRR,
                    steps = c(0.025, 1),
                    table = TRUE ) )
# actually makes the estimate larger
# get SEs via the Hessian
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
                   value = format_stat( 2 * ( 1 - pnorm( abs(m1[[2]]$par[2]) / ses[2] ) ), cutoffs = c(0.10, pval.cutoff) ),
                   print = FALSE )

# sanity check for CI lower limit
# exp(0.2855 - 1.96*0.04209)


##### Worst-Case Meta-Analysis ######

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


# ##### S-values ######
# omitted because not very informative given the worst-case results above
# # s-values to reduce to null
# ( res = svalue( yi = d$logRR,
#                 vi = d$varlogRR,
#                 q = log(1), 
#                 clustervar = d$authoryear,
#                 model = "robust" ) )
# # N.P. for both point estimate and CI
# update_result_csv( name = "sval est to 1",
#                    section = 2,
#                    value = res$sval.est,
#                    print = FALSE )
# update_result_csv( name = "sval CI to 1",
#                    section = 2,
#                    value = res$sval.ci,
#                    print = FALSE )
# 
# 
# # s-values to reduce effect size to RR=1.1
# ( res = svalue( yi = d$logRR,
#                 vi = d$varlogRR,
#                 q = log(1.1), 
#                 clustervar = d$authoryear,
#                 model = "robust" ) )
# # N.P. for estimate
# update_result_csv( name = "sval est to 1.1",
#                    section = 2,
#                    value = res$sval.est,
#                    print = FALSE )
# update_result_csv( name = "sval CI to 1.1",
#                    section = 2,
#                    value = round( res$sval.ci, 2 ),
#                    print = FALSE )


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

# just for fun: regular contour-enhanced funnel
# first remove the huge point
temp = d[ !d$logRR > 1.5, ]
m.temp = rma.uni( yi = temp$logRR,
                  vi = temp$varlogRR )
funnel.rma(m.temp, level = 0.95, legend = TRUE)


##### Fit Selection Model to Just Unpublished, or Just Published, Studies #####

# these results are only reported qualitatively
# so not saving the numerical results
( weightfunct( effect = d$logRR[ d$published == 1 ],
                    v = d$varlogRR[ d$published == 1 ],
                    steps = c(0.025, 1),
                    table = TRUE ) )
significance_funnel2(yi = d$logRR[ d$published == 1 ],
                     vi = d$varlogRR[ d$published == 1 ] )

( weightfunct( effect = d$logRR[ d$published == 0 ],
               v = d$varlogRR[ d$published == 0 ],
               steps = c(0.025, 1),
               table = TRUE ) )
significance_funnel2(yi = d$logRR[ d$published == 0 ],
                     vi = d$varlogRR[ d$published == 0 ] )


################################# SOCIAL DESIRABILITY BIAS ################################# 

# MM audited 2020-8-8

##### Shapiro Normality Test #####
# see if reasonable to use the parametric versions in JASA paper
# normality a bit borderline, so will avoid parametric
shapiro.test(d$ens)
qqnorm(d$ens); qqline(d$ens)


##### Calibrated That for Shifting Point Estimate to Null #####
# reduce to less than 100*r% the proportion of effects above q
B.vec = seq(1, 2, 0.01)

That.est = That_causal_bt( original = d,
                           indices = 1:nrow(d),  # don't bootstrap (keep original sample)
                           .calib.name = "ens",
                           .B.vec = B.vec,
                           .estimand = "est" )

update_result_csv( name = "SDB That-1 shift est to 1",
                   section = 2,
                   value = round( 100*(That.est-1), 0 ),
                   print = TRUE )


if (npphat.from.scratch == TRUE) {
  
  # CI for That
  # reduce bootstrap reps here because takes forever (has to fit robumeta each time)
  boot.res = suppressWarnings( boot( data = d,
                                     parallel = "multicore",
                                     R = 500, 
                                     statistic = That_causal_bt,
                                     # below are passed to fn above
                                     .calib.name = "ens",
                                     .B.vec = B.vec,
                                     .estimand = "est" ) )
  
  bootCIs = boot.ci(boot.res,
                    type="bca",
                    conf = 0.95 )
  
  lo.bt = bootCIs$bca[4]
  hi.bt = bootCIs$bca[5]
  SE.bt = sd(boot.res$t)
  
  res = data.frame(est = That.est,
                   lo = lo.bt, 
                   hi = hi.bt)
  
  setwd(results.dir)
  write.csv( res,
             "That_shift_est_to_null.csv" )
  
} else {  # if not bootstrapping from scratch
  
  setwd(results.dir)
  res = read.csv("That_shift_est_to_null.csv")
}


update_result_csv( name = "SDB That-1 shift est to 1",
                   section = 2,
                   value = round( 100*(res$est - 1), 0 ),
                   print = FALSE )

update_result_csv( name = "SDB That-1 lo shift est to 1",
                   section = 2,
                   value = round( 100*(res$lo - 1), 0 ),
                   print = FALSE )

update_result_csv( name = "SDB That-1 hi shift est to 1",
                   section = 2,
                   value = round( 100*(res$hi - 1), 0 ),
                   print = TRUE )


##### Calibrated That for Phat(RR>1.1) < 0.10 #####
# now considering homogeneous bias
# and to shift Phat(RR>1.1) to less than 10%

# note: not looking at shifting CI to null because that would require homogeneous bias
#  since heterogeneous bias affects tau^2 and therefore the CI limit

q = log(1.1)
r = 0.10

That.p = That_causal_bt( original = d,
                         indices = 1:nrow(d),  # don't bootstrap (keep original sample)
                         .calib.name = "ens",
                         .q = q,
                         .r = r,
                         .B.vec = B.vec,
                         .tail = "above",
                         .estimand = "Phat" )

update_result_csv( name = "SDB That-1 shift est to 1",
                   section = 2,
                   value = round( 100*(That.p-1), 0 ),
                   print = FALSE )


if (npphat.from.scratch == TRUE) {
  
  # CI for That
  boot.res = suppressWarnings( boot( data = d,
                                     parallel = "multicore",
                                     R = boot.reps, 
                                     statistic = That_causal_bt,
                                     # below are passed to fn above
                                     .calib.name = "ens",
                                     .q = q,
                                     .r = r,
                                     .B.vec = B.vec,
                                     .tail = "above",
                                     .estimand = "Phat" ) )
  
  bootCIs = boot.ci(boot.res,
                    type="bca",
                    conf = 0.95 )
  
  lo.bt = bootCIs$bca[4]
  hi.bt = bootCIs$bca[5]
  SE.bt = sd(boot.res$t)
  
  res = data.frame(est = That.p,
                   lo = lo.bt, 
                   hi = hi.bt)
  
  setwd(results.dir)
  write.csv( res,
             "That_shift_phat.csv" )
  
} else {  # if not bootstrapping from scratch
  
  setwd(results.dir)
  res = read.csv("That_shift_phat.csv")
}


update_result_csv( name = "SDB That-1 shift phat",
                   section = 2,
                   value = round( 100*(res$est - 1), 0 ),
                   print = FALSE )

update_result_csv( name = "SDB That-1 lo shift phat",
                   section = 2,
                   value = round( 100*(res$lo - 1), 0 ),
                   print = FALSE )

update_result_csv( name = "SDB That-1 hi shift phat",
                   section = 2,
                   value = round( 100*(res$hi - 1), 0 ),
                   print = TRUE )


##### Plot Homogeneous Bias Factor vs. Phat(RR>1.1) #####
# not in paper
if ( npphat.from.scratch == TRUE ){
  
  B.vec = seq( 1, 1.5, 0.01 )
  Bl = as.list(B.vec)
  
  # again pass threshold and calibrated estimates on log-RR scale
  Phat.t.vec = lapply( Bl,
                       FUN = function(B) Phat_causal( .q = q, 
                                                      .B = B,
                                                      .calib = d$ens,
                                                      .tail = "above",
                                                      .give.CI = FALSE,
                                                      .dat = dat,
                                                      .calib.name = "calib") )
  
  resb = data.frame( B = B.vec,
                     Est = unlist(Phat.t.vec) )
  
  # selective bootstrapping
  # look at just the values of B at which Phat jumps
  #  this will not exceed the number of point estimates in the meta-analysis
  res.short = resb[ diff(resb$Est) != 0, ]
  
  
  # bootstrap a CI for each entry in res.short
  library(boot)
  temp = res.short %>% rowwise() %>%
    do( Phat_causal( .q = q, 
                     .B = .$B,
                     .calib = d$ens,
                     .tail = "above",
                     
                     .give.CI = TRUE,
                     .dat = d,
                     .R = boot.reps,
                     .calib.name = "ens") )
  
  
  # merge this with the full-length res dataframe, merging by Phat itself
  resb = merge( resb, temp, by.x = "Est", by.y = "Est")
  
  # sanity check for That for shifting Phat(RR>1.1) to 10%
  resb$Est[ resb$B == 1.5]
  resb$Est[ resb$B == 1.4]
  resb$Est[ resb$B == 1.37]
  resb$Est[ resb$B == 1.3]
  
  
  # make plot
  # black dashed line: That/Ghat
  # red dashed line: proportion threshold r
  p = ggplot( data = resb,
              aes( x = B,
                   y = Est ) ) +
    theme_bw() +
    
    # proportion "r" line
    geom_hline( yintercept = .10, 
                lty = 2,
                color = "red" ) +
    
    # That line
    geom_vline( xintercept = That.p,
                lty = 2,
                color = "black" ) +
    
    scale_y_continuous( limits=c(0,1), breaks=seq(0, 1, .1)) +
    scale_x_continuous( breaks = seq(1, 1.5, .1) ) +
    geom_line(lwd=1.2) +
    
    xlab("Hypothetical multiplicative bias in all studies (RR scale)") +
    ylab( paste( "Estimated proportion of studies with population causal RR >", exp(q) ) ) +
    
    geom_ribbon( aes(ymin=resb$lo, ymax=resb$hi), alpha=0.15, fill = "black" )
  
  setwd(results.dir)
  ggsave( "phat_causal.pdf",
          width = 9,
          height = 6 )
  setwd(overleaf.dir)
  ggsave( "phat_causal.pdf",
          width = 9,
          height = 6 )
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              3. SUBSET ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# RUN ALL SUBSET ANALYSES #################################

# MM audited 2020-2-2

if( exists("resE") ) rm(resE)

# for Bonferroni
# don't penalize since these are sensitivity analyses
n.tests = 1

# for Phat
ql = c(log(1), log(1.1), log(1.2))
boot.reps = 2000 

digits = 2


# two studies have really huge logRRs, as can be seen in significance funnel
d$non.extreme.logRR = exp(d$logRR) < 3
table(d$non.extreme.logRR)


##### Overall and Subset Analyses #####

subsets = list( d,
                d %>% filter( !is.na(borderline) & borderline == 0 ),
                d %>% filter( !is.na(x.pure.animals) & x.pure.animals == 1 ),
                d %>% filter( !is.na(hi.qual) & hi.qual == 1 ),
                d %>% filter( !is.na(qual.y.prox2) & qual.y.prox2 == "b.Actual or self-reported" ),
                d %>% filter( !is.na(design) & randomized == 1 ),
                d %>% filter( !is.na(reproducible) & reproducible == 1 ),
                d %>% filter( !is.na(published) & published == 1 ),
                d %>% filter( !is.na(published) & published == 0 ),
                d %>% filter( !is.na(non.extreme.logRR) & non.extreme.logRR == 1 ),
                d.chal )

# make sure none of the subsets is length 0 due to messed up variables
lapply( X = subsets, FUN = nrow )


subset.labels = c( "Overall",
                   "Non-borderline",
                   "Animal welfare only",
                   "Lowest risk of bias",
                   "Actual or self-reported past behavior",
                   "Randomized",
                   "Preregistered with open data",
                   "Published studies",
                   "Unpublished studies",
                   "Exclude one extreme estimate",
                   "Include SSWS studies" )

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
resE = resE %>% select( Meta,
                        k,
                        Est,
                        Pval,
                        Tau,
                        `Percent above 1`,
                        `Percent above 1.1`,
                        `Percent above 1.2`)

# save results
setwd(results.dir)
setwd("Tables to prettify")
write.csv(resE, "subsets_table.csv", row.names = FALSE)

# note: for some subsets, tau = 0, hence 0 estimate for certain Phats and inestimable CI



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              4. MODERATOR ANALYSES (MAIN ONES)           
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# SFIG: CORRELATIONS AMONG ALL MODERATORS #################################

# MM audited 2020-8-7

#includes all moderators from both meta-regression models
vars = c( "x.has.text",
          "x.has.visuals",
          "x.suffer",
          "x.mind.attr",
          "x.soc.norm",
          "x.id.victim",
          "x.pets",
          "x.impl",
          "x.makes.request",
          "x.long",
          "y.long.lag",
          "perc.male.10",
          "age" )


corrs = d %>% select(vars) %>%
  correlate( use = "pairwise.complete.obs" ) %>%
  stretch() %>%
  arrange(desc(r)) %>%
  group_by(r) %>%
  filter(row_number()==1)

corrs$x = dplyr::recode( corrs$x,
                         "x.has.text" = "Text",
                         "x.has.visuals" = "Visuals",
                         "x.suffer" = "Graphic",
                         "x.mind.attr" = "Mind attribution",
                         "x.soc.norm" = "Social norms",
                         "x.id.victim" = "ID victim",
                         "x.pets" = "Pets",
                         "x.impl" = "Implementation suggestions",
                         "x.makes.request" = "Makes request",
                         "x.long" = "Intervention >5 min",
                         "y.long.lag" = "Follow-up >=7 days",
                         "perc.male.10" = "Male",
                         "age" = "Age")
corrs$y = dplyr::recode( corrs$y,
                         "x.has.text" = "Text",
                         "x.has.visuals" = "Visuals",
                         "x.suffer" = "Graphic",
                         "x.mind.attr" = "Mind attribution",
                         "x.soc.norm" = "Social norms",
                         "x.id.victim" = "ID victim",
                         "x.pets" = "Pets",
                         "x.impl" = "Implementation suggestions",
                         "x.makes.request" = "Makes request",
                         "x.long" = "Intervention >5 min",
                         "y.long.lag" = "Follow-up >=7 days",
                         "perc.male.10" = "Male",
                         "age" = "Age")
corrs$r = round(corrs$r, 2)

corrs = corrs[ !is.na(corrs$r), ]
View(corrs)

setwd(results.dir)
setwd("Tables to prettify")
write.csv(corrs, "moderator_cormat.csv")

# this is what I actually used since the table is in the LaTeX supplement:
print( xtable(corrs), include.rownames = FALSE )



################################# META-REGRESSION 1: WITHOUT INTERVENTION COMPONENTS #################################

# MM audited 2020-2-2

# can't include country; causes sparsity and eigendecomposition 
#  problems because has too much missing data

mod.sets = list( c( "x.has.text",
                    "x.has.visuals",
                    "x.suffer",
                    "x.rec",
                    "x.long",
                    "y.long.lag",
                    "perc.male.10",
                    "age.5" ),
                 
                 c( # omit x.has.text due to collinearity
                   "x.has.visuals",
                   "x.suffer",
                   "x.mind.attr",
                   "x.soc.norm",
                   "x.id.victim",
                   "x.pets",
                   "x.impl",
                   "x.rec",
                   "x.long",
                   "y.long.lag",
                   "perc.male.10",
                   "age.5" ) )


for ( i in 1:length(mod.sets) ) {
  moderators = mod.sets[[i]]
  
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
  
  # estimates for text
  ests = round( exp(est), 2 )
  pvals2 = format_stat(pval)
  # get rid of scientific notation; instead use more digits
  pvals2[ pval < 0.01 ] = round( pval[ pval < 0.01 ], 3 )
  
  
  # only for main meta-regression
  if ( i == 1 ) {
    # report tau in this model
    update_result_csv( name = "Meta-regression tau",
                       section = 4,
                       value = round( sqrt(t2), 2 ),
                       print = TRUE )

    update_result_csv( name = "k in meta-regression",
                       section = 4,
                       value = nrow(meta$data.full),
                       print = TRUE )
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
    
    # for reporting in the Discussion in terms of percent more effective
    update_result_csv( name = paste( "Meta-regression go vegan RR-1" ),
                       section = 4,
                       value = 100*( ests[ meta$labels == "x.recd.Go.vegan"] - 1),
                       print = TRUE )
    update_result_csv( name = paste( "Meta-regression graphic RR-1" ),
                       section = 4,
                       value = 100*( ests[ meta$labels == "x.suffer"] - 1),
                       print = TRUE )
    update_result_csv( name = paste( "Meta-regression long lag RR-1" ),
                       section = 4,
                       value = 100*( 1 - ests[ meta$labels == "y.long.lagTRUE"] ),
                       print = TRUE )
    
  }  # end "if i == 1" (main meta-regression only)
  
  if ( i == 2 ){
    update_result_csv( name = "k in meta-regression2",
                       section = 4,
                       value = nrow(meta$data.full),
                       print = TRUE )
    
    update_result_csv( name = paste( "Meta-regression est x.impl" ),
                       section = 4,
                       value = round( exp(meta$b.r[meta$labels == "x.impl"]), 2 ),
                       print = TRUE )
    
    update_result_csv( name = paste( "Meta-regression lo x.impl" ),
                       section = 4,
                       value = round( exp(mu.lo[meta$labels == "x.impl"]), 2 ),
                       print = TRUE )
    
    update_result_csv( name = paste( "Meta-regression hi x.impl" ),
                       section = 4,
                       value = round( exp(mu.hi[meta$labels == "x.impl"]), 2 ),
                       print = TRUE )
    
    update_result_csv( name = paste( "Meta-regression pval x.impl" ),
                       section = 4,
                       value = pvals2[meta$labels == "x.impl"],
                       print = TRUE )
    
  }
  

  ##### Meta-Regression Table #####
  CIs = format_CI( exp( mu.lo ),
                   exp( mu.hi ) )
  temp = data.frame( Moderator = meta$labels, 
                     EstCI = paste( ests, CIs, sep = " " ),
                     Pval = pvals2 )
  
  # save results
  setwd(results.dir)
  setwd("Tables to prettify")
  
  if ( i == 1 ) write.csv(temp, "meta_regression1_table.csv", row.names = FALSE)
  if ( i == 2 ) write.csv(temp, "meta_regression2_table.csv", row.names = FALSE)
}




################################# CONTINUOUS MODERATOR PLOTS #################################

# set up sufficiently large color and shape ranges
n = 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

#N = 100; M = 1000
good.shapes = c(1:25,33:127)

##### SFig4: Time Exposed to Intervention #####
temp = dp %>% filter( !is.na(x.min.exposed) & !is.na(ens) )
temp = droplevels(temp)

# choose good limits
exp(min(temp$ens))
exp(max(temp$ens))
max(temp$x.min.exposed)

ggplot( data = temp, aes( x = x.min.exposed,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4) + 
  xlab("Total minutes exposed to intervention") +
  ylab("Calibrated estimate (RR)") +
  
  # null
  geom_hline(yintercept = 1, lty = 2) +

  scale_y_continuous( limits = c(.8, 2.5),
                      breaks = seq(.8, 2.5, .1) ) +
  scale_x_log10( limits = c(1,100),
                 breaks = c(1,5,10,30,100)) +
  
  scale_shape_manual(values = good.shapes,
                     name = "Study") +
  labs(color = "Study") +
  theme_bw()

setwd(results.dir)
ggsave( "continuous_x_duration.pdf",
        width = 10,
        height = 6 )
setwd(overleaf.dir)
ggsave( "continuous_x_duration.pdf",
        width = 10,
        height = 6 )


##### SFig5: Time Lag Between Intervention and Outcome Measurement #####
temp = dp %>% filter( !is.na(y.lag.days) & !is.na(ens) )
temp = droplevels(temp)

# choose good limits
exp(min(temp$ens))
exp(max(temp$ens))
min(temp$y.lag.days)
max(temp$y.lag.days)

ggplot( data = temp, aes( x = y.lag.days,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4,
             alpha = 1) + 

  xlab("Days elapsed between intervention and outcome measurement") +
  ylab("Calibrated estimate (RR)") +
  
  # null
  geom_hline(yintercept = 1, lty = 2) +
  
  scale_x_continuous(limits = c(0,130),
                     breaks = seq(0,130,10)) +
  scale_y_continuous( limits = c(.8, 2.5),
                      breaks = seq(.8, 2.5, .1) ) +
  
  scale_shape_manual(values = good.shapes,
                     name = "Study") +
  labs(color = "Study") +
  #scale_colour_brewer(palette = "Set1") +
  #scale_color_manual(values = col_vector) +
  theme_bw()

setwd(results.dir)
ggsave( "continuous_time_lag.pdf",
        width = 10,
        height = 6 )
setwd(overleaf.dir)
ggsave( "continuous_time_lag.pdf",
        width = 10,
        height = 6 )


##### SFig6: Average Age #####

temp = dp %>% filter( !is.na(age) & !is.na(ens) )
temp = droplevels(temp)

# choose good limits
exp(min(temp$ens))
exp(max(temp$ens))
min(temp$age)
max(temp$age)

ggplot( data = temp, aes( x = age,
                          y = exp(ens),
                          color = authoryear,
                          shape = authoryear) ) + 
  geom_point(size = 4,
             alpha = 1) + 
  
  xlab("Average age of subjects") +
  ylab("Calibrated estimate (RR)") +
  
  # null
  geom_hline(yintercept = 1, lty = 2) +
  
  scale_x_continuous(limits = c(8,46),
                     breaks = seq(8,46,2)) +
  scale_y_continuous( limits = c(.8, 2.5),
                      breaks = seq(.8, 2.5, .1) ) +
  
  scale_shape_manual(values = good.shapes,
                     name = "Study") +
  labs(color = "Study") +
  #scale_colour_brewer(palette = "Set1") +
  #scale_color_manual(values = col_vector) +
  theme_bw()

setwd(results.dir)
ggsave( "continuous_age.pdf",
        width = 10,
        height = 6 )
setwd(overleaf.dir)
ggsave( "continuous_age.pdf",
        width = 10,
        height = 6 )





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              5. INTERRATER RELIABILITY            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

############################### IRR FOR ARTICLE INCLUSION ###############################

##### Title/Abstract Screening #####
# calculate total ratings shared by each pair of reviewers to help with weighted average
d4 = d4 %>% rowwise() %>%
  mutate(tot = sum(A.Yes..B.Yes, A.Yes..B.No, A.No..B.Yes, A.No..B.No))

mean.kappa = sum(d4$Cohen.s.Kappa * d4$tot) / sum(d4$tot)

update_result_csv( name = "Mean IRR tiab screen",
                   section = 5,
                   value = round(mean.kappa, digits),
                   print = TRUE )

##### Full-Text Screening #####
# calculate total ratings shared by each pair of reviewers to help with weighted average
d5 = d5 %>% rowwise() %>%
  mutate(tot = sum(A.Include..B.Include, A.Include..B.Exclude, A.Exclude..B.Include, A.Exclude..B.Exclude))

mean.kappa = sum(d5$Cohen.s.Kappa * d5$tot) / sum(d5$tot)

update_result_csv( name = "Mean IRR full text",
                   section = 5,
                   value = round(mean.kappa, digits),
                   print = TRUE )


############################### IRR FOR SUBJECTIVE QUALITY VARIABLES ###############################

# look at pairwise interrater agreement
qual.vars = c("sdb", "gen", "exch")

for ( v in qual.vars ){
  var.names = paste( v, c("DR", "JN", "MM"), sep = "." )
  temp = d3[ , var.names]
  
  temp[ temp == "Unclear" ] = NA
  
  my_recode = function(x) recode_factor( x, Weak = "a.Weak", Medium = "b.Medium", Strong = "c.Strong" )
  
  temp2 = temp %>% mutate_at( .vars = var.names, funs(factor) ) %>%
    mutate_at( .vars = var.names, funs(my_recode) )
  
  droplevels(temp2)
  
  levels(temp2[,1])
  levels(temp2[,2])
  levels(temp2[,3])

  # weighted Cohen's kappa for ordinal data
  # look at raters pairwise because of missing data
  #  (otherwise does a CC analysis)
  # omits missing data listwise
  ( k12 = kappa2( temp[ , c(1,2) ], weight = "equal" )$value )
  ( k13 = kappa2( temp[ , c(1,3) ], weight = "equal" )$value )
  ( k23 = kappa2( temp[ , c(2,3) ], weight = "equal" )$value )
  
  ( mean.kappa = mean( c(k12, k13, k23) ) )
  
  update_result_csv( name = paste("Mean IRR ", v, sep=""),
                     section = 5,
                     value = round(mean.kappa, digits),
                     print = TRUE )
}


############################### IRR FOR INTERVENTION COMPONENTS ###############################

vars = c("mind.attribution", "social.norms", "id.victim", "impl.suggest", "pets")

for ( v in vars ){

  # unweighted Cohen's kappa since these are binary
  # omits missing data listwise, so throws out situations when one coder
  #  thought they didn't have enough info to rate the component, but another thought they did
  ( kap = kappa2( data.frame( dd[[v]], dm[[v]] ), weight = "unweighted" )$value )
  
  update_result_csv( name = paste("IRR ", v, sep=""),
                     section = 5,
                     value = round(kap, digits),
                     print = TRUE )
}








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              6. SMALL MISCELLANIA            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# for Introduction
# total animals consumed per day, per capita in each country
ds$tot = rowSums(ds[,-1])

# sanity check based on their stat on page 252
expect_equal( round(ds$tot[ ds$country == "China" ], 1), 2.4 )

# total animals consumed per human lifetime
# assume human lifespans of 75 years, as in Scherer
ds$per.lifetime = ds$tot * 75 * 365

# don't pipe in these numbers because of need for comma
round( min(ds$per.lifetime) )
round( max(ds$per.lifetime) )

