
################################ FOR ANALYSIS ################################

# analyze a subset or a moderator
# n.tests: for Bonferroni
analyze_one_meta = function( dat,
                             yi.name,
                             vi.name,
                             meta.name,
                             moderator = "",
                             mod.continuous = FALSE,
                             ql,
                             take.exp,
                             boot.reps = 2000,
                             n.tests=1,
                             digits = 2) {
  
  dat$yi = dat[[yi.name]]
  dat$vyi = dat[[vi.name]]
  
  ##### Regular Meta-Analysis (Possibly of Moderator) #####
  if ( moderator == "" ) {
    
    library(robumeta)
    ( meta = robu( logRR ~ 1, 
                   data = dat, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )
    
    est = meta$b.r
    t2 = meta$mod_info$tau.sq
    mu.lo = meta$reg_table$CI.L
    mu.hi = meta$reg_table$CI.U
    mu.se = meta$reg_table$SE
    mu.pval = meta$reg_table$prob
    
    
    # Phat from calibrated estimates
    Phat.l = lapply( ql,
                     FUN = function(q) {
                       
                       ens = my_ens( yi = dat$yi, 
                                     sei = sqrt(dat$vyi) )
                       
                       # set tail based on sign of q
                       if (q >= 0) tail = "above"
                       else tail = "below"
                       if ( tail == "above" ) Phat.NP.ens = sum(ens > c(q)) / length(ens)
                       if ( tail == "below" ) Phat.NP.ens = sum(ens < c(q)) / length(ens)
                       
                       library(boot)
                       Note = NA
                       tryCatch({
                         boot.res.ens = boot( data = dat, 
                                              parallel = "multicore",
                                              R = boot.reps, 
                                              statistic = function(original, indices) {
                                                
                                                b = original[indices,]
                                                
                                                ens.b = my_ens( yi = b$yi, 
                                                                sei = sqrt(b$vyi) )
                                                if ( tail == "above" ) return( sum(ens.b > c(q)) / length(ens.b) )
                                                if ( tail == "below" ) return( sum(ens.b < c(q)) / length(ens.b) )
                                              }
                         )
                         
                         bootCIs.ens = boot.ci(boot.res.ens, type="bca")
                         boot.lo.ens = bootCIs.ens$bca[4]
                         boot.hi.ens = bootCIs.ens$bca[5]
                         
                       }, error = function(err){
                         boot.lo.ens <<- NA
                         boot.hi.ens <<- NA
                         Note <<- err$message
                       } )  # end tryCatch
                       
                       return( data.frame( Est = Phat.NP.ens,
                                           lo = boot.lo.ens,
                                           hi = boot.hi.ens,
                                           boot.note = Note ) )
                     } )  # end lapply
    
    
    Phat.df = do.call( rbind, 
                       Phat.l )
    Phat.df$string = paste( round( 100*Phat.df$Est,
                                   digits = 0 ),
                            format_CI( 100*Phat.df$lo, 
                                       100*Phat.df$hi,
                                       digits = 0 ),
                            sep = " " )
    
    levels = ""
    k = nrow(dat)
  }
  
  ##### Meta-Regression #####
  if (moderator != "") {
    library(robumeta)
    ( meta = robu( logRR ~ d[[moderator]], 
                   data = d, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )
    
    # for factor moderator, include each level
    if (! mod.continuous ) {
      levels = levels( as.factor(d[[moderator]]) )
      est = meta$b.r  # all of these are vectors
      t2 = meta$mod_info$tau.sq
      mu.lo = meta$reg_table$CI.L
      mu.hi = meta$reg_table$CI.U
      mu.se = meta$reg_table$SE
      mu.pval = meta$reg_table$prob
      k = as.numeric( table( as.factor(d[[moderator]]) ) ) # k in the relevant level
    } 
    
    # for continuous moderator, avoid the intercept
    else {
      levels = "1-unit increase"
      est = meta$b.r[2]
      t2 = meta$mod_info$tau.sq[2]
      mu.lo = meta$reg_table$CI.L[2]
      mu.hi = meta$reg_table$CI.U[2]
      mu.se = meta$reg_table$SE[2]
      mu.pval = meta$reg_table$prob[2]
      k = nrow(dat)
    }
    
  }
  
  
  ##### Put Results in Dataframe #####
  if (take.exp == TRUE) {
    est = exp(est)
    lo = exp(mu.lo)
    hi = exp(mu.hi)
  }
  
  est.string = paste( round( est, digits ),
                      format_CI( lo, 
                                 hi,
                                 digits),
                      sep = " " )
  
  tau.string = round( sqrt(t2), digits)
  
  
  new.row = data.frame( Meta = meta.name,
                        Moderator = moderator,
                        Level = levels,
                        k = k,
                        Est = est.string,
                        Pval = format_stat(mu.pval),
                        Pval.Bonf = format_stat( pmin(mu.pval*n.tests, 1) ),
                        Tau = tau.string
  )
  
  # tail is now just for string purposes
  if ( moderator == "" ) {
    tail = rep("above", length(unlist(ql)))
    tail[unlist(ql) < 0] = "below"
    if (take.exp == TRUE) q.vec = exp(unlist(ql)) else q.vec = unlist(ql)
    Phat.names = paste( "Percent ", tail, " ", q.vec, sep = "" )
    # new.row[, Phat.names ] = NA
    
    new.row[ , Phat.names ] = Phat.df$string
  }
  
  
  # this should be a global variable
  if ( !exists("resE") ){
    resE <<- new.row
  } else {
    library(plyr)
    resE <<- rbind.fill(resE, new.row)
    detach("package:plyr", unload=TRUE)
  }
} 

################################ ENSEMBLE ESTIMATES ################################

# my calculation of ensemble estimates
# see Wang paper
my_ens = function(yi,
                  sei ) {
  
  meta = rma.uni( yi = yi, 
                  sei = sei, 
                  method = "DL" )
  
  muhat = meta$b
  t2 = meta$tau2
  
  # return ensemble estimates
  c(muhat) + ( c(t2) / ( c(t2) + sei^2 ) )^(1/2) * ( yi - c(muhat) )
}



################################ MISCELLANEOUS ################################

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res = read.csv( "stats_for_paper.csv",
                    stringsAsFactors = FALSE,
                    colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res$name) ) res[ res$name %in% name, ] = new.rows
    else res = rbind(res, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res = new.rows
  }
  
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res)
  }
}


# make my own Table 1
# type: "cat", "bin01", "cont"
# countNA: should we count NA as its own category for cat and bin01?
# tab1: the current table 1 (if NA, starts generating one from scratch)
table1_add_row = function( x, # vector
                          var.header,  # variable name to use in table
                          type,
                          perc.digits = 0,
                          num.digits = 2,
                          countNA = TRUE,
                          .tab1 = NULL,
                          print = TRUE ) {
  
  # x = d$x.suffer
  # x = d$country
  # x = d$y.lag.days
  # 
  # var.header = "Varname"
  
  
  useNA = ifelse( countNA == TRUE, "ifany", "no" )
  
  if ( type == "cat" ) {
    t = table(x, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "bin01" ) {
    # force "1" entry to be ordered first
    t = table(x == 0, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    # reverse the coding again
    row.names[ row.names == "FALSE" ] = "Yes"
    row.names[ row.names == "TRUE" ] = "No"
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "cont") {
    # assume we want the median and IQR
    if ( countNA == TRUE ) {
      
      stat.string = paste( round( median( x, na.rm = TRUE ), digits = num.digits ),
                           " (", 
                           round( quantile( x, 0.25, na.rm = TRUE ), digits = num.digits ),
                           ", ",
                           round( quantile( x, 0.75, na.rm = TRUE ), digits = num.digits ),
                           ")", 
                           sep = "" )
      
      n.NA = sum( is.na(x) )
      perc.NA = mean( is.na(x) )
      
      stat.string2 = paste( n.NA, " (", round( 100 * perc.NA, digits = perc.digits ), "%)", sep = "" )
      
      # first row is just the median, so no row name
      row.names = c("Not reported")
    }
    # haven't written the case of countNA == FALSE yet
    
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( stat.string, stat.string2 ) )
  }
  
  if ( type %in% c("cat", "bin01") ) {
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( NA, stat.string ) )
  }
  
  
  
  # add the new row to existing table 1, if applicable
  if ( !is.null(.tab1) ) .tab1 = rbind(.tab1, new.row)
  else .tab1 = new.row
  if ( print == TRUE ) print(.tab1)
  return(.tab1)
}

# return percent true for 0/1 variable, counting NA as own category
percTRUE_incl_NA = function(x) {
  prop.table( table(x, useNA = "ifany") )[2]
}



my_quality_table = function(.dat) {
  t = table1_add_row( x = .dat$design,
                      var.header = "Design",  # variable name to use in table
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = NULL )
  
  t = table1_add_row( x = .dat$qual.y.prox,
                      var.header = "Outcome measurement",  # variable name to use in table
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.missing,
                      var.header = "Percent missing data",  # variable name to use in table
                      type = "cont",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.exch,
                      var.header = "Exchangeability",  # variable name to use in table
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.sdb,
                      var.header = "Avoidance of social desirability bias",  # variable name to use in table
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.gen,
                      var.header = "External generalizability",  # variable name to use in table
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.prereg == "Yes",
                      var.header = "Preregistered",  # variable name to use in table
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.public.data == "Yes",
                      var.header = "Public data",  # variable name to use in table
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .dat$qual.public.data == "Yes",
                      var.header = "Public code",  # variable name to use in table
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t )
  
  
  t = table1_add_row( x = .dat$hi.qual,
                      var.header = "Overall lowest risk of bias",  # variable name to use in table
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t )
}



################################ SIGNIFICANCE FUNNEL ################################

# edited from the package version (marked with "~~") to fix the colored blocks issues

significance_funnel2 = function( yi,
                                vi,
                                xmin = min(yi),
                                ymin = min( sqrt(vi) ),
                                xmax = max(yi),
                                ymax = max( sqrt(vi) ),
                                est.N = NA,
                                est.all = NA,
                                plot.pooled = TRUE ) {
  
  d = data.frame(yi, vi)
  d$sei = sqrt(vi)
  
  # calculate p-values
  d$pval = 2 * ( 1 - pnorm( abs(yi) / sqrt(vi) ) )
  
  # variable for positive vs. nonpositive studies
  d$positive = rep(NA, nrow(d))
  d$positive[ (d$yi > 0) & (d$pval < 0.05) ] = "Affirmative"
  d$positive[ (d$yi < 0) | (d$pval >= 0.05) ] = "Non-affirmative"
  
  # sanity check
  d$positive2 = d$yi/sqrt(d$vi)>qnorm(.975)
  table(d$positive, d$positive2)
  
  # reorder levels for plotting joy
  d$positive = factor( d$positive, c("Non-affirmative", "Affirmative") )
  
  # stop if no studies in either group
  if ( sum( d$positive == "Non-affirmative" ) == 0 ) {
    stop("There are no non-affirmative studies. The plot would look silly.")
  }
  
  if ( sum( d$positive == "Affirmative" ) == 0 ) {
    stop("There are no affirmative studies. The plot would look silly.")
  }
  
  # pooled fixed-effects estimates
  # ~~ NEW: use FE model unless they are passed as arguments
  if ( is.na(est.N) & is.na(est.all) ) {
    est.N = rma.uni(yi = d$yi[ d$positive == "Non-affirmative" ],
                    vi = d$vi[ d$positive == "Non-affirmative" ],
                    method="FE")$b
    
    est.all = rma.uni(yi = d$yi,
                      vi = d$vi,
                      method="FE")$b
  }

  
  # negative sei positions them below the horizontal divider line
  pooled.pts = data.frame( yi = c(est.N, est.all),
                           sei = c(0,0) )
  
  # for a given SE (y-value), return the "just significant" point estimate value (x-value)
  just_signif_est = function( .sei ) .sei * qnorm(.975)
  
  # polygon coordinates for the blue and orange shading
  # remember ymin is the min SE, etc.
  # if ( !any( is.na( c(ymin, xmin, ymax, xmax) ) ) ) {
  #   
  #   poly.blue=data.frame(yi=c(xmin, just_signif_est(ymin), just_signif_est(ymax), xmin ),
  #                        sei=c(ymin, ymin, ymax, ymax),
  #                        positive = rep("Non-affirmative", 4),
  #                        alpha = 0.3)
  #   
  #   poly.orange=data.frame(yi=c(just_signif_est(ymin), xmax, xmax, just_signif_est(ymax)),
  #                          sei=c(ymin, ymin, ymax, ymax ),
  #                          positive = rep("Affirmative", 4),
  #                          alpha = 0.3)
  # } else {
  #   # remove objects if they happen to exist
  #   # because will check for existence during plotting
  #   suppressWarnings( rm(poly.blue) )
  #   suppressWarnings( rm(poly.orange) )
  # }
  
  colors = c("blue", "orange")
  
  p.funnel = ggplot( data = d, aes( x = d$yi,
                                    y = d$sei,
                                    color = d$positive ) )
  
  # if ( exists("poly.orange") ) {
  #   p.funnel = p.funnel + geom_polygon(data=poly.blue, mapping=aes(x=poly.blue$yi, y=poly.blue$sei),
  #                                      fill = colors[1],
  #                                      alpha = 0.2,
  #                                      color = NA)
  #   
  #   p.funnel = p.funnel +  geom_polygon(data=poly.orange, mapping=aes(x=poly.orange$yi, y=poly.orange$sei),
  #                                       fill = colors[2],
  #                                       alpha = 0.2,
  #                                       color = NA)
  # }
  
  # TESTING
  # bm
  # min.sei = min( d$sei )
  # max.sei = max( d$sei )
  # mod = lm( y ~ x,
  #           data = data.frame( x = c( just_signif_est( min.sei ), just_signif_est( max.sei ) ),
  #                              y = c(min.sei, max.sei) ) )
  # 
  # sl = coef(mod)[2]
  
  # because sl * 1 should be 
  sl = 1/qnorm(.975)
  int = 0
  # sanity check: should be exactly 0.05
  2 * ( 1 - pnorm( abs(1) / 0.5102135 ) )
  
  # sl <- ( coords2[2] - coords1[2] ) / ( coords2[1] - coords1[1] )
  # int <- coords2[2] - (sl*coords2[1])
  
  #Build the polygon
  datPoly <- buildPoly(range( d$yi ), range( d$sei ),
                       slope=sl,intercept=int,above=FALSE)
  names(datPoly) = c("yi", "sei")
  datPoly$positive = "Affirmative"
  datPoly$alpha = 0.3
  # bm: kind of works? but doesn't line up with the studies' own coloring
  
  # build the second polygon
  # exactly the same but with above=FALSE
  datPoly2 <- buildPoly(range( d$yi ), range( d$sei ),
                       slope=sl,intercept=int,above=TRUE)
  names(datPoly2) = c("yi", "sei")
  datPoly2$positive = "Affirmative"
  datPoly2$alpha = 0.3
  
  if ( plot.pooled == TRUE ) {
    
    p.funnel = p.funnel + geom_point(
      data = pooled.pts,
      aes( x = pooled.pts$yi, y = pooled.pts$sei ),
      size = 4,
      shape = 5,
      fill = NA,
      color = c(colors[1], "black")
    ) +
      
      geom_point(
        data = pooled.pts,
        aes( x = pooled.pts$yi, y = pooled.pts$sei ),
        size = 4,
        shape = 18,
        color = c(colors[1], "black"),
        alpha = .3
      ) +
      
      # just for visual separation of pooled ests
      geom_hline( yintercept = 0 )
  }
  
  p.funnel = p.funnel +
    
    # semi-transparent points with solid circles around them
    geom_point( size = 3, alpha=.3) +
    geom_point( size = 3, shape = 1) +
    
    scale_color_manual(values = colors) +
    
    xlab("Estimated log-risk ratio") +
    ylab("Estimated standard error") +
    
    # ~~ specific to AWR
    scale_y_continuous( limits = c(0, 1.5),
                        breaks = seq(0, 1.5, .25) ) +
    scale_x_continuous( limits = c(-0.5, 2),
                        breaks = seq(-0.5, 2, .25) ) +
    
    # xlab( bquote( hat(theta) ) ) +
    # ylab( bquote( hat(SE) ) ) +
    
    theme_classic() +
    theme(legend.title=element_blank())
  
  # add the poly
  
  p.funnel = p.funnel +
    geom_abline(slope=sl,intercept = int, color = "gray") +
    annotate(geom = "text", x = 1.25, y = .57, label = "Z = 1.96", color = "gray",
             angle = 38) 
  
   
  
  
    # geom_polygon( data = datPoly, mapping=aes(x=datPoly$yi, y=datPoly$sei),
    #               fill=colors[2],alpha=0.2,color=NA) +
    # geom_polygon( data = datPoly2, mapping=aes(x=datPoly2$yi, y=datPoly2$sei),
    #               fill=colors[1],alpha=0.2,color=NA)
  
  plot(p.funnel)
  return(p.funnel)
}



# for a given SE (y-value), return the "just significant" point estimate value (x-value)
just_signif_est = function( .sei ) .sei * qnorm(.975)

buildPoly <- function(xr, yr, slope = 1, intercept = 0, above = TRUE){
  #Assumes ggplot default of expand = c(0.05,0)
  xrTru <- xr + 0.05*diff(xr)*c(-1,1)
  yrTru <- yr + 0.05*diff(yr)*c(-1,1)
  
  #Find where the line crosses the plot edges
  yCross <- (yrTru - intercept) / slope
  xCross <- (slope * xrTru) + intercept
  
  #Build polygon by cases
  if (above & (slope >= 0)){
    rs <- data.frame(x=-Inf,y=Inf)
    if (xCross[1] < yrTru[1]){
      rs <- rbind(rs,c(-Inf,-Inf),c(yCross[1],-Inf))
    }
    else{
      rs <- rbind(rs,c(-Inf,xCross[1]))
    }
    if (xCross[2] < yrTru[2]){
      rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,Inf))
    }
    else{
      rs <- rbind(rs,c(yCross[2],Inf))
    }
  }
  if (!above & (slope >= 0)){
    rs <- data.frame(x= Inf,y= -Inf)
    if (xCross[1] > yrTru[1]){
      rs <- rbind(rs,c(-Inf,-Inf),c(-Inf,xCross[1]))
    }
    else{
      rs <- rbind(rs,c(yCross[1],-Inf))
    }
    if (xCross[2] > yrTru[2]){
      rs <- rbind(rs,c(yCross[2],Inf),c(Inf,Inf))
    }
    else{
      rs <- rbind(rs,c(Inf,xCross[2]))
    }
  }
  if (above & (slope < 0)){
    rs <- data.frame(x=Inf,y=Inf)
    if (xCross[1] < yrTru[2]){
      rs <- rbind(rs,c(-Inf,Inf),c(-Inf,xCross[1]))
    }
    else{
      rs <- rbind(rs,c(yCross[2],Inf))
    }
    if (xCross[2] < yrTru[1]){
      rs <- rbind(rs,c(yCross[1],-Inf),c(Inf,-Inf))
    }
    else{
      rs <- rbind(rs,c(Inf,xCross[2]))
    }
  }
  if (!above & (slope < 0)){
    rs <- data.frame(x= -Inf,y= -Inf)
    if (xCross[1] > yrTru[2]){
      rs <- rbind(rs,c(-Inf,Inf),c(yCross[2],Inf))
    }
    else{
      rs <- rbind(rs,c(-Inf,xCross[1]))
    }
    if (xCross[2] > yrTru[1]){
      rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,-Inf))
    }
    else{
      rs <- rbind(rs,c(yCross[1],-Inf))
    }
  }
  
  return(rs)
}



# #Generate some data
# dat <- data.frame(x=runif(10),y=runif(10))
# 
# #Select two of the points to define the line
# pts <- dat[sample(1:nrow(dat),size=2,replace=FALSE),]
# 
# 
# 
# min.sei = min( sqrt(d$varlogRR) )
# max.sei = max( sqrt(d$varlogRR) )
# 
# #Slope and intercept of line through those points
# mod = lm( y ~ x,
#           data = data.frame( x = c( just_signif_est( min.sei ), just_signif_est( max.sei ) ),
#                              y = c(min.sei, max.sei) ) )
# 
# sl = coef(mod)[2]
# int = 0
# 
# # sl <- ( coords2[2] - coords1[2] ) / ( coords2[1] - coords1[1] )
# # int <- coords2[2] - (sl*coords2[1])
# 
# #Build the polygon
# datPoly <- buildPoly(range( d$logRR ), range( sqrt(d$varlogRR) ),
#                      slope=sl,intercept=int,above=FALSE)
# 
# #Make the plot
# p <- ggplot(d, aes(x=logRR,y=sqrt(varlogRR))) +
#   geom_point() +
#   geom_abline(slope=sl,intercept = int) +
#   geom_polygon(data=datPoly,aes(x=x,y=y),alpha=0.2,fill="blue")
# print(p)
# # 
# 
