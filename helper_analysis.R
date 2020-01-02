
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
