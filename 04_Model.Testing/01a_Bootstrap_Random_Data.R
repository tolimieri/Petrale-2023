### Randomly resample dependent varible but not predictors ###
### gives expected R2 if everything was random ###

# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,'/04_Model.Testing/00_bootstrap_random'))
library(MuMIn)

# brind in fish & roms data 
data_1 = df0
# get fitted model for comparison ####
# add to file below ###

fm = lm(form_best, data=data_1)
sfm = summary(fm)
fmcoef = t(data.frame(sfm$coefficients[,1]))
fitted.model  = data.frame(cbind( sfm$r.squared,  sfm$adj.r.squared, 
                sfm$fstatistic[1], sfm$fstatistic[2], sfm$fstatistic[3], fmcoef))

data_2 = data_1 # duplicate so can bootstrap

for(k in 1:1000) {# refit model 1000 times
  # create new recrutment time series
  # BOOTSTRAP sampling with replacement ####
    print(paste("k = ", k))
    ROW = sample(1:nrow(data_1),replace=TRUE)
    data_2$dev = data_1$dev[ROW]   
    fit = lm(form_best, data=data_2, na.action = na.fail)
    s1 = summary(fit)
    Coeffs = t(data.frame(s1$coefficients[,1]))
    r1  = data.frame(cbind(s1$r.squared, s1$adj.r.squared,s1$fstatistic[1],s1$fstatistic[2],s1$fstatistic[3], Coeffs))
    if(k == 1){results = r1}else{results = rbind(results,r1)}
  } # end k loop
  colnames(results)[1:6] = c('r2','adjr2','F','df1','df2', 'Intercept')
  results$p = 1-pf(results$F,results$df1, results$df2)
  
  write.csv(results, "R_boot_random_deviations.csv")

  
# get mean and 95% CLs
  mn = apply(results,2,mean)
  md = apply(results,2,median)
  se = apply(results,2,sd)
# quantile function
  qt = function(x){quantile(x,c(0.025,0.975))}
  ci = apply(results,2, qt)
  boot.stats = rbind(mn,md, se, ci)
  fitted.model$p = NA
  colnames(fitted.model) <- colnames(boot.stats)  
  fitted.model$p = 1-pf(fitted.model$F,fitted.model$df1, fitted.model$df2)
  boot.stats2 = data.frame(rbind(fitted.model,boot.stats))
  bias = mn - boot.stats2[1,] 
  mn.corrected = boot.stats2[1,] - bias
  # mn.corrected = 2*boot.stats2[1,] - mn
  boot.stats3 = rbind(boot.stats2, bias, mn.corrected)
  x = rownames(boot.stats3)
  x[1] <- "fitted model"
  x[length(x)-1] = 'bias'
  x[length(x)] = 'mn.corrected'
  boot.results <- cbind(x,boot.stats3)

  boot.results
  write.csv(boot.results,'Stats_boot_random_deviations.csv')

  setwd(main)

