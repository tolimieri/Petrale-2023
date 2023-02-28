### resample individual points using mean and sd from stock assessment for recruitment
# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,"/04_Model.Testing/03_resample.points"))

library(MuMIn)
data_1 = df0


fm = lm(form_best, data=data_1)
sfm = summary(fm)
fmcoef = t(data.frame(sfm$coefficients[,1]))
fitted.model  = data.frame(cbind( sfm$r.squared, sfm$adj.r.squared, sfm$fstatistic[1], sfm$fstatistic[2], sfm$fstatistic[3], fmcoef))

## data_1$sd = recdev$SD[match(data_1$year,recdev$Year)]
# sd = data_1$sd
data_2 = data_1 # switch name so can overwrite below

n = nrow(data_2)

for(k in 1:1000) {# refit model 1000 times
  # create new recrutment time series
  print(paste("k = ", k))
  data_2$dev = log(rlnorm(n, data_1$dev, data_1$devsd))
  fit = lm(form_best, data=data_2, na.action = na.fail)
  s1 = summary(fit)
  Coeffs = t(data.frame(s1$coefficients[,1]))
    r1  = data.frame(cbind(s1$r.squared, s1$adj.r.squared,s1$fstatistic[1],s1$fstatistic[2],s1$fstatistic[3], Coeffs))
    if(k == 1){results = r1}else{results = rbind(results,r1)}
  } # end k loop
  colnames(results)[1:6] = c('r2','adjr2','F','df1','df2', 'Intercept')
  results$p = 1-pf(results$F,results$df1, results$df2)
  
  write.table(results, 'R_boot_sd.csv', sep=',',col.names = TRUE, row.names = FALSE)
  
  # get mean and 95% CLs
  mn = apply(results,2,mean)
  md = apply(results,2,median)
  # quantile function
  qt = function(x){quantile(x,c(0.025,0.975))}
  ci = apply(results,2, qt)
  boot.stats = rbind(mn,md, ci)
  fitted.model$p = NA
  colnames(fitted.model) <- colnames(boot.stats)
  fitted.model$p = 1-pf(fitted.model$F,fitted.model$df1, fitted.model$df2)
  boot.stats2 = data.frame(rbind(fitted.model,boot.stats))
  x = rownames(boot.stats2)
  x[1] <- "fitted model"
  write.csv(boot.stats2,'R_boot.stats_sd.csv')
boot.stats2

setwd(main)

