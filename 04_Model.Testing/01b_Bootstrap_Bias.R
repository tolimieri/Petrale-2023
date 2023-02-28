### bootstrap to get bias ###
### produces Table 3 ###

# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,'/04_Model.Testing/01_bootstrap'))
library(MuMIn)

# brind in fish & roms data 
data_1 = df0
### bring in model table ####

  # get fitted model for comparison ####
  # add to file below ###
fm = lm(form_best, data=data_1)
sfm = summary(fm)
fmcoef = t(data.frame(sfm$coefficients[,1]))
fitted.model  = data.frame(cbind( sfm$r.squared,  sfm$adj.r.squared, 
                sfm$fstatistic[1], sfm$fstatistic[2], sfm$fstatistic[3], fmcoef))

data_2 = data_1 # duplicate so can bootstrap

for(z in 1:2){
  if(z==1){ # raw coefficients
    data_2 = data_1
    fm = lm(form_best, data=data_2)
    sfm = summary(fm)
    fmcoef = t(data.frame(sfm$coefficients[,1]))
    fitted.model  = data.frame(cbind( sfm$r.squared,  sfm$adj.r.squared, 
                    sfm$fstatistic[1], sfm$fstatistic[2], sfm$fstatistic[3], fmcoef))
  }  
  
  
  if(z == 2){ # standardized coefficients
    std = apply(data_1[,parms2],2, FUN=scale)
    data_2[,parms2] <- std
    fm = lm(form_best, data=data_2)
    sfm = summary(fm)
    fmcoef = t(data.frame(sfm$coefficients[,1]))
    fitted.model  = data.frame(cbind( sfm$r.squared,  sfm$adj.r.squared, 
                    sfm$fstatistic[1], sfm$fstatistic[2], sfm$fstatistic[3], fmcoef))
    }
 
  for(k in 1:1000) {# refit model 1000 times
  # create new recrutment time series
  # BOOTSTRAP sampling with replacement ####
    print(paste("k = ", k))
    ROW = sample(1:nrow(data_2),replace=TRUE)
    data_3 = data_2[ROW,]   
    fit = lm(form_best, data=data_3, na.action = na.fail)
    s1 = summary(fit)
    Coeffs = t(data.frame(s1$coefficients[,1]))
    r1  = data.frame(cbind(s1$r.squared, s1$adj.r.squared,s1$fstatistic[1],s1$fstatistic[2],s1$fstatistic[3], Coeffs))
    if(k == 1){results = r1}else{results = rbind(results,r1)}
  } # end k loop
  colnames(results)[1:6] = c('r2','adjr2','F','df1','df2', 'Intercept')
  results$p = 1-pf(results$F,results$df1, results$df2)
  
  if(z==1){write.csv(results, "R_boot_raw.csv")}
  if(z==2){write.csv(results, "R_boot_standardized.csv")}
  
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

  if(z==1){write.csv(boot.results,'Stats_boot_raw.csv')}
  if(z==2){write.csv(boot.results,'Stats_boot_standardized.csv')}
}

boot.raw = read.csv('Stats_boot_raw.csv')
boot.std = read.csv('Stats_boot_standardized.csv')

rownames(boot.raw) <- boot.raw$x
rownames(boot.std) <- boot.std$x

# rearrange

(Predictor = c("Intercept",parms))
(fm = t(boot.raw['fitted model',Predictor]))
(bias = t(boot.raw['bias',Predictor]))
(se = t(boot.raw['se',Predictor]))

(PredictorS = c("Intercept",parms))
(fmS = t(boot.std['fitted model',Predictor]))
(biasS = t(boot.std['bias',Predictor]))
(seS = t(boot.std['se',Predictor]))

Table3 = data.frame(cbind(Predictor, fm, bias, se,fmS, biasS, seS))
colnames(Table3) <- c('Predictor','Coefficient','Bias','SE', 'Std Coefficient', 'Std bias', 'Std SE')
Table3
write.csv(Table3, "Table_3.csv")
write.csv(Table3, paste0(FigLoc,'/Table_3.csv'))

setwd(main)

