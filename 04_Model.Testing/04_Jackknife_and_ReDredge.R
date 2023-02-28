# setwd(paste0(main,'/04_Model.Testing/04_Jackknife & Refit'))
library(MuMIn)

######## LOAD DATA  ############
data_1 = df0


# bring up original dredge model
form_dredge = readRDS(paste0(data.loc,"/Formula_for_Dredge.rds"))
form_dredge

##########################################
#### RUN MODEL FITTING ###################
##########################################



#### Fit base model -- all terms ####
fit = lm(form_dredge, data=data_1, na.action = na.fail)

for(k in 1:nrow(data_1)){
     print(k)
     data_2 = data_1[-k,]
     #### FIT BASE MODEL ####
     fit = lm(form_dredge, data=data_2, na.action = na.fail)

     # RUN DREDGE ####
     mtable = dredge(fit, rank = AICc, m.lim = c(0,5),
                     subset= # block correlated terms from the model
                       !(CSTbjuv.a && CSTbjuv.b) &&
                       !(CSTbjuv.a && LSTbjuv.a) &&
                       !(CSTbjuv.a && LSTbjuv.b) &&
                       !(CSTbjuv.b && LSTbjuv.a) &&
                       !(CSTbjuv.b && LSTbjuv.b) &&
                       !(CSTlarv && CSTpjuv) &&  # 83
                       !(DDegg1 && DDegg2) &&  #   80
                       !(DDegg1 && DDlarv) &&  #   82
                       !(DDegg1 && DDpjuv) &&  #   75
                       !(DDlarv && DDpjuv) &&  #   84
                       !(LSTegg && LSTegg2) &&   # 77
                       !(LSTbjuv.a && LSTbjuv.b) &&  
                       !(Tpre.a && Tpre.b) &&    
                       !(DDegg2 && Tpre.a) &&
                       !(DDegg2 && Tpre.b) &&
                       dc(DDlarv, I(DDlarv^2)) &&
                       dc(DDegg1, I(DDegg1^2)),
                     extra = list(R2 = function(x)
                       summary(x)$r.squared[[1]], 
                       F = function(y)
                         summary(y)$fstatistic[[1]]),
                     trace=2 )
     #mtable = dredge(fit, rank = AICc, m.lim = c(0, max.parms))
     saveRDS(mtable, file=paste('Table_model.fits','_k.',k,"_2010.rds", sep="" ))
}  # end K

year = 1981:2010


x = dir()
y = x[grep('Table',x)]
y

write.table(y,'Model_order.csv',row.names=FALSE, col.names=TRUE, sep=',')

w = strsplit(y, fixed = TRUE, "_")

# get models with AIC < 2 and fewest DF

for(i in 1:length(y)){
     print(i)
     a = readRDS(y[i])
     a_2 = subset(a, delta<2)
     a1 = a_2[a_2$df == min(a_2$df),]
     rownames(a1) = paste(rep(y[i], nrow(a1)), 1:nrow(a1), sep='_')
     print(y[i])
     print(a1)
     if(i==1){A = a1}else{A = rbind(A,a1)}
}

A
saveRDS(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.rds")
write.table(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.csv",col.names = TRUE,sep = ',',row.names = FALSE)

A = readRDS("RESULTS_JACKKNIFE.REFIT_top.models.by.year.rds")

B = data.frame(A)
B[!is.na(B)] <- 1

cs = colSums(B, na.rm = TRUE)
cs
Table5 = data.frame(cs)
Table5

write.csv(Table5, paste0(main,"/Figures/Table_5.csv"), row.names = TRUE)

# return to best model info settings
setwd(main)
