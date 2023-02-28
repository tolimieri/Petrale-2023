# setwd(paste0(main,'/04_Model.Testing/06_Dredge_and_jackknife_to_2005'))
library(MuMIn)

######## LOAD DATA  ############
data_1 = df0[df0$year %in% 1981:2005,]


# bring up original dredge model
# form_dredge = readRDS(paste0(data.loc,"/Formula_for_Dredge.rds"))
# form_dredge

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

x = dir()
y = x[grep('Table_model',x)]
y

write.table(y,'Model_order.csv',row.names=FALSE, col.names=TRUE, sep=',')

w = strsplit(y, fixed = TRUE, "_")

# get models with AIC < 2 and fewest DF

for(i in 1:length(y)){
     print(i)
     a = readRDS(y[i])
     a_2 = subset(a, delta<2)
     a_2
     a1 = a_2[a_2$df == min(a_2$df),]
     rownames(a1) = paste(rep(y[i], nrow(a1)), 1:nrow(a1), sep='_')
     print(y[i])
     print(a1)
     if(i==1){A = a1}else{A = rbind(A,a1)}
}

A
saveRDS(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.rds")
write.table(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.csv",col.names = TRUE,sep = ',',row.names = FALSE)

A = readRDS('RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.rds')

A = data.frame(A)
colnames(A)
A[is.na(A)] <- 0
A[A!=0] <- 1
A

B = data.frame(colSums(A))
B
write.csv(B,'Table_7.csv')
write.csv(B,paste0(main,"/Figures/Table_7.csv"))


########### predict last 1981-2010 ###################
m3 = lm(form_best, data = data_1 )
(s3 = summary(m3))
capture.output(s3, file = "Model_1981_2005.txt")
new.data = df0[df0$year %in% 1981:2010,]
new.data$color = ifelse(new.data$year < 2006, 'black','red')

p1 = predict(m3, newdata = new.data, se.fit = TRUE)

ymin = min(new.data$log.resids, (p1$fit-p1$se.fit))*1.1
ymax = max(new.data$log.resids, (p1$fit+p1$se.fit))*1.1
p1 = data.frame(p1)
data.out = cbind(new.data,p1)

write.csv(data.out, "D_jackknife_to_2005.csv", row.names = FALSE)

graphics.off()
png("F_jackknife_2005.png", units='in',res=600, width = 5, height = 3)
plot(new.data$year, new.data$log.resids, ylim = c(ymin,ymax), 
     col = new.data$color, pch=19,
     xlab = "Year", ylab = "Log residuals")
lines(new.data$year, p1$fit)
lines(new.data$year, p1$fit + 1.96*p1$se.fit, lty='dotted')
lines(new.data$year, p1$fit - 1.96*p1$se.fit, lty='dotted')
axis(side=1, at=1981:2010, labels = NA, tck = -0.02)

# return to best model info settings

setwd(main)
