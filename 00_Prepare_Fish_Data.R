##### prepare fish data ####

# main = getwd()
MainFile = "C:/Users/nick.tolimieri/Desktop/Petrale_Example"

setwd(paste0(MainFile,'/00_Data'))

fish = data.frame(read.table(paste0(MainFile,'/00_Data/petrale.data_raw.csv'), header=TRUE, sep=","))


#add some columns to lag spawning biomass one year = sp.bio.1
fish$yrminusone = fish$year - 1
fish$sp.bio.1 = fish$sp.bio[match(fish$yrminusone,fish$year)]

### add deviations ####

devs = data.frame(read.table(paste0(MainFile,'/00_Data/PetraleRecDevsForNick.csv'), header=TRUE, sep=","))

devs$Label = as.character(devs$Label)
devs$year = substring(devs$Label,nchar(devs$Label)-3,nchar(devs$Label)) 
colnames(devs) <- c("Label","dev",'devsd','year')

# calculate residuals 
# analysis doesn't use residulas calculated here.  This is just for reference.  
# Analysis uses recruitment deviations (and se) from the stock assessment.

h = 0.9               
Ro = exp(9.64)   # lnRo = 9.64  or 15483?
So = 33477 #  mt
b = (Ro*0.2*So)/(h*Ro)-(0.2*So)
S = fish$sp.bio.1
R.pred <- (Ro*S/(b+S))  
R.ass = fish$age.0 # recruitment from the assessment
resids = R.ass - R.pred
resids
# add to fish
fish$resids = resids
head(fish)
fish$log.resids = log(R.ass) - log(R.pred)
fish$SR_pred = R.pred

fish.out = merge(fish,devs,'year',all = TRUE)
write.csv(fish.out,'petrale.data.csv')

head(fish.out)
graphics.off()
pdf("Devs_vs_Resids.pdf")
plot(fish.out$resids,fish.out$dev)
graphics.off()
# if running 00_RUN.ALL.R
setwd(MainFile)

x = na.omit(fish.out)

cor(x$dev, x$log.resids)
