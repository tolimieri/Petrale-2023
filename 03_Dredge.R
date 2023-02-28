MainFile = "C:/Users/nick.tolimieri/Desktop/Petrale_Example"
DredgeFile = paste0(MainFile,"/03_Dredge")
dir.create(DredgeFile)
setwd(DredgeFile)
################################################
##### BRING IN PETRALE DATA ####################
################################################
fish00 = data.frame(read.table(paste0(main,'/00_Data/petrale.data.csv'), header=TRUE, sep=","))
head(fish00)
max(fish00$year)
#years = 1980:max(fish00$year)
years = 1980:2010  # only have ROMS data 1980-2010
fish = fish00[fish00$year %in% years,]

roms = data.frame(read.csv(paste0(main,'/01_ROMS_Prelim/Data_ROMS.for.analysis.mean.csv'), header=TRUE))
roms = roms[roms$year %in% years,]

##########################################
#### RUN MODEL FITTING ###################
##########################################
# roms = the roms data after processing


#### Build equation #### YEAR IS COLUMN 1 ####
for(j in 2:ncol(roms)){# build equation
  x1 = colnames(roms)[j]
  if(j==ncol(roms)){x2 = x1}else{x2 = paste(x1,"+")}
  if(j==2){ROMS = x2}else{ROMS = paste(ROMS,x2)}
}
ROMS

#### Set up Formula ####
form_dredge = as.formula(paste('dev ~ I(DDegg1^2) + I(DDlarv^2) +', ROMS, sep=''))
# 
form_dredge

saveRDS(form_dredge, "Formula_for_Dredge.rds")

#### Prep & combine data ####
# data_1 = # combine roms data and resids
data_1 = merge(fish,roms, 'year')
data_1 = na.omit(data_1)
write.table(data_1, 'Analyzed.data.csv',sep=',',col.names = TRUE, row.names = FALSE)


###############################################################################
##### run dredge twice ########################################################
##### once full, once minus the last five years ###############################
##### save output                               ###############################
###############################################################################

fit = lm(form_dredge, data=data_1, na.action = na.fail)

#### Run Dredge and save results ####
library(MuMIn)

mtable = dredge(fit, rank = AICc, m.lim = c(NA,5),
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
mtable2 = subset(mtable, delta<2)
mtable2

print(mtable2, abbrev.names = FALSE)
saveRDS(mtable, file='Table_model.fits.rds')
write.csv(mtable2,"R_Table_Delta2.csv")



mtable = readRDS('Table_model.fits.rds')
mtable2 = subset(mtable, delta<2)
mtable2

setwd(MainFile)






