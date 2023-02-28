rm(list=ls())
###### Set some stuff here  ##############

# main = getwd()
main = "C:/Users/nick.tolimieri/Desktop/Petrale_Example"
results.loc = "/03_Dredge"
data.loc = paste0(main,results.loc)
FigLoc = paste0(main,'/Figures')

# variables in the best model, including the dependent variable
# first slot is the dependent variable

best_fit_vars = c("dev","DDpre",'MLDegg','CSTlarv','CSTbjuv.a')

####### bring in model info and set BEST MODEL ################
# mostly just to look at

df0 = data.frame(read.csv(paste0(data.loc,"/Analyzed.data.csv")))
# choose model output

mtable = readRDS(paste0(data.loc,"/Table_model.fits.rds"))
mtable2 = subset(mtable, delta<2)
mtable2

# set the best model based on mtable2

# create formula 
form0 = paste0(best_fit_vars[1]," ~ ",best_fit_vars[2])
for(i in 3:length(best_fit_vars)){
 form0 = paste0(form0, " + ", best_fit_vars[i])
}
form0
form_best = as.formula(form0)

form_dredge = readRDS(paste0(data.loc,"/Formula_for_Dredge.rds"))
form_dredge

# some data manipulaiton for later
parms = best_fit_vars[-1] # used later
x = grep("^", parms, fixed = TRUE)
if(length(x) > 0){parms2 = parms[-x]}else{parms2 = parms}
# vars2 = c(vars[1], parms2)

# run 'Best Model'
# df1 = df0[,vars2]
m1 = lm(form_best, data=df0)
summary(m1)

######

setwd(main)

# Initial diagnostics  RUN AFTER doing the DREDGE analysis
FileDir = paste0(main,"/03_Dredge")
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/00_Initial_Diagnostics.R'))

# Bootstrap - randomize dependent variable ####
FileDir = paste0(main,'/04_Model.Testing/01a_Bootstrap_Random_Data')
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/01a_Bootstrap_Random_Data.r'))

# Bootstrap - to get bias ####
FileDir = paste0(main,'/04_Model.Testing/01b_Bootstrap_Bias')
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/01b_Bootstrap_Bias.r'))
# 
# Jackknife - by year  ####
FileDir = paste0(main,'/04_Model.Testing/02_Jackknife')
dir.create(FileDir)
setwd(FileDir)
JackLoc = FileDir
source(paste0(main,'/04_Model.Testing/02_Jackknife.r'))

# Jackknife - by year  ####
FileDir = paste0(main,"/04_Model.Testing/03_Resample_Recruitment_w_error")
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/03_Resample_Recruitment_w_error.R'))

# Jackknife - by year  ####
FileDir = paste0(main,'/04_Model.Testing/04_Jackknife_and_ReDredge')
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/04_Jackknife_and_ReDredge.r'))

# Jackknife - by year  ####
FileDir = paste0(main,'/04_Model.Testing/05_Resample_Recuitment_and_ReDredge')
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/05_Resample_Recuitment_and_ReDredge.r'))

# Jackknife - by year  ####
FileDir = paste0(main,'/04_Model.Testing/06_Jackknife_and_ReDredge_to_2005')
dir.create(FileDir)
setwd(FileDir)
source(paste0(main,'/04_Model.Testing/06_Jackknife_and_ReDredge_to_2005.r'))

# Figures & Tables ####

setwd(FigLoc)

source(paste0(main,'/Figure_1&2.r'))

source(paste0(main,'/Figure_3.r'))

# jk2010_loc = paste0(main,'/04_Model.Testing/06_Jackknife_and_ReDredge_to_2005/D_jackknife_to_2005.csv')
# source(paste0(main,'/Figure_3_2pane.r'))

### fix from here on
# need to open Figure_r code and set things.
source(paste0(main,'/Figure_4.r'))

source(paste0(main,'/Figure_5.r'))


























