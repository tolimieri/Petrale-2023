rm(list=ls())
library(tidyverse)

MainFile = getwd()
Data_loc = paste0(MainFile,'/00_Data/')
ROMSPrelim = paste0(MainFile,'/01_ROMS_Prelim')
Fig_loc = paste0(MainFile,'/Figures/')
dir.create(ROMSPrelim)
# setwd(ROMSPrelim)

# bring in data
#1980-2010
ROMS1 = data.frame(read.table(paste0(Data_loc,'/ROMS_vars_petrale.csv'),sep=',',header=TRUE))
#2011-2022
ROMS2 = data.frame(read.table(paste0(Data_loc,'/petrale_roms_variables_20221025.csv'),sep=',',header=TRUE))

dim(ROMS1)
dim(ROMS2)

colnames(ROMS1)
colnames(ROMS2)

# combined data files and prepare #################################
ROMS2 = ROMS2 %>% dplyr::select(!hours_since_20110102) %>% 
  rename(Year = year, Month = month, Day = day)

# match up column names to correct hypotheses

Hyp_names = data.frame(array(
  c(
  "temp_bottom_40_47N_50_200m",      "H2",
  "temp_bottom_40_47N_250_500m",     "H3",
  "temp_250_500m_40_47N_250_500m",   "H4",
  "temp_50_200m_40_47N_250_500m",    "H8",
  "temp_200_500m_40_47N_250_500m" ,  "H11",
  "temp_0_50m_40_47N_50_150km",      "H15",
  "temp_0_150m_40_47N_80_120km",     "H19",
  
  "mld_40_47N_250_500m",             "H5", 
  
  "ualong_50_200m_40_47N_250_500m",  "H6",
  "ualong_200_500m_40_47N_250_500m", "H10",
  "ualong_0_50m_40_47N_50_150km",    "H13",
  "ualong_0_150m_40_47N_80_120km",   "H17",
  "ualong_bottom_40_47N_50_150m",    "H20a",
  "ualong_bottom_40_47N_150_500m",   "H20b",
  
  "ucross_50_200m_40_47N_250_500m",  "H7",
  "ucross_200_500m_40_47N_250_500m", "H9",
  "ucross_0_50m_40_47N_50_150km"  ,  "H14",
  "ucross_0_150m_40_47N_80_120km",   "H18",
  "ucross_bottom_40_47N_50_150m",    "H21a",
  "ucross_bottom_40_47N_150_500m",   "H21b"
  ) , dim=c(2,20)))

Hyp_names=t(Hyp_names)
colnames(Hyp_names) = c('roms2','roms1')
Hyp_names = Hyp_names[,c('roms1','roms2')]
Hyp_names = data.frame(Hyp_names)
head(Hyp_names)

cnames = data.frame(roms2 = colnames(ROMS2))
cnames$roms2_hyp = Hyp_names$roms1[ match (cnames$roms2,Hyp_names$roms2) ]
cnames$roms2_hyp[1:3] = cnames$roms2[1:3]

# change colnames to hypothesis
colnames(ROMS2) <- cnames$roms2_hyp
# join
ROMS = full_join(ROMS1,ROMS2) 

write.csv( ROMS, paste0(Data_loc,"/RAW_ROMS_Data_combined_1980-2022.csv"), 
           row.names = FALSE)

############ Begin processesing data ##########################################
# make new var for winter values that cross years
ROMS$y2 = ROMS$Year
ROMS$y2 = ifelse(ROMS$Month %in% c(11,12),ROMS$Year+1,ROMS$Year)
head(ROMS)
#### calcuate periods specific values for each variable ####
fun = c('sum','mean','sd','max','min','median')
for(i in 1:length(fun)){
     print(i)
     roms <- ROMS  # reset dd calculations
     roms_2 = data.frame(1980:2022)
     colnames(roms_2)<-'year'
     # H2: Degree days preconditioning # need to set temp reference ####
     temp.ref = 3.5
     roms$H2 = roms$H2-temp.ref
     H2 = aggregate(H2 ~ Year, data=roms[roms$Month %in% 5:10,], FUN=fun[i])           
     roms_2$H2 = H2[match(roms_2$year, H2$Year+1),2] # match previous year
     # H3: Bottom temp as a spawning cue
     H3  = aggregate(H3 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3),], FUN=fun[i])               
     roms_2$H3 = H3[match(roms_2$year, H3$y2),2]
     # H4: Water column temp as a spawning cue 
     H4  = aggregate(H4 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3),], FUN=fun[i])               
     roms_2$H4 = H4[match(roms_2$year, H4$y2),2]
     # H5: MLD
     H5  = aggregate(H5 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])           
     roms_2$H5 = H5[match(roms_2$year, H5$y2),2]
     # H6: Cross shelf 
     H6  = aggregate(H6 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])             
     roms_2$H6 = H6[match(roms_2$year, H6$y2),2]
     # H7: Long shore
     H7  = aggregate(H7 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])             
     roms_2$H7 = H7[match(roms_2$year, H7$y2),2]
     # H8: Degree Days
     roms$H8 = roms$H8-temp.ref
     H8 = aggregate(H8 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])  
     roms_2$H8 = H8[match(roms_2$year, H8$y2),2]
     # H9: Cross shelf
     H9  = aggregate(H9 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])             
     roms_2$H9 = H9[match(roms_2$year, H9$y2),2]
     # H10: Long shore
     H10  = aggregate(H10 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])           
     roms_2$H10 = H10[match(roms_2$year, H10$y2),2]
     # H11: Degree Days
     roms$H11 = roms$H11-temp.ref
     H11 = aggregate(H11 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])        
     roms_2$H11 = H11[match(roms_2$year, H11$y2),2]
     # H12 Food.  Same as H13
     # H13 Longshore
     H13 = aggregate(H13 ~ y2, data=roms[roms$Month %in% c(12,1,2,3,4,5),], FUN=fun[i])        
     roms_2$H13 = H13[match(roms_2$year, H13$y2),2]
     # H14 Crossshelf
     H14 = aggregate(H14 ~ y2, data=roms[roms$Month %in% c(12,1,2,3,4,5),], FUN=fun[i])        
     roms_2$H14 = H14[match(roms_2$year, H14$y2),2]
     # H15 Degree days
     roms$H15 = roms$H15-temp.ref
     H15 = aggregate(H15 ~ y2, data=roms[roms$Month %in% c(11,12,1,2,3,4),], FUN=fun[i])        
     roms_2$H15 = H15[match(roms_2$year, H15$y2),2]
     # H16 NA. Zooplankon. No data.  Transport same as H17
     # H17 Longshore
     H17 = aggregate(H17 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
     roms_2$H17 = H17[match(roms_2$year, H17$y2),2]
     # H18 Crossshelf
     H18 = aggregate(H18 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
     roms_2$H18 = H18[match(roms_2$year, H18$y2),2]
     # H19 Degree days
     roms$H19 = roms$H19-temp.ref
     H19 = aggregate(H19 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
     roms_2$H19 = H19[match(roms_2$year, H19$y2),2]
     # H20 Longshore
     H20a = aggregate(H20a ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])        
     roms_2$H20a = H20a[match(roms_2$year, H20a$y2),2]
     # H21 Crossshelf
     H21a = aggregate(H21a ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])        
     roms_2$H21a = H21a[match(roms_2$year, H21a$y2),2]
     # H20 Longshore
     H20b = aggregate(H20b ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])        
     roms_2$H20b = H20b[match(roms_2$year, H20b$y2),2]
     # H21 Crossshelf
     H21b = aggregate(H21b ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])        
     roms_2$H21b = H21b[match(roms_2$year, H21b$y2),2]

     write.table(roms_2, paste0(Data_loc,'/Data_ROMS_',fun[i],'.csv'), sep=',', 
                 col.names = TRUE, row.names = FALSE)

} # end i ####
#### analysis using means for tranport mech ####
df = data.frame(read.table( paste0(Data_loc,"/Data_ROMS_mean.csv"),header=TRUE, sep=','))
# temps = data.frame(read.table( paste0(Data_loc,"/Data_ROMS_sum.csv"),header=TRUE, sep=','))
# # 
# # # bring in summed degree days into main file ####
# # # Replace temp means with sum degree days for some values.
# df$H2 = temps$H2
# df$H8 = temps$H8
# df$H11 = temps$H11
# df$H15 = temps$H15
# df$H19 = temps$H19

vnam=data.frame(matrix(c(
  "H2", "DDpre",
  "H3", "Tpre.a",
  "H4", "Tpre.b",
  "H5", "MLDegg",
  "H6", "LSTegg",
  "H7", "CSTegg1",
  "H8", "DDegg1",
  "H9", "CSTegg2",
  "H10", "LSTegg2",
  "H11", "DDegg2",
  "H12", NA,
  "H13", "LSTlarv",
  "H14", "CSTlarv",
  "H15", "DDlarv",
  "H16", NA,
  "H17", "LSTpjuv",
  "H18", "CSTpjuv",
  "H19", "DDpjuv",
  "H20a", "LSTbjuv.a",
  "H21a", "CSTbjuv.a",
  "H20b","LSTbjuv.b",
  "H21b","CSTbjuv.b"), byrow = TRUE, ncol=2)
)
colnames(vnam) =c('cn','nam')

cn = data.frame(colnames(df)[-1])
colnames(cn) = "cn"
cn$cnam = vnam$nam[match(cn$cn,vnam$cn)]
cnames = c('year',as.character(cn$cnam))
colnames(df) <- cnames

# some more adjustments
# old file is 4-day average
# new file is 1-day average
# need to change divide new DD by 4 to match up with old file

# df$DDpre[ df$year>2010 ] = df$DDpre[ df$year>2010 ]/4
# df$DDegg1[ df$year>2010 ] = df$DDegg1[ df$year>2010 ]/4
# df$DDegg2[ df$year>2010 ] = df$DDegg2[ df$year>2010 ]/4
# df$DDlarv[ df$year>2010 ] = df$DDlarv[ df$year>2010 ]/4
# df$DDpjuv[ df$year>2010 ] = df$DDpjuv[ df$year>2010 ]/4

write.table(df,paste0(Data_loc,"/Data_ROMS.for.analysis.mean.csv"), col.names=TRUE, row.names=FALSE, sep=',')

colnames(vnam) = c('roms1','var')

x = full_join(Hyp_names, vnam)

write.csv( x , "Variable_Names.csv" , row.names = FALSE)

