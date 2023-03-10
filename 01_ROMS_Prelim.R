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
ROMSx = data.frame(read.table(paste0(Data_loc,'/petrale_roms_variables_20221025.csv'),sep=',',header=TRUE))

ROMS2 = ROMSx

ROMSx$d = 1:4
ROMSx$d2 = NA
for(i in 1:nrow(ROMS2)){
  ROMSx$d2[i] = ifelse( ROMSx$d[i] == 4, ROMSx$day[i],
                        ifelse( ROMSx$d[i] == 3, ROMSx$day[i+1],
                        ifelse( ROMSx$d[i] == 2, ROMSx$day[i+2],ROMSx$day[i+3])))
}


# 4 day average instead for 2011-2022
ROMS4 = ROMSx %>% dplyr::select(!hours_since_20110102) %>% 
  pivot_longer(., cols = temp_bottom_40_47N_50_200m:ucross_bottom_40_47N_150_500m) %>% 
  group_by(year, month, d2, name) %>% 
  summarise( mn = mean(value)) %>%
  pivot_wider(., names_from = name,
                 values_from = mn) %>%
  rename(Year = year, Month = month, Day = d2)
  
# combined data files and prepare #################################
ROMS2 = ROMS2 %>% dplyr::select(!hours_since_20110102) %>% 
  rename(Year = year, Month = month, Day = day)

#replace roms2 with roms 4

################### Replace roms2 with roms4? #############################
Four_Day = '1day' # '4day' or '1day'
if(Four_Day == '4day'){ROMS2 <- ROMS4}else{ROMS2 = ROMS2}

###########################################################################

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
  "ualong_200_500m_40_47N_250_500m", "H9",
  "ualong_0_50m_40_47N_50_150km",    "H13",
  "ualong_0_150m_40_47N_80_120km",   "H17",
  "ualong_bottom_40_47N_50_150m",    "H20a",
  "ualong_bottom_40_47N_150_500m",   "H20b",
  
  "ucross_50_200m_40_47N_250_500m",  "H7",
  "ucross_200_500m_40_47N_250_500m", "H10",
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

write.csv( ROMS, paste0(Data_loc,"/RAW_ROMS_Data_combined_",Four_Day,"_1980-2022.csv"), 
           row.names = FALSE)

########## preliminary examination of time series ########

ROMSlong = pivot_longer(ROMS, cols = H2:H21b) 

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
  "H21b","CSTbjuv.b"), byrow = TRUE, ncol=2))
colnames(vnam) =c('cn','nam')
# correct month and make a date column
ROMSlong$roms = vnam$nam[ match(ROMSlong$name, vnam$cn)]
ROMSlong$Month = ifelse(nchar(ROMSlong$Month)==1, paste0('0',ROMSlong$Month), ROMSlong$Month)
ROMSlong$date = as.POSIXct( paste(ROMSlong$Year,ROMSlong$Month,ROMSlong$Day, sep='-'))

graphics.off()
png( paste0(Fig_loc, "Raw-time-series_",Four_Day,".png"), units='in', res=300, width = 9, height= 9)
ggplot(ROMSlong, aes(x=date, y=value)) + 
  geom_line(  ) + xlab("") + ylab("") + 
  facet_wrap( facets = 'roms', ncol=3, scales = 'free_y') +
  theme_bw()
dev.off()


############ Begin processesing data ##########################################
# make new var for winter values that cross years
ROMS$y2 = ROMS$Year
ROMS$y2 = ifelse(ROMS$Month %in% c(11,12),ROMS$Year+1,ROMS$Year)
head(ROMS)
#### calcuate periods specific values for each variable ####
fun = c('sum','mean','sd','max','min','median')
for(i in 1:length(fun)){
     print(fun[i])
     roms <- ROMS  # reset dd calculations because converted to DD below
     roms_2 = data.frame(1980:2022) # file to add stuff to
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

     write.table(roms_2, paste0(Data_loc,'/Data_ROMS_',Four_Day,"_", fun[i],'.csv'), sep=',', 
                 col.names = TRUE, row.names = FALSE)

} # end i ####
#### analysis using means for tranport mech ####
df = data.frame(read.table( paste0(Data_loc,"/Data_ROMS_",Four_Day,"_mean.csv"),header=TRUE, sep=','))
cn = data.frame(colnames(df)[-1])
colnames(cn) = "cn"
cn$cnam = vnam$nam[match(cn$cn,vnam$cn)]
cnames = c('year',as.character(cn$cnam))
colnames(df) <- cnames

write.table(df,paste0(Data_loc,"/Data_ROMS.for.analysis.",Four_Day,".mean.csv"), col.names=TRUE, row.names=FALSE, sep=',')

colnames(vnam) = c('roms1','var')
x = full_join(Hyp_names, vnam)
write.csv( x , "Variable_Names.csv" , row.names = FALSE)

