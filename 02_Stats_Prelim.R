# preliminaries
rm(list=ls())
MainFile = getwd()
Data_loc = paste0(MainFile,'/00_Data/')
StatsPrelim = paste0(paste0(MainFile,'/02_Stats_prelim'))
Fig_loc = paste0(MainFile,'/Figures/')
dir.create(StatsPrelim)
library(tidyverse)

# load data ####
roms = data.frame(read.table(paste0(Data_loc,'/Data_ROMS.for.analysis.mean.csv'), header=TRUE, sep=','))
head(roms)

##### plot ROMS variables showing break in 2010 to visually evaluate change in patterns

# conver to long for gg plot

roms_long = roms %>% 
  pivot_longer(., cols=DDpre:CSTbjuv.b,
               names_to = "roms_param",
               values_to = "roms_data") %>%
  mutate(Period = ifelse(year<2011,'before','after'))
  
graphics.off()
png( paste0(Fig_loc,'/Timeseries_roms.png'), units = 'in',res=300, width=6.5, height=8)

ggplot(roms_long, aes(x=year, y = roms_data)) + 
  geom_line() + geom_point()+
  facet_wrap(facets='roms_param', ncol=3, scales = 'free_y')+
  xlab("") + ylab("Value")+
  theme_bw()
  
dev.off()


graphics.off()
png( paste0(Fig_loc,'/Timeseries_roms_pre.png'), units = 'in',res=300, width=6.5, height=8)

ggplot(roms_long %>% filter(year<2011), aes(x=year, y = roms_data)) + 
  geom_line() + geom_point()+
  facet_wrap(facets='roms_param', ncol=3, scales = 'free_y')+
  xlab("") + ylab("Value")+
  theme_bw()

dev.off()


graphics.off()
png( paste0(Fig_loc,'/Timeseries_roms_post.png'), units = 'in',res=300, width=6.5, height=8)

ggplot(roms_long %>% filter(year>2010), aes(x=year, y = roms_data)) + 
  geom_line() + geom_point()+
  facet_wrap(facets='roms_param', ncol=3, scales = 'free_y')+
  xlab("") + ylab("Value")+
  theme_bw()

dev.off()

# some summary tables

sum_tab = roms_long %>% group_by(Period, roms_param) %>%
  summarise(Mean = mean(roms_data, na.rm=TRUE), 
            Var = var(roms_data, na.rm=TRUE),
            SD = sd(roms_data, na.rm=TRUE))

a = sum_tab[ sum_tab$Period=='after',]
b = sum_tab[ sum_tab$Period=='before',]
Sum_Table = cbind(a,b)
Sum_Table

write.csv(Sum_Table, "Summary_stats_table.csv", row.names = FALSE)

graphics.off()
png( paste0( Fig_loc,'/Summary_stats_tseries.png'), units='in', res=300, width=5, height=4)
ggplot(sum_tab, aes(x=roms_param , y=Mean, color=Period)) + 
  geom_point() + geom_errorbar( aes(x=roms_param, ymin = Mean-SD, ymax=Mean+SD) ) +
  theme_bw(  ) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0))
dev.off()

graphics.off()
png( paste0(Fig_loc,'/Summary_stats_tseries_temp.png'), units='in', res=300, width=5, height=4)
ggplot(sum_tab %>% filter(Mean>1), aes(x=roms_param , y=Mean, color=Period)) + 
  geom_point() + geom_errorbar( aes(x=roms_param, ymin = Mean-SD, ymax=Mean+SD) ) +
  theme_bw(  ) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0))
dev.off()

graphics.off()
png( paste0(Fig_loc,'/Summary_stats_tseries-other.png'), units='in', res=300, width=5, height=4)
ggplot(sum_tab %>% filter(Mean<1), aes(x=roms_param , y=Mean, color=Period)) + 
  geom_point() + geom_errorbar( aes(x=roms_param, ymin = Mean-SD, ymax=Mean+SD) ) +
  theme_bw(  ) +
  theme(axis.text.x=element_text(angle = 90, hjust = 0))
dev.off()


#### begin fish stuff #####################################################

#### big table of correlations ####
df2 = roms[roms$year %in% 1981:2022,]
df3 = df2[,-1] # remove year
df3 = df3[,order(colnames(df3))]
cor1 = cor(df3)
write.csv(cor1,"R_corr.among.roms.csv")

# BRING IN PETRALE DATA AND SELECT YEARS ####

fish1 = data.frame(read.table(paste0(Data_loc,"/Petrale Sole recruitment deviations 2019.csv"),
                              header=TRUE, sep=','))
fish = fish1[fish1$Year>1980,]

fish = fish %>% 
  rename( year = Year, recdev = Value, SD = Parm_StDev ) %>%
  select(year, recdev, SD)

fish = left_join(roms,fish)
fish$period = ifelse(fish$year <2011 , 'before','after')
fish$period = as.factor(fish$period)


# FIXED EFFECTS MODELS
# exclude 2011 from analysis because of weird over winter calculations

m1 = lm( recdev ~ DDpre + MLDegg + CSTlarv + CSTbjuv.a , 
         data=fish %>% filter(year !=2011))
anova(m1)
summary(m1)

m2 = lm( recdev ~ DDpre + MLDegg + CSTlarv + CSTbjuv.a + period , 
         data=fish %>% filter(year !=2011))
anova(m2)
summary(m2)


m3 = lm( recdev ~ DDpre*period + MLDegg*period + CSTlarv*period + CSTbjuv.a*period , 
         data=fish %>% filter(year !=2011))
anova(m3)
summary(m3)

m4 = lm( recdev ~ DDpre*period + MLDegg*period + CSTlarv + CSTbjuv.a , 
         data=fish %>% filter(year !=2011))
anova(m4)
summary(m4)

capture.output( summary(m1), file = paste0(Fig_loc,"Model-noPeriod.txt"))
capture.output( summary(m1), file = paste0(Fig_loc,"Model-Period.txt"))
capture.output( summary(m3), file = paste0(Fig_loc,"Model-with-interactions.txt"))
capture.output( summary(m4), file = paste0(Fig_loc,"Model-with-interactions-backfit.txt"))

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)

pred = predict(m4, se.fit = TRUE, newdata = fish)
pred_fixed = data.frame(year = fish$year, fit = pred$fit, se = pred$se.fit)

fish = left_join(fish, pred_fixed)

write.csv( fish, paste0( Data_loc, "/Peteral-ROMS-data-w-predicted-recdev.csv"), row.names = FALSE)

graphics.off()
png( paste0(Fig_loc,"Predicted_time-series-fixed-effects.png") , units = 'in', res=300, width=3.5, height = 2)

ggplot( fish, aes(x=year, y=recdev)) +
  geom_point() + 
  geom_line( data=fish %>% filter(year!=2011), aes(x = year, y = fit) ) + 
  geom_ribbon(data=fish %>% filter(year!=2011), aes(ymin=fit-se, ymax=fit+se), alpha=0.05, color='lightgrey' ) +
  xlab("") + ylab('Recruitment deviations') +
  scale_x_continuous( breaks=seq(1980,2020,5) , minor_breaks = 1980:2022) +
  geom_segment( aes(x=2010.5, xend=2010.5, y = -1, yend=1) , color='red' , linetype='dotted') +
  geom_segment( aes(x=1980, xend=2022,y=0,yend=0), linetype='dashed') +
  theme_bw()

dev.off()

# RANDOM EFFECTS MODELS

library(nlme)

me1 = lme( recdev ~ DDpre + MLDegg + CSTlarv + CSTbjuv.a ,
            random = ~1|period, 
            data=fish %>% filter(year !=2011, !is.na(recdev)))
anova(me1)
summary(me1)


vf_p = varIdent(form=~1|period)
me2 = lme( recdev ~ DDpre + MLDegg + CSTlarv + CSTbjuv.a ,
           random = ~1|period, 
           weights = vf_p,
           data=fish %>% filter(year !=2011, !is.na(recdev)))
AIC(me1)
AIC(me2)

pred_mixed = data.frame(
  year = 1981:2022,
  fit_mixed = predict(me2, se.fit = TRUE, newdata = fish %>% filter(year %in% 1981:2022))
)

fish = left_join(fish,pred_mixed)

graphics.off()
png( paste0(Fig_loc,"Predicted_time-series-mixed.png") , units = 'in', res=300, width=3.5, height = 2)

ggplot( fish, aes(x=year, y=recdev)) +
  geom_point() + 
  geom_line( data=fish %>% filter(year!=2011), aes(x = year, y = fit_mixed) ) + 
  # geom_ribbon(aes(ymin=fit-se, ymax=fit+se), alpha=0.05, color='lightgrey' ) +
  xlab("") + ylab('Recruitment deviations') +
  scale_x_continuous( breaks=seq(1980,2020,5) , minor_breaks = 1980:2022) +
  geom_segment( aes(x=2010.5, xend=2010.5, y = -1, yend=1) , color='red' , linetype='dotted') +
  geom_segment( aes(x=1980, xend=2022,y=0,yend=0), linetype='dashed') +
  theme_bw()

dev.off()

write.csv( fish, paste0( Data_loc, "/Peteral-ROMS-data-w-predicted-recdev.csv"), row.names = FALSE)