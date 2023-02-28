library(MuMIn)
# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,"/Figures"))

retro1 = data.frame(read.table(paste0(data.loc,'/D_Retro1.results.csv'),header=TRUE, sep=','))
retro5 = data.frame(read.table(paste0(data.loc,'/D_Retro5.results.csv'),header=TRUE, sep=','))
retro5a = data.frame(read.table(paste0(data.loc,'/D_Retro5a.results.csv'),header=TRUE, sep=','))
retro.full = data.frame(read.table(paste0(data.loc,'/D_full.retro.csv'),header=TRUE, sep=','))

# set ouput location
setwd(paste0(main,'/Figures'))

m1 = lm(form_best, data=df0)
(s1 = summary(m1))
p1 = predict(m1, se.fit=TRUE)

#### Figure 3 ########
#### Predicted vs Observed Figure ####

graphics.off()

pdf("Figure_3_2pane.pdf", width = 3.2, height=5)
# png("Figure_3_2.png", units='in', res=600, width = 3.2, height=5)
#tiff("Figure_3_2pane.tif", units='in', res=300, width = 3.2, height=5)

par(mar=c(4,4,1,1), mfrow = c(2,1))
pt.cex = 0.6

ymin = min(p1$fit-1.96*p1$se.fit, df0$dev)
ymax = max(p1$fit+1.96*p1$se.fit, df0$dev)
plot(1981:2010, df0$dev, type='p', ylim=c(ymin, ymax), 
     xlab="Year", ylab = 'Log deviations', pch=21, cex=pt.cex)

lines(1981:2010,p1$fit)
lines(1981:2010,p1$fit+1.96*p1$se.fit, lty='dotted')
lines(1981:2010,p1$fit-1.96*p1$se.fit, lty='dotted')
axis(side=1, labels=FALSE, at=1981:2010, tick = TRUE, tck = -0.01)
points(retro1$year, retro1$pred, pch=8, cex=pt.cex)
points(retro5a$year, retro5a$pred, pch=21, bg='red', cex=pt.cex)

legend('toplef', legend = 'a)', cex=0.7, bty='n')
#legend('topleft',legend=parms, cex=0.5, bty='n')

jk2010 = data.frame(read.csv(jk2010_loc))
jk2005 = jk2010[jk2010$year %in% 1981:2005,]
ymin = min(jk2005$fit-1.96*jk2005$se.fit, jk2005$dev)
ymax = max(jk2005$fit+1.96*jk2005$se.fit, jk2005$dev)

plot(df0$year, df0$dev,xlab = "Year", ylab = "Log deviations", pch=21,cex =  pt.cex)
lines(jk2005$year, jk2005$fit)
lines(jk2005$year, jk2005$fit + 1.96*jk2005$se.fit, lty='dotted')
lines(jk2005$year, jk2005$fit - 1.96*jk2005$se.fit, lty='dotted')

jk2006 = jk2010[jk2010$year %in% 2006:2010,]
lines(jk2006$year, jk2006$fit, col='red')
lines(jk2006$year, jk2006$fit + 1.96*jk2006$se.fit, lty='dotted', col='red')
lines(jk2006$year, jk2006$fit - 1.96*jk2006$se.fit, lty='dotted', col='red')

axis(side=1, at=1981:2010, labels = NA, tck = -0.02)
legend('toplef', legend = 'b)', cex=0.7, bty='n')
dev.off()


