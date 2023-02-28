# need to find boot stuff first
library(MuMIn)
# source(paste0(main,'/00_BestModelInfo.R'))

############## FIGURE ##################
# setwd(paste0(main,'/04_Model.Testing/02_jackknife'))
results = data.frame(read.csv(paste0(JackLoc,'/R_jackknife_raw.csv')))
results = na.omit(results)
boot.final = data.frame(read.csv(paste0(JackLoc,'/R_jackknife_stats_raw.csv')))

graphics.off()
 pdf("Figure_5.pdf", width = 4, height =6)
# png("Figure_5.png", units='in',res=600, width=4, height=6)
# tiff("Figure_5.tif", units='in',res=300, width=4, height=6)
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(results$r2, xlab = 'R-squared', main=NA)
xy = par()$usr
text(xy[1]+0.1*(xy[2]-xy[1]), xy[3]+0.95*(xy[4]-xy[3]), "(a)")
plot(1981:2010, results$r2, xlab="Year (removed)", ylab = "R-squared", bty='l', pch=19, cex=0.8, cex.axis=0.8, ylim= c(0,1))
xy = par()$usr
text(xy[1]+0.1*(xy[2]-xy[1]), xy[3]+0.95*(xy[4]-xy[3]), "(b)")
segments(1980,boot.final$r2[1], 2010, boot.final$r2[1], lty='dotted')

dev.off()

setwd(main)