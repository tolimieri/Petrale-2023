# source(paste0(main,'/00_BestModelInfo.R'))

# setwd(paste0(main,"/Figures"))
################################################
##### BRING IN PETRALE DATA ####################
################################################
fish00 = data.frame(read.table(paste0(main, '/00_Data/petrale.data.csv'), header=TRUE, sep=","))
head(fish00)
max(fish00$year)
# years = 1981:max(fish00$year)
years = 1981:2010
fish = fish00[fish00$year %in% years,]



#### Background figures  #### 
graphics.off()


####### fish background ########
# png("Figure_1.png", units='in', res=600, width = 6, height=6)
# tiff("Figure_1.tif", units='in', res=300, width = 6, height=6)
pdf("Figure_1.pdf", width = 6, height=6)
par(mfrow=c(2,2))
# spawning biomass
plot(fish$year, fish$sp.bio.1, type='l', pch=19, cex=0.5, xlab = "Year", ylab = "Spawning biomass (mt)")

# recruitment through time
plot(fish$year, fish$age.0, type='l', pch=19, xlab = "Year", ylab = "Age-0 (x 1000)")

# recuits per spawner
plot(fish$sp.bio.1, fish$age.0, type='p', pch=19,  cex=0.5, xlab = "Spawning biomass  (mt)", ylab = "Age-0 (x 1000)")

# recruitment residuals
plot(fish$year, fish$dev, type='l', pch=19, cex=0.7, xlab=NA, ylab='Log deviations', ylim=c(-1,1))
lines(fish$year, fish$dev + fish$devsd, lty='dotted')
lines(fish$year, fish$dev - fish$devsd, lty='dotted')

# lines(fish$year,rep(0,length(fish$year)), lty='dotted')

dev.off()

######## ROMS Variables in final model #########
roms = data.frame(read.csv(paste0(main, '/01_ROMS.Prelim/Data_ROMS.for.analysis.mean.csv'), header=TRUE))
roms = roms[roms$year %in% 1981:2010,]
parms
Inset = -0.08

graphics.off()
# png("Figure_2.png", width = 6, height=5, units='in', res=600)

# tiff("Figure_2.tif", width = 6, height=5, units='in', res=300, compression=c("lzw"))
pdf("Figure_2.pdf", width = 6, height=5)
par(mfrow=c(2,2), mar=c(4,4,1,1))

plot(roms$year,roms$DDpre, type='l', xlab = 'Year', ylab="DD - precond", bty='l')
legend('topleft', legend='(a)', bty='n', inset= Inset)

plot(roms$year,roms$MLDegg, type='l', xlab = 'Year', ylab = "MLD (m) - egg", bty='l')
legend('topleft', legend='(b)', bty='n', inset= Inset)

plot(roms$year,roms$CSTlarv, type='l', xlab = 'Year', ylab="CST (m/s) - larvae", bty='l')
legend('topleft', legend='(c)', bty='n', inset= Inset)

plot(roms$year,roms$CSTbjuv.a, type='l', xlab = 'Year', ylab="CST (m/s) - benthic juv.", bty='l')
legend('topleft', legend='(d)', bty='n', inset=Inset)

dev.off()



