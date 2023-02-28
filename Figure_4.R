
# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,'/Figures'))

dfstd = apply(df0[,parms2],2,scale)

df2 = data.frame(cbind(df0$year, df0$dev, dfstd))
colnames(df2)[1:2] <- c('year','dev')
#### TO SET UP ####
  
m1 = lm(form_best, data=df2)
(s1 = summary(m1))
 
##### PARTIAL RESIDUAL PLOTS ####
XLIM = c(-3,2) # set for year lables to fit on graph
# start plot

graphics.off()


lp = length(parms)

if(lp == 2){
  pdf("Figure_4.pdf",width=2.5, height=4)
  par(mfrow=c(2,1), mar=c(4,4,1,2), pch=19, cex=0.7)
  }
if(lp == 3){
  pdf("Figure_4.pdf",width=2.5, height=6)
  par(mfrow=c(3,1), mar=c(4,4,1,2), pch=19, cex=0.7)}
if(lp == 4){
  pdf("Figure_4.pdf",width=6, height=5)
  par(mfrow=c(2,2), mar=c(4,4,1,2), pch=19, cex=0.7)}
if(lp == 5){
  pdf("Figure_4.pdf",width=6, height=7)
  par(mfrow=c(3,2), mar=c(4,4,1,2), pch=19, cex=0.7)}

ABC = c('a','b','c','d,','e')

Xlab = NA # option to set x-axis lables

for(i in 1:lp){
  print(i)
  x1 = df2[,parms[i]] 
  x1 = df2$DDpre
  x2 = s1$coefficients[i+1,1]*x1 + s1$residuals
  
  if(is.na(Xlab)==TRUE){XLAB = parms[i]}else(XLAB = Xlab)  
  
  plot(x1, x2,xlab = XLAB, xlim=XLIM, ylab = "Partial residual", bty="l")
  
  m2 = lm(x2  ~ x1)
  p1 = predict(m2, se.fit=TRUE)
  p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
  colnames(p2) = c('x','y','se')
  p2 = p2[order(p2$x),]
  lines(p2$x, p2$y)
  lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
  lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
  xy = par()$usr
  text(x1,x2,df2$year, cex=0.6, pos=2)
  legend('topleft',paste0("(",ABC[i],")"), bty='n')

}

graphics.off()
  

#setwd(main)
 