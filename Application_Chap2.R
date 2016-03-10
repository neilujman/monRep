# =======================
# ==== Exercice 9  ======
# =======================

# == Q1 ==
install.packages("ismev")
library(ismev)
data(glass)
#attach(glass)
boxplot(glass, main="Boxplot de glass")
hist(glass, freq=FALSE, main="histogramme de glass")


# == Q2 ==
minGlass = -glass
hist(-glass,freq=FALSE, main="distrib de -glass")


# == Q3 ==
library(evir)
mod=gpd(-glass,threshold=-1.5)
hist(-glass,freq=F)
curve(dgpd(x,mu=-1.5,xi=0.244,beta=0.155),add=T,col=2)

mod2=gpd(-glass,threshold=-1)
hist(-glass,freq=F)
curve(dgpd(x,mu=-1,xi=0.244,beta=0.155),add=T,col=2)

library(extRemes)
mod3=fevd(-glass,threshold = -1)
hist(-glass,freq=F)
curve(devd(x,loc=-1.64,shape=-0.08,scale=0.277),add=T,col=2)


mod4=fevd(-glass,threshold = -1.5)
hist(-glass,freq=F,main="seuil=-1.5")
# mod4$results$par
# location       scale       shape 
# -1.64162644  0.27286361 -0.08434668 
curve(devd(x,loc=-1.64162,scale=0.27286,shape=-0.08434),add=T,col=2)


# == Q4 ==
n=length(glass)
pvec=(1:n)/(n+1)
qemp=quantile(-glass,prob=pvec)
qtheo=qevd(pvec,loc=-1.64162,scale=0.27286,shape=-0.08434)
plot(qemp,qtheo,main="QQplot de seuil -1.5")
abline(lm(qtheo~qemp))



# == Q5 ==
# u= -1.5
ks.test(-glass,"pevd",loc=-1.64162,scale=0.27286,shape=-0.08434,alternative = "less")

# u=-1
# ??????? même paramètres 



# == Q6 ==
resMax=gev(-glass)
hist(-glass,freq=F)
curve(dgev(x, xi=resMax$par.ests[1],sigma=resMax$par.ests[2],mu=resMax$par.ests[3]),add=T,col=2)


n=length(glass)
pvec=(1:n)/(n+1)
qemp=quantile(glass,prob=pvec)
qtheo=qgev(pvec,xi=resMax$par.ests[1],sigma=resMax$par.ests[2],mu=resMax$par.ests[3])
plot(qemp,qtheo,main="QQplot de max")
abline(lm(qtheo~qemp))


ks.test(-glass,"pgev",xi=resMax$par.ests[1],sigma=resMax$par.ests[2],mu=resMax$par.ests[3])

#________#
mod=evd::fgev(-glass)
mn=mod$estimate[[1]]
sigma=mod$estimate[[2]]
xi==mod$estimate[[3]]

plot(mod)
