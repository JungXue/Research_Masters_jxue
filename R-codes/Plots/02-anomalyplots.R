
mu = 100
n = c(2:5)
point = mu


prob.normal = list()
max.normal = c()
for (i in 1:length(n)){
  prob.normal[[i]] = dnorm(1:n[i], mean = (1+n[i])/2, sd = sd(c(1:n[i])))
  max.normal[i] = max(prob.normal[[i]],na.rm = T)
  prob.normal[[i]] = prob.normal[[i]]/max.normal[i]
}
prob.normal
max.normal

dnorm(1:2, mean = (1+2)/2, sd=sd(c(1:2)))


prob.gamma = list()
max.gamma = c()
for (i in 1:length(n)){
  prob.gamma[[i]]= dgamma(1:n[i], shape=1, rate = 1)
  max.gamma[i] = max(prob.gamma[[i]],na.rm = T)
  prob.gamma[[i]] = prob.gamma[[i]]/max.gamma[i]
}

prob.gamma
max.gamma

dgamma(1:2, shape=2, rate = 2)

barplot(prob.gamma[[1]],col="lightblue",border=F)
barplot(prob.gamma[[2]],col="lightblue",border=F)
barplot(prob.gamma[[3]],col="lightblue",border=F)


count.normal = list()
count.gamma = list()
for (i in 1:length(n)){
count.normal[[i]] = prob.normal[[i]]*mu
count.gamma[[i]] = prob.gamma[[i]]*mu
}
count.normal
count.gamma

length = (sum(1:5)+2 + 2*5)
x = 1:length 
normal.y = rep(0,length)
gamma.y = rep(0,length)

location = c(3,7,8,11,12,13,16,17,18,19,22,23,24,25,26)
  
normal.y[location] = c(mu,unlist(count.normal))
 gamma.y[location] = c(mu,unlist(count.gamma ))

par(mfrow=c(2,1))
par(mai=c(0.2,1,0.5,1))

barplot(normal.y,col="lightblue",border=F,ylab = "Normal distribution",ylim = c(0,150))
text(c(2,17),rep(135,2),c("Point Anomaly","Period anomaly"))
text(c(3,8,14,20,28),rep(110,5),c("N=1","N=2","N=3","N=4","N=5"),col="darkgrey")
abline(v=5,col="grey",lty = 2)

barplot(gamma.y,col="lightblue",border=F, ylab = "Gamma(1,1) distribution",ylim = c(0,150))
text(c(2,17),rep(135,2),c("Point Anomaly","Period anomaly"))
text(c(3,8,14,20,28),rep(110,5),c("N=1","N=2","N=3","N=4","N=5"),col="darkgrey")
abline(v=5,col="grey",lty = 2)

par(mfrow=c(1,1))
par(mai=c(1,1,1,1))
