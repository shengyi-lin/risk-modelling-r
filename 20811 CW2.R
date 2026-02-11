#2
matrix <- matrix(c(41,1194,3320,7,1020,4064,21,873,5257,25,501,8476,8,271,2129), nrow=5, byrow=T)
rownames(matrix)<-c("Pedestrain","Pedal Cycle","Motorcycle","Car","Other Vehicle Occupants")
colnames(matrix)<-c("Fatal","Serious","Slight")
names(dimnames(matrix))<-c("Mode of Transport","Casualty Severity")
matrix
pt<-prop.table(matrix)
pt

R=5

ns=0
for(j in 1:R){
	ns[j]<-sum(matrix[j,])
}

mdp <- matrix / ns
pdf("barplot.pdf")
barplot(pt,beside=FALSE,legend.text=TRUE,args.legend = list(x = "topleft"),ylim=c(0,1),ylab="Propotions",xlab="Casualty Severity",main="the road casualties data")
dev.off()
pdf("modebarplot.pdf")
barplot(t(mdp),
        legend.text = TRUE, beside = FALSE,
        ylab = "Proportion in each mode",
        main = "Proportions of severity")
dev.off()

############
#3 
chisq.test(matrix)$expected
chisq.test(matrix)

############
#4
chisq.test(matrix)$residuals
chisq.test(matrix)$stdres

############
#5
R=5
C=3

N=sum(matrix)
N

phat=0
for(k in 1:C){
	phat[k]<- sum(matrix[,k])/N
}
phat
sum(phat)

B=5000
ysim=matrix(,nrow=R, ncol=C)
test.sim=0
for(i in 1:B){
	for(k in 1:R){
		ysim[k,]<-rmultinom(n=1, size=ns[k],prob=phat)
	}
	test.sim[i] <- chisq.test(ysim)$statistic
}
pdf("histo.pdf")
hist(test.sim,freq=F,ylim=c(0,0.15),main="Simulation of 5000 value with empirical and theoretical distribution",xlab="chi-square",ylab="Density")
lines(density(test.sim),col="red")
xx=seq(from=0, to=35, length.out=600)
dxx=dchisq(xx,df=(R-1)*(C-1))
lines(xx,dxx,col="blue")
dev.off()

#######
#6(i)
ser_pedal <- matrix["Pedal Cycle","Serious"]
n_pedal   <- sum(matrix["Pedal Cycle",])

ser_car <- matrix["Car","Serious"]
n_car   <- sum(matrix["Car",])

p_pedal <- ser_pedal / n_pedal
p_car   <- ser_car / n_car

diff <- p_pedal - p_car

se <- sqrt( p_pedal*(1-p_pedal)/n_pedal + p_car*(1-p_car)/n_car )

lower <- diff - 1.96*se
upper <- diff + 1.96*se

c(lower, upper)

#6(ii)
ser_mot <- matrix["Motorcycle","Serious"]
sli_mot <- matrix["Motorcycle","Slight"]
n_mot <- sum(matrix["Motorcycle",])

p_ser <- ser_mot / n_mot
p_sli <- sli_mot / n_mot

diff_mot <- p_ser - p_sli
se_mot <- sqrt( p_ser*(1-p_ser)/n_mot + p_sli*(1-p_sli)/n_mot )

lower2 <- diff_mot - 1.96*se_mot
upper2 <- diff_mot + 1.96*se_mot

c(lower2, upper2)