
d=0#cohen's d
#function to simulate p-value for 2 sample t test with n=10, d=1

get_p <- function(d=1,n=10) {
  group1 <- rnorm(n)
  group2 <- rnorm(n,mean=d)
  t.test(group1,group2,var.equal = TRUE)$p.value
}

#function to simulate distribution of p-value from get_p function
dist_p <- function(d=1,n=10,reps=1000) {
  pvals <- rep(0,reps)
  for (i in 1:reps) {
    pvals[i] <- get_p(d,n)
  }
  pvals
}

n=20 #sample size
d=0.5

reps=5000 #number of simulations

temp_p <- dist_p(d,n,reps) #get simulated p-values

h <- hist(temp_p,breaks=seq(from=0,to=1,by=0.025),plot=FALSE) #save histogram as h

max_count <- max(h$counts) #height of tallest bar on histogram

offset <- 0.02/((max_count-mean(h$counts))/reps+0.05) #code for adding space above bars when power is low

cutoff <- cut(h$breaks,c(-Inf,0.049,Inf)) #code for coloring histogram

plot(h,main=paste("Simulated distribution of p-value, d = ",d," n = ",n," per group"),cex.main=1,
     xlim=c(0,1),ylim=c(0,max_count*(1+offset)),xlab="p-value",col=c("dodgerblue4","dodgerblue")[cutoff])
mtext(paste("(From ",reps," simulated two-sample t-tests)"))

p_0.05 <-  sum(temp_p<=0.05) #how many significant p-values?

text(x=0.5,col="dodgerblue4",y=max_count*(0.9+offset),labels=paste("# of p-values < 0.05 = ",p_0.05))
text(x=0.5,col="dodgerblue4",y=max_count*(0.75+offset),labels=paste("power = ",p_0.05,"/",reps," = ",round(p_0.05/reps,3)))

#plot cohen's d

x <- seq(from=-4,to=7,by=0.01)
y1 <- dnorm(x,mean=0,sd=1)

d <- 2

y2 <- dnorm(x,mean=d,sd=1)
plot(x,y1,type="l",col="dodgerblue4",yaxt="n",bty="n",ylab="",
     main=paste("Two populations, Cohen's d = ",d),xlim=c(-3,5))
lines(x,y2)
abline(v=0,col="dodgerblue4",lty=2)
abline(v=d,lty=2)
abline(h=0)

#plot power
d=0.2
n=40
t1=dt(x,df=2*n-2)
t2=dt(x,df=2*n-2,ncp=d/sqrt(2/n))

crit <- qt(0.975,df=2*n-2)
x.shade <- c(crit,seq(from=crit,to=7,by=0.01),7)
y.shade <- c(0,dt(x.shade[2:(length(x.shade)-1)],df=2*n-2,ncp=d/sqrt(2/n)),0)

plot(x,t2,type="l",col="dodgerblue4",xlab="t",yaxt="n",bty="n",ylab="",ylim=c(0,0.5),
     xlim=c(-3,7),main=paste("Power for a two sample t-test, n = ",n," per group, d = ",d))
mtext(paste("Left distribution is null, right is (true) alternative, shaded area is power"))
polygon(x.shade,y.shade,col="dodgerblue")
lines(x,t1,lty=2)
abline(v=qt(0.975,df=2*n-2),lty=2,col="dodgerblue4")
abline(v=qt(0.025,df=2*n-2),lty=2,col="dodgerblue4")
abline(h=0)



power <-  round(power.t.test(n=n,delta=d,type="two.sample",alternative="two.sided")$power,2)

text(x=crit+0.5,y=0.43,col="dodgerblue4",labels=paste("power = ",power),pos=4)

#########################################

t_empirical <- seq(from=-5,to=5,by=0.1)
p_empirical <- rep(0,length(t_empirical))
for (i in 1:length(t_empirical)) {
  p_empirical[i] <- pt(abs(t_empirical[i]),df=200,lower.tail = FALSE)*2
}

hist(p_empirical)
cbind(p_empirical,t_empirical)
