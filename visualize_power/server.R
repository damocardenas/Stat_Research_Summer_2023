shinyServer(function(input, output) {

  x <- seq(from=-4,to=7,by=0.01)
  y1 <- dnorm(x,mean=0,sd=1)
  line_height <- max(y1)
  
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
  

  ### This gives an action button to simulate.  I chose to use reactive plotting instead
#  sim=eventReactive(input$go, {
#    d=input$d
#    temp_p <- dist_p(d,n,reps) #get simulated p-values
#    h <- hist(temp_p,breaks=seq(from=0,to=1,by=0.025),plot=FALSE) #save histogram as h
#    h
#  })
  
## create plot of distribution of p-values
  output$pdist=renderPlot({
#   h <- sim()
    d=input$d #Cohen's d
    n=input$n #Sample size per group
    reps=input$reps #number of simulations
    temp_p <- dist_p(d,n,reps) #get simulated p-values
    h <- hist(temp_p,breaks=seq(from=0,to=1,by=0.025),plot=FALSE) #save histogram as h
    
    max_count <- max(h$counts) #height of tallest bar on histogram
    offset <- 0.02/((max_count-mean(h$counts))/reps+0.05) #code for adding space above bars when power is low
    cutoff <- cut(h$breaks,c(-Inf,0.049,Inf)) #code for coloring histogram
    p_0.05 <-  sum(temp_p<=0.05) #how many significant p-values?
    plot(h,main=paste("Simulated sampling distribution of p-values\n d = ",d," n = ",n," per group"),
         xlim=c(0,1),ylim=c(0,max_count*(1+offset)),xlab="p-value",col=c("dodgerblue4","dodgerblue")[cutoff])
    mtext(paste("(From ",reps," simulated two-sample t-tests)"))
        text(x=0.5,col="dodgerblue4",y=max_count*(0.9+offset),labels=paste("# of p-values < 0.05 = ",p_0.05))
    text(x=0.5,col="dodgerblue4",y=max_count*(0.75+offset),labels=paste("simulated power = ",p_0.05,"/",reps," = ",round(p_0.05/reps,2)))
  })
  
##create plot of Cohen's d
  output$cohen=renderPlot({
    d=input$d
    n=input$n
    y2 <- dnorm(x,mean=d,sd=1)
    plot(x,y1,type="l",lty=2,col="dodgerblue4",yaxt="n",bty="n",ylab="",
         main=paste("Two populations, Cohen's d = ",d),xlim=c(-3,5),ylim=c(0,0.5))
    mtext(paste("These are not sampling distributions.  
                They are the distributions that the raw data for each group come from."),
          padj=1)
    lines(x,y2)
    lines(x=c(0,0),y=c(0,line_height),col="dodgerblue4",lty=2)
    lines(x=c(d,d),y=c(0,line_height))
    abline(h=0)

  })
  
##create plot of power using t-distributions
  output$tdist=renderPlot({
    d=input$d
    n=input$n
    t1=dt(x,df=2*n-2)
    t2=dt(x,df=2*n-2,ncp=d/sqrt(2/n))
    crit <- qt(0.975,df=2*n-2)
    crit.left <- qt(0.025,df=2*n-2)
    x.shade <- c(crit,seq(from=crit,to=7,by=0.01),7)
    y.shade <- c(0,dt(x.shade[2:(length(x.shade)-1)],df=2*n-2,ncp=d/sqrt(2/n)),0)
    
    x.shade.left <- c(-4,seq(from=-4,to=crit.left,by=0.01),crit.left)
    y.shade.left <- c(0,dt(x.shade.left[2:(length(x.shade.left)-1)],df=2*n-2,ncp=d/sqrt(2/n)),0)
    
    x.shade.middle <- c(crit.left,seq(from=crit.left,to=crit,by=0.01),crit)
    y.shade.middle <- c(0,dt(x.shade.middle[2:(length(x.shade.middle)-1)],df=2*n-2,ncp=d/sqrt(2/n)),0)
    
    plot(x,t2,type="l",col="dodgerblue4",xlab="t",yaxt="n",bty="n",ylab="",ylim=c(0,0.5),
         xlim=c(-3,7),main=paste("Power for a two sample t-test, n = ",n," per group, d = ",d))
    mtext(paste("Left t-distribution is under the null, right is under the (true) alternative.
                Light shaded area is P(Type II error), dark shaded area is power"),
          padj=1)
    polygon(x.shade,y.shade,col="dodgerblue4")
    polygon(x.shade.left,y.shade.left,col="dodgerblue4")
    polygon(x.shade.middle,y.shade.middle,col="dodgerblue")    
    lines(x,t1,lty=2)
    
    lines(x=c(qt(0.975,df=2*n-2),qt(0.975,df=2*n-2)),y=c(0,0.2),lty=2,col="dodgerblue4") #left crit
    lines(x=c(qt(0.025,df=2*n-2),qt(0.025,df=2*n-2)),y=c(0,0.2),lty=2,col="dodgerblue4") #right crit

    abline(h=0)
#    The line below gives power for Welch's unequal variance t.  
#    To keep this simulation consistent with JMP, it has been modified to use equal variance via power.anova.test

#    power <-  round(power.t.test(n=n,delta=d,type="two.sample",alternative="two.sided")$power,2)

    power <- round(power.anova.test(groups=2,n=n,within.var = 1, between.var = (d^2/2))$power,2)
      
    text(x=crit+1,y=0.43,col="dodgerblue4",labels=paste("power = ",power),pos=4,cex=1.2)
    
  })

  
})