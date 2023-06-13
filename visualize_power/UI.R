shinyUI(fluidPage(
  titlePanel("Visualize power for a two-sample t-test, in terms of the t-distribution (top right) and the sampling distribution of p-values (bottom)"),
  fluidRow(
    column(6,
           plotOutput("cohen"),
           
           sliderInput("d",
                       label = "Cohen's d",
                       min = 0, max = 1.5, value = 0.2,step=0.05,width="350px"),
           
           sliderInput("n",label="sample size (per group)", min=3,max=60,value=20,width="350px"),
           
           sliderInput("reps", label = "Number of replications for p-value simulation
                       (Larger values slow down simulation)", min=1000,max=10000,value=2000,step=1000,
                       width="350px")
           
    ),
    
    column(6,
           plotOutput("tdist"),
           
           plotOutput("pdist")
  )
           
  )


           


))