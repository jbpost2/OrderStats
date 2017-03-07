library(shiny)
library(rgl)

#Set number of samples to do
N<-2000

#function need to find sample order statistic
quickfun<-function(data,ord){
  sort(data)[ord]
}

#set default color for bars
clr<-c(rep("white",length(seq(from=0,to=1,by=0.05))))

# Define server logic required to draw the plots
shinyServer(function(input, output,session) {

  #now we'll take N samples of n observations from the beta(alpha, beta) distribution
  #Create the data and store it in a matrix - rows represent data sets
  
  simData<-reactive({
    input$Param1
    input$Param2
    input$sampleSize
    
    #sample size
    n<-input$sampleSize
    par1<-input$Param1
    par2<-input$Param2
    
    samples<-matrix(rbeta(n*N,shape1=par1,shape2=par2),nrow=N,ncol=n)
    samples
  })
  
    
  output$truedist<-renderPlot({
    #Get parameters
    alpha<-input$Param1
    beta<-input$Param2
    #plotting sequence
    ps<-seq(from=0,to=1,by=0.005)
      
    #create plot
    plot(x=ps,dbeta(ps,shape1=alpha,shape2=beta),main="Parent Population",xlab="y",ylab="fY(y)",type="l",ylim=c(0,min(max(dbeta(ps,shape1=alpha,shape2=beta)+0.5),10)))
  })

  output$truedistRepeat<-renderPlot({
    #Get parameters
    alpha<-input$Param1
    beta<-input$Param2
    #plotting sequence
    ps<-seq(from=0,to=1,by=0.005)
    
    #create plot
    plot(x=ps,dbeta(ps,shape1=alpha,shape2=beta),main="Parent Population",xlab="y",ylab="fY(y)",type="l",ylim=c(0,min(max(dbeta(ps,shape1=alpha,shape2=beta)+0.5),10)))
  })
  
  
  output$samplehist<-renderPlot({
    index<-input$numDataSets
    samples<-simData()

    ordnumber1<-input$ord1
    ordnumber2<-input$ord2
     
    ordvalues1<-quickfun(samples[index,],ord=ordnumber1)
    ordvalues2<-quickfun(samples[index,],ord=ordnumber2)
     
    ord1breaks<-cut(ordvalues1,breaks=seq(from=0,to=1,by=0.05))
    ord2breaks<-cut(ordvalues2,breaks=seq(from=0,to=1,by=0.05))
    clr[ord1breaks]<-"red"
    clr[ord2breaks]<-"blue"
     
    hist(samples[index,],main=paste("Sample ",index,"'s histogram of values"),freq=TRUE,xlab="Observed y values",xlim=c(0,1),breaks=seq(from=0,to=1,by=0.05),col=clr)
    abline(h=0)
  })

  output$order1<-renderPlot({
    samples<-simData()
    index<-input$numDataSets
    #Now find the appropriate order stat for each sample
    ordnumber1<-input$ord1
 
    
    if(index==1){
      ordvalues1<-quickfun(samples[index,],ord=ordnumber1)
    } else {
      ordvalues1<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber1)
    }
   
    ord1breaks<-cut(ordvalues1[index],breaks=seq(from=0,to=1,by=0.05))
    clr[ord1breaks]<-"red"
    
    hist(ordvalues1,main=paste("Histogram of order statistic ",ordnumber1," using ",index," sample(s)"),
         freq=FALSE,xlab=paste("Observed values of order statistic ",ordnumber1),xlim=c(0,1),
         breaks=seq(from=0,to=1,by=0.05),col=clr)
    if (input$overlay){
      x<-seq(from=0,to=1,by=0.01)
      n<-input$sampleSize
      al<-input$Param1
      be<-input$Param2
      curve(n*choose(n-1,ordnumber1-1)*dbeta(x,shape1=al,shape2=be)*(pbeta(x,shape1=al,shape2=be))^(ordnumber1-1)*(1-pbeta(x,shape1=al,shape2=be))^(n-ordnumber1),add=TRUE)
    }
  })
  
  output$order2<-renderPlot({
    samples<-simData()
    index<-input$numDataSets  
    #Now find the appropriate order stat for each sample
    ordnumber2<-input$ord2
    quickfun<-function(data,ord){
      sort(data)[ord]
    }
  
    if (index==1){
      ordvalues2<-quickfun(samples[1:index,],ord=ordnumber2)
    } else {
      ordvalues2<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber2)
    }

    ord2breaks<-cut(ordvalues2[index],breaks=seq(from=0,to=1,by=0.05))
    clr[ord2breaks]<-"blue"
  
    hist(ordvalues2,main=paste("Histogram of order statistic ",ordnumber2," using ",index," sample(s)"),
      freq=FALSE,xlab=paste("Observed values of order statistic ",ordnumber2),xlim=c(0,1),
      breaks=seq(from=0,to=1,by=0.05),col=clr)
    if (input$overlay){
      x<-seq(from=0,to=1,by=0.01)
      n<-input$sampleSize
      al<-input$Param1
      be<-input$Param2
      curve(n*choose(n-1,ordnumber2-1)*dbeta(x,shape1=al,shape2=be)*(pbeta(x,shape1=al,shape2=be))^(ordnumber2-1)*(1-pbeta(x,shape1=al,shape2=be))^(n-ordnumber2),add=TRUE)
    }
  })


  output$order1Repeat<-renderPlot({
    samples<-simData()
    index<-input$numDataSets
    #Now find the appropriate order stat for each sample
    ordnumber1<-input$ord1
  
    if(index==1){
      ordvalues1<-quickfun(samples[index,],ord=ordnumber1)
    } else {
      ordvalues1<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber1)
    }
  
    ord1breaks<-cut(ordvalues1[index],breaks=seq(from=0,to=1,by=0.1))
    clr[ord1breaks]<-"red"
  
    hist(ordvalues1,main=paste("Histogram of order statistic ",ordnumber1," using ",index," sample(s)"),
      freq=FALSE,xlab=paste("Observed values of order statistic ",ordnumber1),xlim=c(0,1),
      breaks=seq(from=0,to=1,by=0.1),col=clr)
    if (input$overlay){
      x<-seq(from=0,to=1,by=0.01)
      n<-input$sampleSize
      al<-input$Param1
      be<-input$Param2
      curve(n*choose(n-1,ordnumber1-1)*dbeta(x,shape1=al,shape2=be)*(pbeta(x,shape1=al,shape2=be))^(ordnumber1-1)*(1-pbeta(x,shape1=al,shape2=be))^(n-ordnumber1),add=TRUE)
    }
})

  output$order2Repeat<-renderPlot({
    samples<-simData()
    index<-input$numDataSets  
    #Now find the appropriate order stat for each sample
    ordnumber2<-input$ord2
    quickfun<-function(data,ord){
      sort(data)[ord]
    }
  
    if (index==1){
      ordvalues2<-quickfun(samples[1:index,],ord=ordnumber2)
    } else {
      ordvalues2<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber2)
    }
  
    ord2breaks<-cut(ordvalues2[index],breaks=seq(from=0,to=1,by=0.1))
    clr[ord2breaks]<-"blue"
  
    hist(ordvalues2,main=paste("Histogram of order statistic ",ordnumber2," using ",index," sample(s)"),
      freq=FALSE,xlab=paste("Observed values of order statistic ",ordnumber2),xlim=c(0,1),
      breaks=seq(from=0,to=1,by=0.1),col=clr)
    if (input$overlay){
      x<-seq(from=0,to=1,by=0.01)
      n<-input$sampleSize
      al<-input$Param1
      be<-input$Param2
      curve(n*choose(n-1,ordnumber2-1)*dbeta(x,shape1=al,shape2=be)*(pbeta(x,shape1=al,shape2=be))^(ordnumber2-1)*(1-pbeta(x,shape1=al,shape2=be))^(n-ordnumber2),add=TRUE)
    }
  })



  output$orderJoint<-renderPlot({
    samples<-simData()
    index<-input$numDataSets
    #Now find the appropriate order stat for each sample
    ordnumber1<-input$ord1
    ordnumber2<-input$ord2
  
    if(index==1){
      ordvalues1<-quickfun(samples[index,],ord=ordnumber1)
      ordvalues2<-quickfun(samples[index,],ord=ordnumber2)
      } else {
      ordvalues1<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber1)
      ordvalues2<-apply(FUN=quickfun,X=samples[1:index,],MARGIN=1,ord=ordnumber2)
    }
  
    ord1breaks<-cut(ordvalues1[index],breaks=seq(from=0,to=1,by=0.1))
    ord2breaks<-cut(ordvalues2[index],breaks=seq(from=0,to=1,by=0.1))
  
    jointclr<-c(rep("white",length(seq(from=0,to=1,by=0.1))^2))

    #jointclr[ord1breaks]<-"red"
  
    #grid of xy values to plot over
    xgrid<-ygrid<-seq(from=0,to=1,by=0.1)
  
    #matrix to store which grid point the value fell into
    z<-matrix(0,nrow=length(xgrid),ncol=length(ygrid))
    for (j in 1:index){
      z[min(which((ordvalues1[j]<=ygrid)==TRUE)),min(which((ordvalues2[j]<=xgrid)
        ==TRUE))]<-z[min(which((ordvalues1[j]<=ygrid)==TRUE)),
        min(which((ordvalues2[j]<=xgrid)==TRUE))]+1
    }

    #plot 3d of joint
    persp(x=xgrid,y=ygrid,z=z,main="Joint Distribution of the Order Stats",
      xlab=paste("Ord Value ",ordnumber1), ylab=paste("Ord Value ",ordnumber2),
      zlab="Joint",theta=-25,phi=25,d=5)
  })

  observe({
    val<-input$sampleSize
    updateNumericInput(session,"ord1",max=val,min=1)
    updateNumericInput(session,"ord2",max=val,min=1)
    update
  })
  
})