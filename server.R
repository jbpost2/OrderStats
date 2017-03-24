library(shiny)
library(rgl)

##################################
##Some setting up of the app

#Set number of samples to do
N<-4000

#function need to find sample order statistic
quickfun<-function(data,ord){
  sort(data)[ord]
}

#set default color for bars
clr<-c(rep("white",length(seq(from=0,to=1,by=0.05))))

#function to plot histograms of order stats
orderPlot<-function(orderStat,index,ordValues,overlay,n,alpha,beta,color){    
  
  #set default color for bars
  clr<-c(rep("white",length(seq(from=0,to=1,by=0.05))))
  
  #Find bar for color coding
  ordBreaks<-cut(ordValues[index],breaks=seq(from=0,to=1,by=0.05))
  clr[ordBreaks]<-color

  #Create histogram
  hist(ordValues[1:index],main=paste("Histogram of order statistic ",orderStat," using ",index," sample(s)"), freq=FALSE,xlab=paste("Observed values of order statistic ",orderStat),xlim=c(0,1),breaks=seq(from=0,to=1,by=0.05),col=clr)
  
  #overlay if requested
  if (overlay){
    x<-seq(from=0,to=1,by=0.01)
    curve(n*choose(n-1,orderStat-1)*dbeta(x,shape1=alpha,shape2=beta)*(pbeta(x,shape1=alpha,shape2=beta))^(orderStat-1)*(1-pbeta(x,shape1=alpha,shape2=beta))^(n-orderStat),add=TRUE)
  }
}

####################################
##Server info
shinyServer(function(input, output,session) {

  #Take N samples of n observations from the beta(alpha, beta) distribution
  #Create the data and store it in a matrix 
  simData<-reactive({
    input$Param1
    input$Param2
    input$sampleSize
    
    #sample size and parameters
    n<-input$sampleSize
    par1<-input$Param1
    par2<-input$Param2
    
    #create data
    samples<-matrix(rbeta(n*N,shape1=par1,shape2=par2),nrow=N,ncol=n)
    samples
  })
  
  #Get order stats
  simOrder<-reactive({
    input$Param1
    input$Param2
    input$sampleSize
    
    #sample data
    samples<-simData()
    ordnumber1<-input$ord1
    ordnumber2<-input$ord2
    index<-input$numDataSets
    
    #find appropriate order stats
    ordValues1<-apply(FUN=quickfun,X=samples,MARGIN=1,ord=ordnumber1)
    ordValues2<-apply(FUN=quickfun,X=samples,MARGIN=1,ord=ordnumber2)
    
    #order stats to return
    ordValues<-data.frame(ordValues1=ordValues1,ordValues2=ordValues2)
    ordValues
  })

    
  #update the values the order stats can be
  observe({
    val<-input$sampleSize
    updateNumericInput(session,"ord1",max=val,min=1)
    updateNumericInput(session,"ord2",max=val,min=1)
    update
  })
  
  
  #plot of true distribution
  output$trueDist<-renderPlot({
    #Get parameters
    alpha<-input$Param1
    beta<-input$Param2
    #plotting sequence
    ps<-seq(from=0,to=1,by=0.005)
      
    #create plot
    plot(x=ps,dbeta(ps,shape1=alpha,shape2=beta),main="Parent Population",xlab="y",ylab="fY(y)",type="l",ylim=c(0,min(max(dbeta(ps,shape1=alpha,shape2=beta)+0.5),10)))
  })

  
  #Need to do this again for separate tab, not sure why...
  output$trueDistRepeat<-renderPlot({
    #Get parameters
    alpha<-input$Param1
    beta<-input$Param2
    #plotting sequence
    ps<-seq(from=0,to=1,by=0.005)
    
    #create plot
    plot(x=ps,dbeta(ps,shape1=alpha,shape2=beta),main="Parent Population",xlab="y",ylab="fY(y)",type="l",ylim=c(0,min(max(dbeta(ps,shape1=alpha,shape2=beta)+0.5),10)))
  })
  
  
  #Histogram of the sample
  output$sampleHist<-renderPlot({
    #get data and inputs
    index<-input$numDataSets
    samples<-simData()[index,]
    ordValues<-simOrder()[index,]
    ordnumber1<-input$ord1
    ordnumber2<-input$ord2
    
    #find appropriate order stats 
    ordvalues1<-ordValues$ordValues1
    ordvalues2<-ordValues$ordValues2
     
    #determine where this stat occurred for color coding
    ord1breaks<-cut(ordvalues1,breaks=seq(from=0,to=1,by=0.05))
    ord2breaks<-cut(ordvalues2,breaks=seq(from=0,to=1,by=0.05))
    clr[ord1breaks]<-"red"
    clr[ord2breaks]<-"blue"

    #Create histogram     
    hist(samples,main=paste("Sample ",index,"'s histogram of values"),freq=TRUE,xlab="Observed y values",xlim=c(0,1),breaks=seq(from=0,to=1,by=0.05),col=clr)
    abline(h=0)
  })

  
  #histogram of 1st selected order stat
  output$order1<-renderPlot({
    #get info and data
    samples<-simData()
    ordValues<-simOrder()
    index<-input$numDataSets
    ordnumber1<-input$ord1
    n<-input$sampleSize
    alpha<-input$Param1
    beta<-input$Param2
    
    #create plot  
    orderPlot(orderStat=ordnumber1,index=index,ordValues=ordValues$ordValues1,overlay=input$overlay,n=n,alpha=alpha,beta=beta,color="red")
 
  })
  
  
  #histogram of 2nd requested order stat
  output$order2<-renderPlot({
    #get info and data
    samples<-simData()
    ordValues<-simOrder()
    index<-input$numDataSets
    ordnumber2<-input$ord2
    n<-input$sampleSize
    alpha<-input$Param1
    beta<-input$Param2
    
    #create plot  
    orderPlot(orderStat=ordnumber2,index=index,ordValues=ordValues$ordValues2,overlay=input$overlay,n=n,alpha=alpha,beta=beta,color="blue")
    
  })


  #repeat for joint part of app
  output$order1Repeat<-renderPlot({
    #get info and data
    samples<-simData()
    ordValues<-simOrder()
    index<-input$numDataSets
    ordnumber1<-input$ord1
    n<-input$sampleSize
    alpha<-input$Param1
    beta<-input$Param2
    
    #create plot  
    orderPlot(orderStat=ordnumber1,index=index,ordValues=ordValues$ordValues1,overlay=input$overlay,n=n,alpha=alpha,beta=beta,color="red")
    
  })

  
  #repeat for joint
  output$order2Repeat<-renderPlot({
    #get info and data
    samples<-simData()
    ordValues<-simOrder()
    index<-input$numDataSets
    ordnumber2<-input$ord2
    n<-input$sampleSize
    alpha<-input$Param1
    beta<-input$Param2
    
    #create plot  
    orderPlot(orderStat=ordnumber2,index=index,ordValues=ordValues$ordValues2,overlay=input$overlay,n=n,alpha=alpha,beta=beta,color="blue")
    
  })


  #Joint graph
  output$orderJoint<-renderPlot({
    #get data and info
    samples<-simData()
    index<-input$numDataSets
    ordValues<-simOrder()
    ordValues1<-ordValues$ordValues1
    ordValues2<-ordValues$ordValues2
    #Now find the appropriate order stat for each sample
    ordnumber1<-input$ord1
    ordnumber2<-input$ord2
    
    #coloring
    ord1breaks<-cut(ordValues1[index],breaks=seq(from=0,to=1,by=0.1))
    ord2breaks<-cut(ordValues2[index],breaks=seq(from=0,to=1,by=0.1))
  
    jointclr<-c(rep("white",length(seq(from=0,to=1,by=0.1))^2))

    #grid of xy values to plot over
    xgrid<-ygrid<-seq(from=0,to=1,by=0.1)
  
    #matrix to store which grid point the value fell into
    z<-matrix(0,nrow=length(xgrid),ncol=length(ygrid))
    for (j in 1:index){
      z[min(which((ordValues1[j]<=ygrid)==TRUE)),min(which((ordValues2[j]<=xgrid)==TRUE))]<-z[min(which((ordValues1[j]<=ygrid)==TRUE)),min(which((ordValues2[j]<=xgrid)==TRUE))]+1
    }

    ##would be nicer to look at and manipulate but too slow.  Need to find a way to speed it up...
    #plot_df<-data.frame(xgrid=xgrid,ygrid=ygrid,z=z)
    #plot_ly(plot_df,x=xgrid,y=ygrid,z=z) %>% add_surface
    #
    #or
    #
    #kd<-with(ordValues,kde2d(ordValues1,ordValues2,n=50))
    #plot_ly(x=kd$x,y=kd$y,z=kd$z) %>% add_surface()

    #plot 3d of joint
    persp(x=xgrid,y=ygrid,z=z,main="Joint Distribution of the Order Stats",xlab=paste("Ord Value ",ordnumber1), ylab=paste("Ord Value ",ordnumber2),zlab="Joint",theta=-25,phi=25,d=5)
    
  })

})
