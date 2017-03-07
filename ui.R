###########################################################################
##R Shiny App to plot the Distribution of order stats and their joint distribution
##Justin Post - Spring 2015
###########################################################################

#Load package
library(shiny)

# Define UI for application that samples from a beta and displays the empirical distribution of order stats
shinyUI(
  fluidPage(
  
    # Sidebar with a slider input for the number of successes
    fluidRow(
      column(3,
        h3("Visualizing the Distribution of Order Statistics"),
        h5("Beta distribution with parameters"),
        numericInput("Param1","Alpha = ",value=1,min=0.1,step=0.1),
        numericInput("Param2","Beta = ",value=1,min=0.1,step=0.1),
        br(),
        sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
        br(),
        sliderInput("numDataSets", "Number of Data Sets:", 
           min = 1, max = 2000, value = 1, step = 1,
           animate=list(TRUE, interval=350,loop=TRUE)),
        h5("Order statistics of interest, choose integers from 1 to n"),
        numericInput("ord1","1st Order Stat",value=1,min=1,max=5),
        numericInput("ord2","2nd Order Stat",value=5,min=1,max=5),
        checkboxInput("overlay",label="Overlay Theoretical Distribution",value=FALSE)
      ), #end column

      #Show a plot of the prior    
      column(9,
        tabsetPanel(
          tabPanel("Univariate Applet",           
            fluidRow(
              column(6,
                plotOutput("truedist"),
                br(),
                plotOutput("samplehist")
              ),
              column(6,
                plotOutput("order1"),
                br(),
                plotOutput("order2")
              )
            )
          ), #end tab panel
          tabPanel("Joint Applet", 
            fluidRow(
              column(6,
                plotOutput("truedistRepeat"),
                br(),
                plotOutput("orderJoint")
              ),
              column(6,
                plotOutput("order1Repeat"),
                br(),
                plotOutput("order2Repeat")
              )
            )        
          ), #end tab panel
          tabPanel("Theoretical Distribution of Order Statistics", 
            tabsetPanel(
              tabPanel("Definition",
                fluidRow(
                  includeHTML("definition.html")
                )                
              ),
              tabPanel("Distribution of the Max",
                fluidRow(
                  includeHTML("max.html")
                )
              ),
              tabPanel("Distribution of the Min",
                fluidRow(
                  includeHTML("min.html")
                )
              ),
              tabPanel("General Order Stat Distribution",
                       fluidRow(
                         includeHTML("general.html")
                       )
              ),
              tabPanel("Joint Distribution",
                fluidRow(
                  includeHTML("joint.html")
                )
              )#end tabPanel
            ) #end tabsetPanel
          ) #end tab panel
        ) #end tab set
      ) #end column
    ) #end fluidrow
  ) #end fluidpage
) #end Shiny


