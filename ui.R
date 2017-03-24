###########################################################################
##R Shiny App to plot the Distribution of order stats and their joint distribution
##Justin Post - Spring 2015 (updated 2017)
###########################################################################

library(shiny)
library(shinydashboard)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="red",
  #add title
  dashboardHeader(title="Visualizing the Distribution of Order Statistics",titleWidth=750),
  
  #define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Application", tabName = "app", icon = icon("laptop"))
  )),
  
  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
        fluidRow(
          #add in latex functionality if needed
          withMathJax(),
            
          #two columns for each of the two items
          column(6,
            #Description of App
            h1("What does this app do?"),
            #box to contain description
            box(background="red",width=12,
              h4("This application visualizes the distribution of order statistics when sampling from a Beta distribution."),
              h4("An order statistic is an ordered value from the sample.  For example, the minimum and the maximum are order statistics.  Order statistics are random variables and therefore have a distribution - which is often of interest."),
              h4("The goal of this applet is to visualize the distribution of an order statistic and the joint distribution of two order statistics."),
              h4("When doing random sampling from the Beta distribution, the distribution of any order statistic has a nice closed form (in fact is a Beta distribution)."),
              h4("The theory for order statistics - both marginally and jointly - is given in the applet as well.")
            )
          ),
          column(6,
            #How to use the app
            h1("How to use the app?"),
            #box to contain description
            box(background="red",width=12,
              h4("The tabs across the top of the applet allow for navigation between the marginal order statistic distribution visualization, the joint order statistic distribution visualization, and the theory underlying order statistics."),
              h4("The controls for the visualization sections of the applet are located to the left and the visualizations are available on the right."),
              h4("The top left input boxes allow the user to change the population from which the sample is taken.  The distribution is assumed to be a Beta, but the parameters are free to be chosen.  The graph of this distribution is seen in the top left."),
              h4("Below those boxes, the user can change the size of the sample and which order statistics to visualize the distribution of.  By default the sample size is five and the order statistics of interest are the minimum and maximum of the sample."),
              h4("The slider on the bottom left allows the user to step through the creation of the sampling distribution of the order statistics through repeated sampling.  By clicking the play button, a new data set is simulated (as seen in the bottom left graph).  The values of the order statistics requested are found and color coded.  These values are then placed their corresponding histograms on the top right and bottom right."),
              h4("Lastly, on the bottom left there is a checkbox to allow for the theoretical distribution of the order statistic to be overlayed on the order statistic histograms."),
              h4("The joint distribution section of the applet is similar except the sample's histogram is replaced with a visualization of the joint distribution of the selected order statistics.")
            )
          )
        )
      ),
      
      #actual app layout      
      tabItem(tabName = "app",
        fluidRow(
          column(3,
            box(width=12,title="Beta distribution with parameters",
              numericInput("Param1","Alpha = ",value=1,min=0.1,step=0.1),
              numericInput("Param2","Beta = ",value=1,min=0.1,step=0.1)
            ),
            sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
            h4("Order statistics of interest, choose integers from 1 to n"),
            numericInput("ord1","1st Order Stat",value=1,min=1,max=5),
            numericInput("ord2","2nd Order Stat",value=5,min=1,max=5),
            sliderInput("numDataSets", "Number of Data Sets:", min = 1, max = 4000, value = 1, step = 1, animate=list(TRUE, interval=350,loop=TRUE)),
            checkboxInput("overlay",label="Overlay Theoretical Distribution",value=FALSE)
          ),
          #Show a plot of the prior    
          column(9,
            tabsetPanel(
              tabPanel("Univariate Applet",           
                fluidRow(
                  column(6,
                    plotOutput("trueDist"),
                    br(),
                    plotOutput("sampleHist")
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
                    plotOutput("trueDistRepeat"),
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
      ) #end tabItem
    ) #end tabItems
  )
)




