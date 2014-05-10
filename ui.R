###############################
### Google Analytics - ui.R ###
###############################

library(shiny)

# test - are they using the newer version of Shiny?

packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] > 8){
  
  shinyNew = TRUE
} else {
  shinyNew = FALSE
}

# UI definition

shinyUI(pageWithSidebar( 
  
  headerPanel("Google Analytics"), 
  
  sidebarPanel(
    
    conditionalPanel(
      condition = "input.theTabs != 'hourly'",      
      dateRangeInput(inputId = "dateRange",  
                     label =  "Date range",
                     start = "2013-04-01",
                     max = Sys.Date()
      )
    ),
    
    checkboxInput(inputId = "smoother",
                  label = "Add smoother?",
                  value = FALSE),
    
    conditionalPanel(
      condition = "input.smoother == true",
      selectInput("linearModel", "Linear or smoothed",
                  list("lm", "loess"))
    ),
    
    conditionalPanel(
      condition = "input.theTabs != 'monthly'",      
      sliderInput(inputId = "minimumTime",
                  label = "Hours of interest- minimum",
                  min = 0,
                  max = 23,
                  value = 0,
                  step = 1
      ),

      sliderInput(inputId = "maximumTime",
                label = "Hours of interest- maximum",
                min = 0,
                max = 23,
                value = 23,
                step = 1)
    ),
    
    checkboxGroupInput(inputId = "domainShow",
                       label = "Show NHS and other domain?",
                       choices = list("NHS users" = "NHS",
                                      "Other" = "Other"),
                       selected = c(ifelse(shinyNew, "NHS", "NHS users"), "Other")
    ),
    
    radioButtons(inputId = "outputType",
                 label = "Output required",
                 choices = list("Visitors" = "visitors",
                                "Bounce rate" = "bounceRate",
                                "Time on site" = "timeOnSite")),
    
    sliderInput("animation", "Trend over time",
                min = 0, max = 80, value = 0, step = 5, 
                animate=animationOptions(interval=1000, loop=FALSE)),
    
   uiOutput("reacDomains")
    
  ),
  mainPanel(
    tabsetPanel(id ="theTabs",
                tabPanel("Summary", textOutput("textDisplay"),
                         textOutput("queryText"), value = "summary"), 
                tabPanel("Monthly figures", plotOutput("monthGraph"),
                         downloadButton("downloadData.trend","Download Graph"),
                         value = "monthly"),
                tabPanel("Hourly figures", plotOutput("hourGraph"),
                         value = "hourly"),
                tabPanel("Animated trend", plotOutput("animateGraph"),
                         value = "animTab")
    )
  )
))