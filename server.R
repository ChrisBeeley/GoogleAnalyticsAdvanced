####################################
#### Google Analytics - server.R ###
####################################

library(shiny)
library(plyr)
library(ggplot2)

load("analytics.Rdata") # load the dataframe

# test - are they using the newer version of Shiny?

packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] > 8){
  
  shinyNew = TRUE
} else {
  shinyNew = FALSE
}

shinyServer(function(input, output, session){ # pass in a session argument
  # for extra functionality
  
  # keep control of the checkbox control
  
  observe({
    
    if(class(input$domainShow) != "character"){
      
      updateCheckboxGroupInput(session, "domainShow",
                               choices = list("NHS users" = "NHS",
                                              "Other" = "Other"),
                               selected = ifelse(shinyNew, "NHS", "NHS users")
      )
      
    }
    
  })
  
  # handle reactive UI 
  
  output$reacDomains <- renderUI({
    
    domainList = unique(as.character(passData()$networkDomain))
    
    selectInput("subDomains", "Choose subdomain", domainList)
    
  })
  
  # handle custom URL
  
  observe({
    
    searchString <- parseQueryString(session$clientData$url_search)
    
    if(length(searchString)>0){
      
      ## deal with first query which indicates the audience
      
      if(searchString[[1]] == "nhs"){
        
        updateCheckboxGroupInput(session, "domainShow",
                                 choices = list("NHS users" = "NHS",
                                                "Other" = "Other"),
                                 selected = "NHS")
        
        updateRadioButtons(session, "outputType",
                           choices = list("Visitors" = "visitors",
                                          "Bounce rate" = "bounceRate",
                                          "Time on site" = "timeOnSite"),
                           selected= "Bounce rate")
        
        updateTabsetPanel(session, inputId = "theTabs", selected = "summary")
        
      }
      
      if(searchString[[1]] == "other"){
        
        updateCheckboxGroupInput(session, "domainShow",
                                 choices = list("NHS users" = "NHS",
                                                "Other" = "Other"),
                                 selected = c("NHS users", "Other"))
        
        updateRadioButtons(session, "outputType",
                           choices = list("Visitors" = "visitors",
                                          "Bounce rate" = "bounceRate",
                                          "Time on site" = "timeOnSite"),
                           selected= "Visitors")
        
        updateTabsetPanel(session, inputId = "theTabs", selected = "monthly")
        
      }
      
      ## do they want a smooth?
      
      if(searchString[[2]] == "yes"){
        
        updateCheckboxInput(session, inputId = "smoother",
                            value = TRUE)
        
      }
      
    }
    
    
  })
  
  # prep data once and then pass around the program
  
  passData <- reactive({
    
    if(input$theTabs != "hourly"){
      
      analytics <- analytics[analytics$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      
    }
    
    if(input$theTabs != "monthly"){
      
      analytics <- analytics[analytics$Hour %in% as.numeric(input$minimumTime) : as.numeric(input$maximumTime),]
      
    }
    
    analytics <- analytics[analytics$Domain %in% unlist(input$domainShow),]
    
    analytics
    
  })
  
  output$monthGraph <- renderPlot({
    
    graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth(method = input$linearModel)
      
    }
    
    print(theGraph)
    
  })
  
  output$animateGraph <- renderPlot({
    
    graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    smoothData = graphData[graphData$Date %in%
                             quantile(graphData$Date, input$animation/100, type=1):
                             quantile(graphData$Date,
                                      (input$animation+20)/100, type=1),]
    
    theGraph <- theGraph + geom_smooth(data = smoothData, method = "lm",
                                       colour = "black") 
    
    print(theGraph)
    
  })
  
  
  output$hourGraph <- renderPlot({
    
    graphData = ddply(passData(), .(Domain, Hour), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth(method = "loess")
      
    }
    
    print(theGraph)
    
  })
  
  
  output$textDisplay <- renderText({
    
    if(is.null(input$dateRange)) return()
    
    paste(
      length(seq.Date(input$dateRange[1], input$dateRange[2], by = "days")),
      " days are summarised. There were", sum(passData()$visitors),
      "visitors in this time period."
    )
    
  })
  
  ### following sections define the code that makes graphs downloadable
  
  myTrend <- reactive({
    
    graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth(method = input$linearModel)
      
    }
    
    print(theGraph)
    
  })
  
  output$downloadData.trend <- downloadHandler(
    
    filename <- function() {
      paste("Trend_plot", Sys.Date(),".png",sep="") },
    content <- function(file) {
      png(file, width = 980, height = 400,
          units = "px", pointsize = 12,
          bg = "white", res = NA)
      
      trend.plot <- myTrend()
      
      print(trend.plot)
      
      dev.off()},
    contentType = 'image/png')
  
})