## Raw test output
##server.r
library(shiny)
library(data.table)
library(ggplot2)

# Create required data tables sub sets for summarizing, plotting and viewing  
    DT_airquality <- data.table(airquality)
    DT_5 <- subset(DT_airquality,Month==5)
    DT_6 <- subset(DT_airquality,Month==6)
    DT_7 <- subset(DT_airquality,Month==7)
    DT_8 <- subset(DT_airquality,Month==8)
    DT_9 <- subset(DT_airquality,Month==9)

##Shiny server fuction begins here
shinyServer(function(input, output) {
  
  # Returns the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "May"  = DT_5,
           "June" = DT_6,
           "July" = DT_7,
           "Aug"  = DT_8,
           "Sep"  = DT_9,)
   
               })
 ## Code for dynamically changing sub heading in the display and changing graph display varaible 
  output$sh <- renderText({
   input$sh
    var<-input$grtp
})
 # Printing changed sub heading  
  output$sh<- renderPrint({
    print(input$sh)
  })
# Generating a summary of the dataset selected
  output$summary <- renderPrint({
   dataset <- datasetInput()
   summary(dataset)
   
})
  
  # Printing  number of observation selected, if check box is clicked
    output$obs <- renderText({
      if(input$tbl)paste("Num. of Observations shown (Below Graph): ", input$n, "( Select required value 
                        using Slider scale shown left): ")
  })
      

# Prining graph based on the radio button selected
  output$distPlot <- renderPlot({
    dataset <- datasetInput()
  if (input$grtp == "1")p<-ggplot(data= dataset, aes(x=Day, y= Ozone))+geom_line(col = "dark blue", size = 1)+geom_point(size = 3)
  if (input$grtp == "2")p<-ggplot(data= dataset, aes(x=Day, y= Solar.R))+geom_line(col = "dark red", size = 1)+geom_point(size = 3)
  if (input$grtp == "3")p<-ggplot(data= dataset, aes(x=Day, y= Wind))+geom_line(col = "dark violet", size = 1)+geom_point(size = 3)
  if (input$grtp == "4")p<-ggplot(data= dataset, aes(x=Day, y= Temp))+geom_line(col = "dark green", size = 1)+geom_point(size = 3)
 suppressWarnings(print(p))
  
 # Number of observations seected by slide bar printed, if check box is true
 output$view <- renderTable({
    if(input$tbl)  head(datasetInput(), n = input$n)
    
  })
  
})
})
