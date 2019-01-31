#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


initPred <- predict.word('',4)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage(
    title = 'SlothKey',
    tabPanel(title = "Home",
       mainPanel(
        textAreaInput("txtType", "type here", "", width = "100%", height = "100px"),
        actionButton("btnSel1", initPred[1]),
        actionButton("btnSel2", initPred[2]),
        actionButton("btnSel3", initPred[3]),
        actionButton("btnSel4", initPred[4]),
        tags$script('Shiny.addCustomMessageHandler("refocus",
                     function(NULL) 
                     {
                       document.getElementById("txtType").focus();
                     });')
        #radioButtons("rbtnPreds", "next word", c("a","b","c","d"),"a") 
        )
       ),
    
    # Show a plot of the generated distribution
    tabPanel(title = "Help")
  )
))





