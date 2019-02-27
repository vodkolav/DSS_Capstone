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
    title = div(img(src="SlothKey-small.png"), "SlothKey",style = "margin:-15px -10px"),
    tabPanel(title = "Home",
       mainPanel(
        textAreaInput("txtType", "Type your thoughts here and the Sloth will suggest words each time you press space", "", width = "100%", height = "400px"),
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
    tabPanel(title = "Help",includeHTML("www/help.html"))
  )
))





