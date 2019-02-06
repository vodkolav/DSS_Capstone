#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output, session) {
  
  vals <- reactiveValues(
    
    text = "your text",
    preds = predict.word("",1)
  )
  
  
  # observeEvent(c(input$btnSel2,input$btnSel1), {
  #     preds <- vals$preds[1]
  #     watt <- paste(input$txtType,preds)
  #     updateTextAreaInput(session, inputId = "txtType", value = watt)
  #     session$sendCustomMessage("refocus",list(NULL))
  #     #input$txtType <- renderText(watt)
  # })
  
  
  observeEvent(input$btnSel1, {
    preds <- vals$preds[1]
    watt <- paste(input$txtType,preds," ", sep = '')
    updateTextAreaInput(session, inputId = "txtType", value = watt)
    session$sendCustomMessage("refocus",list(NULL))
    #input$txtType <- renderText(watt)
  })


  
  
  
   observeEvent(input$txtType, {
      l <-stri_length(input$txtType)
      wat <- input$txtType
      if(substr(input$txtType,l,l+1)==" ")
      {
        tic<<-Sys.time()
        vals$preds <-  predict.word(wat,4)
        #updateRadioButtons(session, inputId = "rbtnPreds", choices = wat)
        updateActionButton(session, inputId = "btnSel1", label = vals$preds[1])
        updateActionButton(session, inputId = "btnSel2", label = vals$preds[2])
        updateActionButton(session, inputId = "btnSel3", label = vals$preds[3])
        updateActionButton(session, inputId = "btnSel4", label = vals$preds[4])
        print(Sys.time()-tic)
      }
   })      
})     




# observeEvent(input$rbtnPreds, {
#   preds <- isolate({input$rbtnPreds})
#   watt <- paste(input$txtType,preds)
#   updateTextAreaInput(session, inputId = "txtType", value = watt)
#   #input$txtType <- renderText(watt)
# })

# else {
      #   updateRadioButtons(session, inputId = "rbtnPreds", choices = c("","","",""))
      # }
      
      

      
      # output$txtConsole <- renderText({wat})      

   


   
   # preds <-""
    #preds <- eventReactive(input$rbtnPreds, {input$rbtnPreds } )
   # 
    #updateTextAreaInput(session, inputId = "txtType", value = paste(input$txtType,preds()))
   
   
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
  


