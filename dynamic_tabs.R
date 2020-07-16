library(shiny)
shinyUI(navbarPage("TiGr",
                   
                   tabPanel("File Input Page",
                            fluidPage("Input")),
                   
                   tabPanel("Summary Statistics and Plots",
                            fluidPage("Statistics")),
                   
                   tabPanel("Time Clusters",
                            fluidPage("cluster"),
                            actionButton("subClust", label = "Create Subcluster"),
                            uiOutput("tabs"),
                            conditionalPanel(condition="input.level==1",
                                             helpText("test work plz")
                            ), 
                            conditionalPanel(condition="input.level==5",
                                             helpText("hohoho")
                            )
                   )
))

shinyServer(function(input, output,session) {
  output$tabs=renderUI({
    
    Tabs<-as.list(rep(0,input$subClust+1))
    for (i in 0:length(Tabs)){
      Tabs[i]=lapply(paste("Layer",i,sep=" "),tabPanel,value=i)
    }
    
    #Tabs <- lapply(paste("Layer",0:input$subClust,sep=" "), tabPanel)
    do.call(tabsetPanel,c(Tabs,id="level"))
  })
}
)
