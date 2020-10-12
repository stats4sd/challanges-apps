#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(stringi)
library(reshape2)
library(tm)
library(wordcloud2)

# Define UI for application that draws a histogram
ui <- fluidPage(


  tabsetPanel(tabPanel("Input", textInput(inputId = "what1",
            label = "What are some of the challenges faced by the farmers you work with?"),
                                                actionButton(inputId = "submit",label="Submit")
  ,

  conditionalPanel(condition="input.submit>0",
                   htmlOutput("Message")
    )),

  tabPanel("Cloud",actionButton(inputId = "refresh2",label="Refresh"),
           wordcloud2Output("Plot1")),
  tabPanel("Results",actionButton(inputId = "refresh",label="Refresh"),
           dataTableOutput("Data1"))



))

# Define server logic required to draw a histogram
server <- function(input,output,session) {

  observeEvent(input$submit,{
    isolate(write.csv(data.frame(What=input$what1),
                      paste("ch/R",stri_rand_strings(1,5),".csv",sep=""),
                      row.names = FALSE))
  })

  observeEvent(input$submit,{

    output$Message<-renderUI({shiny::HTML('Thank you for your submission.')})
      })

 observeEvent(input$submit|input$refresh|input$refresh2,{
   l1<-paste("ch/",list.files("ch"),sep="")
   What<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE))


   t1<-removePunctuation(tolower(unlist(str_split(stripWhitespace(What)," "))))
   t1<-t1[!t1%in%stopwords("english")]
    t1.1<-data.frame(table(t1))


output$Plot1<-renderWordcloud2({
      wordcloud2(t1.1,size=0.4)})


  t2.2<-data.frame("S"=What)
  colnames(t2.2)<-c("What are some of the challenges faced by the farmers you work with?")
  rownames(t2.2)<-NULL
 output$Data1<-renderDataTable({
  t2.2
 })


})


}

# Run the application
shinyApp(ui = ui, server = server)

