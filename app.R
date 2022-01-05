library(shiny)
library(DT)
library(openxlsx)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DayCentScheduler"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Upload Schedule File",
                    multiple = FALSE,
                    accept = c(".xlsx")),
          uiOutput("fn"),
        uiOutput("dl")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Schedule",
          htmlOutput("schedule")),
           tabPanel("Input",
          DTOutput("data1"))
        )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$file1,{
    
  
  schedule<-read.xlsx(input$file1$datapath,1)
  lookups<-read.xlsx(input$file1$datapath,"lookups")

  out1<-schedule %>%
    mutate(siembra=ifelse(is.na(cultivo),
                          NA,"FRST"),
           doy=yday(as.Date(schedule$fecha,origin="1899-12-30"))
    ) %>%
    select(-fecha) %>%
    pivot_longer(cols=labranza:siembra,
                 names_to="parameter",values_to = "val",values_drop_na = TRUE) %>%
    left_join(lookups,by=c("parameter"="variable","val"="text")) %>%
    mutate(code=ifelse(parameter=="siembra",val,code),
           daycent=paste(doy,code)) %>%
    select(daycent,doy) 

  output$fn <- renderUI({
    req(is.null(out1)==FALSE)
    textInput("filename","Enter Output File Name",value="myschedule")
  })
    
  output$dl <- renderUI({
    req(is.null(out1)==FALSE)
    downloadLink('downloadData', 'Download Schedule')
  })
  
  
  output$schedule<-renderUI({
    HTML(paste(out1$daycent,collapse="<br>"))
  })
  
  output$data1<-DT::renderDT({
    
    out1 %>%
      group_by(doy) %>%
       summarise(daycent=paste(daycent,collapse=" "))->out2
    
    schedule %>%
      mutate(siembra=ifelse(is.na(cultivo),
                            NA,"FRST"),
             date=as.Date(schedule$fecha,origin="1899-12-30"),
             doy=yday(as.Date(schedule$fecha,origin="1899-12-30"))) %>%
      select(date,doy,labranza:siembra) %>%
      full_join(out2,by="doy") %>%
      filter(is.na(daycent)==FALSE)
  },options = list(pageLength = 100))
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$filename, '.txt', sep='')
    },
    content = function(con) {
      write.table(select(out1,-doy),con,row.names=FALSE,quote = FALSE,col.names = FALSE)
    }
)
  })  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
