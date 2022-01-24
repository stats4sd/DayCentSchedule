library(shiny)
library(DT)
library(openxlsx)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Create DayCent Scheduler File From Input"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          htmlOutput("downloader"),
          fileInput("file1", "Upload Completed Excel Schedule Template",
                    multiple = FALSE,
                    accept = c(".xlsx")),
          uiOutput("fn"),
        uiOutput("dl")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("View Schedule",
          htmlOutput("schedule")),
           tabPanel("View Inputs",
          DTOutput("data1"))
        )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$downloader<-renderText(
    
    HTML('Download Blank Excel Template File: <a href="https://github.com/stats4sd/DayCentSchedule/raw/main/scheduleFile_template.xlsx">Here</a><br><br>')
    
  )
  observeEvent(input$file1,{
    
  
    nms<-getSheetNames(input$file1$datapath)
    nms<-nms[nms!="lookups"]
    
    schedule<-NULL
    for(i in nms){
      tmp<-read.xlsx(input$file1$datapath,i)
      if(colnames(tmp)[1]=="fecha"){
        schedule<-rbind(tmp,schedule)
      }
    }
    
  lookups<-read.xlsx(input$file1$datapath,"lookups")

  out1<-schedule %>%
    arrange(fecha) %>%
    mutate(siembra=ifelse(is.na(cultivo),
                          NA,"FRST"),
           date=as.Date(fecha,origin="1899-12-30"),
           doy=yday(date),
           year=as.numeric(ceiling((date-
                                      min(date)+1)/365.25))
    ) %>%
    select(-fecha,-date) %>%
    pivot_longer(cols=labranza:siembra,
                 names_to="parameter",values_to = "val",values_drop_na = TRUE) %>%
    left_join(lookups,by=c("parameter"="variable","val"="text")) %>%
    mutate(code=ifelse(parameter=="siembra",val,code),
           daycent=paste(year,doy,code)) %>%
    select(daycent,year,doy)
  
output$fn <- renderUI({
    req(is.null(out1)==FALSE)
    textInput("filename","Enter Output File Name",value="myschedule")
  })
    
  output$dl <- renderUI({
    req(is.null(out1)==FALSE)
    downloadLink('downloadData', 'Download Daycent Schedule File')
  })
  
  
  output$schedule<-renderUI({
    HTML(paste(out1$daycent,collapse="<br>"))
  })
  
  output$data1<-DT::renderDT({
    
    out1 %>%
      group_by(year,doy) %>%
       summarise(daycent=paste(daycent,collapse=" "))->out2
    
    schedule %>%
      arrange(fecha) %>%
      mutate(siembra=ifelse(is.na(cultivo),
                            NA,"FRST"),
             date=as.Date(fecha,origin="1899-12-30"),
             doy=yday(date),
             year=as.numeric(ceiling((date-
                                        min(date)+1)/365.25))
      ) %>%
      select(date,doy,year,labranza:siembra) %>%
      full_join(out2,by=c("doy","year")) %>%
      filter(is.na(daycent)==FALSE)
  },options = list(pageLength = 100))
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$filename, '.txt', sep='')
    },
    content = function(con) {
      write.table(select(out1,-doy,-year),con,row.names=FALSE,quote = FALSE,col.names = FALSE)
    }
)
  })  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
