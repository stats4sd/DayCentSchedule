library(shiny)
library(DT)
library(openxlsx)
library(tidyverse)
library(lubridate)

#validentries<-read.xlsx("dot100_variableLists.xlsx",1)
#write.csv(validentries,"validentries.csv",row.names = FALSE)
validentries<-read.csv("validentries.csv")
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
                     htmlOutput("warning"),
                     htmlOutput("schedule")),
           tabPanel("View Inputs",
          DTOutput("data1"))
        )
    )
    )
)

file1<-"C:/Users/sdumb/Documents/scheduleFile_template_SV2_test.xlsx"

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
  lookups$code<-toupper(lookups$code)
  output$warning<-renderText({
    
    fails<-anti_join(lookups,validentries)
    if(nrow(fails)==0){
      message<-""
    }
    else{
      message<-paste("<p style='color:red;'>WARNING: ",nrow(fails)," code(s) specified in lookup list that were not found in the global dictionary.<br>",
                     "These are:<br>",paste(fails$code,collapse="<br>"),"<br><br>",
                     "Schedule file has been created with these codes still included.
                     Please check codes, and correct them if needed.<br></p>")
    }
    
    message
  })


  
  out1<-schedule %>%
    arrange(fecha) %>%
    mutate(siembra=ifelse(is.na(cultivo),
                          NA,"FRST"),
           date=as.Date(fecha,origin="1899-12-30"),
           doy=yday(date),
           year=1
    )
  for(i in 2:nrow(out1)){
    out1$year[i]<-ifelse(out1$doy[i]>out1$doy[i-1],out1$year[i-1],out1$year[i-1]+1)
  }
  out1<-out1  %>%
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
    
    schedule<- schedule %>%
      arrange(fecha) %>%
      mutate(siembra=ifelse(is.na(cultivo),
                            NA,"FRST"),
             date=as.Date(fecha,origin="1899-12-30"),
             doy=yday(date),
             year=1
      )
    for(i in 2:nrow(schedule)){
      schedule$year[i]<-ifelse(schedule$doy[i]>schedule$doy[i-1],
                               schedule$year[i-1],schedule$year[i-1]+1)
    }
  
   schedule  %>%
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
