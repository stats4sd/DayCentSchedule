library(shiny)
library(DT)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(shiny.i18n)

i18n <- Translator$new(translation_csvs_path =  "translation")
i18n$set_translation_language("en")

#validentries<-read.xlsx("dot100_variableLists.xlsx",1)
#write.csv(validentries,"validentries.csv",row.names = FALSE)
validentries<-read.csv("validentries.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(i18n$t("Create DayCent Scheduler File From Input"), windowTitle = NULL),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          htmlOutput("downloader"),
          uiOutput("selections"),
          uiOutput("headerin"),
          uiOutput("fn"),
        uiOutput("dl")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          shiny.i18n::usei18n(i18n),
          div(style = "float: right;",
              selectInput('selected_language',
                          i18n$t("Language"),
                          choices = i18n$get_languages(),
                          selected = i18n$get_key_translation())
          ),
          tabsetPanel(
            tabPanel(i18n$t("View Schedule"),
                     htmlOutput("warning"),
                     htmlOutput("schedule")),
           tabPanel(i18n$t("View Inputs"),
          DTOutput("data1"))
        )
    )
    )
)

#file1<-"C:/Users/sdumb/Documents/scheduleFile_template_SV2_test.xlsx"

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$selections <- renderUI({
    fileInput("file1", i18n$t("Upload Completed Excel Schedule Template"),
              multiple = FALSE,
              accept = c(".xlsx"))
  })
  output$headerin <- renderUI({
    checkboxInput("header",i18n$t("Include header/footer in schedule?"),value = TRUE)
  })
  
  
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  output$downloader<-renderText(
    
    HTML(paste(i18n$t('Download Blank Excel Template File:'),
    '<a href="https://github.com/stats4sd/DayCentSchedule/raw/main/scheduleFile_template.xlsx">Here</a><br><br>'))
    
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
      message<-paste("<p style='color:red;'>",
                     i18n$t("WARNING"),":",
                            nrow(fails),
                     i18n$t("code(s) specified in lookup list that were not found in the global dictionary"),".",
                     "<br>",
                     i18n$t("These are:"),"<br>",paste(fails$code,collapse="<br>"),"<br><br>",
                     i18n$t("Schedule file has been created with these codes still included.  Please check codes, and correct them if needed."),
                     "<br></p>")
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
    textInput("filename",i18n$t("Enter Output File Name"),value="myschedule")
  })
    
  output$dl <- renderUI({
    req(is.null(out1)==FALSE)
    downloadLink('downloadData',i18n$t('Download Daycent Schedule File'))
  })
  
  
  output$schedule<-renderUI({
    
    dl_vers1<-select(out1,-doy,-year)
    if(input$header==TRUE){
      col1<-c("[XXX]","[XXX]","[XXX].100",0,-1,"-1.00",-1,-1,-1,0,0,-1,1,"[XXX]","","","Year Month Option",
              "[XXX]","[XXX]","[XXX]","[XXX]","12","0.08333","F","[XXX].wth","")
      col2<-c("Starting Year","Last Year","Site file name","Labeling type","Labeling Year","Microcosm",
              "CO2 Systems","pH effect","Soil Warming","N input scalar option","OMAD scalar option",
              "Climate scalar option","Initial system","Initial crop","Initial tree","","","Block",
              "Last year","Repeats","Output starting year","Output month","Output interval","Weather choice","","")  
      dl_vers1<-data.frame(col1,col2) %>%
        rbind(data.frame(col1="&nbsp;&nbsp;",col2=paste(" ",dl_vers1$daycent))) %>%
        rbind(data.frame(col1="-999 -999 X",col2=""))
      
      tx1<-paste(paste(dl_vers1$col1,dl_vers1$col2,sep="\t"),collapse="<br>")

    }
    if(input$header==FALSE){
      tx1<-paste(paste0("&nbsp;&nbsp;",dl_vers1$daycent),collapse="<br>")
    }
    HTML(tx1)
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
      
      dl_vers<-select(out1,-doy,-year)
        if(input$header==TRUE){
          col1<-c("[XXX]","[XXX]","[XXX].100",0,-1,"-1.00",-1,-1,-1,0,0,-1,1,"[XXX]","","","Year Month Option",
                  "[XXX]","[XXX]","[XXX]","[XXX]","12","0.08333","F","[XXX].wth","")
          col2<-c("Starting Year","Last Year","Site file name","Labeling type","Labeling Year","Microcosm",
                  "CO2 Systems","pH effect","Soil Warming","N input scalar option","OMAD scalar option",
                  "Climate scalar option","Initial system","Initial crop","Initial tree","","","Block",
                  "Last year","Repeats","Output starting year","Output month","Output interval","Weather choice","","")  
          dl_vers<-data.frame(col1,col2) %>%
            rbind(data.frame(col1=" ",col2=dl_vers$daycent)) %>%
            rbind(data.frame(col1="-999 -999 X",col2=""))
        }
     else{
       dl_vers$daycent<-paste0("  ",dl_vers$daycent)
     }
      write.table(dl_vers,con,row.names=FALSE,quote = FALSE,col.names = FALSE
                    )
    }
)
  })  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
