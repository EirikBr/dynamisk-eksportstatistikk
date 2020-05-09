
# package needed
library(shiny)

options(encoding="UTF-8")
library(reshape)
library(kableExtra)
library (tidyverse)
library(data.table)
library(DT)
library(httr)
library(rjson)
library(RJSONIO)
# use rjstat JSON-stat library
library(rjstat)
library(checkmate)
library(backports)
library(ggplot2)
library(shinydashboard)
library(htmltools)
library(shiny)
library(shinyjs)
library(httpuv)
library(xtable)
# bruk dplyr for Ã¥ rydde tabeller og data
library(dplyr)
# bruk writexl for Ã¥ eksportere til excel
#library(xlsx)
# Adress to eksport   JSON-Stat dataset for eksoprtstat - Index 
url <- "https://data.ssb.no/api/v0/no/table/08817/"
data <- '{
"query": 
[
  {
  "code": "Region",
  "selection": {
  "filter": "item",
  "values": [
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12",
  "14",
  "15",
  "50",
  "16",
  "17",
  "18",
  "19",
  "20",
  "21",
  "22",
  "NVFF",
  "PUDT",
  "99"
  ]
  }
  },
  {
  "code": "SITC",
  "selection": {
  "filter": "item",
  "values": [
  "SITC0-1",
  "SITC03u",
  "SITC2_4",
  "SITC3b",
  "SITC5-9",
  "SITCT"
  ]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
  }
  '
  eksport <- data.frame()
  
  d.tmp <- POST(url , body = data, encode = "json", verbose())
  
  eksport <- fromJSONstat(content(d.tmp, "text"))
  
  eksport2 <- data.frame(eksport)
  
  colnames(eksport2) <- c("Region","Varegruppe","Variabel","Måned","Beløp")
  
  
  
  aar <- data.frame()
  as.integer(aar <- substr(eksport2$Måned, 1, 4))
  as.Date.character(aar, "%y")
  
  samleteksport <- cbind(aar, eksport2)
  
  sort_aar <- group_by(samleteksport, aar)
  samleteksport$Varegruppe[samleteksport$Varegruppe=="¬ Fisk"] <- "Fisk"
  
  I<-(samleteksport$Varegruppe=="Fisk")
  fisk<-samleteksport[I, ]
  
  fisk<-samleteksport[(samleteksport$Varegruppe=="Fisk"), ]
  
  mat<-samleteksport[(samleteksport$Varegruppe=="Matvarer, drikkevarer, tobakk"),] 
  
  
  mat_fisk<-merge(fisk,mat, by=c("aar","Region","Måned","Variabel"))
  
  mat_fisk$Beløp<-mat_fisk$Beløp.y-mat_fisk$Beløp.x
  
  mat_fisk<-mat_fisk[,c("aar","Region","Måned","Variabel", "Beløp")]
  
  mat_fisk$Varegruppe<-"Matvarer, drikkevarer, tobakk (excl fisk)"
  
  samleteksport_excl_fisk_mat<-samleteksport[(samleteksport$Varegruppe!="Matvarer, drikkevarer, tobakk"|samleteksport$Varegruppe!="Varer i alt"),]
  
  
  mat_fisk <- mat_fisk[(mat_fisk$Varegruppe!= "Matvarer, drikkevarer, tobakk"),]
  
  new<-merge(mat_fisk,samleteksport_excl_fisk_mat,
             by=c("aar","Region","Måned","Variabel", "Beløp","Varegruppe"),all.x=TRUE,all.y=TRUE)
  
  new$Måned <- substr(new$Måned, 6, 7)
  
  samleteksport_hittil <- filter(new, Variabel == "Verdi hittil i år")
  samleteksport_mnd <- filter(new, Variabel == "Verdi")
  
  
  # Define user interface
  
  ui <- fluidPage(
    title = "Velg hvilke regioner du vil summere",
    
    downloadButton('download',"Last ned tabellen"),
    fluidRow(column(10,dataTableOutput('dto'))),
    
    downloadButton("graf", "grafikk"),
    
  
    
    selectInput(inputId = "Region", "Region:",
                c("Hordaland" = "Hordaland (-2019)",
                  "Sogn og Fjordane" = "Sogn og Fjordane (-2019)",
                  "Akershus" = "Akershus (-2019)" ,
                  "Aust-Agder" ="Aust-Agder (-2019)" ,
                  "Buskerud"="Buskerud (-2019)" ,
                  "Finnmark"="Finnmark - Finnmárku (-2019)", 
                  "Hedmark"="Hedmark (-2019)" ,
                  "Jan Mayen" ="Jan Mayen" ,
                  "Møre og Romsdal" = "Møre og Romsdal", 
                  "Nord-Trøndelag" ="Nord-Trøndelag (-2017)",  
                  "Nordland" = "Nordland" ,
                  "Norske varer fra flere fylker" ="Norske varer med opprinnelse fra flere fylker",
                  "Oppland" ="Oppland (-2019)" ,
                  "Oslo" = "Oslo"  ,
                  "Reeksport av utenl. varer" = "Reeksport av varer produsert i utlandet" ,
                  "Rogaland" ="Rogaland"  ,
                  "Svalbard " ="Svalbard" ,
                  "Sør-Trøndelag" ="Sør-Trøndelag (-2017)", 
                  "Telemark" = "Telemark (-2019)" ,
                  "Troms" ="Troms - Romsa (-2019)", 
                  "Trøndelag" ="Trøndelag - Trööndelage" ,
                  "Uoppgitt" ="Uoppgitt fylke" ,
                  "Vest-Agder" ="Vest-Agder (-2019)", 
                  "Vestfold" ="Vestfold (-2019)" ,
                  "Østfold" ="Østfold (-2019)" ),
                
                multiple = TRUE),
    
    selectInput(inputId = "aar", "Årstall:",
                c("1999" = "1999",
                  "2000"="2000",
                  "2001"="2001",
                  "2002"="2002",
                  "2003"="2003",
                  "2004"="2004",
                  "2005"="2005",
                  "2006"="2006",
                  "2007"="2007",
                  "2008"="2008",
                  "2009"="2009",
                  "2010"="2010",
                  "2011"="2011",
                  "2012"="2012",
                  "2013"="2013",
                  "2014"="2014",
                  "2015"="2015",
                  "2016"="2016",
                  "2017"="2017",
                  "2018"="2018",
                  "2019"="2019",
                  "2020"="2020" ),
                
                multiple = TRUE
    ),
    
    
    selectInput(inputId = "Varegruppe", "Varegruppe:",
                c( "Brenselsstoffer" = "Brenselsstoffer",
                   "Matvarer, drikkevarer, tobakk" = "Matvarer, drikkevarer, tobakk (excl fisk)",
                   "Råvarer, unntatt brenselsstoffer" = "Råvarer, unntatt brenselsstoffer" ,
                   "Fisk"="Fisk",
                   "Matvarer, drikkevarer, tobakk" = "Matvarer, drikkevarer, tobakk" ,
                   "Bearbeidde varer, unntatt matvarer, drikkevarer, tobakk"= "Bearbeidde varer, unntatt matvarer, drikkevarer, tobakk",
                   "Varer i alt"   = "Varer i alt"   )
    ),
    
    DT::dataTableOutput("tabell")
    
  )
  
  
  
  # Define server logic
  
  server <- function(input, output) {
    
    tabell <- reactive({
      filter(new, (Region %in% input$Region) &
               
               (aar %in% input$aar)&
               
               (Varegruppe %in% input$Varegruppe) &
               (Variabel == "Verdi")
             
      )
      
    }) 
    
    
    output$tabell <- DT::renderDataTable({
      DT::datatable(data = tabell(), options = list(pageLength = 12), 
                    
                    rownames = FALSE, class = 'display', escape = FALSE)
      
      
      
    })
    
    output$download <- downloadHandler(
      filename = function(){"nedlast.csv"}, 
      content = function(fname){
        write.csv(tabell(), fname)
      }
    )
  }
  #'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''  
    table_rmd <- reactive({
      filter(new, (Region %in% input$Region) &
               
               (aar %in% input$aar)&
               
               (Varegruppe %in% input$Varegruppe) &
               (Variabel == "Verdi")
             
      )
      output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$dataset, '.png', sep='') },
        content = function(file) {
          png(file)
          print(plotInput())
          dev.off()
        })
    })
  
    
    

  
    
    
  
  
    
    
    
  
   
  
    
    
    
    
    #.............................................................
    
    
    # output$tabell = DT::renderDataTable({new})
    
    #output$tabell = DT::renderDataTable(samleteksport_hittil[, input$inputID, drop = FALSE])
    
    

  
  # Run the app
  shinyApp(ui = ui, server = server)
  