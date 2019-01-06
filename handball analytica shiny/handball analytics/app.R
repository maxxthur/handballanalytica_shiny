#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#options(unzip = 'internal')
#devtools::install_github("maxxthur/handball.analytica")
library(handball.analytica)
#library(dplyr)


#functions
#load("Daten/Data")
Data <- readRDS("data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Handball Analytics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
            selectizeInput(inputId = "Mannschaft",
                        label="Mannschaft", 
                        choices= c("",as.character(unique(Data[[1]]$Mannschaft))), 
                        selected = "DJK Grün-Weiß Nottuln"),
            
            uiOutput("player_selection")),
            
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          column(width = 12,
                 div(style = "height:20px; font-size:35px;",
                     'Übersicht'))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     'Mannschaft'))
        ),
        
         tableOutput("fundamentals"),
         plotOutput("Mannschaftsanalyse", height=200),
         
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     'Auffälligste Spieler über die Gesamtsaison'))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
        plotOutput("auffaelligste_Spieler"),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     'Performance letzte 3 Spiele'))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
        DT::dataTableOutput("performance"),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     'Ganze Mannschaft/Saison'))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
        ),
        
         DT::dataTableOutput("table_team"),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     'Einzelspieleranalyse'))
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "height:30px; font-size:25px;",
                     ''))
      ),
      
      tableOutput("fundamentals_player"),
      plotOutput("einzelspieler", height=800)
)
)
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$player_selection <- renderUI({
    selectizeInput(inputId = "Spieler",
                   label="Einzelspieleranalyse", 
                   choices= unique(Data[[1]] %>% dplyr::filter(Mannschaft==input$Mannschaft & !(`#` %in% c("OA", "OB", "OC", "OD", "OE", "OF"))) %>% .$Name))
  })
  
  output$fundamentals <- renderTable({Fundamentals_Team(Team=input$Mannschaft, Data = Data)})
   
  output$fundamentals_player <- renderTable({Einzelspieleranalyse(Player=input$Spieler, Team=input$Mannschaft, Data=Data) %>% .[[2]]})
   
  output$einzelspieler <- renderPlot({
    Einzelspieleranalyse(Player=input$Spieler, Team=input$Mannschaft, Data=Data)[[1]]
  })
  
  output$Mannschaftsanalyse <- renderPlot({
     a1 <- Tore_Spielminute_Mannschaft(input$Mannschaft, Data = Data)
     a2 <-   Zeitstrafen_Spielminute_Mannschaft(input$Mannschaft, Data = Data)
     a3 <-  GelbeKarten_Spielminute_Mannschaft(input$Mannschaft, Data = Data)
     plot <- ggpubr::ggarrange(a1, a2, a3, ncol=3, nrow=1)
     plot
   })
   
   output$performance <- DT::renderDataTable({Performance_last(Team=input$Mannschaft, Data = Data)})
   
   output$table_team <- DT::renderDataTable({Kader(Team=input$Mannschaft, Data, order="Tore")})
   #output$table_team <- renderTable({Kader(Team=input$Mannschaft, Data, order=input$Tabellenordnung)})
   
   output$auffaelligste_Spieler <- renderPlot({
     #par(mfrow=c(3,3))
     a <- Tore_Spielminute_Spieler(Kader(input$Mannschaft, Data, "Tore")$Name[1], input$Mannschaft, Data)
     b <- Zeitstrafen_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Zeitstrafen")$Name[1], Data)
     c <- GelbeKarten_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Gelbe Karten")$Name[1], Data)
     d <- Tore_Spielminute_Spieler(Kader(input$Mannschaft, Data, "Tore")$Name[2], input$Mannschaft , Data)
     e <- Zeitstrafen_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Zeitstrafen")$Name[2], Data)
     f <- GelbeKarten_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Gelbe Karten")$Name[2], Data)
     g <- Tore_Spielminute_Spieler(Kader(input$Mannschaft, Data, "Tore")$Name[3], input$Mannschaft, Data)
     h <- Zeitstrafen_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Zeitstrafen")$Name[3], Data)
     i <- GelbeKarten_Spielverlauf_Spieler(Kader(input$Mannschaft, Data, "Gelbe Karten")$Name[3], Data)
     
     ggpubr::ggarrange(a, b, c, d, e, f, g, h, i, ncol=3, nrow=3)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
#library(rsconnect)
#rsconnect::deployApp(getwd())
