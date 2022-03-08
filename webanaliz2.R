library(shiny)
library(data.table)

veri68<- read.csv(file ="C:/Users/Victory/OneDrive/Belgeler/veri68.csv", header = FALSE, sep = ";")

veri68s <- data.matrix(veri68)

ui <- fluidPage(
    
    titlePanel("Web Ortaminda Veri Analizi"),
    
    sidebarLayout(
        
        sidebarPanel(
    
            
            sliderInput(inputId = "aralik",
                        label = "Bir aralik belirleyiniz.",
                        min = 1,
                        max =300,
                        value = 150),
            
            selectInput(inputId = "degisken",
                        label = "Bir degisken seciniz:",
                        choices = c("state"=1,
                                    "time"=2,
                                    "net"=3,
                                    "age"=4,
                                    "sex"=5)),
            
            radioButtons(inputId = "renk",
                         label = "Grafik rengi belirleyiniz:",
                         choices = c("green", "red", "yellow", "purple"),
                         selected = "red")
            
            
        ),
        
        mainPanel(
            
            tabsetPanel(
                
                tabPanel(title = "Summary",
                         verbatimTextOutput("summary")),
                
                tabPanel(title = "Str",
                         verbatimTextOutput("str")),
                
                tabPanel(title = "Veri",
                         tableOutput("data_out")),
                
                tabPanel(title = "Histogram",
                         plotOutput("histogram")),
                
                tabPanel(title = "Kutu Grafigi",
                         plotOutput("boxplot")),
                
                tabPanel(title = "Normallik Grafigi",
                         plotOutput("qqnorm"))
              
                
               )
            
            
        )
    )
  
)

server <- function(input, output, session) {
    
    output$summary <- renderPrint({
        
        summary(veri68s)
    })
    
    output$str <- renderPrint({
        
        str(veri68)
    })
    
    output$data_out <- renderTable({
        
        veri68
    })
    
    output$histogram <- renderPlot({
      
        colm <- as.numeric(input$degisken)
      
             hist(veri68s[,colm],
             breaks = seq(1,300),
             col = input$renk,
             border = "red",
             xlab = "aralik")
      
})
    
    output$boxplot<- renderPlot({
      
        colm <- as.numeric(input$degisken)
        boxplot(veri68s[, colm], col = input$renk)
      
    })
    
    output$qqnorm <- renderPlot({
      
        colm <- as.numeric(input$degisken)
      
        qqnorm(veri68s[,colm])
      
        qqline(veri68s[,colm])
        
        
    })
    
    
}

shinyApp(ui, server)
