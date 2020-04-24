

library(shiny)
library(plotly)
library(tidyverse)

print(getwd())
read_rds('dat_result.rds')
dat <- read_rds('dat.rds')

ui <- fluidPage(

    # Application title
    titlePanel("Volcano and boxplot"),

    sidebarLayout(
        sidebarPanel(

            selectInput("gene",
                        "Select a gene...",
                        dat_result$geneId)
        ),

        mainPanel(
            plotlyOutput("volcano"),
            plotlyOutput("boxPlot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    volcano_df <- reactive({
        df <- dat_result
        df$size <- 1
        df$size[which(df$geneId == input$gene)] <- 2
        return(df)
    })
    
    output$boxPlot <- renderPlotly({
        dat %>% 
            filter(geneId == input$gene) %>% 
            plot_ly(type = 'box',
                    x = ~group,
                    y = ~expression)
    })
    

    output$volcano <- renderPlotly({
        plot_dat <- volcano_df()
        plot_ly(plot_dat, 
                type = 'scatter',
                mode = 'markers',
                x = ~measured_fc, 
                y = ~log10(1/pvalue), 
                size = ~size, 
                color = ~size,
                colors = c("grey", "red"),
                text = ~geneId,
                hoverinfo = 'text',
                hovertemplate = paste('<B>%{text} </B>:', '%{y:.2f}', 
                                      '<extra></extra>'),
                showlegend = FALSE) %>% hide_colorbar()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
