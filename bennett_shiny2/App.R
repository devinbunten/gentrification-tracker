library(shiny)
library(dplyr)
library("ggplot2")
library(stringr)
library(bslib)

d <- read.csv("./inc_zhvi_annual_q_zcta.csv", stringsAsFactors = F)

d$zcta <- sprintf("%05d", d$zcta)
d$zcta <- as.factor(d$zcta)

ui <- fluidPage(
  
  theme = bs_theme(
    base_font = font_google("Nunito")
  ),
  
  tags$style(HTML(
    "
    .plot-caption {
      margin-top:40px;
      text-align: right; 
      font-size: 12px;
    }
    .chart-title {
      margin-bottom:40px;
    }
    mainPanel {
      padding-right:20em;
    }
    h1 {
      margin-left:.75rem;
    }
    "
  )),
  
  headerPanel("Gentrification Tracker", windowTitle = "Gentrification Tracker"),
  
  fluidRow(style="margin-left:.5rem; margin-top:2rem",
    
    column(4,
    p(style="margin-top:1.5rem","Want to see how your neighborhood is changing?"),
    p("Researchers at MIT are studying how house prices and incomes in a neighborhood can be indicators of gentrification."),
    p("Enter a Zip Code from any Metropolitan Statistical Area (MSA) in the United States to see how House Prices and Income have changed over time. If you see a large gap where house prices are much higher than incomes, that means the neighborhood is likely to be gentrifying."),
    p(style="text-decoration:bold", selectizeInput("zcta", "Zip Code:", 
                choices = NULL,
                multiple = T))
  ),
  
  column(6,offset=1,
    
    h3(class="chart-title", "House Price and Income Percentiles by Zip Code"),
            
            plotOutput(outputId="gentrificationPlot"),
    
    p(class="plot-caption", "The gentrification tracker is based on the paper 'Re-Measuring Gentrification'. Income data is from the Internal Revenue Service; house price data is from Zillow. For more information, see", a("here.", href =  "https://doi.org/10.1177/00420980231173846")))
  
))

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "zcta", choices = as.character(unique(d$zcta)), server = TRUE)
  
  formulaText <- reactive({
    paste("Zip Code", input$zcta)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  dataset <- reactive({
    subset(d, zcta %in% input$zcta)
  })
  
  output$gentrificationPlot <- renderPlot(execOnResize=FALSE, {
  
    p <- ggplot(data = dataset(), aes(x = year, col=interaction(zcta))) +
      geom_line(aes(y=hvalue_quantile, group = interaction(zcta), linetype="House Price Percentile"), linewidth=1) +
      geom_line(aes(y=income_quantile, group = interaction(zcta), linetype="Income Percentile"), linewidth=1) +
      labs(x="Year", y="Percentile") +
      theme_classic() +
      theme(axis.title.x = element_text(size=14, vjust=-1.25),
            axis.title.y = element_text(size=14, vjust=2.5)) +
      theme(axis.text.x=element_text(size=12, vjust=0.5, face="italic"),
            axis.text.y=element_text(size=12, vjust=0.5, face="italic")) +
      theme(axis.line = element_line(linewidth=.25)) +
      theme(axis.ticks = element_line(linewidth=.25)) +
      ylim(c(0,1)) +
      xlim(c(1995,2020)) +
      scale_linetype_manual(values=c("House Price Percentile"=1,"Income Percentile"=3)) +
      guides(linetype = guide_legend(title = "Data Type",title.position="top", order=1, nrow=2)) +
      guides(color = guide_legend(title = "Zip Code", title.position="top", order=2, nrow=3)) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_text(size=14)) +
      theme(legend.text = element_text(size=12))
      theme(legend.margin = margin(c(0,0,0,0)))
    print(p)
  })
  
}

shinyApp(ui, server)