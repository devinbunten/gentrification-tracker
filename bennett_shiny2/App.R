library(shiny)
library(dplyr)
library("ggplot2")
library(stringr)

d <- read.csv("./inc_zhvi_annual_q_zcta.csv", stringsAsFactors = F)

#d %>% mutate(zcta = sprintf("%05d", zcta))
d$zcta <- sprintf("%05d", d$zcta)
d$zcta <- as.factor(d$zcta)

print(d)

ui <- fluidPage(
  
  tags$style(
    "
    .well {
      margin-top:20px;
    }
    .plot-caption {
      margin-top:40px;
      text-align: right; 
    }
    .chart-title {
      margin-bottom:40px;
    }
    "
  ),
  
  headerPanel("Gentrification Tracker", windowTitle = "Gentrification Tracker"),
  
  sidebarPanel(
    
    p("Want to see how your neighborhood is changing?"),
    p("Enter a Zip Code from any Metropolitan Statistical Area (MSA) in the United States to see how House Prices and Income have changed over time."),
    selectInput("zcta", "Zip Code:", 
                c(as.character(unique(d$zcta))),
                multiple = T)
  ),
  
  mainPanel(
    
    h3(class="chart-title", "House Price and Income Percentiles by Zip Code"),
            
            # Output: Plot of the requested variable against mpg ----
            plotOutput("gentrificationPlot"),
    
    p(class="plot-caption", "The gentrification tracker is based on the paper 'Re-Measuring Gentrification'. Income data is from the Internal Revenue Service; house price data is from Zillow. For more information, see", a("here.", href =  "https://doi.org/10.1177/00420980231173846")))
  
)

server <- function(input, output) {
  
  formulaText <- reactive({
    paste("Zip Code", input$zcta)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  dataset <- reactive({
    subset(d, zcta %in% input$zcta)
  })
  
  output$gentrificationPlot <- renderPlot({
  
    p <- ggplot(data = dataset(), aes(x = year, col=interaction(zcta))) +
      geom_line(aes(y=hvalue_quantile, group = interaction(zcta), linetype="House Price Percentile")) +
      geom_line(aes(y=income_quantile, group = interaction(zcta), linetype="Income Percentile")) +
      labs(x="Year", y="Percentile") +
      theme_classic() +
      theme(axis.title.x = element_text(vjust=-2.5),
            axis.title.y = element_text(vjust=2.5)) +
      ylim(c(0,1)) +
      xlim(c(1995,2020)) +
      scale_linetype_manual(values=c("House Price Percentile"=1,"Income Percentile"=3)) +
      guides(linetype = guide_legend(title = "Data Type")) +
      guides(color = guide_legend(title = "Zip Code"))
    print(p)
    
    # x <- dataset$year
    # y1 <- dataset$hvalue_quantile
    # y2 <- dataset$income_quantile
  
    # par(mar = c(5, 4, 4, 4) + 0.3)
    # plot(dataset(year, hvalue_quantile),
    #     xlab = "Time",
    #     ylab = "Percentile",
    #     type = "l",
    #     lty = 1,
    #     lwd = 4,
    #     col = "red")
    # par(new=TRUE)
    # plot(x, y2,
    #     axes = FALSE,
    #     xlab = "",
    #     ylab = "",
    #     type = "l",
    #     lty = 3,
    #     lwd = 4,
    #     col = "red")
  })
  
}

shinyApp(ui, server)