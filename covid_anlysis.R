library(shiny)
library(ggplot2)
library(ggridges)

dataset_dir <- "data/"
conf_path <- paste0(dataset_dir, "time_series_covid19_confirmed_global.csv")
rec_path <- paste0(dataset_dir, "time_series_covid19_recovered_global.csv")
death_path <- paste0(dataset_dir, "time_series_covid19_deaths_global.csv")

df_conf <- read.csv(conf_path)
df_rec <- read.csv(rec_path)
df_deaths <- read.csv(death_path)

last_col_name_conf <- colnames(df_conf)[length(colnames(df_conf))]
top_10_value <- sort(df_conf[, last_col_name_conf], decreasing = TRUE)[10]
top_10_countries_conf <- df_conf[df_conf[last_col_name_conf] >= top_10_value,]


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Confirmed analysis",
      selectInput(inputId = "country", label = "Choose a country",
                  choices = unique(top_10_countries_conf$Country.Region)),
      plotOutput(outputId = "conf_analysis")
    ),
    tabPanel("All data visualisation", 
      fluidRow(
        column(4,
               wellPanel(
                 plotOutput(outputId = "vis_conf")
               )),
        column(4,
               wellPanel(
                 plotOutput(outputId = "vis_rec")
               )),
        column(4,
               wellPanel(
                 plotOutput(outputId = "vis_death")
               ))
      )  
    ),
    tabPanel("Aggregated forecasts", 
      navlistPanel(
        tabPanel("deaths forecast",
                 plotOutput("forec_death")
        ),
        tabPanel("recovered forecast", 
                 plotOutput("forec_rec")
        ),
        tabPanel("confirmed forecast",
                 plotOutput("forec_conf")

        )
      )
    )
  )
)


server <- function(input, output, session) {
  output$conf_analysis <- renderPlot({
    current_country <- top_10_countries_conf[top_10_countries_conf$Country.Region == input$country, ]
    diff_ <- unname(current_country[, 6:ncol(top_10_countries_conf)] - current_country[, 5:(ncol(top_10_countries_conf) - 1)])
    col_names <- colnames(current_country)[6:colnum]
    monthes <- format(as.Date(col_names, format='X%m.%d.%Y'), "%B")
    month_to_diff <- data.frame("Month" = factor(monthes, levels=rev(unique(monthes))))
    month_to_diff$diff <- array(as.numeric(unlist(diff_)))
    ggplot(month_to_diff, aes(x = diff, y = Month, fill=stat(x))) +
      geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
      scale_fill_viridis_c(name = "Confirmed people\n per day") +
      labs(title = paste0("Confirmed people for each month in country ", input$country)) + 
      xlab("Confirmed people\n per day")
  })
 
  output$vis_conf <- renderPlot({
    
  }) 
}

shinyApp(ui, server)
