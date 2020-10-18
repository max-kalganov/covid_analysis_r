library(shiny)
library(ggplot2)
library(ggridges)
library(gganimate)

dataset_dir <- "data/"
conf_path <- paste0(dataset_dir, "time_series_covid19_confirmed_global.csv")
rec_path <- paste0(dataset_dir, "time_series_covid19_recovered_global.csv")
death_path <- paste0(dataset_dir, "time_series_covid19_deaths_global.csv")

df_conf <- read.csv(conf_path)
df_rec <- read.csv(rec_path)
df_deaths <- read.csv(death_path)

last_col_name <- colnames(df_conf)[length(colnames(df_conf))]
top_10_value_conf <- sort(df_conf[, last_col_name], decreasing = TRUE)[10]
top_10_countries_conf <- df_conf[df_conf[last_col_name] >= top_10_value_conf,]

top_10_value_death <- sort(df_deaths[, last_col_name], decreasing = TRUE)[10]
top_10_countries_death <- df_deaths[df_deaths[last_col_name] >= top_10_value_death,]

top_10_value_rec <- sort(df_rec[, last_col_name], decreasing = TRUE)[10]
top_10_countries_rec <- df_rec[df_rec[last_col_name] >= top_10_value_rec,]

convert_country <- function(top_countries, current_country_name){
  current_country_df <- top_countries[top_countries$Country.Region == current_country_name, ]
  diff_ <- unname(current_country_df[, 6:ncol(top_countries)])
  colnum <- length(colnames(current_country_df))
  col_names <- colnames(current_country_df)[6:colnum]
  #monthes <- format(as.Date(col_names, format='X%m.%d.%Y'), "%B %d")
  #month_to_diff <- data.frame("Month" = factor(monthes, levels=rev(unique(monthes))))
  
  monthes <- as.Date(col_names, format='X%m.%d.%Y')
  month_to_diff <- data.frame("Month" = monthes)
  
  month_to_diff$diff <- array(as.numeric(unlist(diff_)))
  month_to_diff$name <- current_country_name
  return(month_to_diff)
}
 

list_of_countries <- function(top_countries){
  #browser()
  full_list <- list()
  for (country_name in top_countries$Country.Region){
    country_name
    cur_df <- convert_country(top_countries, country_name)
    cur_list <- list(cur_df)
    names(cur_list) <- c(country_name)
    full_list <- append(full_list, cur_list)
  }
  return(full_list)
}

list_conf <- list_of_countries(top_10_countries_conf)
list_rec <- list_of_countries(top_10_countries_rec)
list_death <- list_of_countries(top_10_countries_death)

vis_data <- function(data_list){
  cur_plot <- ggplot(mapping = aes(y=diff))
  for (i in 1:length(list_conf)){
    cur_df <- list_conf[[i]]
    cur_name <- names(list_conf)[i] 
    cur_plot <- cur_plot + geom_col(data=cur_df, aes(x = name))
  }
  cur_plot <- cur_plot +
    transition_time(Month) +
    labs(title = 'Date: {format(frame_time, "%B %d")}', x = 'Country', y = 'Number of actions') +
    ease_aes('linear') + 
    shadow_mark()
  return(cur_plot)  
}

vis_conf <- vis_data(list_conf)
vis_rec <- vis_data(list_rec)
vis_death <- vis_data(list_death)

anim_save("vis_conf.gif", animate(vis_conf, fps=8))
anim_save("vis_rec.gif", animate(vis_rec, fps=8))
anim_save("vis_death.gif", animate(vis_death, fps=8))

colnum <- length(col)
# aggregated_conf <- df_conf[, 5:]


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Confirmed analysis",
      selectInput(inputId = "country", label = "Choose a country",
                  choices = unique(top_10_countries_conf$Country.Region)),
      plotOutput(outputId = "conf_analysis")
    ),
    tabPanel("All data visualisation", 
             textOutput(outputId="vis_conf_text"),
             imageOutput(outputId = "vis_conf", width="500px", height="500px"),
             textOutput(outputId="vis_rec_text"),
             imageOutput(outputId = "vis_rec", width="500px", height="500px"),
             textOutput(outputId="vis_death_text"),
             imageOutput(outputId = "vis_death", width="500px", height="500px")
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
    colnum <- length(colnames(current_country))
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
 
  output$vis_conf_text <- renderText("Visualisation for grouth of confirmed people")
  
  output$vis_rec_text <- renderText("\nVisualisation for grouth of recovered people")
  
  output$vis_death_text <- renderText("\n\nVisualisation for grouth of deaths")
  
  
  output$vis_conf <- renderImage({
    list(src = "vis_conf.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  output$vis_rec <- renderImage({
    list(src = "vis_rec.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  output$vis_death <- renderImage({
    list(src = "vis_death.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
}

shinyApp(ui, server)
