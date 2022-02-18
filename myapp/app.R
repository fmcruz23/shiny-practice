# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(shinyWidgets)
library(bslib)

# user interface ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  
  navbarPage(
    "TITLE HERE",
    tabPanel("Background",
             "Penguin photos go here"),
    
    tabPanel("Penguin Plots", 
             tabsetPanel(
               tabPanel("Scatterplot",
                        # body mass slider input ----
                        sliderInput(inputId = "body_mass",
                                    label = "Select a range of body masses (g):",
                                    min = 2700, max = 6300, value = c(3000, 4000)),
                        # body mass output ----
                        plotOutput(outputId = "bodyMass_scatterPlot")), 
               tabPanel("Histogram",
                        shinyWidgets::pickerInput(
                          inputId = "island",
                          label = "Select an island(s):",
                          choices = c("Torgersen", "Dream", "Biscoe"),
                          selected = c("Torgersen", "Dream", "Biscoe"),
                          multiple = TRUE,
                          options = pickerOptions(actionsBox = TRUE)),
                        # reactive histogram output here: 
                        plotOutput(outputId = "flipperLength_histogram")
                        
                        ))),
    
    tabPanel("Antarctic Weather Plots",
             tabsetPanel(
               tabPanel("Weather Plot"),
               tabPanel("Weather Table"))),
    
    tabPanel("Data Exploration",
             "insert DT table here",
             # placeholder for dt
             DT::dataTableOutput(outputId = "data_table"))
  ))


# break ----------------



# server instructions ---- 
server <- function(input, output){
 
  # filter body masses ----
  body_mass_df <- reactive({

    penguins %>%
      filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])

  })

# render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({

     # code to generate scatterplot here

    ggplot(na.omit(body_mass_df()),
           aes(x = flipper_length_mm, y = bill_length_mm,
               color = species, shape = species)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Adelie" = "#FEA346",
                                    "Chinstrap" = "#B251F1",
                                    "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19,
                                    "Chinstrap" = 17,
                                    "Gentoo" = 15)) +
      labs(x = "Flipper Length (mm)", y = "Bill Length (mm)",
           color = "Penguin Species",
           shape = "Penguin Species")

  })
   
# DT
  output$data_table <- DT::renderDataTable({
    DT::datatable(penguins,
                  options = list(pageLength = 5),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 1: ', tags$em('Penguin data')))

  })
  
# # reactive histogram 
#   
  # filter islands  ----
  
  island_df <- reactive({
    validate(
      need(length(input$island) > 0, "Please choose at least one island to visualize.")
    )
    penguins %>% 
    filter(island == input$island)

  })

  # render histogram 
  output$flipperLength_histogram <- renderPlot({
    ggplot(data = na.omit(island_df()), aes(x = flipper_length_mm)) +
      geom_histogram(aes(fill = species), alpha = 0.6) +
      labs(x = "Flipper Length (mm)",
           y = "Frequency",
           fill = "Species") +
      scale_fill_manual(values = c("Adelie" = "#FEA346",
                                   "Chinstrap" = "#B251F1",
                                   "Gentoo" = "#4BA4A4"))
    
  })
 
}


# combine ui and server into an app ----
shinyApp(ui = ui, server = server)

