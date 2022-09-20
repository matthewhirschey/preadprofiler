library(shiny)

#load functions
source('helper.R')

#load data
tissue_data <- qs::qread("tissue_data.qs")

# Define UI for application that draws a plot
ui <- fluidPage(

  # Application title
  titlePanel("PreadProfiler"),

  sidebarLayout(
    sidebarPanel(
      textInput("name", "Gene Name"),
      actionButton("search", "Search")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxPlot")
    )
  )
)

# Define server logic required to draw a boxplot
server <- function(input, output) {
  plot <- eventReactive(input$search, {
    gene_string <- stringr::str_split(input$name, pattern = ", ") %>% unlist()
    make_boxplot(dataframe = tissue_data,
                 fav_gene = gene_string)
  })
  output$boxPlot <- renderPlot({
    validate(
      need(nrow(plot()$data) != 0, "Query not found"))
    plot()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
