library(shiny)
library(DT)
# Define the UI
ui <- fluidPage(
  titlePanel("Genome Sequencing Project Entry Portal"),
  sidebarLayout(
    sidebarPanel(
      img(src = "https://rajalakshmi.org/image/logo2.png", height = 120, width = 300),
      h4("Dr.A.Arun", style = "color: blue; text-align: center;"),
      p("Assistant Professor", style = "color: blue; text-align: center;"),
      p("Department of Biotechnology", style = "color: blue; text-align: center;"),
      p("arun.a@rajalakshmi.edu.in", style = "color: blue; text-align: center;"),
      tags$hr(),
      h5("OrcID"),
      p("Webapp maintained by"),
      p("Ms.Achsha Babu"),
      tags$hr(),
      downloadButton("saveData", "Save Data"),
      tags$hr(),
      textOutput("currentTime")
    ),
    mainPanel(
      DTOutput("genome_table"),
      actionButton("addRow", "Add Row"),
      actionButton("removeRow", "Remove Selected Row")
    )
  )
)
# Define the server logic
server <- function(input, output, session) {
  data_file <- "genome_data.RData"
  if (file.exists(data_file)) {
    saved_data <- readRDS(data_file)
    genome_data <- reactiveVal(saved_data)
  } else {
    genome_data <- reactiveVal(data.frame(
      Organism.common.name = character(0),
      Organism.scientific.name = character(0),
      Pl.name = character(0),
      Lab.address = character(0),
      Country = character(0),
      Project.status = character(0),
      Project.funded.by = character(0),
      Email = character(0),
      stringsAsFactors = FALSE
    ))
  }
  observe({
    saveRDS(genome_data(), file = data_file)
  })
  output$genome_table <- renderDT({
    datatable(
      genome_data(), 
      editable = list(target = 'cell', disable = list(columns = c())),
      selection = 'single',
      options = list(pageLength = 10)
    )
  }, server = FALSE)
  
  observeEvent(input$genome_table_cell_edit, {
    info <- input$genome_table_cell_edit
    str(info)
    new_data <- genome_data()
    new_data[info$row, info$col] <- info$value
    genome_data(new_data)
  })
  observeEvent(input$addRow, {
    genome_data(rbind(genome_data(), data.frame(
      Organism.common.name = "", 
      Organism.scientific.name = "", 
      Pl.name = "", 
      Lab.address = "", 
      Country = "", 
      Project.status = "", 
      Project.funded.by = "", 
      Email = "", 
      stringsAsFactors = FALSE
    )))
  })
  observeEvent(input$removeRow, {
    selected <- input$genome_table_rows_selected
    if (length(selected) > 0) {
      genome_data(genome_data()[-selected, ])
    }
  })
  output$saveData <- downloadHandler(
    filename = function() {
      paste("genome_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(genome_data(), file, row.names = FALSE)
    }
  )
  autoInvalidate <- reactiveTimer(1000)  # 1000 milliseconds = 1 second
  output$currentTime <- renderText({
    autoInvalidate()
    paste("Current time:", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
}
shinyApp(ui = ui, server = server)

shiny::runApp()
