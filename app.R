# ============================================================
# ANIMAL DATA PLOTTER
# ============================================================
#
# This Shiny app allows a user to:
#
#   1. Upload an animal database xlsx
#   2. Automatically clean and standardize the data
#   3. Choose which measurement to plot
#   4. Generate a plot
#   5. Download the cleaned data
#   6. Download the resulting plot
#
# The actual data cleaning and plotting are handled by
# functions stored in the R/ folder:
#
#   standardize_animal_data.R
#   make_animal_plot.R
#
# ============================================================

# ------------------------------------------------------------
# Load packages
# ------------------------------------------------------------

library(shiny)
library(tidyverse)


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  # ----------------------------------------------------------
  # Title
  # ----------------------------------------------------------

  titlePanel(
    "Animal Data Plotter"
  ),

  # ----------------------------------------------------------
  # About this app (always visible)
  # ----------------------------------------------------------

  wellPanel(
    p(
      "This app allows you to upload a dataset,",
      "automatically standardize the data, and create plots for analysis.",
      "The goal is to go from an experimental spreadsheet to a",
      "consistent plot without manually cleaning or aggregating",
      "the data each time."
    ),

    h5("How to use the app"),
    tags$ol(
      tags$li(
        "Prepare your data using the ",
        tags$code("Data_template-v3.1-ac.xlsx"),
        " template."
      ),
      tags$li(
        "Upload your ",
        tags$code(".xlsx"),
        " file."
      ),
      tags$li("Select a variable from the dropdown."),
      tags$li("Click ", tags$strong("Generate Plot"), "."),
      tags$li(
        "Download the cleaned data or plot as needed."
      )
    ),

    h5(
      "The code for this app is on GitHub. Many customizations are possible:"
    ),
    tags$ul(
      tags$li(
        "To rename a column or change a data type, edit ",
        tags$a(
          "standardize_animal_data.R",
          href = "https://github.com/hannaheow/cardona_lab/blob/main/standarize_data.R",
          target = "_blank"
        )
      ),
      tags$li(
        "To change plot colors, labels, grouping, or error bars, edit ",
        tags$a(
          "make_animal_plot.R",
          href = "https://github.com/hannaheow/cardona_lab/blob/main/R/make_animal_plot.R",
          target = "_blank"
        )
      ),
      tags$li(
        "To change the app interface or file upload behavior, edit ",
        tags$a(
          "app.R",
          href = "https://github.com/hannaheow/cardona_lab/blob/main/app.R",
          target = "_blank"
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # Sidebar
  # ----------------------------------------------------------

  sidebarLayout(
    sidebarPanel(
      # ------------------------------------------------------
      # Step 1: Upload data
      # ------------------------------------------------------

      h4("1. Upload your data"),

      # The user selects a CSV file from their computer.

      fileInput(
        inputId = "file",
        label = "Choose your animal database:",
        accept = ".xlsx"
      ),

      # ------------------------------------------------------
      # Step 2: Select measurement
      # ------------------------------------------------------

      h4("2. Choose a measurement"),

      # This dropdown will eventually contain all of the
      # measurements that the user can choose to plot.
      #
      # For now, we will populate it after the user uploads
      # their data.

      selectInput(
        inputId = "outcome",
        label = "Measurement:",
        choices = NULL
      ),

      # ------------------------------------------------------
      # Step 3: Generate plot
      # ------------------------------------------------------

      actionButton(
        inputId = "generate_plot",
        label = "Generate Plot",
        class = "btn-primary"
      ),

      br(),
      br(),

      # ------------------------------------------------------
      # Download cleaned data
      # ------------------------------------------------------

      downloadButton(
        outputId = "download_cleaned",
        label = "Download Cleaned Data"
      ),

      br(),
      br(),

      # ------------------------------------------------------
      # Download plot
      # ------------------------------------------------------

      downloadButton(
        outputId = "download_plot",
        label = "Download Plot"
      )
    ),

    # ----------------------------------------------------------
    # Main panel
    # ----------------------------------------------------------

    mainPanel(
      # --------------------------------------------------------
      # Data summary
      # --------------------------------------------------------

      h3("Data Summary"),

      verbatimTextOutput(
        outputId = "data_summary"
      ),

      hr(),

      # --------------------------------------------------------
      # Plot
      # --------------------------------------------------------

      h3("Plot"),

      plotOutput(
        outputId = "animal_plot",
        height = "700px"
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  # ==========================================================
  # STEP 1: Read the uploaded file
  # ==========================================================

  # reactive() creates an object that automatically updates
  # whenever the user uploads a different file.
  #
  # req(input$file) tells Shiny:
  #
  #     "Do not try to run this code until the user has
  #      uploaded a file."

  uploaded_data <- reactive({
    req(input$file)

    readxl::read_excel(
      input$file$datapath
    )
  })

  # ==========================================================
  # STEP 2: Clean the uploaded data
  # ==========================================================

  # Once the user uploads a file, send it through the
  # standardization function we created earlier.
  #
  # The result is a cleaned dataset that we can use for
  # analysis and plotting.

  cleaned_data <- reactive({
    standardize_animal_data(
      uploaded_data()
    )
  })

  # ==========================================================
  # STEP 3: Determine which measurements are available
  # ==========================================================

  # After the data have been cleaned, we look for numeric
  # columns that could potentially be plotted.
  #
  # This means the user does NOT have to know the R column
  # names.
  #
  # For example, we could eventually display:
  #
  #     NeuN count
  #     Iba1 count
  #     Glucose
  #
  # instead of:
  #
  #     final_neu_n
  #     final_iba1
  #     glucose_mg_dl

  observeEvent(
    cleaned_data(),
    {
      # Find all numeric columns.

      numeric_columns <- names(
        cleaned_data()
      )[
        vapply(
          cleaned_data(),
          is.numeric,
          logical(1)
        )
      ]

      # Update the measurement dropdown.

      updateSelectInput(
        session = session,
        inputId = "outcome",
        choices = numeric_columns
      )
    }
  )

  # ==========================================================
  # STEP 4: Create a data summary
  # ==========================================================

  # Show the user some basic information about the uploaded
  # and cleaned data.

  output$data_summary <- renderPrint({
    req(cleaned_data())

    data <- cleaned_data()

    cat(
      "Number of rows:",
      nrow(data),
      "\n"
    )

    cat(
      "Number of animals:",
      n_distinct(data$animal_id),
      "\n"
    )

    cat(
      "Regions:",
      paste(
        unique(data$region),
        collapse = ", "
      ),
      "\n"
    )

    cat(
      "Diets:",
      paste(
        unique(data$diet_group),
        collapse = ", "
      ),
      "\n"
    )

    cat(
      "Time points:",
      paste(
        unique(data$time_point),
        collapse = ", "
      ),
      "\n"
    )
  })

  # ==========================================================
  # STEP 5: Generate the plot
  # ==========================================================

  # eventReactive() means that the plot is only regenerated
  # when the user clicks "Generate Plot".
  #
  # This prevents the plot from changing every time something
  # else in the app changes.

  plot_data <- eventReactive(
    input$generate_plot,
    {
      req(
        cleaned_data(),
        input$outcome
      )

      # Send the cleaned data and selected measurement
      # to our plotting function.

      make_animal_plot(
        data = cleaned_data(),
        outcome = input$outcome
      )
    }
  )

  # ==========================================================
  # STEP 6: Display the plot
  # ==========================================================

  output$animal_plot <- renderPlot({
    req(plot_data())

    plot_data()
  })

  # ==========================================================
  # STEP 7: Allow user to download cleaned data
  # ==========================================================

  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste0(
        "animal_database_cleaned_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {
      write_csv(
        cleaned_data(),
        file
      )
    }
  )

  # ==========================================================
  # STEP 8: Allow user to download the plot
  # ==========================================================

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(
        input$outcome,
        "_plot_",
        Sys.Date(),
        ".png"
      )
    },

    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_data(),
        width = 10,
        height = 7,
        dpi = 300
      )
    }
  )
}


# ============================================================
# RUN THE APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
