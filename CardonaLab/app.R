#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyvalidate)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Animal Imaging Metadata"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Animal Information"),
      
      textInput("animal","Animal ID"),
      
      radioButtons(
        "sex",
        "Sex",
        choices = c("F","M"),
        inline = TRUE
      ),
      
      dateInput(
        "dob",
        "Date of Birth"
      ),
      
      numericInput(
        "age",
        "Age (weeks)",
        value = NA,
        min = 0
      ),
      
      selectInput(
        "genotype",
        "Genotype",
        choices = c(
          "CamK2a+/ctFKN+",
          "CamK2a+/ctFKN-",
          "CamK2a-/ctFKN+",
          "CamK2a-/ctFKN-",
          "Other"
        )
      ),
      
      selectInput(
        "diet_type",
        "Diet Type",
        choices = c(
          "Normal Diet",
          "Dox Diet", 
          "Other"
        )
      ),
      
      selectInput(
        "diet_length",
        "Weeks of Diet",
        choices = c(
          "4 weeks" = "4w",
          "10 weeks" = "10w", 
          "Other"
        )
      ),
      
      dateInput("diet_start","Diet Start"),
      
      dateInput("diet_end","Diet End"),
      
      selectInput(
        "diabetes_status",
        "Diabetes Status",
        choices = c(
          "Diabetic",
          "Nondiabetic"
        )
      ),
      
      numericInput(
        "glucose",
        "Glucose (mg/dL)",
        value = NA,
        min = 0
      ),
      
      selectInput(
        "biological_replica",
        "Biological Replica",
        choices = c(
          1:100
        )
      ),
      
      selectInput(
        "technical_replica",
        "Technical Replica",
        choices = c(
          "B1", "B2", "B3", "B4", "B5", "B6", 
          "C1", "C2", "C3", "C4", "C5", "C6", 
          "D1", "D2", "D3", "D4", "D5", "D6"
        )
      ),
      
      hr(),
      
      h4("Experiment"),
      
      dateInput("exp_start","Experiment Start"),
      
      dateInput("exp_end","Experiment End"),
      
     
      
    ),
    
    mainPanel(
      
      h4("Imaging"),
      
      selectInput(
        "tissue",
        "Tissue Type",
        c(
          "Brain",
          "Retina"
        )
      ),
      
      selectInput(
        "region",
        "Region",
        c(
          "Brain_LGN",
          "Brain_SC",
          "Retina_Central",
          "Retina_Peripheral"
        )
      ),
      
      selectInput(
        "tissue_processing",
        "Tissue Processing",
        c(
          "free_floating", "WM", "OCT"
        )
      ),
      
      dateInput("stain_completion_date", "Date of Stain Completion"), 
      textInput("stain_researcher", "Stain Researcher"), 
      dateInput("imaging_date", "Date of Imaging"),
      
      textInput("imagist", "Imagist"), 
      
      textInput("imageid","Image ID"),
      
      numericInput("iba1_count","IBA1 Count",NA,0),
      
      numericInput("NeuN_count","NeuN Count",NA,0),
      
      numericInput("x_confocal","X confocal", 512),
      
      numericInput("y_confocal","Y confocal",512),
      
      numericInput("z_confocal","Z confocal",NA),
      
      
      
      textAreaInput(
        "notes",
        "Notes",
        rows = 5
      ),
      
      actionButton(
        "submit",
        "Submit Entry"
      ),
      
      hr(),
      
      
      tabsetPanel(
        
        tabPanel(
          "Current Entry",
          tableOutput("preview")
        ),
        
        tabPanel(
          "Glucose",
          tableOutput("glucose_table")
        ),
        
        tabPanel(
          "NeuN",
          tableOutput("neun_table")
        ),
        
        tabPanel(
          "IBA1",
          tableOutput("iba1_table")
        ),
        tabPanel(
          "Visualizations",
          
          selectInput(
            "plot_variable",
            "Outcome",
            choices = c(
              "Glucose" = "glucose_mg_dl",
              "NeuN Count" = "neun_count",
              "IBA1 Count" = "iba1_count"
            )
          ),
          
          checkboxGroupInput(
            "group_vars",
            "Group By",
            choices = c(
              "Diabetes Status" = "diabetes_status",
              "Diet Group" = "diet_group",
              "Time Point" = "time_point",
              "Sex" = "sex",
              "Region" = "region"
            ),
            selected = "diabetes_status"
          ),
          
          plotOutput("summary_plot", height = "600px")
        )
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session){
  if (!dir.exists("data")) {
    dir.create("data")
  }
  database <- reactiveVal()
  
  if(file.exists("data/animal_database_clean.csv")){
    
    database(
      read.csv(
        "data/animal_database_clean.csv",
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          dob = as.Date(dob),
          sex = as.character(sex), 
          image_id = as.character(image_id),
          experiment_start_date = as.Date(experiment_start_date),
          experiment_end_date = as.Date(experiment_end_date),
          diet_start_date = as.Date(diet_start_date),
          diet_end_date = as.Date(diet_end_date),
          date_tissue_sectioning = as.Date(date_tissue_sectioning),
          stain_completion_date = as.Date(stain_completion_date),
          imaging_date = as.Date(imaging_date),
          notes = as.character(notes)
        )
    )
    
  } else {
    
    database(tibble())
    
  }
  
  plot_data <- reactive({
    
    req(input$plot_variable)
    
    df <- database() %>%
      mutate(
        glucose_mg_dl = as.numeric(glucose_mg_dl),
        neun_count = as.numeric(neun_count),
        iba1_count = as.numeric(iba1_count)
      )
    
    
    req(nrow(df) > 0)
    
    grouping <- input$group_vars
    
    if(length(grouping) == 0){
      
      df %>%
        summarise(
          mean_value = mean(
            .data[[input$plot_variable]],
            na.rm = TRUE
          )
        )
      
    } else {
      
      df %>%
        group_by(
          animal_id,
          across(all_of(grouping))
        ) %>%
        summarise(
          animal_mean = mean(
            .data[[input$plot_variable]],
            na.rm = TRUE
          ),
          .groups="drop"
        ) %>%
        group_by(across(all_of(grouping))) %>%
        summarise(
          mean_value = mean(animal_mean, na.rm=TRUE),
          .groups="drop"
        )
      
    }
    
  })
  
  iv <- InputValidator$new()
  
  iv$add_rule(
    "animal",
    sv_required()
  )
  
  iv$add_rule(
    "glucose",
    sv_between(0,1000)
  )
  
  iv$enable()
  
  data_entry <- reactive({
    
    tibble(
      
      animal_id = input$animal,
      sex = as.character(input$sex),
      id_sex = paste(input$animal, input$sex),
      
      dob = as.Date(input$dob),
      age_weeks = input$age,
      
      genotype = input$genotype,
      
      treatment_group = paste(
        input$diabetes_status,
        input$diet_type,
        input$diet_length,
        sep = "-"
      ),
      
      time_point = input$diet_length,
      
      experiment_start_date = input$exp_start,
      experiment_end_date = input$exp_end,
      
      diabetes_status = input$diabetes_status,
      glucose_mg_dl = input$glucose,
      
      biological_replica = as.numeric(input$biological_replica),
      
      diet_group = input$diet_type,
      diet_start_date = input$diet_start,
      diet_end_date = input$diet_end,
      
      tissue_type = input$tissue,
      tissue_processing = input$tissue_processing,
      
      date_tissue_sectioning = as.Date(NA),
      
      region = input$region,
      
      image_id = as.character(input$imageid),
      
      technical_replica = input$technical_replica,
      
      stain_researcher = input$stain_researcher, 
      stain_completion_date = input$stain_completion_date,
      
      imagist = input$imagist,
      imaging_date = input$imaging_date,
      
      iba1_count = input$iba1_count,
      neun_count = input$NeuN_count,
      
      x_confocal = input$x_confocal,
      y_confocal = input$y_confocal,
      z_confocal = input$z_confocal,
      
      final_neu_n = NA_real_,
      final_iba1 = NA_real_,
      
      notes = input$notes
      
    )
    
  })
    
  glucose_summary <- reactive({
    
    database() %>%
      
      group_by(
        animal_id,
        diabetes_status,
        diet_group,
        time_point
      ) %>%
      
      summarise(
        avg_glucose = mean(as.numeric(glucose_mg_dl), na.rm = TRUE),
        .groups = "drop"
      )
    
  })
    
  neun_summary <- reactive({
    
    database() %>%
      
      group_by(
        animal_id,
        diabetes_status,
        diet_group,
        time_point,
        region
      ) %>%
      
      summarise(
        avg_neun = mean(as.numeric(neun_count), na.rm = TRUE),
        .groups = "drop"
      )
    
  })
    
  iba1_summary <- reactive({
    
    database() %>%
      
      group_by(
        animal_id,
        diabetes_status,
        diet_group,
        time_point,
        region
      ) %>%
      
      summarise(
        avg_iba1 = mean(as.numeric(iba1_count), na.rm = TRUE),
        .groups = "drop"
      )
    
  })
    
    
    
    
  
  output$preview <- renderTable(
    data_entry()
  )
  
  observeEvent(input$submit,{
    
    req(iv$is_valid())
    
    db <- database()
    
    new_row <- tibble(
      
      animal_id = input$animal,
      sex = input$sex,
      id_sex = paste(input$animal, input$sex),
      
      dob = as.Date(input$dob),
      age_weeks = as.numeric(input$age),
      genotype = input$genotype,
      
      treatment_group = paste(
        input$diabetes_status,
        input$diet_type,
        input$diet_length,
        sep = "-"
      ),
      
      time_point = input$diet_length,
      
      experiment_start_date = as.Date(input$exp_start),
      experiment_end_date = as.Date(input$exp_end),
      
      diabetes_status = input$diabetes_status,
      glucose_mg_dl = as.numeric(input$glucose),
      
      biological_replica = as.numeric(input$biological_replica),
      
      diet_group = input$diet_type,
      diet_start_date = as.Date(input$diet_start),
      diet_end_date = as.Date(input$diet_end),
      
      tissue_type = input$tissue,
      tissue_processing = input$tissue_processing,
      date_tissue_sectioning = as.Date(NA),
      
      region = input$region,
      image_id = as.character(input$imageid),
      technical_replica = input$technical_replica,
      
      stain_researcher = input$stain_researcher,
      stain_completion_date = as.Date(input$stain_completion_date),
      
      imagist = input$imagist,
      imaging_date = as.Date(input$imaging_date),
      
      iba1_count = as.numeric(input$iba1_count),
      neun_count = as.numeric(input$NeuN_count),
      
      x_confocal = as.numeric(input$x_confocal),
      y_confocal = as.numeric(input$y_confocal),
      z_confocal = as.numeric(input$z_confocal),
      
      final_neu_n = NA_real_,
      final_iba1 = NA_real_,
      
      notes = as.character(input$notes)
      
    )
    
    new_row <- new_row %>%
      mutate(
        across(
          c(animal_id, sex, id_sex, genotype, treatment_group,
            time_point, diabetes_status, diet_group,
            tissue_type, tissue_processing, region,
            technical_replica, stain_researcher,
            imagist, notes),
          as.character
        )
      )
    
    db <- bind_rows(
      database(),
      new_row
    )
    
    database(db)
    
    write.csv(
      db,
      "data/animal_database_clean.csv",
      row.names = FALSE
    )
    
    showNotification("Record saved!")
    
  })
    
  
  output$glucose_table <- renderTable({
    glucose_summary()
  })
  
  output$neun_table <- renderTable({
    neun_summary()
  })
  
  output$iba1_table <- renderTable({
    iba1_summary()
  })
  
  output$summary_plot <- renderPlot({
    
    df <- plot_data()
    
    req(nrow(df) > 0)
    
    groups <- input$group_vars
    
    if(length(groups) == 0){
      
      ggplot(df,
             aes(x = "All", y = mean_value)) +
        geom_col() +
        labs(
          x = "",
          y = "Mean"
        )
      
    } else {
      
      df$Group <- apply(
        df[, groups, drop = FALSE],
        1,
        paste,
        collapse = " | "
      )
      
      ggplot(df,
             aes(
               x = reorder(Group, mean_value),
               y = mean_value
             )) +
        geom_col() +
        coord_flip() +
        labs(
          x = "",
          y = "Mean",
          title = paste(
            "Average",
            input$plot_variable
          )
        ) +
        theme_bw()
      
    }
    
  })
  
}

shinyApp(ui, server)
