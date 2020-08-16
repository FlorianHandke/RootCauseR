library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(here)

my_autocomplete_list <- c("John Doe","Ash","Ajay sharma","Ken Chong","Will Smith","Neo")

ui <- fluidPage(
    
    titlePanel("Edit Root Cause"),
    
    sidebarLayout(
        
        sidebarPanel(
            actionButton("load",
                         "Initial Load"),
            uiOutput("ui_input")
        ),
        
        mainPanel(
            DTOutput("table")
        )
    )
    
)

server <- function(input, output, session) {
    
    causes <- eventReactive(c(input$load, input$save),{
        res <-  
    })
    
    output$ui_input <- renderUI({
        req(causes())
        topic_autocomplete <- causes()$Topic
        general_autocomplete <- c("Machine",
                                  "Materials",
                                  "Measurements",
                                  "Methods/Procedures",
                                  "Manpower",
                                  "Environment",
                                  "Management")
        group_autocomplete <- causes()$Group
        specific_autocomplete <- causes()$Specific
        factor_autocomplete <- causes()$Factor
        source_autocomplete <- causes()$Source
        
        
        if (is.numeric(input$table_rows_selected)) {
            pre <- causes() %>% slice(input$table_rows_selected)
            value_topic <- pre$Topic
            value_general <- pre$General
            value_group <- pre$Group
            value_specific <- pre$Specific
            value_factor <- pre$Factor
            source_factor <- pre$Factor
            
        } else {
            value_topic <- NULL
            value_general <- NULL
            value_group <- NULL
            value_specific <- NULL
            value_factor <- NULL
            source_factor <- NULL
            
        }
        
        tagList(selectizeInput(inputId = 'topic',
                               label = 'Topic',
                               choices = topic_autocomplete,
                               selected = value_topic,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = TRUE)),
                pickerInput(inputId = "general",
                            label = "General",
                            selected = value_general,
                            choices = general_autocomplete),
                selectizeInput(inputId = 'group',
                               label = 'Group',
                               choices = group_autocomplete,
                               selected = value_group,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = TRUE)),
                selectizeInput(inputId = 'specific',
                               label = 'Specific',
                               choices = specific_autocomplete,
                               selected = value_specific,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = TRUE)),
                selectizeInput(inputId = 'factor',
                               label = 'Factor',
                               choices = factor_autocomplete,
                               selected = value_factor,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = TRUE)),
                selectizeInput(inputId = 'source',
                               label = 'Source',
                               choices = source_autocomplete,
                               selected = source_factor,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = TRUE)),
                actionButton("save",
                             "Save Entry")
        )
    })
    
    output$table <- renderDataTable({
        req(causes())
        
        causes() %>% 
            DT::datatable(selection = "single")
    })
    
    
    observeEvent(input$save, {
        
        if (is.numeric(input$table_rows_selected)) {
            df <- causes() %>% 
                slice(-input$table_rows_selected) %>% 
                add_row("Topic" = input$topic,
                        "General" = input$general,
                        "Group" = input$group,
                        "Specific" = input$specific,
                        "Factor" = input$factor,
                        "Source" = input$source)
        } else {
            df <- causes() %>% 
                add_row("Topic" = input$topic,
                        "General" = input$general,
                        "Group" = input$group,
                        "Specific" = input$specific,
                        "Factor" = input$factor,
                        "Source" = input$source)
        }
        
        saveRDS(df, paste0(here(), "/inst/Rds/root_causes.Rds"))
    })
    
    
}
shinyApp(ui = ui, server = server)