# =============================================================================
# Author: Halley Brantley
# Description: Complex patients shiny app
# ==========================================================================================================================================================

library(shiny)
library(tidyverse)
library(magrittr)
library(glue)
library(scales)
library(Matrix)

n_months <- 12
start_date <- '2017-01-01'
data_name <- glue("ER_{start_date}_{n_months}mo.RData")
load(data_name)
source("functions.R")
mod_df$ct_DX <- mod_df %>% select(paste0("DX_", DX_levels)) %>% rowSums()
theme_set(theme_classic(base_size = 16))

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Multi-morbid Member Summary (Updated 2020-03-12)"),
    
    fluidRow(
        column(12, 
               h4("Population Description"),
               tags$div(
                   tags$ul(
                       tags$li("NHI population with claims, EMR, and 3 days with 
                               a healthcare system encounter and diagnosis in 2017."),
                       tags$ul(
                       tags$li("Encounter types include: clinic, 
                               ER, inpatient, outpatient, urgent care, home visit, 
                               nursing home, and skilled nursing inpatient facility.")
                       ),
                       tags$li("The date of a patient's third encounter is 
                               considered the anchor date."),
                       tags$li("Members all have a prescription in the 12 months 
                               prior to the anchor date."),
                       tags$li("Conditions are labeled using a member's EMR 
                               from the anchor date and 12 previous months. 
                               Two relevant ICD10 diagnoses are required.", 
                               tags$a(href="https://www2.ccwdata.org/web/guest/condition-categories", 
                                      "CCW chronic condition ICD codes are available here.")
                       ),
                       tags$li("Counts shown are members with at least the labeled conditions in the 12 
                               months prior. Members with multiple conditions 
                               appear in multiple squares.")

                   )
               ),
               h4("Definitions"),
               tags$div(
                   tags$ul(
                       tags$li("Frequency of ER+IP is the proportion (count) 
                       of members with  any ER enounter followed by an IP 
                       encounter in the 12 months  after the anchor date."),
                       tags$li("Average PMPM is calculated using the 12 months after 
                               the anchor date."),
                       tags$li("'Base Condition 1' and 'Base Condition 2' enable 
                               the exploration of populations with 3 or 4 
                               conditions. When both are set to 'None' members 
                               with at least one condition are shown on the 
                               diagonal and members with at least 2 conditions 
                               are shown on the off-diagonal.")
                   )
               ),
               h4("Population Statistics"),    
               tags$div(
                   tags$ul(
                       tags$li(glue("Total population: {comma(nrow(mod_df))}")),
                       tags$li(glue("Count with no conditions: 
                                    {comma(sum(mod_df$ct_DX == 0))}")),
                       tags$li(glue("Proportion with ER+IP with no conditions: 
                                    {round(mean(mod_df$CASE_INT[which(mod_df$ct_DX == 0)]),2)}")),
                       tags$li(glue("PMPM with no conditions: 
                                    ${comma(round(mean(mod_df$TOTAL_COST[which(mod_df$ct_DX == 0)])/12))}"))
                   )
               )
        )),
    
    fluidRow(
        column(3,
               selectInput("base", "Base Condition 1:", 
                           
                           choices = c("None", DX_labels), 
                           selected = "None",
                           multiple = FALSE,
                           selectize = TRUE, width = '50%', size = NULL)
        ),
        
        column(3,
               selectInput("base2", "Base Condition 2:", 
                           choices = c("None", DX_labels), 
                           selected = "None",
                           multiple = FALSE,
                           selectize = TRUE, width = '50%', size = NULL)
        ),
        column(2,
               selectInput("er", "Previous ER:", 
                           choices = c("Yes"= 1, 
                                       "No" = 0,
                                       "Both" = -1), 
                           selected = -1,
                           multiple = FALSE,
                           selectize = TRUE, width = '50%', size = NULL)
        ), 
        column(2,
               selectInput("ip", "Previous IP:", 
                           choices = c("Yes"= 1, 
                                       "No" = 0,
                                       "Both" = -1), 
                           selected = -1,
                           multiple = FALSE,
                           selectize = TRUE, width = '50%', size = NULL)
        ),
        column(2,
               checkboxInput("show_labels", "Show Labels", 
                             value = TRUE)
        )
    ),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(12, 
               checkboxGroupInput("condition",
                                  "Choose which conditions to include:",
                                  choiceNames = DX_labels, 
                                  choiceValues = DX_labels_short,
                                  select = c("Diabetes", "CKD", "COPD", "Heart Failure"), 
                                  inline = TRUE)
        ) 
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
        column(4,
               h2("Frequency of Conditions"),
               # textOutput("conditions"),
               plotOutput("freqPlot")
        ),
        column(4,
               h2("Frequency of ER+IP"),
               plotOutput("casePlot")
        ),
        column(4,
               h2("Avg. Allowed Amt. PMPM"),
               plotOutput("costPlot")
        )
    ), 
    fluidRow(
      column(6,
             h2("Inpatient Allowed Amt"),
             # textOutput("conditions"),
             plotOutput("ipPlot")
      ),
      column(6,
             h2("Outpatient Allowed Amt"),
             plotOutput("opPlot")
      )
    ),
    fluidRow(
      column(6,
             h2("Physician Allowed Amt"),
             plotOutput("drPlot")
      ),
      column(6,
             h2("ER Allowed Amt"),
             plotOutput("erPlot")
      )
    ),
    
    fluidRow(
        column(12, 
               p("Maintained by Halley Brantley"),
               p("Created by Halley Brantley, Megan Backhaus, and Sam Amodeo"))
    )
    
)

# For de-bugging
# input <- {}
# input$base = "None"
# input$base2 = "None"
# input$er = -1
# input$ip = -1

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dyads <- reactive({
        # Filter by base condition
        if (input$base != "None") {
            mod_filter <- 
              mod_df[mod_df[ , 
                             paste0("DX_", 
                                    DX_levels[which(DX_labels == 
                                                      input$base)])] == 1, ]
        } else {
            mod_filter <- mod_df
        }
        
        if (input$base2 != "None") {
            mod_filter <- 
              mod_filter[mod_filter[ , 
                                     paste0("DX_", 
                                            DX_levels[which(DX_labels == 
                                                              input$base2)])] == 1, ]
        } else {
            mod_filter <- mod_filter
        }
        
        if(input$er > -1){
            mod_filter <- mod_filter %>% filter(VISIT_ER == input$er) 
        }
        
        if(input$ip > -1){
            mod_filter <- mod_filter %>% filter(VISIT_IP == input$ip) 
        }
        # Get dyads
        get_dyads(mod_filter, n_months)
    })
    
    
    
    output$freqPlot <- renderPlot({
        
        
     ct_plot <- dyads() %>%
            filter(Condition1 %in% input$condition, Condition2 %in% input$condition) %>%
            ggplot(aes(x=Condition1, y=Condition2)) + 
            geom_tile(aes(fill = Frequency)) +
            scale_fill_distiller(palette="Spectral", limits = c(0,1)) + 
            theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
            labs(x = NULL, y = NULL, fill = "Condition \n Prevalence",
                 title = "Count of members")
     if (input$show_labels){
         ct_plot + geom_label(aes(label = comma(Counts))) 
     } else {
         ct_plot
     }
        
    })
    
    output$casePlot <- renderPlot({
        
        case_plot <- dyads() %>%
            filter(Condition1 %in% input$condition, Condition2 %in% input$condition) %>%
            ggplot(aes(x=Condition1, y=Condition2)) + 
            geom_tile(aes(fill=Case_Frequency)) +
            scale_fill_distiller(palette="Spectral") + 
            theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
            labs(x = NULL, y = NULL, fill = "ER+IP \n Proportion", 
                 title = "Count of cases")
        
        if (input$show_labels){
            case_plot + geom_label(aes(label = comma(Case_Counts))) 
        } else {
            case_plot
        }
        
    })
    
    output$costPlot <- renderPlot({
        
        make_plot(input, dyads(), PMPM, "Total Cost")
        
    })
    
    output$ipPlot <- renderPlot({
      
      make_plot(input, dyads(), IP, "Inpatient Cost")
      
    })
    
    output$opPlot <- renderPlot({
      
      make_plot(input, dyads(), OP, "Outpatient Cost")
      
    })
    
    output$drPlot <- renderPlot({
      
      make_plot(input, dyads(), DR, "Physician Cost")
      
    })
    
    output$erPlot <- renderPlot({
      
      make_plot(input, dyads(), ER, "ER Cost")
      
    })
    
    output$conditions <- renderText({
        
        paste(input$condition, collapse = " ")
        
    })
    
}

shinyApp(ui = ui, server = server)
