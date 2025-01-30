library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)

medical_data <- read.csv("medical_costs.csv")

medical_data <- medical_data %>% 
  distinct()

medical_data <- medical_data %>% 
  na.omit()

medical_data <- medical_data %>% 
  mutate(
    smoker = factor(smoker, levels = c("no", "yes"), labels = c("Non-Smoker", "Smoker")),
    region = as.factor(region),
    age_group = cut(
      age, 
      breaks = c(0, 18, 35, 50, 65, 100),
      labels = c("Child", "Young Adult", "Middle-Aged", "Senior", "Elderly"),
      right = FALSE
    ),
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 24.9 ~ "Normal",
      bmi >= 25 & bmi < 29.9 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ "Unknown"
    )
  )

ui <- dashboardPage(
  dashboardHeader(title = "Medical Cost Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", icon = icon("home"), tabName = "dashboard"),
      menuItem("Analysis", icon = icon("chart-line"), tabName = "analysis"),
      menuItem("Data Summary", icon = icon("table"), tabName = "summary")
    )
  ),
  dashboardBody(
    theme = shinytheme("cerulean"),
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Welcome to the Medical Cost Dashboard"),
              p("Explore medical cost data by regions, smokers, age groups, and more."),
              box(title = "Guide", status = "primary", solidHeader = TRUE, width = 12,
                  h3("BMI Categories"),
                  p("Underweight: BMI < 18.5"),
                  p("Normal: 18.5 <= BMI < 24.9"),
                  p("Overweight: 25 <= BMI < 29.9"),
                  p("Obese: BMI >= 30"),
                  h3("Age Groups"),
                  p("Child: 0 <= Age < 18"),
                  p("Young Adult: 18 <= Age < 35"),
                  p("Middle-Aged: 35 <= Age < 50"),
                  p("Senior: 50 <= Age < 65"),
                  p("Elderly: 65 <= Age <= 100"),
                  h3("Regions"),
                  p("Southwest, Southeast, Northwest, and Northeast")
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Region Comparison", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("scatter_plot"))
              ),
              fluidRow(
                box(title = "Charges by Smoking Status", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("box_plot"))
              ),
              fluidRow(
                box(title = "Medical Cost Comparison by Age, Smoking, and BMI", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("age_group_input", "Select Age Group:", choices = c("All", "Child", "Young Adult", "Middle-Aged", "Senior", "Elderly"), selected = "All"),
                    selectInput("smoker_input", "Select Smoking Status:", choices = c("All", "Non-Smoker", "Smoker"), selected = "All"),
                    selectInput("bmi_category_input", "Select BMI Category:", choices = c("All", "Underweight", "Normal", "Overweight", "Obese"), selected = "All"),
                    plotOutput("filtered_comparison_plot"))
              ),
              fluidRow(
                box(title = "Charges by BMI and Smoking Status", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("bmi_smoker_plot"))
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary Statistics by Region", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("summary_table"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- medical_data
    
    if (input$age_group_input != "All") {
      data <- data %>% filter(age_group == input$age_group_input)
    }
    
    if (input$smoker_input != "All") {
      data <- data %>% filter(smoker == input$smoker_input)
    }
    
    if (input$bmi_category_input != "All") {
      data <- data %>% filter(bmi_category == input$bmi_category_input)
    }
    
    return(data)
  })
  
  output$summary_table <- renderTable({
    filtered_data() %>%
      group_by(region, smoker) %>%
      summarise(
        mean_charges = mean(charges, na.rm = TRUE),
        median_charges = median(charges, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(medical_data, aes(x = bmi, y = charges, color = region)) +
      geom_point(alpha = 0.7, size = 2) +
      labs(
        title = "Medical Charges vs BMI by Region",
        x = "Body Mass Index (BMI)",
        y = "Medical Charges"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$box_plot <- renderPlot({
    ggplot(medical_data, aes(x = smoker, y = charges, fill = smoker)) +
      geom_boxplot(alpha = 0.8) +
      labs(
        title = "Charges by Smoking Status",
        x = "Smoking Status",
        y = "Medical Charges"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$filtered_comparison_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = bmi, y = charges, color = smoker, shape = age_group)) +
      geom_point(alpha = 0.7, size = 2) +
      labs(
        title = "Filtered Medical Cost Comparison by Age, Smoking Status, and BMI",
        x = "Body Mass Index (BMI)",
        y = "Medical Charges"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$bmi_smoker_plot <- renderPlot({
    filtered_data_for_bmi_smoker <- medical_data %>% 
      filter(bmi_category != "Unknown")
    
    ggplot(filtered_data_for_bmi_smoker, aes(x = bmi_category, y = charges, fill = smoker)) +
      geom_boxplot(alpha = 0.8) +
      labs(
        title = "Charges by BMI and Smoking Status",
        x = "BMI Category",
        y = "Medical Charges"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}
shinyApp(ui = ui, server = server)

