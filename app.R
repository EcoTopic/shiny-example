library(shiny)
library(dplyr)
library(readr)

# Feature_A <- c(1, 2,1, 4,2)
# Feature_B <- c(4,5,6,6,6)
# Feature_C <- c(22,4,3,1,5)
# df<- data.frame(Feature_A ,Feature_B ,Feature_C)
df <- read_csv("./fertility.csv") %>%
  mutate(across(where(is.character), as.factor)) %>%
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  mutate(Number_of_hours_spent_sitting_per_day = if_else(Number_of_hours_spent_sitting_per_day > 24,
                                                         24, Number_of_hours_spent_sitting_per_day))

# Define UI for application
ui= fluidPage(

  # Header or Title Panel
  titlePanel(title = h4("Regression")),
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      selectInput('dp', 'Select Dependent Variable', colnames(select(df, where(is.numeric)))),
      selectInput('ip', 'Select an Explanatory Variable', colnames(df), multiple = F),
      actionButton(inputId = "btn1",label="Regression Plot"),
      actionButton(inputId = "btn2",label="Show Stats")),



    # Main Panel
    mainPanel(#"main panel",
              plotOutput("regplot"),
              verbatimTextOutput("summary"))

  ))
server = function(input, output,session) {

  #code for regression
  lm_fit <- reactive({
    lm(as.formula(paste0(input$dp, " ~ ", input$ip)), data=df)
  })

  summary_stats <- eventReactive(input$btn2,{
    summary(lm_fit())
  })


  regression_plot<- eventReactive(input$btn1, {
    ggplot(data = df, aes_string(x = input$ip, y = input$dp)) +
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = T)

  })
  #end of regression code



  output$regplot <- renderPlot({
    regression_plot()
  })
  output$summary <- renderPrint({
    summary_stats()
  })

}

shinyApp(ui,server)
