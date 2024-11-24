library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(corrplot)

# Load data
data <- read.csv("bone-marrow.csv")

# Impute missing values
data$RecipientRh[is.na(data$RecipientRh)] <- median(data$RecipientRh, na.rm = TRUE)
data$CD3dCD34[is.na(data$CD3dCD34)] <- median(data$CD3dCD34, na.rm = TRUE)
data$CD3dkgx10d8[is.na(data$CD3dkgx10d8)] <- median(data$CD3dkgx10d8, na.rm = TRUE)
data$Rbodymass[is.na(data$Rbodymass)] <- median(data$Rbodymass, na.rm = TRUE)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data$RecipientABO[is.na(data$RecipientABO)] <- Mode(data$RecipientABO)
data$ABOmatch[is.na(data$ABOmatch)] <- Mode(data$ABOmatch)
data$CMVstatus[is.na(data$CMVstatus)] <- Mode(data$CMVstatus)

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Bone Marrow Transplant Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Analysis"),
      selectInput("analysis_type", "Choose Plot Type:",
                  choices = list("Survival Status Distribution" = "survival_dist",
                                 "Donor Age Distribution" = "donor_age",
                                 "Survival Time Distribution" = "survival_time",
                                 "Gender Match vs. Survival Time" = "gender_match",
                                 "Stem Cell Source vs. Survival Time" = "stem_cell_source")),
      hr(),
      h4("Filter Data"),
      sliderInput("donor_age", "Donor Age Range:",
                  min = min(data$Donorage, na.rm = TRUE),
                  max = max(data$Donorage, na.rm = TRUE),
                  value = c(min(data$Donorage, na.rm = TRUE), max(data$Donorage, na.rm = TRUE)))
    ),
    mainPanel(
      plotlyOutput("main_plot"),
      hr(),
      h4("Correlation Heatmap"),
      plotOutput("correlation_plot")
    )
  )
)

# Define server logic for the dashboard
server <- function(input, output) {
  
  # Reactive data filtering based on user input
  filtered_data <- reactive({
    data %>% filter(Donorage >= input$donor_age[1] & Donorage <= input$donor_age[2])
  })
  
  # Main plot based on user selection
  output$main_plot <- renderPlotly({
    plot_data <- filtered_data()
    p <- switch(input$analysis_type,
                "survival_dist" = ggplot(plot_data, aes(x = as.factor(survival_status))) +
                  geom_bar(fill = "lightblue", color = "black") +
                  ggtitle("Distribution of Survival Status") +
                  xlab("Survival Status (0 = No, 1 = Yes)") +
                  ylab("Count"),
                "donor_age" = ggplot(plot_data, aes(x = Donorage)) +
                  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
                  ggtitle("Distribution of Donor Age") +
                  xlab("Donor Age") +
                  ylab("Frequency"),
                "survival_time" = ggplot(plot_data, aes(x = survival_time)) +
                  geom_histogram(binwidth = 100, fill = "salmon", color = "black") +
                  ggtitle("Distribution of Survival Time") +
                  xlab("Survival Time (days)") +
                  ylab("Frequency"),
                "gender_match" = ggplot(plot_data, aes(x = as.factor(Gendermatch), y = survival_time)) +
                  geom_boxplot(fill = c("lightblue", "lightgreen")) +
                  ggtitle("Survival Time by Gender Match") +
                  xlab("Gender Match (0 = Mismatch, 1 = Match)") +
                  ylab("Survival Time (days)"),
                "stem_cell_source" = ggplot(plot_data, aes(x = as.factor(Stemcellsource), y = survival_time)) +
                  geom_boxplot(fill = c("orange", "lightblue")) +
                  ggtitle("Survival Time by Stem Cell Source") +
                  xlab("Stem Cell Source") +
                  ylab("Survival Time (days)")
    )
    ggplotly(p)
  })
  
  # Correlation heatmap
  output$correlation_plot <- renderPlot({
    numeric_cols <- filtered_data() %>% select(Donorage, survival_time, Rbodymass, CD34kgx10d6, CD3dCD34, CD3dkgx10d8)
    corr_matrix <- cor(numeric_cols, use = "complete.obs")
    corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.cex = 0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
