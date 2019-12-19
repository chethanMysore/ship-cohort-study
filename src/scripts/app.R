
feature_imp <- importance(ship_study_results$model$finalModel)

var_list <- feature_imp$varimps$varname

var_values <- feature_imp$varimps$imp

df <- data.frame(features = var_list, feature_values = var_values)

df$feature_values

rownames(df) <- df[,1]

df[,1] <- NULL

rownames(df)

show <- barplot(height = df$feature_values , xlab = "Features", ylab = "Importance", names.arg = rownames(df))

show

##########################################------------  UI.R ---------- ##################################################


header <- dashboardHeader(title = "SHIP Cohort Study")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("EDA Plots", tabName = "eda", icon = icon("th"))
  )
)

body <- dashboardBody(   
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            
            
            fluidRow(
              titlePanel("Feature Importance"),
                sidebarLayout(
                  sidebarPanel(selectInput(inputId = 'features',
                                           label='List of Features',
                                           choices = rownames(df))),
                  mainPanel(
                    box(
                      title = "ICE Plot", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                      plotOutput(outputId = 'fea_imp'))
                )
              ),
              
              ###Second column of chart and its input###
              
            fluidRow(
              column(width = 4,
                     box(
                       title = "ICE Plot", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       plotOutput("plot2", height = 250)),
                     
                     box(
                       title = "Input", width = NULL, color = "light-blue", solidHeader = TRUE,background = "black",
                       sliderInput("slider", label = "Range of interest:", min = 0, max = 100, value = c(0, 100)))
              ),
              
              ###Third column of chart and its input###
              column(width = 4,
                     box(
                       title = "Minimal Change in Participant", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       plotOutput("plot3", height = 250)),
                     
                     box(
                       title = "Input", width = NULL, color = "light-blue", solidHeader = TRUE,background = "black",
                       sliderInput("slider", label = "Range of interest:", min = 0, max = 100, value = c(0, 100)))
              )
            )
            )
      ),
    
    # Second tab content
    tabItem(tabName = "eda",
            
            fluidRow(
             box(
                title = "Outliers", width = 4 ,status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotOutput("plot2", height = 350)),
            box(title = "Title 5", width = 4, background = "light-blue", 
                "A box with a solid light-blue background")
            ),
            
            fluidRow(
              box(
                title = "Label Count", width = 4 ,status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotOutput("plot2", height = 250)),
              box(title = "Title 5", width = 4, background = "light-blue", "Content")
        ),
        
        fluidRow(
          tabBox(
            title = "Missing Values", width = 12, id = "tabset1", height = "250px",
            tabPanel("Set1", imageOutput(outputId = "miss_val_1")),
            tabPanel("Set2", imageOutput(outputId = "miss_val_2")),
            tabPanel("Set3", png(str_c(getwd(),"/visualization/image1.png")))
          )
        )
    )
  )
) 

ui <- dashboardPage(skin = "red", header, sidebar, body) 


##########################################------------  SERVER.R ---------- ##################################################


server <- function(input, output) {  
  
  
  output$miss_val_1 <- renderImage({
    
    png(str_c(getwd(),"/visualization/image1.png"))
    
  }) 
  
  
#  output$plot1 <- renderPlot({
#    barplot(height = df$feature_values, names.arg = rownames(df))
#    })
 
  
  output$fea_imp <- renderPlot({
    barplot(height = df$feature_values , xlab = "Features", ylab = "Importance", names.arg = rownames(df))
  })
  
}



##########################################------------  App Run ---------- ##################################################


shinyApp(ui, server)


library(shinydashboard)
library(shiny)
