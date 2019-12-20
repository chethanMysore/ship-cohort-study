
feature_imp <- importance(ship_study_results$model$finalModel)

var_list <- feature_imp$varimps$varname
var_values <- feature_imp$varimps$imp


df <- data.frame(features = var_list, feature_values = var_values)
rownames(df) <- df[,1]
df[,1] <- NULL
rownames(df)


str(df$feature_values)

df$feature_values_1 <- (df$feature_values) * 2
df$feature_values_2 <- (df$feature_values) * 3
df$feature_values_3 <- (df$feature_values) * 4
df$feature_values_4 <- (df$feature_values) * 5
df$feature_values_5 <- (df$feature_values) * 6


df






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
                                           choices = colnames(df))),
                  mainPanel(
                    box(
                      width = 10, status = "primary", solidHeader = FALSE, collapsible = TRUE,
                      plotOutput(outputId = 'fea_imp'))
                )
              )
              ),
            
            
            fluidRow(
              titlePanel("ICE Plot"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = 'features',
                                                           label='List of Features',
                                                           choices = rownames(df))),
                mainPanel(
                  box(
                    width = 10,status = "primary", solidHeader = FALSE, collapsible = TRUE, 
                    plotOutput(outputId = 'ice_plot'))
                   
                )
              )
            )
    ),
            
              ###Second column of chart and its input###

            # fluidRow(
            #   column(width = 4,
            #          box(
            #            title = "ICE Plot", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            #            plotOutput("dummy")),
            # 
            #          box(
            #            title = "Input", width = NULL, color = "light-blue", solidHeader = TRUE,background = "black",
            #            sliderInput("slider", label = "Range of interest:", min = 0, max = 100, value = c(0, 100)))
            #   ),
            # 
            # 
            #   ###Third column of chart and its input###
            #   column(width = 4,
            #          box(
            #            title = "Minimal Change in Participant", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            #            plotOutput("plot3", height = 250)),
            # 
            #          box(
            #            title = "Input", width = NULL, color = "light-blue", solidHeader = TRUE,background = "black",
            #            sliderInput("slider", label = "Range of interest:", min = 0, max = 100, value = c(0, 100)))
            #   )
            # ),
            # 

#         ####Second tab content
        tabItem(tabName = "eda",

                  fluidRow(
                      box(
                          title = "Missing Data", width = 7 , height = "350px",  status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          plotOutput("plot1")),
                      box(title = "Info", width = 4, background = "light-blue",
                          "Plot to show Mssing dat for complete set of features")
        ),

        fluidRow(
          box(
            title = "Matrix Plot", width = 7 , height = "350px", status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("plot2")),
          box(title = "Info", width = 4, background = "light-blue", "Another representation of missing data with the help of a matrix plot")
    ),

    fluidRow(
      tabBox(
        title = "Missing Values", width = 12, height = "75px", id = "tabset1",
        tabPanel("Threshold: 0 to 5%", plotOutput(outputId = "miss_val_1")),
        tabPanel("Threshold: 5 to 10%", plotOutput(outputId = "miss_val_2")),
        tabPanel("Threshold: 10 to 20%", plotOutput(outputId = "miss_val_3")),
        tabPanel("Threshold: Greater_than_20%", plotOutput(outputId = "miss_val_4"))
      )
    )
   )

  )
)

ui <- dashboardPage(skin = "red", header, sidebar, body) 


##########################################------------  SERVER.R ---------- ##################################################


server <- shinyServer(function(input, output,session) {  
  
  
  
  output$fea_imp <- renderPlot({
    barplot(height = df[,input$features] ,main = input$features, names.arg = rownames(df))
  })
  

  output$ice_plot <- renderImage({
    
    list(src = str_c(getwd(),'/visualization/ICE_plot.PNG'),
         contentType = 'image/png',
         width = "100%",
         height = "400px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  
  output$miss_val_1 <- renderImage({
    list(src = str_c(getwd(),'/visualization/0_to_5.PNG'),
         contentType = 'image/png',
         width = "75%",
         height = "350px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_2 <- renderImage({
    list(src = str_c(getwd(),'/visualization/image2.PNG'),
         contentType = 'image/png',
         width = "75%",
         height = "350px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_3 <- renderImage({
    list(src = str_c(getwd(),'/visualization/image3.PNG'),
         contentType = 'image/png',
         width = "75%",
         height = "350px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_4 <- renderImage({
    list(src = str_c(getwd(),'/visualization/image4.PNG'),
         contentType = 'image/png',
         width = "75%",
         height = "350px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$plot1 <- renderImage({
    list(src = str_c(getwd(),'/visualization/Missingness_in_percent.PNG'),
         contentType = 'image/png',
         width = "50%",
         height = "300px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$plot2 <- renderImage({
    list(src = str_c(getwd(),'/visualization/matrix_plot.PNG'),
         contentType = 'image/png',
         width = "50%",
         height = "300px",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  # output$ice_plot <- renderImage({
  # 
  #   # outfile <- str_c(getwd(),"/visualization/abc.png")
  #   # png()
  #   # hist(rnorm(500))
  #   # dev.off()
  # 
  # 
  #   list(src = str_c(getwd(),'/visualization/image2.PNG'))
  # 
  # }, deleteFile = FALSE)




})


##########################################------------  App Run ---------- ##################################################


shinyApp(ui, server)


library(shinydashboard)
library(shiny)
