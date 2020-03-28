# ##Missingness plot
# data <- data.frame(
#   id=seq(1,60),
#   individual=paste( "Mister ", seq(1,60), sep=""),
#   value=sample( seq(10,100), 60, replace=T)
# )
# 
# # Make the plot
# p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   
#   # This add the bars with a blue color
#   geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
#   
#   # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
#   ylim(-100,120) +
#   
#   # Custom the theme: no axis title and no cartesian grid
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
#   ) +
#   
#   # This makes the coordinate polar instead of cartesian.
#   coord_polar(start = 0)


##Feature importance plot
feature_imp <- importance(ship_study_results$model$finalModel)
var_list <- feature_imp$varimps$varname
var_values <- feature_imp$varimps$imp


## Model dataframe
rules_coeff_1 <- select(feature_imp$baseimps, c("rule", "description", "coefficient"))
rules_coeff_1

## Validate Model Prediction
validation_set <- ship_study_results$validation_set
actual_labels <- validation_set$liver_fat
validation_set <- validation_set[, !names(validation_set) %in% c("liver_fat")]
model_predictions <- predict(ship_study_results$model, validation_set)
cmp_table <- table(factor(model_predictions, levels = levels(model_predictions)),
                   factor(actual_labels, levels = levels(actual_labels)))
cmatrix <- confusionMatrix(cmp_table)
cmatrix_table <- as.data.frame(cmatrix$table)
generalization_overall <- as.data.frame(cmatrix$overall)
generalization_byClass <- as.data.frame(cmatrix$byClass)
colnames(generalization_byClass) <- c("Performance Value")
colnames(generalization_overall) <- c("Performance Value")

##########################################------------  UI.R ---------- ##################################################


library(shinydashboard)
library(shiny)
library(data.table)
library(DT)

header <- dashboardHeader(title = "SHIP Cohort Study")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("EDA Plots", tabName = "eda", icon = icon("th")),
    menuItem("ICE Plot", tabName = "ice", icon = icon("th"))
  )
)

body <- dashboardBody(   
  tabItems(
    
    # First tab content
    tabItem(tabName = "dashboard",
            
            fluidRow(
              tabBox(
                width = "100%", height = "100vh", id = "tabset1",
                tabPanel("Feature Importance Plot", plotOutput(outputId = "fea_imp", width = "100%", height = "100vh")),
                tabPanel("Model", DT::dataTableOutput(outputId = "model1")),
                tabPanel("Performance Analysis", fluidRow(
                  column(6,
                         box(title = "Generalization accuracy of the model over test set", width = "100%", background = "green", style="color: black;",
                             DT::dataTableOutput(outputId = "performance_overall"))
                  ), 
                  column(6, 
                         box(title = "Generalization performance over test set with different metrics", width = "100%", background = "blue", style="color: black;",
                             DT::dataTableOutput(outputId = "performance_byClass"))
                         )
                  ), 
                  fluidRow(
                    # column(6, 
                    #        div( style="color: black;",
                    #       box(title = "Confusion Matrix", width = "100%", background = "light-blue", style="color: black;" ,
                    #           DT::dataTableOutput(outputId = "conf_matrix"))
                    #        )
                    #        ), 
                    )
                  )
              )
            )
    ),
            

#         ####Second tab content
        tabItem(tabName = "eda",

    #               fluidRow(
    #                   box(
    #                       title = "Missing Data", width = 7 , height = "350px",  status = "primary", solidHeader = TRUE, collapsible = TRUE,
    #                       plotOutput("plot1")),
    #                   box(title = "Info", width = 4, background = "light-blue",
    #                       "Plot to show Mssing dat for complete set of features")
    #     ),
    # 
    #     fluidRow(
    #       box(
    #         title = "Matrix Plot", width = 7 , height = "350px", status = "primary", solidHeader = TRUE, collapsible = TRUE,
    #         plotOutput("plot2")),
    #       box(title = "Info", width = 4, background = "light-blue", "Another representation of missing data with the help of a matrix plot")
    # ),

    fluidRow(
      tabBox(
        width = "100%", height = "100vh", title = "Missing Values", id = "tabset1",
        tabPanel("Threshold: 0 to 5%", fluidRow(title = "Features with wave suffix s0", column(12, plotOutput(outputId = "miss_val_1", height = "100%", width = "100%")))),
        tabPanel("Threshold: 5 to 10%", fluidRow(title = "Features with wave suffix s0", column(12, plotOutput(outputId = "miss_val_2", height = "100%", width = "100%"))), fluidRow(title = "Features with wave suffix s1", column(12, plotOutput(outputId = "miss_val_2_copy1", height = "100%", width = "100%")))),
        tabPanel("Threshold: 10 to 20%", fluidRow(title = "Features with wave suffix s0", column(12, plotOutput(outputId = "miss_val_3", height = "100%", width = "100%"))),fluidRow(title = "Features with wave suffix s2", column(12, plotOutput(outputId = "miss_val_3_copy1", height = "100%", width = "100%")))),
        tabPanel("Threshold: Greater_than_20%", fluidRow(title = "Features with wave suffix s1", column(12, plotOutput(outputId = "miss_val_4", height = "100%", width = "100%"))), fluidRow(title = "Features with wave suffix s1", column(12, plotOutput(outputId = "miss_val_4_copy1", height = "100%", width = "100%"))), fluidRow(title = "Features with wave suffix s2", column(12,  plotOutput(outputId = "miss_val_4_copy2", height = "100%", width = "100%"))))
      )
    )
   ),


  ####Third tab content 
    tabItem(tabName = "ice",
            fluidRow(
              titlePanel("ICE Plot"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = 'features',
                                         label='List of Features',
                                         choices = var_list)),
                mainPanel(
                  box(
                    width = 10,status = "primary", solidHeader = FALSE, collapsible = TRUE, 
                    plotOutput(outputId = 'ice_plot'))
                  
                )
              )
            ),
          
            fluidRow(
              titlePanel("Centered ICE Plot"),
              sidebarLayout(
                sidebarPanel(selectInput(inputId = 'centered_features',
                                         label='List of Features',
                                         choices = var_list)),
                mainPanel(
                  box(
                    width = 10,status = "primary", solidHeader = FALSE, collapsible = TRUE, 
                    plotOutput(outputId = 'centered_ice_plot'))
                  
                )
              )
            )  
            
          )
  )
) 

ui <- dashboardPage(skin = "red", header, sidebar, body)  


##########################################------------  SERVER.R ---------- ##################################################


server <- shinyServer(function(input, output,session) {  
  
  output$ice_plot <- renderPlot({
    effect <- FeatureEffect$new(predictor = model, feature = input$features, method = "ice")
    plot(effect)
  })
  
  output$centered_ice_plot <- renderPlot({
    effect <- FeatureEffect$new(predictor = model, feature = input$centered_features, center.at = 0,  method = "ice")
    plot(effect)
  })

  output$fea_imp <- renderPlot({
    ggplot(data = check, aes(x = check$description, y = check$coefficient, fill = check$coefficient > 0)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(name = "Coefficients > 0", labels = c("Negative Values", "Positive Values"), values = c("FALSE"="#d43943", "TRUE"="#29ab9c")) + 
      labs(x= "Features", y="Importance") + 
      coord_flip()
  
  })

  
  output$model <- renderImage({
    
    list(src = str_c(getwd(),'/images/MODEL.PNG'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$miss_val_1 <- renderImage({
    list(src = str_c(getwd(),'/images/s0_0_to_5.jpeg'),
         width = "100%",
         height = "100%",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_2 <- renderImage({
    list(src = str_c(getwd(),'/images/s0_5_to_10.jpeg'),
         title = "Features with wave suffix s0",
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$miss_val_2_copy1 <- renderImage({
    list(src = str_c(getwd(),'/images/s1_5_to_10.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_3 <- renderImage({
    list(src = str_c(getwd(),'/images/s0_10_to_20.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$miss_val_3_copy1 <- renderImage({
    list(src = str_c(getwd(),'/images/s2_10_to_20.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$miss_val_4 <- renderImage({
    list(src = str_c(getwd(),'/images/s0_20_above.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$miss_val_4_copy1 <- renderImage({
    list(src = str_c(getwd(),'/images/s1_20_above.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$miss_val_4_copy2 <- renderImage({
    list(src = str_c(getwd(),'/images/s2_20_above.jpeg'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$model1 <- DT::renderDataTable(rules_coeff_1,
                                   options = list(
                                     searching = FALSE,
                                     processing=FALSE,
                                     pageLength = 50,
                                     initComplete = I("function(settings, json) {alert('DONE')}")
                                   )) 

  output$conf_matrix <- DT::renderDataTable(cmatrix_table,
                                       options = list(
                                         paging=FALSE,
                                         searching = FALSE,
                                         processing=FALSE,
                                         initComplete = I("function(settings, json) {alert('DONE')}")
                                       )) 

  output$performance_overall <- DT::renderDataTable(generalization_overall,
                                       options = list(
                                         caption = "Overall Model Performance on test set with generalization accuracy",
                                         searching = FALSE,
                                         processing=FALSE,
                                         paging=FALSE,
                                         initComplete = I("function(settings, json) {alert('DONE')}")
                                       )) 

  output$performance_byClass <- DT::renderDataTable(generalization_byClass,
                                       options = list(
                                         color="black",
                                         searching = FALSE,
                                         processing=FALSE,
                                         paging=FALSE,
                                         initComplete = I("function(settings, json) {alert('DONE')}")
                                       )) 
  
}

) 


##########################################------------  App Run ---------- ##################################################


shinyApp(ui, server)

