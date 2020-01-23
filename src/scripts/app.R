
##variable importance
feature_imp <- importance(ship_study_results$model$finalModel)
var_list <- feature_imp$varimps$varname
var_values <- feature_imp$varimps$imp

str(var_list)

var_list <- list(var_list)

##Missingness plot
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)


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
            
            
            # fluidRow(
            #   titlePanel("Feature Importance"),
            #     sidebarLayout(
            #       sidebarPanel(selectInput(inputId = 'features',
            #                                label='List of Features',
            #                                choices = c("var_list")),
            #       mainPanel(
            #         box(
            #           width = 10, status = "primary", solidHeader = FALSE, collapsible = TRUE,
            #           plotOutput(outputId = 'fea_imp'))
            #     )
            #   )
            #   )
            #   ),
            
            fluidRow(
              tabBox(
                title = "Feature Importance", width = "100%", height = "100%", id = "tabset1",
                tabPanel("Feature Importance Plot", plotOutput(outputId = "fea_imp", width = "100%", height = "100%")),
                tabPanel("Model", plotOutput(outputId = "model", width = "100%", height = "100%"))
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
        title = "Missing Values", width = 12, height = "75px", id = "tabset1",
        tabPanel("Threshold: 0 to 5%", plotOutput(outputId = "miss_val_1")),
        tabPanel("Threshold: 5 to 10%", plotOutput(outputId = "miss_val_2")),
        tabPanel("Threshold: 10 to 20%", plotOutput(outputId = "miss_val_3")),
        tabPanel("Threshold: Greater_than_20%", plotOutput(outputId = "miss_val_4"))
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
                                         choices = rownames(df))),
                mainPanel(
                  box(
                    width = 10,status = "primary", solidHeader = FALSE, collapsible = TRUE, 
                    plotOutput(outputId = 'ice_plot'))
                  
                )
              )
            )
            
            
            
            
            )

  )
) 

ui <- dashboardPage(skin = "red", header, sidebar, body)  


##########################################------------  SERVER.R ---------- ##################################################


server <- shinyServer(function(input, output,session) {  
  
  
  
  # output$ice_plot <- renderPlot({
  #   barplot(height = df[,input$features] ,main = input$features, names.arg = rownames(df))
  # })


  output$fea_imp <- renderImage({
    
    list(src = str_c(getwd(),'/images/FEATURE_IMP_Rplot.PNG'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$model <- renderImage({
    
    list(src = str_c(getwd(),'/images/MODEL.PNG'),
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  # output$ice_plot <- renderImage({
  #   
  #   list(src = str_c(getwd(),'/visualization/ICE_plot.PNG'),
  #        contentType = 'image/png',
  #        width = "100%",
  #        height = "400px",
  #        alt = "This is alternate text")
  # }, deleteFile = FALSE)
  
  
  
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
  
  
  # output$plot1 <- renderImage({
  #   list(src = str_c(getwd(),'/visualization/Missingness_in_percent.PNG'),
  #        contentType = 'image/png',
  #        width = "50%",
  #        height = "300px",
  #        alt = "This is alternate text")
  # }, deleteFile = FALSE)
  
  
  # output$plot2 <- renderImage({
  #   list(src = str_c(getwd(),'/visualization/matrix_plot.PNG'),
  #        contentType = 'image/png',
  #        width = "50%",
  #        height = "300px",
  #        alt = "This is alternate text")
  # }, deleteFile = FALSE)
  
  
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




}
) 


##########################################------------  App Run ---------- ##################################################


shinyApp(ui, server)


library(shinydashboard)
library(shiny)
