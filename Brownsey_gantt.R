library(shiny)
library(timevis)
library(dplyr)
library(tidyverse)
library(htmltools)
library(htmlwidgets)

ui <- fluidPage(
  
  #Basically everything to do with the styling is contained here.
  #the .vis-ite.X is what we use to give a specific item styling under the className property of that item
  #we can dynamically set this using string detection on the contents variable (done inside the variable main)
  #Press F12 to get it up in css format in terms of the shiny page :O!!!
  tags$head(
    tags$style(HTML("
                    .vis-item .vis-item-overflow { overflow: visible; } 
                    .vis-timeline {border: 2px solid #e21b35;font-size: 12pt}
                    .vis-item {border-color: red;font-size: 10pt;}
                    .vis-item.vis-dot {border-width: 10px;border-radius: 15px;}
                    .vis-time-axis .vis-text {color: #e21b35; padding-top: 10px;padding-left: 10px;}
                    .vis-item.vis-selected {border-color: red;}
                    .vis-item.Planning { color: black; background-color: #1ac6ff; border-color: #1ac6ff; }
                    .vis-item.FSFV { color: black; border-color: grey; }
                    .vis-item.Research { color: black; background-color: #00e600; border-color: #00e600; }
                    .vis-item.Analysis { color: black; background-color: #ffff00; border-color: #ffff00;  }
                    .vis-item.Write_up { color: black; border-color: red;  }
                    "))
    
    
    ),
  # .vis-timeline {line-color: #e21b35;} would quite like to change added line from blue to red and i'm sure it's doable above
  
  titlePanel("Brownsey's Gantt Creator"),
  
  fluidPage(
    
    fluidRow(
      
      # Input: Select a file ----
      column(1,
             fileInput("file1", "CSV Input",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
             
             checkboxInput("subgrouping", "Apply Subgrouping", FALSE),
             
             actionButton("fit","Fit all items"),
             actionButton("time","Add Line")

            
      ),
      
      # area for displaying the gantt diagram
      column(11,
             timevisOutput('contents',width = "100%",height = "auto")
             
      )
      
    ))
    )


server <- function(input, output, session) {
  
  output$contents <- renderTimevis({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath )

    #Added checkbox if logic
    if(!input$subgrouping){
      main <- df %>%
        select(content, start,end,group,type,className)
    }else {
      main <- df %>%
        arrange(desc(subgroup))%>%
        select(content, start,end,group,subgroup,type,className)
    }
    
    
    group <- df %>% filter(group != "NA") %>%
      mutate(id = group, content = group )%>%
      select(id,content) %>% 
      unique()
    
    
    
    
    
    tv <<- timevis(main,group, showZoom =  FALSE,options = list(
      editable = TRUE,stack = FALSE, showCurrentTime = FALSE,multiselect = TRUE,align = "center")) 
    tv
    
  })  
  
  observeEvent(input$fit, {
    fitWindow("contents")
  })
  
  observeEvent(input$time, {
    addCustomTime("contents", Sys.Date(), "line")
  })
  
}

# }
# Run the app ----
shinyApp(ui, server)