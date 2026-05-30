library(shiny)
library(tidyverse)
library(DT)
library(openintro)



# Load injuries dataset
dir.create("neiss")
download<-function(name) {
  url<-"https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")
injuries<-vroom::vroom("neiss/injuries.tsv.gz")

# Convert categorical variables to factors
injuries<-injuries %>%
  mutate(
    sex=as.factor(sex),
    race=as.factor(race),
    body_part=as.factor(body_part),
    diag=as.factor(diag),
    location=as.factor(location)
  )


ui<-navbarPage(
  
  title="ER Injuries Explorer",
  
  tabPanel(
    "Data Summary",
    
    sidebarLayout(
      
      sidebarPanel(
        
        # Variable selector
        selectInput(
          inputId="variable",
          label="Choose a variable:",
          choices=c(
            "age",
            "sex",
            "body_part",
            "diagnosis",
            "disposition",
            "location"
          ),
          selected="body_part"
        )
      ),
      
      mainPanel(
        
        h3("Summary Statistics"),
        
        # Numerical summary table
        tableOutput("summary_table"),
        
        br(),
        
        h3("Visualization"),
        
        # Plot output
        plotOutput("summary_plot")
      )
    )
  ),
  
  tabPanel(
    "Patient Narratives",
    
    sidebarLayout(
      
      sidebarPanel(
        
        # Body part selector
        selectInput(
          inputId="body_filter",
          label="Select body part:",
          choices=sort(unique(injuries$body_part)),
          selected="Head"
        )
      ),
      
      mainPanel(
        
        h3("Filtered Patient Narratives"),
        
        # Display narratives table
        DTOutput("narrative_table")
      )
    )
  ),
  
  
  tabPanel(
    "Extra Exploration",
    
    fluidPage(
      
      h3("Age Distribution by Sex"),
      
      plotOutput("age_plot"),
      
      br(),
      
      h3("Counts by Diagnosis"),
      
      plotOutput("diagnosis_plot")
    )
  )
)

server<-function(input, output, session) {
  
  output$summary_table<-renderTable({
    
    # Selected variable
    var<-injuries[[input$variable]]
    
    # If numeric variable
    if (is.numeric(var)) {
      
      tibble(
        Statistic=c(
          "Mean",
          "Median",
          "Standard Deviation",
          "Minimum",
          "Maximum"
        ),
        Value=c(
          mean(var, na.rm=TRUE),
          median(var, na.rm=TRUE),
          sd(var, na.rm=TRUE),
          min(var, na.rm=TRUE),
          max(var, na.rm=TRUE)
        )
      )
      
    } else {
      
      # Frequency table for categorical variables
      as.data.frame(table(var))
    }
  })
  
  output$summary_plot<-renderPlot({
    
    # Selected variable
    var_name<-input$variable
    var<-injuries[[var_name]]
    
    # Numeric variable plot
    if (is.numeric(var)) {
      
      ggplot(injuries, aes(x=.data[[var_name]])) +
        geom_histogram(
          bins=30,
          fill="steelblue",
          color="black"
        ) +
        labs(
          title=paste("Distribution of", var_name),
          x=var_name,
          y="Count"
        ) +
        theme_minimal()
      
    } else {
      
      # Categorical variable plot
      ggplot(injuries, aes(x=.data[[var_name]])) +
        geom_bar(fill = "darkorange") +
        labs(
          title=paste("Counts of", var_name),
          x=var_name,
          y="Count"
        ) +
        theme_minimal() +
        theme(
          axis.text.x=element_text(
            angle=45,
            hjust=1
          )
        )
    }
  })
  
  filtered_narratives<-reactive({
    
    injuries %>%
      filter(body_part==input$body_filter) %>%
      select(age, sex, diagnosis, disposition, narrative)
  })
  
  output$narrative_table<-renderDT({
    
    datatable(
      filtered_narratives(),
      options=list(
        pageLength=10,
        scrollX=TRUE
      )
    )
  })
  
  output$age_plot<-renderPlot({
    
    ggplot(injuries, aes(x=age, fill=sex)) +
      geom_histogram(
        bins=30,
        alpha=0.7,
        position="identity"
      ) +
      labs(
        title="Age Distribution by Sex",
        x="Age",
        y="Count"
      ) +
      theme_minimal()
  })
  
  output$diagnosis_plot<-renderPlot({
    
    injuries %>%
      count(diagnosis) %>%
      ggplot(aes(
        x=reorder(diagnosis, n),
        y=n
      )) +
      geom_col(fill="purple") +
      coord_flip() +
      labs(
        title="Number of Injuries by Diagnosis",
        x="Diagnosis",
        y="Count"
      ) +
      theme_minimal()
  })
}


shinyApp(ui = ui, server = server)
run_app<-function() {
  
  data("injuries", package="openintro")
  
  ui<-navbarPage(
    
    "ER Injuries App",
    
    tabPanel(
      "Summary",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "var",
            "Choose a variable:",
            choices=c(
              "age",
              "sex",
              "race",
              "body_part",
              "diag",
              "location"
            )
          )
        ),
        
        mainPanel(
          plotOutput("plot"),
          br(),
          tableOutput("table")
        )
      )
    ),
    
    tabPanel(
      "Narratives",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "body",
            "Choose body part:",
            choices=sort(unique(injuries$body_part))
          )
        ),
        
        mainPanel(
          DTOutput("narratives_table")
        )
      )
    )
  )
  
  server<-function(input, output, session) {
    
    output$plot<-renderPlot({
      
      x<-injuries[[input$var]]
      
      req(!is.null(x))
      
      if (is.numeric(x)) {
        
        hist(
          x,
          col="lightblue",
          main=paste("Histogram of", input$var),
          xlab=input$var
        )
        
      } else {
        
        barplot(
          table(x),
          col="orange",
          las=2,
          main=paste("Barplot of", input$var)
        )
      }
    })
    
    output$table<-renderTable({
      
      x<-injuries[[input$var]]
      
      if (is.numeric(x)) {
        
        data.frame(
          Statistic=c("Mean", "Median", "SD"),
          Value=c(
            mean(x, na.rm=TRUE),
            median(x, na.rm=TRUE),
            sd(x, na.rm=TRUE)
          )
        )
        
      } else {
        
        as.data.frame(table(x))
      }
    })
    
    filtered_data<-reactive({
      
      injuries %>%
        filter(body_part==input$body)
    })
    
    output$narratives_table<-renderDT({
      
      datatable(
        filtered_data() %>%
          select(age, sex, diag, narrative),
        options=list(pageLength=10)
      )
    })
  }
  
  shinyApp(ui, server)
}

run_app()