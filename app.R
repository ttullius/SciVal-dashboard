

### this app works with new format spreadsheet input files.  ###############

# Load packages ----

library(shiny)
library(shinydashboard)
#library(gridlayout)
library(tidyverse)
library(DT)
library(dplyr)
require(httr)
library(xml2)
library(XML)
library(ggplot2)
library(bslib)
require(vroom)
require(tools)
require(stats)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(thematic)

# Source helper functions ----

source("helpers.R")

my_theme <- bs_theme(bootswatch = "cerulean",
                     base_font = font_google("Open Sans"))

thematic_shiny(font = "auto")

# import location data  ----

data_locations <- read.csv("trainee_locations.csv")


# Define ui  ----

ui <- dashboardPage(
  dashboardHeader(title = "BU Bioinformatics PhD"),
  
  dashboardSidebar(
  
      fileInput("file1",
                label = h3("Choose CSV File with trainee metadata:"),
                accept = ".csv"
      ),
      
      radioButtons("group", label = h3("Group metrics data by:"),
                   choices = c(Cohort = "enter_date",
                               Gender = "gender",
                               Job = "job",
                               Fellowship = "fellowship"),
                   selected = "enter_date"
      ),
      
      tags$hr(),
      
      checkboxGroupInput("checkGroup", label = h3("Choose jobs to compare for 'years out' plots:"), 
                         choices = list("Academia" = "Academia", "Academia (non-faculty)" = "Academia (non-faculty)", "Industry" = "Industry", "Research Institute" = "Research Institute", "Government Lab" = "Government Lab", "Entrepreneur" = "Entrepreneur"), 
                         selected = "Academia"
       ),
      
      tags$hr()

    ),
    
    dashboardBody(
      
      fluidRow(
     
        h2("complete program (2000-2021)", align = "center"),
        
        infoBoxOutput("PhDs"),
        infoBoxOutput("ave_t_to_d"),
        infoBoxOutput("total_papers"),
        infoBoxOutput("papers_mean"),
       ),
      
      tags$hr(),
      
      fluidRow(
         
          infoBoxOutput("T32"),
          infoBoxOutput("IGERT"),
          infoBoxOutput("GPP"),
          infoBoxOutput("U_fellow"),
          
      ),
          #tags$hr(),
      
      fluidRow(
        
        h2("past 10 years (2011-2021)", align = "center"),
        
          infoBoxOutput("citations_mean"),
          infoBoxOutput("h_index_mean"),
          infoBoxOutput("fwci_mean"),
          
        ),
      
      tags$hr(),
    
      fluidRow(
        
      tabsetPanel(
        
        tabPanel("map",h2("Where are our PhD graduates?"),leafletOutput(outputId = "mymap")),
        
        tabPanel("jobs",h2("What are our PhD graduates doing now?"),plotOutput("plotJobsPie")),
        
        tabPanel("test", DT::dataTableOutput("testTable")),
        
        tabPanel("trainees", DT::dataTableOutput("traineesMetricsTable")),
        tabPanel("metrics", DT::dataTableOutput("metricsTable")),
        tabPanel("metrics plots", 
                
                tags$hr(),
                 plotOutput("plotPapersBox"), 
                 
                tags$hr(),
                 plotOutput("plotFWCIBox"), 
                 
                tags$hr(),
                 plotOutput("plotCitationsBox"), 
                 
                tags$hr(),
                 plotOutput("plotH_indexBox")),

        tabPanel("papers by year", 
                 
                 tags$hr(),
                 
                 plotOutput("plotPapers_all_years")),
        
        tabPanel("years out plots", 
                 
                 tags$hr(),
                 
                 plotOutput("plotYearsOutPapersAveraged"), 
                 
                 tags$hr(),
                 
                 plotOutput("plotYearsOutPapers")),
        
       ),
  )))



# Define server logic  ----

server <- function(input, output) {
  
  
# Define the color for  graduates' jobs  ----
  
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red', 'green'),
    domain = data_locations$job
  )
  
# create the map  ----
  
  output$mymap <- renderLeaflet({
    leaflet(data_locations) |> 
      setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() |> 
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(job), label = ~as.character(paste0("Job: ", sep = " ", job)), color = ~pal2(job), fillOpacity = 0.5)
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(company, sep = ", ",city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
    addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
  })
  


  #########  Use the load_file helper function (from helpers.R file) to load the user-supplied file that contains trainee metadata (Scopus ID, gender, enter date, finish date, job type. etc.)  #####
  
  Trainees <- reactive({
    
    req(input$file1)
    
    load_file(input$file1$datapath)
  }) 
  
  ###### produce reactive variables for Ifo Boxes in Dashboard.  ####
  
  trainee_support <- reactive({
    
    trainee_support <- Trainees() |> 
      group_by(fellowship) |> 
      summarise(n = n())
  })
  
  IGERT_no <- reactive({
    IGERT_no <- 
      filter(trainee_support(), fellowship == "IGERT")$n
  })
  
  T32_no <- reactive({
    T32_no <- 
      filter(trainee_support(), fellowship == "T32")$n
  })
  
  GPP_no <- reactive({
    GPP_no <- 
      filter(trainee_support(), fellowship == "GPP")$n
  })
    
  U_fellow_no <- reactive({
    U_fellow_no <- 
      filter(trainee_support(), fellowship == "University Fellow")$n
  })
  
  #metrics_mean <- metrics_df() %>% 
    #summarise(mean_number_of_citations = mean(number_of_citations), mean_FWCI = mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
    #mutate_if(is.numeric, round, digits = 0)

  
  time_to_degree_mean <- reactive({
    time_to_degree_mean <- 
      metrics_df() |> 
      summarise(mean_months = mean(months)/12) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
  citations_mean <- reactive({
    citations_mean <- metrics_df() |> 
      summarise(mean_cites = mean(number_of_citations)) |>
      mutate_if(is.numeric, round, digits = 0)
  })
  
  papers_mean <- reactive({
    all_papers <- papers_years_out_df()
    
    total_papers <- all_papers |>
      summarise(sum(papers, na.rm = TRUE))
    
    papers_mean <- total_papers/nrow(Trainees())
    
    papers_mean <- format(round(papers_mean, 1), nsmall = 1)
  })
  
  total_papers <- reactive({
    all_papers <- papers_years_out_df()
    
    total_papers <- all_papers |>
      summarise(sum(papers, na.rm = TRUE))
  })
  
  h_index_mean <- reactive({
    h_index_mean <- metrics_df() |> 
      summarise(mean_h_index = mean(H_index)) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
  fwci_mean <- reactive({
    fwci_mean <- metrics_df() |> 
      summarise(mean_fwci = mean(FWCI)) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
  ###########.   produce Info Boxes.   ####################
  
  output$PhDs <- renderInfoBox({ 
    infoBox(
      "BU Bioinformatics PhDs", paste0(num_rows()), 
      icon = icon("user-graduate"),
      color = "yellow"
    )
  })
  
  output$ave_t_to_d <- renderInfoBox({ 
    infoBox(
      "average time to degree", paste0(time_to_degree_mean(), " years"), 
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  output$total_papers <- renderInfoBox({
    infoBox(
      "total number of papers", paste0(total_papers()), 
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  
  output$papers_mean <- renderInfoBox({
    infoBox(
      "average number of papers", paste0(papers_mean()), 
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  
  output$IGERT <- renderInfoBox({
    infoBox(
      "NSF IGERT fellows", paste0(IGERT_no()), 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$T32 <- renderInfoBox({
    infoBox(
      "NIH T32 fellows", paste0(T32_no()), 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$GPP <- renderInfoBox({
    infoBox(
      "NIH GPP fellows", paste0(GPP_no()), 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$U_fellow <- renderInfoBox({
    infoBox(
      "University fellows", paste0(U_fellow_no()), 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  
  output$citations_mean <- renderInfoBox({
    infoBox(
      "average number of citations", paste0(citations_mean()), 
      icon = icon("person"),
      color = "aqua"
    )
  })
  
  output$h_index_mean <- renderInfoBox({
    infoBox(
      "average H-index", paste0(h_index_mean()), 
      icon = icon("person"),
      color = "aqua"
    )
  })
  
  output$fwci_mean <- renderInfoBox({
    infoBox(
      "average field-weighted citation impact", paste0(fwci_mean()), 
      icon = icon("person"),
      color = "aqua"
    )
  })
  
  #########################.  end produce Info Boxes.   ###################
  
  num_rows <- reactive ({
    num_rows <- nrow(Trainees())
                     })
  
  ##############  SciVal throws an error if the number of Scopus ID's in the request is >100   ###       
  ##############  Check how many trainees are listed in the trainee metadata file (nrow = number of rows in the dataframe). If >100, subset into two lists of trainees.  #########
  ##############  If number of trainees is <101, make a single list of trainees for input into code for making Scopus ID list   ####### 
  
  trainee_list <- reactive ({
    
    if(num_rows() > 100) {
      Trainees1 <- Trainees()[1:100, ]
      Trainees2 <- Trainees()[101:num_rows(), ]
      trainee_list <- list(Trainees1, Trainees2)
    }  else  {
      trainee_list <- list(Trainees())
    }
    
  })
  
  ###  construct a list of Scopus ID's from the trainee metadata file for submitting the SciVal API calls   #######
  ###  uses get_ID_list function in helpers.R
  
  ID_list <- reactive ({
    sapply(trainee_list(), get_ID_list)
    })
  
####### collect and summarise metrics for each trainee from SciVal    #####################
  
  metrics_df <- reactive ({
    
##############  produce dataframes for each SciVal metric of interest. Here are four example metrics - more can easily be added! 
##############  uses functions in helpers.R
    
    
    SciValMetric <-  "ScholarlyOutput"
    metric_name <- "number_of_papers"
    full_df_scholarly_output <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    
    
    SciValMetric <-  "FieldWeightedCitationImpact"
    metric_name <- "FWCI"
    full_df_fwci <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
    
    
    SciValMetric <-  "CitationCount"
    metric_name <- "number_of_citations"
    full_df_citation_count <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "CitationCount", metric_name = "number_of_citations")
    
    
    SciValMetric <-  "HIndices"
    metric_name <- "H_index"
    full_df_h_index <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "HIndices", metric_name = "H_index")
    
   
#######  combine the metrics dataframes, merge with the trainee metadata dataframe that was read in as a CSV, and clean up variable types  ############### 
    
    df_list <- mget(ls(pattern = "full_df"))
    
    all_df <- df_list |> 
      reduce(full_join, by = 'id')
    
    metrics_df <- Trainees() |> 
      inner_join(all_df, by = 'id')
    
    factor_list <- c('id', 'enter_date', 'finish', 'gender', 'fellowship', 'job')
    metrics_df <- metrics_df |> 
      mutate(across(factor_list, ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI', 'H_index')
    metrics_df <- metrics_df |> 
      mutate(across(double_list, ~as.double(.)))
    
    metrics_df <- metrics_df |> 
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
    
  }) 
  
  ####  sum all papers from 2000 - 2021, and make a new variable total_papers  ################
  
  all_df_papers_sum <- reactive ({
    
    metrics_df() |> 
      rowwise() |> 
      mutate(total_papers = sum(c_across(starts_with("2")), number_of_papers, na.rm=T))
    
  }) 
    
  ####  group entries by selected criterion and summarise  ################
  
    metrics_summarised <- reactive ({
      
      all_df_papers_sum() |>
        group_by(.data[[input$group]]) |> 
        summarise(number = n(), average_number_of_papers = mean(total_papers), average_number_of_citations = mean(number_of_citations), average_FWCI = mean(FWCI), average_H_index = mean(H_index)) |>
        mutate(across(c('average_FWCI'), round, 1)) |>
        mutate(across(c('average_number_of_citations', 'average_H_index','average_number_of_papers' ), round, 0))
    
  })
  

  
  ####  Call the makeSciValPapersAllYearsDF helper function to retrieve number of papers by year for each trainee   ###########
  
   papers_all_years_df <- reactive ({
    
     full_df_papers_by_year <- makeSciValPapersAllYearsDF(ID_list(), num_rows())
     
     full_df_papers_by_year <- Trainees() |> 
       inner_join(full_df_papers_by_year, by="id") |> 
       select(-name)
     
     metric_list <- c('id', 'gender', 'fellowship', 'job')
     
     full_df_papers_by_year <- full_df_papers_by_year |> 
       mutate(across(metric_list, ~as.factor(.)))
     
     full_df_papers_by_year <- full_df_papers_by_year |>
       mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
     
     as_tibble(full_df_papers_by_year)
     
   })   
   
   ####  group entries by selected criterion  ################
   
   allPapers_summarised <- reactive ({
     
     papers_all_years_df() |> 
       group_by(.data[[input$group]]) |> 
       summarise(number = n(), '2000' = sum(`2000`), '2001' = sum(`2001`), '2002' = sum(`2002`), '2003' = sum(`2003`), '2004' = sum(`2004`), '2005' = sum(`2005`), '2006' = sum(`2006`), '2007' = sum(`2007`), '2008' = sum(`2008`), '2009' = sum(`2009`), '2010' = sum(`2010`), '2011' = sum(`2011`),
       '2012' = sum(`2012`), '2013' = sum(`2013`),'2014' = sum(`2014`),'2015' = sum(`2015`),'2016' = sum(`2016`),'2017' = sum(`2017`), '2018' = sum(`2018`), '2019' = sum(`2019`), '2020' = sum(`2020`), '2021' = sum(`2021`))
   })   
   
  
  
  #################  transform "papers by year" dataset (papers_all_years_df()) into numbers of papers published each year relative to year of finishing PhD (years out)
  
  papers_years_out_df <- reactive ({
    
    tidy_df <- papers_all_years_df() |>
        pivot_longer(
        cols = starts_with("2"),
        names_to = "year",
        values_to = "papers",
        values_drop_na = TRUE)
  
  tidy_df <- transform(tidy_df, year = as.numeric(year))
  
  tidy_df <- mutate(tidy_df, years_out = year - finish)
  
  full_metric_list <- c('id', 'gender', 'fellowship', 'job')
  
  tidy_df <- tidy_df |> 
    mutate(across(full_metric_list, ~as.factor(.)))
  
  tidy_df <- tidy_df |>
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  
  as_tibble(tidy_df)
  
  })
  
  ############.  subset  papers_years_out by list of jobs ##########
  
  papers_years_out_jobs <- reactive ({
  
  job_list <- input$checkGroup
  
  papers_years_out_jobs <- filter(papers_years_out_df(), job %in% job_list)
  
  as_tibble(papers_years_out_jobs)
  
  })
  
  #############   calculate total number of papers for each value of years_out and grouping  #########
  
  papers_years_out_summarised_df <- reactive ({
  
    tidy_sum <- papers_years_out_jobs() |> 
      group_by(job, years_out) |>
      summarise(number = n(), papers = sum(papers))
 
  as_tibble(tidy_sum)
  
  })
  
  #############   calculate average number of papers for each value of years_out and grouping  #########
  
  papers_years_out_summarised_averaged_df <- reactive ({
    
    tidy_sum_avg <- mutate(papers_years_out_summarised_df(), avg_papers = papers/number)
  
  as_tibble(tidy_sum_avg)
  
  })
   
   ###############   produce tables  ####################
   
  output$testTable <- DT::renderDataTable({
    
    #papers_all_years_df()
    all_df_papers_sum()
    
  })
  
 
   output$researcherTable <- DT::renderDataTable({
    
    researcherTable <- Trainees() |>
      rename("last" = author, "SciVal ID" = id, "cohort" = enter_date) |> 
      select(-starts_with("20"), -ends_with("1"), -"zip_code")
    
 })
  
  output$metricsTable <- DT::renderDataTable({
    
    metricsTable <- 
      metrics_summarised() 
    #|> 
    #  rename("cohort" = enter_date, "number in cohort" = number, "mean # of citations" = mean_number_of_citations, "mean H-index" = mean_H_index, "mean FWCI" = mean_FWCI, "mean # of papers" = mean_number_of_papers)
    
  })
  
  
  output$traineesMetricsTable <- DT::renderDataTable({
    
    df <- metrics_df() |> 
      select(-id, -starts_with("2"), -zip_code) |>
      mutate(across(c('FWCI'), round, 1)) |>
      rename("last" = author, "cohort" = enter_date, "citations" = number_of_citations, "H-index" = H_index, "number of papers" = number_of_papers)
   
 })

 
  output$yearsOutTable <- DT::renderDataTable({
    
    papers_years_out_summarised_averaged_df() |>
      rename("years since PhD" = years_out, "number in cohort" = number, "total papers" = papers, "average # of papers" = avg_papers)
    
  })
  
  ############# make plots ##################
  
  output$plotJobsPie <- renderPlot({
    
    df_jobs <- Trainees() |> 
      group_by(job) %>%
      summarise(n = n())
    
    ggplot(df_jobs, aes(x = "", y = n, fill = job)) +
      geom_col(color = "black") +
      geom_text(aes(label = n),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer() +
      theme_void()
    
  })
  
  
  output$plotPapersBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = total_papers)) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      ylim(0, 100) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotFWCIBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = FWCI)) + 
      geom_hline(yintercept=1, color = "red") +
      geom_boxplot() +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  
  output$plotCitationsBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = number_of_citations)) + 
      geom_boxplot() +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      #ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotH_indexBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = H_index)) + 
      geom_boxplot() +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      #ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  
  output$plotPapers_all_years <- renderPlot({
    
    tidy_metrics <- gather(data = allPapers_summarised(), 
                           key = year, value = papers, -number, -.data[[input$group]])
    
    ggplot(tidy_metrics) +
      geom_line(aes(x = year, y = papers, 
                    group = .data[[input$group]], color = .data[[input$group]])) +
      theme_minimal() +
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray")) +
      ylab("number of papers") +
      theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10)) +
      
      
      ggtitle("Number of papers published each year by BU Bioinformatics alumni ", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
    
  }) 
  
  
  output$plotYearsOutPapersAveraged <- renderPlot({
    
    papers_years_out_summarised_averaged_df() |> 
      
      ggplot(aes(x = years_out, y = avg_papers,
                 group = job, color = job)) +
      geom_line()+
      xlim(-10, NA) +
      theme_minimal() +
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray")) +
      
      xlab("years since PhD") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
      ylab("average number of papers") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
      ggtitle("Average number of papers published each year since PhD", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
    
  }) 
  
  
  output$plotYearsOutPapers <- renderPlot({
    
    papers_years_out_summarised_df() |> 
    
    ggplot(aes(x = years_out, y = papers,
               group = job, color = job)) +
    geom_line()+
      xlim(-10, NA) +
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
      
    xlab("years since PhD") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
    ylab("total number of papers") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
    ggtitle("Total number of papers published each year since PhD", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
  
  }) 
  
  
}

shinyApp(ui, server)
