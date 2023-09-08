

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
library(viridis)
library(DescTools)


############      SOURCE HELPER FUNCTIONS   #######################

source("helpers.R")

my_theme <- bs_theme(bootswatch = "cerulean",
                     base_font = font_google("Open Sans"))

thematic_shiny(font = "auto")

# import location data  ----

data_locations <- vroom::vroom("trainee_locations.csv", delim = ",",  show_col_types = FALSE, .name_repair = "unique_quiet")

zip_codes_database <- vroom::vroom("zip_codes_states.csv", delim = ",",  show_col_types = FALSE, .name_repair = "unique_quiet")


##################   UI    ###############################

ui <- dashboardPage(
  dashboardHeader(title = "BU Bioinformatics PhD"),
  
  dashboardSidebar(
  
    textInput("APIkey",
              label = h3("Enter the SciVal API key:")
    ),  
    
    textInput("instToken",
              label = h3("Enter the SciVal token:")
    ),  
    
    fileInput("file1",
                label = h3("Choose CSV File with trainee metadata:"),
                accept = ".csv"
      ),
      
      fileInput("file2",
                label = h3("Choose CSV File with number of first author papers:"),
                accept = ".csv"
      ),
      
      fileInput("file3",
                label = h3("Choose CSV File with number of last/corresponding author papers:"),
                accept = ".csv"
      ),
      
      radioButtons("group", label = h3("Group metrics data by:"),
                   choices = c(Cohort = "enter_date",
                               Mentor = "mentor",
                               Campus = "campus",
                               Gender = "gender",
                               Job = "job",
                               Fellowship = "fellowship",
                               URM = "urm"),
                   selected = "enter_date"
      ),
      
      tags$hr(),
      
      checkboxGroupInput("checkGroup", label = h3("Choose jobs to compare for 'years out' plots:"), 
                         choices = list("Academia" = "Academia", 
                                        "Academia (non-faculty)" = "Academia (non-faculty)", 
                                        "Industry" = "Industry", 
                                        "Research Institute" = "Research Institute", 
                                        "Government Lab" = "Government Lab", 
                                        "Entrepreneur" = "Entrepreneur"), 
                         selected = "Academia"
       ),
      
      tags$hr()

    ),
    
    dashboardBody(
      
      fluidRow(
     
        h2("complete program (2000-2023)", align = "center"),
        
        infoBoxOutput("PhDs"),
        infoBoxOutput("total_papers"),
        infoBoxOutput("program_h_index"),
        
      ),
      
      tags$hr(),
      
      fluidRow(
        
        infoBoxOutput("ave_t_to_d"),
        infoBoxOutput("papers_mean"),
        infoBoxOutput("h_index_mean"),
        infoBoxOutput("first_author_papers_mean"),
        
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
        
        h2("past 10 years (2013-2022)", align = "center"),
        
          infoBoxOutput("citations_mean"),
          infoBoxOutput("fwci_mean"),
          
        ),
      
      tags$hr(),
    
      fluidRow(
        
      tabsetPanel(
        
        tabPanel("map",h2("Where are our PhD graduates?"),leafletOutput(outputId = "mymap")),
        
        tabPanel("jobs",h2("What are our PhD graduates doing now?"),plotOutput("plotJobsPie")),
        
        #tabPanel("test", DT::dataTableOutput("testTable")),
        
        #tabPanel("test2", DT::dataTableOutput("testTable2")),
        
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
                 plotOutput("plotH_indexBox"),
        
                tags$hr(),
                plotOutput("plotTime_to_degree_Box")),
      

        tabPanel("papers by year", 
                 
                 tags$hr(),
                 
                 plotOutput("plotPapers_all_years")),
        
        tabPanel("years out plots", 
                 
                 tags$hr(),
                 
                 plotOutput("plotYearsOutPapersAveraged"), 
                 
                 tags$hr(),
                 
                 plotOutput("plotYearsOutPapers")),
        
        
        tabPanel("first author papers", 
                 
                 tags$hr(),
                 
                 plotOutput("plotFirstAuthorAveraged"), 
                 
                 tags$hr(),
                 
                 plotOutput("plotFirstAuthor")),
        
        tabPanel("last/corresponding author papers", 
                 
                 tags$hr(),
                 
                 plotOutput("plotLastCorrespAuthorAveraged"), 
                 
                 tags$hr(),
                 
                 plotOutput("plotLastCorrespAuthor")),
        
       ),
  )))



##################          SERVER     ####################################


server <- function(input, output) {
  


  #########  Use the load_file helper function (from helpers.R file) to load user-supplied files. ##################
  
  #########  One file contains trainee metadata (Scopus ID, gender, enter date, finish date, job type., etc.).   ##########
  
  #########  Additional files can be loaded that contain first-author paper data, and last/corresponding author paper data.   ########## 
  
  
  Trainees <- reactive({
    
    req(input$file1)
    
    load_file(input$file1$datapath)
  }) 
  
  
  FirstAuthor <- reactive({
    
    req(input$file2)
    
    load_file(input$file2$datapath)
  }) 
  
  
  LastCorrespAuthor <- reactive({
    
    req(input$file3)
    
    load_file(input$file3$datapath)
  }) 
  
  
  ######      produce reactive variable for plotting trainee job locations  ####
  
  job_locations <- reactive({
    
    inner_join(Trainees(), zip_codes_database, by = "zip_code")
    
  })
  
  
  
  ######      produce reactive variables for Info Boxes in Dashboard.  ####
  
  total_papers <- reactive({
    all_papers <- papers_years_out_df()
    
    total_papers <- all_papers |>
      summarise(sum(papers, na.rm = TRUE))
  })
  
  
   time_to_degree_mean <- reactive({
    time_to_degree_mean <- metrics_df() |> 
      summarise(mean_months = mean(months)/12) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
   
  papers_mean <- reactive({
    all_papers <- papers_years_out_df()
    
    total_papers <- all_papers |>
      summarise(sum(papers, na.rm = TRUE))
    
    papers_mean <- total_papers/nrow(Trainees())
    
    papers_mean <- format(round(papers_mean, 1), nsmall = 1)
  })
  
  
  h_index_mean <- reactive({
    h_index_mean <- metrics_df() |> 
      summarise(mean_h_index = mean(H_index)) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
  
  first_author_papers_mean <- reactive({
    all_first_author_papers <- first_author_years_out_df()
    
    total_first_author_papers <- all_first_author_papers |>
      summarise(sum(first_author_papers, na.rm = TRUE))
    
    first_author_papers_mean <- total_first_author_papers/nrow(Trainees())
    
    first_author_papers_mean <- format(round(first_author_papers_mean, 1), nsmall = 1)
  })
  
  
  trainee_support <- reactive({
    
    trainee_support <- Trainees() |> 
      group_by(fellowship) |> 
      summarise(n = n())
  })
  
  T32_no <- reactive({
    T32_no <- 
      filter(trainee_support(), fellowship == "T32")$n
  })
  
  
  IGERT_no <- reactive({
    IGERT_no <- 
      filter(trainee_support(), fellowship == "IGERT")$n
  })
  
  
  GPP_no <- reactive({
    GPP_no <- 
      filter(trainee_support(), fellowship == "GPP")$n
  })
    
  
  U_fellow_no <- reactive({
    U_fellow_no <- 
      filter(trainee_support(), fellowship == "University Fellow")$n
  })
  
  
  citations_mean <- reactive({
    citations_mean <- metrics_df() |> 
      summarise(mean_cites = mean(number_of_citations)) |>
      mutate_if(is.numeric, round, digits = 0)
  })
  
 
  fwci_mean <- reactive({
    fwci_mean <- metrics_df() |> 
      summarise(mean_fwci = mean(FWCI)) |>
      mutate_if(is.numeric, round, digits = 1)
  })
  
  
  ###########   END produce reactive variables for Info Boxes in Dashboard  ####
  
  
  ###########.   produce Info Boxes   ####################
  
  output$PhDs <- renderInfoBox({ 
    infoBox(
      "BU Bioinformatics PhDs", paste0(num_rows()), 
      icon = icon("user-graduate"),
      color = "red"
    )
  })
  
  
  output$total_papers <- renderInfoBox({
    infoBox(
      "total number of papers", paste0(total_papers()), 
      icon = icon("users"),
      color = "red"
    )
  })
  
  
  output$program_h_index <- renderInfoBox({
    infoBox(
      "BU Bioinformatics Program H-index", paste0(358), 
      icon = icon("users"),
      color = "red"
    )
  })
  
  
  output$ave_t_to_d <- renderInfoBox({ 
    infoBox(
      "average trainee time to degree", paste0(time_to_degree_mean(), " years"), 
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  
  output$papers_mean <- renderInfoBox({
    infoBox(
      "average number of papers per trainee", paste0(papers_mean()), 
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  
 
  output$h_index_mean <- renderInfoBox({
    infoBox(
      "average H-index per trainee", paste0(h_index_mean()), 
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  
  output$first_author_papers_mean <- renderInfoBox({
    infoBox(
      "average number of first-author papers per trainee", paste0(first_author_papers_mean()), 
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
      "average number of citations per trainee", paste0(citations_mean()), 
      icon = icon("person"),
      color = "aqua"
    )
  })
  
  output$fwci_mean <- renderInfoBox({
    infoBox(
      "average field-weighted citation impact per trainee", paste0(fwci_mean()), 
      icon = icon("person"),
      color = "aqua"
    )
  })
  
  #########################  END produce Info Boxes   ################
  
  
  
  num_rows <- reactive ({
    
    num_rows <- nrow(Trainees())
    
                     })
  
  
  APIkey <- reactive ({
    
    req(input$APIkey)
    APIkey <- input$APIkey
    
  })
  
  instToken <- reactive ({
    
    req(input$instToken)
    instToken <- input$instToken
    
  })
  
  
  ##############  SciVal throws an error if the number of Scopus ID's in the request is >100   ###       
  ##############  Check how many trainees are listed in the trainee metadata file (nrow = number of rows in the dataframe). 
  ##############  If >200, subset into three groups of trainees.  #########
  ##############  If >101 and <200, subset into two groups of trainees.  #########
  ##############  If <101, make a single group of trainees
  ##############  Combine the trainee group(s) into a list, trainee_list(), which is the input to code for making Scopus ID list   ####### 
  
  trainee_list <- reactive ({
    
    if (num_rows() > 200) {
      
      Trainees1 <- Trainees()[1:100, ]
      Trainees2 <- Trainees()[101:200, ]
      Trainees3 <- Trainees()[201:num_rows(), ]
      trainee_list <- list(Trainees1, Trainees2, Trainees3)
      
    #}  else if (num_rows() %()% c(101, 200)) {
    }  else if (between (num_rows(), 101, 200)) {
      
      Trainees1 <- Trainees()[1:100, ]
      Trainees2 <- Trainees()[101:num_rows(), ]
      trainee_list <- list(Trainees1, Trainees2)
      
    }  else  {
      
      trainee_list <- list(Trainees())
    }
    
  })
  
  ###  from the trainee metadata file, construct a list of Scopus ID's for submitting SciVal API calls   #######
  ###  uses get_ID_list function in helpers.R    
  
  ID_list <- reactive ({
    
    sapply(trainee_list(), get_ID_list)
    
    })
  
####### collect and summarise metrics for each trainee from SciVal    #####################
  
  metrics_df <- reactive ({
    
##############  produce dataframes for each SciVal metric of interest. 
##############  Here are four example metrics - more can easily be added! 
##############  uses functions in helpers.R
    
    
    SciValMetric <-  "ScholarlyOutput"
    metric_name <- "number_of_papers"
    #full_df_scholarly_output <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    full_df_scholarly_output <- makeSciValMetricDF(ID_list(), num_rows(), APIkey = APIkey(), instToken = instToken(), SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    
    
    SciValMetric <-  "FieldWeightedCitationImpact"
    metric_name <- "FWCI"
    #full_df_fwci <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
    full_df_fwci <- makeSciValMetricDF(ID_list(), num_rows(), APIkey = APIkey(), instToken = instToken(), SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
    
    
    SciValMetric <-  "CitationCount"
    metric_name <- "number_of_citations"
    #full_df_citation_count <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "CitationCount", metric_name = "number_of_citations")
    full_df_citation_count <- makeSciValMetricDF(ID_list(), num_rows(), APIkey = APIkey(), instToken = instToken(), SciValMetric = "CitationCount", metric_name = "number_of_citations")
    
    
    SciValMetric <-  "HIndices"
    metric_name <- "H_index"
    #full_df_h_index <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "HIndices", metric_name = "H_index")
    full_df_h_index <- makeSciValMetricDF(ID_list(), num_rows(), APIkey = APIkey(), instToken = instToken(), SciValMetric = "HIndices", metric_name = "H_index")
    
   
#######  combine the metrics dataframes, merge with the trainee metadata dataframe that was read in as a CSV, and clean up variable types  ############### 
    
    df_list <- mget(ls(pattern = "full_df"))
    
    all_df <- df_list |> 
      reduce(full_join, by = 'id')
    
    metrics_df <- Trainees() |> 
      inner_join(all_df, by = 'id')
    
    factor_list <- c('id', 'enter_date', 'finish', 'gender', 'fellowship', 'job')
    metrics_df <- metrics_df |> 
      mutate(across(all_of(factor_list), ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI', 'H_index')
    metrics_df <- metrics_df |> 
      mutate(across(all_of(double_list), ~as.double(.)))
    
    metrics_df <- metrics_df |>
    mutate(time_to_degree = months/12)
    
    metrics_df <- metrics_df |> 
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
    
  }) 
  
  ####  sum all papers from over the years that were obtained from SciVal, and make a new variable total_papers  ################
  
  all_df_papers_sum <- reactive ({
    
    metrics_df() |> 
      rowwise() |> 
      mutate(total_papers = sum(c_across(starts_with("2")), number_of_papers, na.rm=T))
    
  }) 
    
  ####  group entries by selected criterion and summarise  ################
  
    metrics_summarised <- reactive ({
      
      all_df_papers_sum() |>
        group_by(.data[[input$group]]) |> 
        summarise(number = n(), average_years_to_degree = mean(time_to_degree), average_number_of_papers = mean(total_papers), average_number_of_citations = mean(number_of_citations), average_FWCI = mean(FWCI), average_H_index = mean(H_index)) |>
        mutate(across(c('average_FWCI', 'average_years_to_degree'), ~round(.x, 1))) |>
        mutate(across(c('average_number_of_citations', 'average_H_index','average_number_of_papers' ), ~round(.x, 0)))
    
  })
  

  
  ####            Call the makeSciValPapersAllYearsDF helper function to retrieve number of papers by year for each trainee   ###########
  ##############  uses functions in helpers.R
  
   papers_all_years_df <- reactive ({
     
     papers_all_years_df <- makeSciValPapersAllYearsDF(ID_list(), num_rows(), APIkey(), instToken())
     
     papers_all_years_df <- Trainees() |> 
       inner_join(papers_all_years_df, by="id") |> 
       select(-name)
     
     metric_list <- c('id', 'gender', 'fellowship', 'job')
     
     papers_all_years_df <- papers_all_years_df |> 
       mutate(across(all_of(metric_list), ~as.factor(.))) |>
       mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
     
   })   
  
  
  allPapers <- reactive ({
    
    papers_all_years_df() |> 
       summarise(number = n(), across(starts_with("2"), sum))
    
  })   
  
  
  tidy_allPapers <-  reactive ({
    
    gather(data = allPapers(), key = year, value = papers, -number)
    
  })
  
 
   ####  group entries by selected criterion  ################
   
   allPapers_summarised <- reactive ({
     
     papers_all_years_df() |> 
       group_by(.data[[input$group]]) |> 
          summarise(number = n(), across(starts_with("2"), sum))
     
     })   
   
  tidy_allPapers_summarised <-  reactive ({  
    
    gather(data = allPapers_summarised(), 
           key = year, value = papers, -number, -.data[[input$group]])
    
  })
  
  #################  transform "papers by year" dataset, papers_all_years_df(), into numbers of papers published each year relative to year of finishing PhD (years out)
  
  papers_years_out_df <- reactive ({
    
    papers_years_out_df <- papers_all_years_df() |>
        pivot_longer(
        cols = starts_with("2"),
        names_to = "year",
        values_to = "papers",
        values_drop_na = TRUE)
  
    papers_years_out_df <- transform(papers_years_out_df, year = as.numeric(year))
  
    papers_years_out_df <- mutate(papers_years_out_df, years_out = year - finish)
  
    full_metric_list <- c('id', 'gender', 'fellowship', 'job')
  
    papers_years_out_df <- papers_years_out_df |> 
      mutate(across(all_of(full_metric_list), ~as.factor(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  
  })
  
  
  papers_years_out_all_trainees <- reactive ({
    
    papers_years_out_df() |> group_by(years_out) |>
    summarise(number = n(), papers = sum(papers)) |>
    mutate(avg_papers = papers/number)
  
  })
  
  
  ############.  subset  papers_years_out by list of jobs ##########
    
  papers_years_out_summarised_df <- reactive ({
  
    job_list <- input$checkGroup
  
    papers_years_out_summarised_df <- filter(papers_years_out_df(), job %in% job_list) |>
      group_by(job, years_out) |>
      summarise(number = n(), papers = sum(papers)) |>
      mutate(avg_papers = papers/number)
  
  })
  

  
  
  ################  first author code ################

  
  ####  sum all first author papers over the range of years obtained from SciVal and make a new variable total_first_author  ################
  
  factor_list_fa <- c('id', 'gender', 'fellowship', 'job')
  double_list_fa <- c('total_first_author', 'enter_date', 'finish')
  
  first_author_total <- reactive ({
    
    FirstAuthor() |> 
      rowwise() |> 
      mutate(total_first_author = sum(c_across(starts_with("2")), na.rm = T)) |>
      mutate(across(all_of(factor_list_fa), ~as.factor(.))) |>
      #mutate(all_of(double_list_fa, ~as.double(.)))
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  }) 
  
  
  ####  group entries by selected criterion  ################
  
  first_author_summarised <- reactive ({
    
    first_author_total() |> 
      group_by(.data[[input$group]]) |> 
         summarise(number = n(), across(starts_with("2"), sum))
    
    })   
  
  
  
  #################  transform "first authors" dataset, first_author_total(), into numbers of first-author papers published each year relative to year of finishing PhD (years out)
  
  first_author_years_out_df <- reactive ({
    
    first_author_years_out_df <- first_author_total() |>
      pivot_longer(
        cols = starts_with("2"),
        names_to = "year",
        values_to = "first_author_papers",
        values_drop_na = TRUE)
    
    first_author_years_out_df <- transform(first_author_years_out_df, year = as.numeric(year))
    first_author_years_out_df <- mutate(first_author_years_out_df, years_out = year - finish)
    
    full_metric_list_first_author <- c('id', 'gender', 'fellowship', 'job')
    
    first_author_years_out_df <- first_author_years_out_df |> 
      mutate(across(all_of(full_metric_list_first_author), ~as.factor(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  })
  
  
  ############### first author papers for all trainees ################
  
  first_author_years_out_all_trainees <- reactive ({
    
    first_author_years_out_df() |> group_by(years_out) |>
      summarise(number = n(), first_author_papers = sum(first_author_papers)) |>
      mutate(avg_first_author_papers = first_author_papers/number)
    
  })
  
  
  ############.  subset first author papers by list of jobs ##########
  
  first_author_years_out_summarised_df <- reactive ({
    
    job_list <- input$checkGroup
    
    first_author_years_out_summarised_df <- filter(first_author_years_out_df(), job %in% job_list) |>
      group_by(job, years_out) |>
      summarise(number = n(), first_author_papers = sum(first_author_papers))|>
      mutate(avg_first_author_papers = first_author_papers/number)
    
  })
  

  
  ################  last/corresponding author code    ################
  
  
  ####  sum all last/corresponding author papers over the range of years obtained from SciVal and make a new variable total_last_corresp_author  ################
  
  factor_list_fa <- c('id', 'gender', 'fellowship', 'job')
  double_list <- c('total_last_corresp_author', 'enter_date', 'finish')
  
  last_corresp_author_total <- reactive ({
    
    LastCorrespAuthor() |> 
      rowwise() |> 
      mutate(total_last_corresp_author = sum(c_across(starts_with("2")), na.rm = T)) |> 
      mutate(across(all_of(factor_list_fa), ~as.factor(.))) |>
      mutate(across(all_of(double_list), ~as.double(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  }) 
  
  
  ####  group entries by selected criterion  ################
  
  last_corresp_author_summarised <- reactive ({
    
    last_corresp_author_total() |> 
      group_by(.data[[input$group]]) |> 
         summarise(number = n(), across(starts_with("2"), sum))
    
    })   
  
  
  #################  transform "last/corresponding authors" dataset, last_corresp_author_total(), into numbers of last/corresponding-author papers published each year relative to year of finishing PhD (years out)
  
  last_corresp_author_years_out_df <- reactive ({
    
    last_corresp_author_years_out_df <- last_corresp_author_total() |>
      pivot_longer(
        cols = starts_with("2"),
        names_to = "year",
        values_to = "last_corresp_author_papers",
        values_drop_na = TRUE)
    
    last_corresp_author_years_out_df <- transform(last_corresp_author_years_out_df, year = as.numeric(year))
    
    last_corresp_author_years_out_df <- mutate(last_corresp_author_years_out_df, years_out = year - finish)
    
    full_metric_list_last_corresp_author <- c('id', 'gender', 'fellowship', 'job')
    
    last_corresp_author_years_out_df <- last_corresp_author_years_out_df |> 
      mutate(across(all_of(full_metric_list_last_corresp_author), ~as.factor(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  })
  
  
  ############### last/corresponding author papers for all trainees   ################
  
  last_corresp_author_years_out_all_trainees <- reactive ({
    
    last_corresp_author_years_out_df() |> group_by(years_out) |>
      summarise(number = n(), last_corresp_author_papers = sum(last_corresp_author_papers)) |>
      mutate(avg_last_corresp_author_papers = last_corresp_author_papers/number)
    
  })
  
  
  ############  subset last/corresponding author papers by list of jobs     ##########
  
  last_corresp_author_years_out_summarised_df <- reactive ({
    
    job_list <- input$checkGroup
    
    last_corresp_author_years_out_summarised_df <- filter(last_corresp_author_years_out_df(), job %in% job_list) |>
      group_by(job, years_out) |>
      summarise(number = n(), last_corresp_author_papers = sum(last_corresp_author_papers)) |>
      mutate(avg_last_corresp_author_papers = last_corresp_author_papers/number)
    
  })
  

  
  ##############################          OUTPUT         ######################################
  

    ###############   produce the map  ####################
  
  # Define the color for  graduates' jobs  ----
  
 
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'orange', 'green', 'red', 'pink','violet'),
    domain = data_locations$job
  )
  
  # create the map  ----
  
  output$mymap <- renderLeaflet({
    
    #leaflet(data_locations) |>
      
  leaflet(job_locations()) |>
      setView(lng = -99, lat = 45, zoom = 2)  |>      #setting the view over ~ center of North America
      addTiles() |> 
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(job), label = ~as.character(paste0("Job: ", sep = " ", job)), color = ~pal2(job), fillOpacity = 0.5)
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(company, sep = ", ",city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
      addCircles(data = job_locations(), lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste(first_name, author, ',', employer, ',', city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 1.0) |>
      addLegend("topright", pal = pal2, values = ~job,
                title = "Trainee Job",
                opacity = 1)
  })
  
  
   ###############   produce tables  ####################
   
  output$testTable <- DT::renderDataTable({
    
    #papers_all_years_df()
    #first_author_years_out_df()
    #first_author_total()
    #first_author_summarised()
    #FirstAuthor()
    #allPapers_summarised()
    #last_corresp_author_years_out_summarised_df()
    #papers_mean()
    #papers_years_out_df()
   #job_locations()
    #last_corresp_author_total()
    #tidy_allPapers()
    #allPapers()
    
    
  })
  
  output$testTable2 <- DT::renderDataTable({
    
    
    #papers_years_out_summarised_df()
    #papers_all_years_df()
    #allPapers()
    #last_corresp_author_total()
    #first_author_total()
    #first_author_papers_mean()
    #first_author_years_out_df()
    #last_corresp_author_summarised()
    
    
  })
  
 
  output$traineesMetricsTable <- DT::renderDataTable({
    
    df <- metrics_df() |> 
      select(-id, -first, -starts_with("2"), -zip_code, -additional_position) |>
      #select(-id, -starts_with("2"), -zip_code, -additional_position) |>
      mutate(across(c('FWCI', 'time_to_degree'), ~round(.x, 1))) |>
      rename("first" = first_name, "last" = author, "cohort" = enter_date, "citations" = number_of_citations, "H-index" = H_index, "number of papers" = number_of_papers)
   
 })

  
  output$metricsTable <- DT::renderDataTable({
    
    metricsTable <- 
      metrics_summarised()
   
  })
  
  
  
  #############    make box plots of metric data     ##################
  
  
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
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      ylim(0, 100) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotFWCIBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = FWCI)) + 
      geom_hline(yintercept = 1, color = "red") +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotCitationsBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = number_of_citations)) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      ylim(0, 15000) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotH_indexBox <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = H_index)) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      #ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  output$plotTime_to_degree_Box <- renderPlot({
    
    all_df_papers_sum() |>
      
      ggplot(aes(x = .data[[input$group]], y = time_to_degree)) + 
      geom_hline(yintercept = 5, color = "red") +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      geom_jitter(shape=16, position=position_jitter(0.2)) +
      #ylim(0, 15) + 
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
  }) 
  
  
  
  #############     make plots of papers data per year     ##################
  
  
  output$plotPapers_all_years <- renderPlot({
    
    ggplot() +
      
      geom_line(data = tidy_allPapers(), aes(x = year, y = papers, group = 1), linewidth = 1.2) +
      
      geom_line(data = tidy_allPapers_summarised(), aes(x = year, y = papers,
                                                    group = .data[[input$group]], color = .data[[input$group]]), size = 1.2) +
      theme_minimal() +
      
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray")) +
      
      ylab("number of papers") +
      theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10)) +
      
      ggtitle("Number of papers published each year by BU Bioinformatics alumni - black line, all trainees", 
      ) + 
      
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
    
  }) 
  
  
  
  #############       make plots of papers per year since PhD       ##################
  
  
  output$plotYearsOutPapersAveraged <- renderPlot({
      
      ggplot() +
      
        geom_line(data = papers_years_out_all_trainees(), aes(x = years_out, y = avg_papers), linewidth = 1.2) +
      
        geom_line(data = papers_years_out_summarised_df(), aes(x = years_out, y = avg_papers,
          group = job, color = job), linewidth = 1.2) +
      
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
      
      ggtitle("Average number of papers published each year since PhD - black line, all trainees", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
    

  }) 
  
  
  output$plotYearsOutPapers <- renderPlot({
      
    ggplot() +
      
      geom_line(data = papers_years_out_all_trainees(), aes(x = years_out, y = papers), linewidth = 1.2) +
      
      geom_line(data = papers_years_out_summarised_df(), aes(x = years_out, y = papers,
                                                             group = job, color = job), linewidth = 1.2) +
      
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
      
    ggtitle("Total number of papers published each year since PhD - black line, all trainees", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
  
  }) 
  

  
  #############      make plots of first-author papers per year since PhD       ##################
  
  
  output$plotFirstAuthorAveraged  <- renderPlot({
      
      ggplot() +
      
      geom_line(data = first_author_years_out_all_trainees(), aes(x = years_out, y = avg_first_author_papers), linewidth = 1.2) +
      
      geom_line(data = first_author_years_out_summarised_df(), aes(x = years_out, y = avg_first_author_papers,
                                                             group = job, color = job), linewidth = 1.2) +
      
      xlim(-10, NA) +
      
      theme_minimal() +
      
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray")) +
      
      xlab("years since PhD") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
      ylab("first author papers") +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
      
      ggtitle("average number of first author papers published each year since PhD - black line, all trainees", 
      ) + 
      theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
    
  }) 
  
output$plotFirstAuthor  <- renderPlot({
  
    ggplot() +
    
    geom_line(data = first_author_years_out_all_trainees(), aes(x = years_out, y = first_author_papers), linewidth = 1.2) +
    
    geom_line(data = first_author_years_out_summarised_df(), aes(x = years_out, y = first_author_papers,
                                                                 group = job, color = job), linewidth = 1.2) +
    
    xlim(-10, NA) +
    
    theme_minimal() +
    
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    
    xlab("years since PhD") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ylab("first author papers") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ggtitle("total number of first author papers published each year since PhD - black line, all trainees", 
    ) + 
    theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
  
}) 



#############      make plots of last/corresponding author papers per year since PhD        ##################


output$plotLastCorrespAuthorAveraged  <- renderPlot({
  
  ggplot() +
    
    geom_line(data = last_corresp_author_years_out_all_trainees(), aes(x = years_out, y = avg_last_corresp_author_papers), linewidth = 1.2) +
    
    geom_line(data = last_corresp_author_years_out_summarised_df(), aes(x = years_out, y = avg_last_corresp_author_papers,
                                                                 group = job, color = job), linewidth = 1.2) +
    
    xlim(-10, NA) +
    
    theme_minimal() +
    
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    
    xlab("years since PhD") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ylab("last/corrrsponding author papers") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ggtitle("average number of last/corresponding author papers published each year since PhD - black line, all trainees", 
    ) + 
    theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
  
}) 

output$plotLastCorrespAuthor  <- renderPlot({
  
  ggplot() +
    
    geom_line(data = last_corresp_author_years_out_all_trainees(), aes(x = years_out, y = last_corresp_author_papers), linewidth = 1.2) +
    
    geom_line(data = last_corresp_author_years_out_summarised_df(), aes(x = years_out, y = last_corresp_author_papers,
                                                                 group = job, color = job), linewidth = 1.2) +
    
    xlim(-10, NA) +
    
    theme_minimal() +
    
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    
    xlab("years since PhD") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ylab("last/correspoonding author papers") +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    
    ggtitle("total number of last/corresponding author papers published each year since PhD - black line, all trainees", 
    ) + 
    theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(size = 15, hjust = 0.5))
  
}) 
}


shinyApp(ui, server)
