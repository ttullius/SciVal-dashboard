require(tidyverse)
require(dplyr)
require(httr)
require(xml2)
require(XML)
require(vroom)
require(tools)
require(stats)
require(DescTools)


############ function to read in the csv file containing trainee names, Scopus ID's,and other metadata (start, finish, gender, URM, etc.)

load_file <- function(path) {

  vroom::vroom(path, delim = ",",  show_col_types = FALSE, .name_repair = "unique_quiet")

}



#######    function to collect the start and end years for a SciVal query   ##############
#######    produces the following dataframe:
#######       lastUpdated    metricEndYear   metricStartYear    sourceName
#######    1  2023-06-07          2024            2018           Scopus
#####

get_SciValDates <- function() {
  
  headers = c(
    `Accept` = 'application/xml'
  )
  
  params = list(
    `metricTypes` = "ScholarlyOutput",
    `authors` = '6701858763,20433296900',
    `yearRange` = '10yrs',
    #`yearRange` = '5yrsAndCurrentAndFuture',
    `includeSelfCitations` = 'true',
    `byYear` = 'false',
    `includedDocs` = 'AllPublicationTypes',
    `journalImpactType` = 'CiteScore',
    `showAsFieldWeighted` = 'false',
    `indexType` = 'hIndex',
    `apiKey` = APIkey,           ### my API key
    `insttoken` = instToken           ### institutional token
    
  )
  
  url_xml <- httr::GET(url = 'https://api.elsevier.com/analytics/scival/author/metrics', httr::add_headers(.headers=headers), query = params) 
  raw_xml <- read_xml(url_xml)
  my_xml = xmlParse(raw_xml)
  
  #######. returns a dataframe with the metricEndYear and metricStartYear for the metrics collected
  
  df_Year <- xmlToDataFrame(my_xml, homogeneous = NA,
                            collectNames = FALSE, nodes = getNodeSet(my_xml, "//dataSource"))
  
}





####### function to collect a metric for each trainee, from SciVal


### use https://curlconverter.com/r/ to generate a SciVal XML call using the curl command from SciVal (e.g. https://dev.elsevier.com/scival.html#!/SciVal_Author_Lookup_API/authorMetrics)

    getSciValMetric <- function(author_ID_list, SciValMetric, metric_name, APIkey, instToken)    {
    headers = c(
      `Accept` = 'application/xml'
    )
    
    params = list(
      `metricTypes` = SciValMetric,
      `authors` = author_ID_list,
      `yearRange` = '10yrs',
      `includeSelfCitations` = 'true',
      `byYear` = 'false',
      `includedDocs` = 'AllPublicationTypes',
      `journalImpactType` = 'CiteScore',
      `showAsFieldWeighted` = 'false',
      `indexType` = 'hIndex',
      `apiKey` = APIkey,  
      `insttoken` = instToken           ### institutional token
      
    )
    
    url_xml <- httr::GET(url = 'https://api.elsevier.com/analytics/scival/author/metrics', httr::add_headers(.headers=headers), query = params) 
    raw_xml <- read_xml(url_xml)
    my_xml = xmlParse(raw_xml)
    
    df_metrics <- xmlToDataFrame(my_xml, homogeneous = NA,
                                 collectNames = FALSE, nodes = getNodeSet(my_xml, "//metrics"))
    df_id <- xmlToDataFrame(my_xml, homogeneous = NA,
                            collectNames = FALSE, nodes = getNodeSet(my_xml, "//id"))
    df_names <- xmlToDataFrame(my_xml, homogeneous = NA,
                               collectNames = FALSE, nodes = getNodeSet(my_xml, "//name"))
    
    df_SciValMetric <- bind_cols(df_names, df_id, df_metrics, .name_repair = c("unique_quiet"))
    colnames(df_SciValMetric) <- c("name", "id", "metric", metric_name)
    
    df_SciValMetric <- as_tibble(df_SciValMetric)
    
    }
    

    
##########  function to query SciVal for a metric, by Year   #############
    
    getSciValMetricAllYears <- function(author_ID_list, APIkey, instToken)    {
      
      headers = c(
        `Accept` = 'application/xml'
      )
      
      params = list(
        `metricTypes` = 'ScholarlyOutput',
        `authors` = author_ID_list,
        `yearRange` = '10yrs',
        `includeSelfCitations` = 'true',
        `byYear` = 'true',
        `includedDocs` = 'AllPublicationTypes',
        `journalImpactType` = 'CiteScore',
        `showAsFieldWeighted` = 'false',
        `indexType` = 'hIndex',
        `apiKey` = APIkey,  
        `insttoken` = instToken           ### institutional token
      )
      
      url_xml <- httr::GET(url = 'https://api.elsevier.com/analytics/scival/author/metrics', httr::add_headers(.headers=headers), query = params) 
      raw_xml <- read_xml(url_xml)
      my_xml = xmlParse(raw_xml)
      
      df_years <- xmlToDataFrame(my_xml, colClasses = c(rep("integer",10)), homogeneous = NA, 
                                 collectNames = FALSE, nodes = getNodeSet(my_xml, "//valueByYear"))
      df_id <- xmlToDataFrame(my_xml, colClasses = c("numeric"), homogeneous = NA,
                              collectNames = FALSE, nodes = getNodeSet(my_xml, "//id"))
      df_names <- xmlToDataFrame(my_xml, colClasses = c("character"), homogeneous = NA,
                                 collectNames = FALSE, nodes = getNodeSet(my_xml, "//name"))
      df_dataSource <- xmlToDataFrame(my_xml, colClasses = c("character", "integer", "integer", "character"), homogeneous = NA,
                                      collectNames = FALSE, nodes = getNodeSet(my_xml, "//dataSource"))
      
      df_allYearSciValMetric <- bind_cols(df_names, df_id, df_years, .name_repair = c("unique_quiet"))
    
      colnames(df_allYearSciValMetric, do.NULL = TRUE)
      colnames(df_allYearSciValMetric) <- c("name", "id", df_dataSource$metricStartYear:df_dataSource$metricEndYear)
      
      
      df_allYearSciValMetric <- as_tibble(df_allYearSciValMetric)
      
    }
    
    #######################   function to produce a dataframe for each SciVal metric    ###########################
    
    makeSciValMetricDF <- function(ID_list, num_rows, APIkey, instToken, SciValMetric, metric_name)    {
      
      df_metric_list <- lapply(ID_list, getSciValMetric, APIkey = APIkey, instToken = instToken, SciValMetric = SciValMetric, metric_name = metric_name)
      
      if(num_rows > 200) {
        full_df_metric <- bind_rows(df_metric_list[[1]], df_metric_list[[2]], df_metric_list[[3]]) |> 
          select(-metric, -name)
        
        } else if (num_rows %()% c(101, 200)) {
      #}  else if (between (num_rows, 101, 200)) {
          full_df_metric <- bind_rows(df_metric_list[[1]], df_metric_list[[2]]) |> 
            select(-metric, -name)
        
        }  else  {
          
        full_df_metric <- df_metric_list[[1]] |> 
          select(-metric, -name)
        }
      
      full_df_metric$id <- as.double(full_df_metric$id)
      return(full_df_metric)
      
    }
    
    #######################   function to produce a dataframe containing number of papers for each year    ###########################
    
    makeSciValPapersAllYearsDF <- function(ID_list, num_rows, APIkey, instToken)    {
      
      df_all_years_metric_list <- lapply(ID_list, getSciValMetricAllYears, APIkey = APIkey, instToken = instToken)
      
      if(num_rows > 200) {
        full_df_all_years_metric <- bind_rows(df_all_years_metric_list[[1]], df_all_years_metric_list[[2]], df_all_years_metric_list[[3]])
  
        }  else if (num_rows %()% c(101, 200)) {
      #}  else if (between (num_rows, 101, 200)) {
          full_df_all_years_metric <- bind_rows(df_all_years_metric_list[[1]], df_all_years_metric_list[[2]])
        
        }  else  {
        full_df_all_years_metric <- df_all_years_metric_list[[1]] 

        }
      
      full_df_all_years_metric$id <- as.double(full_df_all_years_metric$id)
      return(full_df_all_years_metric)
      
    }
    
    
     #######################  function to produce a list of Scopus ID's from the trainee metadata CSV that is read in      #####################################  
    
    get_ID_list <- function(trainee_list)    {
      ID_list <- trainee_list %>% pull(id) %>% paste(collapse =",")
      
    }
    
    
  