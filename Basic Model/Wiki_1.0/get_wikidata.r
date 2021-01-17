#Importing necessary libraries


# Extraction of data
 
# Loading required libraries (silently)

suppressPackageStartupMessages({
  library(ggplot2)
  library(magrittr)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(deSolve)
})

 

 
# Extracting data from Wikipedia

url = 'https://en.wikipedia.org/w/index.php?title=Template:2019%E2%80%9320_Wuhan_coronavirus_data/China_medical_cases_by_province&oldid=941235662';

sample1 = url %>% read_html() %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% html_table(fill=TRUE);
sample2 = url %>% read_html() %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% html_table(fill=TRUE)
sample3 = url %>% read_html() %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>% html_table(fill=TRUE)
 


# Data cleaning

 
# function to clean data and remove Wikipedia tags

data_manip <- function(data){
  for (col in colnames(data)){
    data[[col]] = data[[col]] %>% 
      gsub(pattern = ",", replacement = "") %>% 
      gsub(pattern = '\\[.*?\\]', replacement = "")
    data[[col]] = ifelse(nchar(data[[col]])==0, '0', data[[col]])
  }
  
  colnames(data) = colnames(data) %>% 
    gsub('\\[.*?\\]', '', .) %>%
    gsub(' ', '', .) %>% 
    tolower() %>% 
    gsub(':', '_', .) %>% 
    gsub(',', '_', .)
  
  colnames(data)[1] = "date";
  
  data[is.na(data)] = 0
  return(data)
}


# dropping unwanted columns from sample1 and cleaning

sample1 = sample1[,!grepl('*clinical', names(sample1))] %>% select(-contains(c('National', 'Excluding')));

sample1 = sample1[-c(36, 37, 38, 39, 40), ]
sample1 = data_manip(sample1)

colnames(sample1)[colnames(sample1)=='innermongolia'] = 'inner_mongolia'
colnames(sample1)[colnames(sample1)=='hubei_outsidewuhan'] = 'hubei_outside_wuhan'

districts = colnames(sample1)[-1]
sample1[districts] = sapply(sample1[districts], as.numeric)


 
# dropping unwanted columns from sample2 and cleaning

sample2 = sample2 %>% select(-contains(c('National', 'Clinical')))

sample2 = sample2[-c(36, 37, 38), ]
sample2 = data_manip(sample2)

colnames(sample2)[colnames(sample2)=='innermongolia'] = 'inner_mongolia'
colnames(sample2)[colnames(sample2)=='hubei_outsidewuhan'] = 'hubei_outside_wuhan'
districts = colnames(sample2)[-1]
sample2[districts] = sapply(sample2[districts], as.numeric)


 
# dropping unwanted columns from sample3 and cleaning

sample3 = sample3[-36, ]

sample3 = data_manip(sample3)
colnames(sample3)[colnames(sample3)=='innermongolia'] = 'inner_mongolia'
sample3 = sample3[,-33]

districts = colnames(sample3)[-1]
sample3[districts] = sapply(sample3[districts], as.numeric)


 
# adding suffixes to identify data related to deaths and recoveries

colnames(sample2)[-1] = paste0(colnames(sample2)[-1], '_D')
colnames(sample3)[-1] = paste0(colnames(sample3)[-1], '_R')

 
# combining all 3 data frames into one and exporting it as csv for future use

dat = merge(sample1, sample2, by='date')
dat = merge(dat, sample3, by='date')
write.csv(dat, "wiki_data.csv", row.names = F)
 



 
