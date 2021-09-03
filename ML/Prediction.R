library(ggplot2)
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(tidyverse)
library(rpart)
library(FactoMineR)
library(cattonum)
library(ggfortify)
library(ggpubr)
library(factoextra)
library(dendextend)
library(NbClust)
library(ggdendro)
library(summarytools)
library(dygraphs)
library(xts)
library(tidytext) 
library(topicmodels) 
library(tidyr) 
library(ggrepel) 
library(gridExtra)
library(formattable)
library(tm) 
library(plotly) 
library(wordcloud2)
library(cleanNLP)
library(ldatuning)
library(SnowballC)
library(data.table)
library(cleanNLP)
library(mlr)



#---------------functions--------------------------------

fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("_`", "-", doc)
  doc <- gsub("'s", "", doc)
  doc <- gsub("â", "", doc)
  doc <- gsub(":fs", "", doc)
  return(doc)
}


removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)




#-------------------------------------------------


library(readxl)
RNCDay <- read_excel("../Encms.xlsx")
RNCDay$RNC <- paste0(RNCDay$`NCR Number`, '-', RNCDay$`Discrepancy Number`)

if (format(Sys.Date(),"%A") == "Monday") {
  Fecha <- format(Sys.Date()-3,"%Y-%m-%d")
} else {
  Fecha <- as.Date(format(Sys.Date()-1,"%Y-%m-%d"))
}


RNCDay$`Discrepancy Creation Date` <- as.Date(RNCDay$`Discrepancy Creation Date`)
RNCDay <- filter(RNCDay,`Discrepancy Creation Date` == Fecha)
RNCDay$`Discrepancy Creation Date` <-as.Date(format(RNCDay$`Discrepancy Creation Date`,"%d-%m-%Y"))
RNCDay$`Discrepancy Text`[is.na(RNCDay$`Discrepancy Text`)] <- "NO TEXT"

RNCDay$`Work Center` <- substr(RNCDay$`Work Center`, 1, 6)

library(readr)
PU11WC <- read_csv("../WC.csv")
PU2WC <- read_csv("../WC_PU2.csv")


RNCPU11 <- filter(RNCDay, `Work Center` %in% PU11WC$`Work Center`)

for (i in 1:nrow(RNCPU11)){
  
  if (str_detect(RNCPU11$`Discrepancy Text`[i], 'METH|DESIGN') == TRUE){
    
    RNCPU11$`Preliminary Cause Code`[i] <- 'METHODS'
    
  }
  
  RNCPU11$Plant[i] <- "PU11" 
  
}

RNCPU11 <- inner_join(RNCPU11,PU11WC, by = 'Work Center')

RNCPU11 <- RNCPU11 %>% select(
  `Zone`,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`,
  `Discrepancy Text`
)

Real_Test <- RNCPU11 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% select(
  Zone,
  `RNC`,
  `Discrepancy Text` 
)
Real_Test <- Real_Test[-3,]
colnames(Real_Test) <- c('Zone', 'RNC', 'Disc_Text')


RNCPU2 <- filter(RNCDay, `Work Center` %in% PU2WC$`Work Center`)

for (i in 1:nrow(RNCPU2)){

  if (str_detect(RNCPU2$`Discrepancy Text`[i], 'METH|DESIGN') == TRUE){

    RNCPU2$`Preliminary Cause Code`[i] <- 'METHODS'

  }

  RNCPU2$Plant[i] <- "PU2"

}

RNCPU2 <- inner_join(RNCPU2,PU2WC, by = 'Work Center')

RNCPU2 <- RNCPU2 %>% select(
  Plant,
  `Zone`,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`,
  `Discrepancy Text`
)

Real_TestPU2 <- RNCPU2 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% select(
  Zone,
  `RNC`,
  `Discrepancy Text`
)

colnames(Real_TestPU2) <- c('Zone', 'RNC', 'Disc_Text')


#------------Group by Zones----------

RNC <- c()
RNC2 <- c()
Zones <- as.data.frame(unique(Real_Test$Zone))
ZonesPU2 <- as.data.frame(unique(Real_TestPU2$Zone))


for (i in 1:nrow(Zones)){
  
  Cur_Zone <- filter(Real_Test, Zone == Zones$`unique(Real_Test$Zone)`[i])
  
  Test <- Cur_Zone[-1]
  
  Test  <- setNames(Test, c("RNC","Discrepancy_Text"))
  
  ORNC <- Test$RNC

  
  if(Cur_Zone$Zone[1] == "Zone 1"){
    
    # --------Z1-------------
    
  top_words <- read.csv("top_words1.csv")
  Sample <-read.csv("Sample1.csv")
  IssueSt <- read_excel("Issues.xlsx",
                        sheet = "Z1")
  
  issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
  issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)

  features_func_issue <- function(data) {
    features <- data %>%
      group_by(RNC) %>%
      mutate(word_frequency = n(),
             lexical_diversity = n_distinct(word),
             lexical_density = lexical_diversity/word_frequency,
             repetition = word_frequency/lexical_diversity,
             rnc_avg_word_length = mean(nchar(word)),
             title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                 RNC)) + 1L,
             title_length = nchar(RNC),
             large_word_count =
               sum(ifelse((nchar(word) > 7), 1, 0)),
             small_word_count =
               sum(ifelse((nchar(word) < 3), 1, 0)),
             #assign more weight to these words using "20" below
             issue1_word_count =
               sum(ifelse(word %in% issue1_word$top_word,1,0)),
             issue2_word_count =
               sum(ifelse(word %in% issue2_word$top_word,1,0))
      ) %>%
      select(-word) %>%
      distinct() %>% #to obtain one record per document
      ungroup()
    
    features$Issue <- as.factor(features$Issue)
    return(features)
    
   
  }
  
  super_model <- readRDS("./final_model1.rds")
  
  } else if (Cur_Zone$Zone[1] == "Zone 2"){
    
    #------------Z2-----------
    top_words <- read.csv("top_words2.csv")
    Sample <-read.csv("Sample2.csv")
    
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z2")
    
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    # issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    # issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    # issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               # issue5_word_count =
               #   sum(ifelse(word %in% issue5_word$top_word,1,0)),
               # issue6_word_count =
               #   sum(ifelse(word %in% issue6_word$top_word,1,0)),
               # issue7_word_count =
               #   sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }

      super_model <- readRDS("./final_model2.rds")
    
  } else if (Cur_Zone$Zone == "Zone 3.1"){
    
    #------------Z3.1----------
    top_words <- read.csv("top_words31.csv")
    Sample <-read.csv("Sample31.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z31")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               issue9_word_count =
                 sum(ifelse(word %in% issue9_word$top_word,1,0)),
               issue10_word_count =
                 sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("./final_model31.rds")
    
    
  } else if (Cur_Zone$Zone[1] == "Zone 3.2"){
    
    #-----------Z32-------------
    Sample <-read.csv("Sample32.csv")
    top_words <- read.csv("top_words32.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z32")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               issue9_word_count =
                 sum(ifelse(word %in% issue9_word$top_word,1,0)),
               issue10_word_count =
                 sum(ifelse(word %in% issue10_word$top_word,1,0)),
               issue11_word_count =
                 sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("./final_model32.rds")
    
    
  } else if (Cur_Zone$Zone[1] == "Zone 3.3"){
    #-----------Z33--------------
    Sample <-read.csv("Sample33.csv")
    top_words <- read.csv("top_words33.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z33")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("./final_model33.rds")
    
  } else if (Cur_Zone$Zone[1] == "Zone 3.4"){
    
    #-----------Z34--------------
    Sample <-read.csv("Sample34.csv")
    top_words <- read.csv("top_words34.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z34")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("./final_model34.rds")
    
  } else if (Cur_Zone$Zone[1] == "Zone 4"){
    
    #-----------Z4----------------
    Sample <-read.csv("Sample4.csv")
    top_words <- read.csv("top_words4.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "Z4")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               issue9_word_count =
                 sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
   
    super_model <- readRDS("./final_model4.rds")
  } else if (Cur_Zone$Zone[1] == "Zone MTS"){
    
    #----------MTS----------------
    Sample <-read.csv("SampleMTS.csv")
    top_words <- read.csv("top_wordsMTS.csv")
    IssueSt <- read_excel("Issues.xlsx",
                          sheet = "MTS")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    # issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    # issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    # issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               # issue5_word_count =
               #   sum(ifelse(word %in% issue5_word$top_word,1,0)),
               # issue6_word_count =
               #   sum(ifelse(word %in% issue6_word$top_word,1,0)),
               # issue7_word_count =
               #   sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("./final_modelMTS.rds")
  }
  
 
  
  Test <- rbind(Sample[2:3],Test)
  SampleWR <- Test
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, fix.contractions)
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, tolower)
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, removeSpecialChars)
  
  undesirable_words <- c("methods", "attachment","attachments","meth")
  
  Test_words_filtered <- Test %>%
    unnest_tokens(word, Discrepancy_Text) %>%
    anti_join(stop_words) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    filter(nchar(word) > 3)
  
  cnlp_init_udpipe()
  
  
  
  TestFAn <- Test %>% select(RNC, Discrepancy_Text)
  Test_annotation <- cnlp_annotate(input = TestFAn, text_name = "Discrepancy_Text", doc_name = "RNC")
  
  
  Test_tidy <- Test_annotation$token %>%
    select(RNC = doc_id, word = token, lemma, upos) %>%
    filter(upos %in% c("VERB","NOUN","NUM")) %>% 
    inner_join(Test_words_filtered, by = c("word", "RNC")) %>%
    select(RNC, word = lemma) %>%
    distinct()
  
  Test_tidy$Issue <- "Unknown"
  
  Test <- features_func_issue(Test_tidy)
  
  task_test <- makeClassifTask(id = "New Data Test",
                               data = Test[-1], target = "Issue")
  
  task_test <- normalizeFeatures(task_test, method = "standardize",
                                 cols = NULL, range = c(0, 1), on.constant = "quiet")

  result_rf <- predict(super_model, task_test, predict.type = "prob")

  SampleWR$Issue <- result_rf$data$response
  
  sum <- 0
  
  for (i in 1:10){
    
    sum <- sum + result_rf$data[i,as.integer(result_rf$data$response[i])+2]
    
  }
  
  prom <- sum/10
  
  SampleWR$Issue <- as.character(SampleWR$Issue)
  for (i in 10:nrow(result_rf$data)){
    
    if(result_rf$data[i,as.integer(result_rf$data$response[i])+2] < prom){
      
      SampleWR$Issue[i] <- "One-Off"
    }
    
  }
   
  SampleWR$Issue <- as.factor(SampleWR$Issue)
  
  Result <- SampleWR %>% filter(RNC %in% ORNC)
 
  
 IssueSt$Issue <- as.factor(IssueSt$Issue)
 Result <- inner_join(Result, IssueSt, by = "Issue")
  
  RNC <- rbind(RNC, Result)
  
  
   
}

for ( i in 1:nrow(RNC)){
  
  if(RNC$Desc.[i] == "One-Off"){
    
    RNC$Repetitive[i] <- "No"
    
  } else{
   
    RNC$Repetitive[i] <- "Yes"  
  }
  
}

RNC <- RNC %>% select(RNC,Repetitive, `Desc.`, Phase, Ref, ECD)

RNC <- RNCPU11 %>% inner_join(RNC, by = 'RNC')


write.csv(RNC, ".\\SupervisedL.csv")


#-----------------FOR PU2------------------

for (i in 1:nrow(ZonesPU2)){
  
  Cur_Zone <- filter(Real_TestPU2, Zone == ZonesPU2$`unique(Real_TestPU2$Zone)`[1])
  
  Test <- Cur_Zone[-1]
  
  Test  <- setNames(Test, c("RNC","Discrepancy_Text"))
  
  ORNC <- Test$RNC
 
  if(Cur_Zone$Zone[1] == "Zone 1"){
    
    # --------Z1-------------
    
    top_words <- read.csv("../ML_PU2/top_wordsZ1.csv")
    Sample <-read.csv("../ML_PU2/SampleZ1.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    IssueSt <- read_excel("../ML_PU2/Issues_PU2.xlsx",
                          sheet = "Z1")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)

        
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
      
      
    }
    
    super_model <- readRDS("../ML_PU2/final_modelZ1.rds")
    
  } else if (Cur_Zone$Zone[1] == "Zone 2"){
    
    #------------Z2-----------
    top_words <- read.csv("../ML_PU2/top_wordsZ2.csv")
    Sample <-read.csv("../ML_PU2/SampleZ2.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    IssueSt <- read_excel("../ML_PU2/Issues_PU2.xlsx",
                          sheet = "Z2")
    
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    # issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    # issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    # issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               # issue5_word_count =
               #   sum(ifelse(word %in% issue5_word$top_word,1,0)),
               # issue6_word_count =
               #   sum(ifelse(word %in% issue6_word$top_word,1,0)),
               # issue7_word_count =
               #   sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("../ML_PU2/final_modelZ2.rds")
    
  } else if (Cur_Zone$Zone == "Zone 3"){
    
    #------------Z3----------
    top_words <- read.csv("../ML_PU2/top_wordsZ3.csv")
    Sample <-read.csv("../ML_PU2/SampleZ3.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    IssueSt <- read_excel("../ML_PU2/Issues_PU2.xlsx",
                          sheet = "Z3")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               issue9_word_count =
                 sum(ifelse(word %in% issue9_word$top_word,1,0)),
               issue10_word_count =
                 sum(ifelse(word %in% issue10_word$top_word,1,0)),
               issue11_word_count =
                 sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("../ML_PU2/final_modelZ3.rds")
    
    
  } else if (Cur_Zone$Zone[1] == "Slats"){
    
    #-----------Slats-------------
    Sample <-read.csv("../ML_PU2/SampleSlats.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    top_words <- read.csv("../ML_PU2/top_wordsSlats.csv")
    IssueSt <- read_excel("../ML_PU2/Issues_PU2.xlsx",
                          sheet = "Slats")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    # issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    # issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    # issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               # issue5_word_count =
               #   sum(ifelse(word %in% issue5_word$top_word,1,0)),
               # issue6_word_count =
               #   sum(ifelse(word %in% issue6_word$top_word,1,0)),
               # issue7_word_count =
               #   sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("../ML_PU2/final_modelSlats.rds")
    
    
  } else if (Cur_Zone$Zone[1] == "TC5-6"){
    #-----------TC5-6--------------
    Sample <-read.csv("../ML_PU2/SampleTC5-6.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    top_words <- read.csv("../ML_PU2/top_wordsTC5-6.csv")
    IssueSt <- read_excel("../ML_PU2/Issues_PU2.xlsx",
                          sheet = "ZTC5-6")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    # issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    # issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    # issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    # issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    # issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    # issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               # issue6_word_count =
               #   sum(ifelse(word %in% issue6_word$top_word,1,0)),
               # issue7_word_count =
               #   sum(ifelse(word %in% issue7_word$top_word,1,0)),
               # issue8_word_count =
               #   sum(ifelse(word %in% issue8_word$top_word,1,0)),
               # issue9_word_count =
               #   sum(ifelse(word %in% issue9_word$top_word,1,0)),
               # issue10_word_count =
               #   sum(ifelse(word %in% issue10_word$top_word,1,0)),
               # issue11_word_count =
               #   sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("../ML_PU2/final_modelTC5-6.rds")
    
  } else if (Cur_Zone$Zone[1] == "TC7-8"){
    
    #-----------TC7-8--------------
    Sample <-read.csv("../ML_PU2/SampleTC7-8.csv")
    colnames(Sample) <- c("RNC","Discrepancy_Text", "Issue")
    top_words <- read.csv("../ML_PU2/top_wordsTC7-8.csv")
    IssueSt <- read_excel("../ML_PU2/Issues.xlsx",
                          sheet = "TC7-8")
    
    issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
    issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
    issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
    issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
    issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
    issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
    issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
    issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
    issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
    issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
    issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
    issue12_word <- lapply(top_words[top_words$Issue == 12,], as.character)
    issue13_word <- lapply(top_words[top_words$Issue == 13,], as.character)
    
    features_func_issue <- function(data) {
      features <- data %>%
        group_by(RNC) %>%
        mutate(word_frequency = n(),
               lexical_diversity = n_distinct(word),
               lexical_density = lexical_diversity/word_frequency,
               repetition = word_frequency/lexical_diversity,
               rnc_avg_word_length = mean(nchar(word)),
               title_word_count = lengths(gregexpr("[A-z]\\W+",
                                                   RNC)) + 1L,
               title_length = nchar(RNC),
               large_word_count =
                 sum(ifelse((nchar(word) > 7), 1, 0)),
               small_word_count =
                 sum(ifelse((nchar(word) < 3), 1, 0)),
               #assign more weight to these words using "20" below
               issue1_word_count =
                 sum(ifelse(word %in% issue1_word$top_word,1,0)),
               issue2_word_count =
                 sum(ifelse(word %in% issue2_word$top_word,1,0)),
               issue3_word_count =
                 sum(ifelse(word %in% issue3_word$top_word,1,0)),
               issue4_word_count =
                 sum(ifelse(word %in% issue4_word$top_word,1,0)),
               issue5_word_count =
                 sum(ifelse(word %in% issue5_word$top_word,1,0)),
               issue6_word_count =
                 sum(ifelse(word %in% issue6_word$top_word,1,0)),
               issue7_word_count =
                 sum(ifelse(word %in% issue7_word$top_word,1,0)),
               issue8_word_count =
                 sum(ifelse(word %in% issue8_word$top_word,1,0)),
               issue9_word_count =
                 sum(ifelse(word %in% issue9_word$top_word,1,0)),
               issue10_word_count =
                 sum(ifelse(word %in% issue10_word$top_word,1,0)),
               issue11_word_count =
                 sum(ifelse(word %in% issue11_word$top_word,1,0)),
               issue12_word_count =
                 sum(ifelse(word %in% issue10_word$top_word,1,0)),
               issue13_word_count =
                 sum(ifelse(word %in% issue11_word$top_word,1,0))
        ) %>%
        select(-word) %>%
        distinct() %>% #to obtain one record per document
        ungroup()
      
      features$Issue <- as.factor(features$Issue)
      return(features)
    }
    
    super_model <- readRDS("../ML_PU2/final_modelTC7-8.rds")
    
  }
  
  
  
  Test <- rbind(Sample[-3],Test)
  SampleWR <- Test
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, fix.contractions)
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, tolower)
  
  Test$Discrepancy_Text <- sapply(Test$Discrepancy_Text, removeSpecialChars)
  
  undesirable_words <- c("methods", "attachment","attachments","meth")
  
  Test_words_filtered <- Test %>%
    unnest_tokens(word, Discrepancy_Text) %>%
    anti_join(stop_words) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    filter(nchar(word) > 3)
  
  cnlp_init_udpipe()
  
  
  
  TestFAn <- Test %>% select(RNC, Discrepancy_Text)
  Test_annotation <- cnlp_annotate(input = TestFAn, text_name = "Discrepancy_Text", doc_name = "RNC")
  
  
  Test_tidy <- Test_annotation$token %>%
    select(RNC = doc_id, word = token, lemma, upos) %>%
    filter(upos %in% c("VERB","NOUN","NUM")) %>% 
    inner_join(Test_words_filtered, by = c("word", "RNC")) %>%
    select(RNC, word = lemma) %>%
    distinct()
  
  Test_tidy$Issue <- "Unknown"
  
  Test <- features_func_issue(Test_tidy)
  
  task_test <- makeClassifTask(id = "New Data Test",
                               data = Test[-1], target = "Issue")
  
  task_test <- normalizeFeatures(task_test, method = "standardize",
                                 cols = NULL, range = c(0, 1), on.constant = "quiet")
  
  result_rf <- predict(super_model, task_test, predict.type = "prob")
  
  SampleWR$Issue <- result_rf$data$response
  
  sum <- 0
  
  for (i in 1:10){
    
    sum <- sum + result_rf$data[i,as.integer(result_rf$data$response[i])+2]
    
  }
  
  prom <- sum/10
  
  SampleWR$Issue <- as.character(SampleWR$Issue)
  for (i in 10:nrow(result_rf$data)){
    
    if(result_rf$data[i,as.integer(result_rf$data$response[i])+2] < prom){
      
      SampleWR$Issue[i] <- "One-Off"
    }
    
  }
  
  SampleWR$Issue <- as.factor(SampleWR$Issue)
  
  Result <- SampleWR %>% filter(RNC %in% ORNC)
  
  
  IssueSt$Issue <- as.factor(IssueSt$Issue)
  Result <- inner_join(Result, IssueSt, by = "Issue")
  
  RNC2 <- rbind(RNC2, Result)
  
  
  
}

for ( i in 1:nrow(RNC2)){
  
  if(RNC2$Desc.[i] == "One-Off"){
    
    RNC2$Repetitive[i] <- "No"
    
  } else{
    
    RNC2$Repetitive[i] <- "Yes"  
  }
  
}

RNC2 <- RNC2 %>% select(RNC,Repetitive, `Desc.`, Phase, Ref, ECD)

RNC2 <- RNCPU2 %>% inner_join(RNC2, by = 'RNC')


write.csv(RNC2, ".\\SupervisedLPU2.csv")






































