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

# -----------------------Required Functions-----------------------

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "1bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}
#----------------------------------------------------------------------

OText <- read.csv('Topics1.csv')

Text <- OText 




# Text Mining

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

Text$Discrepancy_Text <- sapply(Text$Discrepancy_Text, fix.contractions)

Text$Discrepancy_Text <- sapply(Text$Discrepancy_Text, tolower)

Text$Discrepancy_Text <- sapply(Text$Discrepancy_Text, removeSpecialChars)

undesirable_words <- c("methods", "attachment","attachments","meth")

Text_words_filtered <- Text %>%
  unnest_tokens(word, Discrepancy_Text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

cnlp_init_udpipe()

TextFAn <- Text %>% select(RNC, Discrepancy_Text)

Text_annotation <- cnlp_annotate(input = TextFAn, text_name = "Discrepancy_Text", doc_name = "RNC")


Text_tidy <- Text_annotation$token %>%
  select(RNC = doc_id, word = token, lemma, upos) %>%
  filter(upos %in% c("VERB","NOUN","NUM")) %>% 
  inner_join(Text_words_filtered, by = c("word", "RNC")) %>%
  select(RNC, word = lemma, Issue) %>% inner_join(Text, by = RNC) %>% select(RNC, Part, Part_Desc, word)
  distinct() 

number_of_words = 5500

top_words_per_issue <- Text_tidy %>%
  group_by(Issue) %>%
  mutate(issue_word_count = n()) %>%
  group_by(Issue, word) %>%
  #note that the percentage is also collected, when really you
  #could have just used the count, but it's good practice to use a %
  mutate(word_count = n(),
         word_pct = word_count / issue_word_count * 100) %>%
  select(word, Issue, issue_word_count, word_count, word_pct) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(word_pct)) %>%
  top_n(number_of_words) %>%
  select(Issue, word, word_pct)


top_words <- top_words_per_issue %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(multi_genre = n()) %>%
  filter(multi_genre < 2) %>%
  select(Issue, top_word = word)  

write.csv(top_words, "top_wordsTC7-8.csv")
n_distinct(Text$Issue)

issue1_word <- lapply(top_words[top_words$Issue == 1,], as.character)
issue2_word <- lapply(top_words[top_words$Issue == 2,], as.character)
# issue3_word <- lapply(top_words[top_words$Issue == 3,], as.character)
# issue4_word <- lapply(top_words[top_words$Issue == 4,], as.character)
# issue5_word <- lapply(top_words[top_words$Issue == 5,], as.character)
# issue6_word <- lapply(top_words[top_words$Issue == 6,], as.character)
# issue7_word <- lapply(top_words[top_words$Issue == 7,], as.character)
# issue8_word <- lapply(top_words[top_words$Issue == 8,], as.character)
# issue9_word <- lapply(top_words[top_words$Issue == 9,], as.character)
# issue10_word <- lapply(top_words[top_words$Issue == 10,], as.character)
# issue11_word <- lapply(top_words[top_words$Issue == 11,], as.character)
# issue12_word <- lapply(top_words[top_words$Issue == 12,], as.character)
# issue13_word <- lapply(top_words[top_words$Issue == 13,], as.character)

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
           # issue3_word_count =
           #   sum(ifelse(word %in% issue3_word$top_word,1,0)),
           # issue4_word_count =
           #   sum(ifelse(word %in% issue4_word$top_word,1,0)),
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
           #   sum(ifelse(word %in% issue11_word$top_word,1,0)),
           # issue12_word_count =
           #   sum(ifelse(word %in% issue12_word$top_word,1,0)),
           # issue13_word_count =
           #   sum(ifelse(word %in% issue13_word$top_word,1,0)),
           
    ) %>%
    select(-word) %>%
    distinct() %>% #to obtain one record per document
    ungroup()
  
  features$Issue <- as.factor(features$Issue)
  return(features)
}


train <- features_func_issue(Text_tidy)


task_train <- makeClassifTask(id = "issue Sources",
                              data = train[-1], target = "Issue")


task_train <- normalizeFeatures(task_train, method = "standardize",
                                cols = NULL, range = c(0, 1), on.constant = "quiet")


rf_model = train(makeLearner("classif.randomForest", id = "Random Forest", predict.type = "prob"), task_train)
saveRDS(rf_model, "./final_modelTC7-8.rds")


#-------------Test-----------------

detach("package:caret", unload=TRUE)

RNC <-read.csv("Test.csv")

Test <- RNC

Test  <- setNames(Test, c("RNC","Discrepancy_Text"))

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

super_model <- readRDS("./final_model4.rds")
result_rf <- predict(super_model, task_test)


RNC$Issue <- result_rf$data$response

Issues <- read_excel("Issues.xlsx", 
                     sheet = "Z34")

Issues$Issue <- as.factor(Issues$Issue)

RNC <- inner_join(RNC, Issues, by = "Issue")
