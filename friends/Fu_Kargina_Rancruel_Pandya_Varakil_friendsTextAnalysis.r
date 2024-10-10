
# create a new project and save all the html files within that folder

# can use the code line below if your files are stored in a sub directory within the project folder
# setwd(paste0(getwd(),"/ALL_Seasons"))

# install the following libraries if not already installed

library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(XML)
library(rstudioapi)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tm)
library(wordcloud2)
library(SnowballC)
library(cowplot)

# ------------------------------- saving the html files into a data frame  ----------------------------------------

#creating a path to read all the local html files

files <- list.files(path = getwd(), pattern = ".html$") # .html$ tag for reading all the html files within the folder

# creating an empty df 
friends_df <- data.frame(Season = character(),
                         Episode = numeric(),
                         Title = character(),
                         Script = character(),
                         stringsAsFactors = FALSE)

# creating a loop to read all the files and save it in the df

for (file in files) {
  episode <- gsub("<.*?>.html", # to extract the episode number
                  "", 
                  file)
  
  season <-  substr(episode, 1, 2) # extracting the season number
  
  # extract the text from the HTML file
  text <- rvest::read_html(x = file,
                           encoding = "UTF-8") #%>% html_text() 
  
  # to extract the title of the episode
  title <- iconv(rvest::html_elements(x = text, 
                                      css ="head > title"),
                 "UTF-8", "ASCII", sub="")
  
  # parsing the script within the paragraph tag
  script <- iconv(rvest::html_elements(x = text,
                                       css ="p"),
                  "UTF-8", "ASCII", sub="")
  
  # special case for certain episodes where the whole script is stuffed in a single paragraph tag or no paragraph tag
  if (length(script) == 0 | length(script) < 20) { # identifying those specific episodes
    
    # creating a new variable to modify the script of these episodes   
    paragraphs <- text %>% rvest::html_text2() 
    
    # replacing the \n css tag with a <p> tag  
    paragraphs <- stringr::str_replace_all(paragraphs, '\n', '<p>')
    
    # saving these episodes as html to parse it with the p tag  
    para_html <- rvest::read_html(paragraphs)
    script <- rvest::html_elements(x = para_html, css = 'p') %>% html_text()
  }
  
  # creating the final df 
  
  friends_df <- rbind(friends_df, data.frame(Season = season, # using rbind to add each episode in a new row
                                             Episode = episode, 
                                             Title = title, 
                                             Script = script, 
                                             stringsAsFactors = FALSE))
}

# checking the number of lines in the script per episode

lines <- friends_df %>% 
  group_by(Episode) %>% 
  summarise(n = length(Episode))



# --------------------------------------------- Data cleaning and preparation -----------------------------------------------

# separating script and speaker 

friends_df <- separate(friends_df, 
                       col = Script, 
                       sep = ":", 
                       into = c("Speaker", "Script"))

# creating a function to clean html tags
# Get rid of html formatting and other unnecessary data

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


friends_df$Speaker <- cleanFun(friends_df$Speaker)
friends_df$Script <- cleanFun(friends_df$Script)
friends_df$Title <- cleanFun(friends_df$Title)
friends_df$Episode <- str_replace(friends_df$Episode, ".html", "")
friends_df$Speaker <- str_replace(friends_df$Speaker, "\\[Scene", "Scene")
friends_df$Script <- str_replace(friends_df$Script, "Written by", "")

# separating Action from everything else

friends_df$Action <- sapply(str_extract_all(friends_df$Script, "\\([^()]+\\)"), paste0, collapse =",")
friends_df$Script <- str_replace(friends_df$Script, "\\([^()]+\\)", "")
friends_df$Script <- str_replace(friends_df$Script, " \\(\\)", "")

friends_df$Action <- sapply(str_extract_all(friends_df$Speaker, "(?<=\\()[^)(]+(?=\\))"), paste0, collapse =",")
friends_df$Speaker <- str_replace(friends_df$Speaker, "(?<=\\()[^)(]+(?=\\))", "Action")
friends_df$Action <- sapply(str_extract_all(friends_df$Speaker, "\\[[^()]+\\]"), paste0, collapse =",")
friends_df$Speaker <- str_replace(friends_df$Speaker, "\\[[^()]+\\]", "(Action)")
friends_df$Action <- str_replace(friends_df$Action, "\\[", "")
friends_df$Action <- str_replace(friends_df$Action, "\\]", "")
friends_df$Script <- str_replace(friends_df$Script, "\\&amp", "and")


# Cleaning Episode and Season

friends_df$Episode <- substr(friends_df$Episode, 3, nchar(friends_df$Episode))
friends_df <- friends_df %>%
  mutate(Episode = substr(Episode, 1, 2))
friends_df$Episode <- gsub("ou", "0", friends_df$Episode)
# gsub to avoid warning
friends_df$Episode <- as.numeric(gsub("[^0-9]", "", friends_df$Episode))
friends_df$Season <- as.numeric(gsub("[^0-9]", "", friends_df$Season))

# Cleaning Speaker

# Capital letters
friends_df$Speaker <- str_to_title(friends_df$Speaker)
# Spaces
friends_df$Speaker <- trimws(friends_df$Speaker, "left")
# Actions
mask_action <- str_detect(friends_df$Speaker, "Action")
friends_df$Action[mask_action] <- friends_df$Speaker[mask_action]
friends_df$Speaker[mask_action] <- ""
rm(mask_action)

# Misplaced or unclean values
pattern <- "(?<=\\s|^)([&<>]|[\\[\\]{}()*])[^\\s]*|&[Ll][Tt];
|(?<=\\w)-(?=\\w)|(?<!\\.)\\.(?!\\.)|(?<=\\s)\\.(?=\\w)"

#friends_df$Cleaning <- NULL

friends_df$Cleaning <- ifelse(str_detect(friends_df$Speaker, pattern), friends_df$Speaker, NA)
action_rows <- grepl(" \\(Action\\)", friends_df$Cleaning)
friends_df$Speaker[action_rows] <- gsub(" \\(Action\\)", "", friends_df$Cleaning[action_rows])
friends_df$Speaker <- gsub("\n", NA, friends_df$Speaker)
friends_df$Speaker <- gsub("The One", NA, friends_df$Speaker)
friends_df$Speaker <- gsub("Thanksgiving", NA, friends_df$Speaker)
friends_df$Speaker <- gsub("To Be", NA, friends_df$Speaker)
friends_df$Speaker <- gsub("Opening", NA, friends_df$Speaker)

friends_df$Speaker <- ifelse(str_detect(friends_df$Speaker, pattern), "", friends_df$Speaker)
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "^(A\\s)", "")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, 
                                      "(?i)^(.*)(written\\sby|teleplay|by|scene)(.*)$", "")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "(?i)\\sAnd$", "")
friends_df$Speaker <- str_trim(friends_df$Speaker)
friends_df$Speaker <- gsub(" To.*", "", friends_df$Speaker)
friends_df$Speaker <- gsub(" And", "", friends_df$Speaker)
friends_df$Speaker <- gsub("/", " ", friends_df$Speaker)


# Typos in characters' names
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Mich", "Michelle")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "(?i)Racel|Rach|Rache", "Rachel")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Chan", "Chandler")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "(?i)Phoe|Phoebebe", "Phoebe")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Chandlerdler", "Chandler")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Rachelel", "Rachel")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Phoebebe", "Phoebe")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Russ", "Russell")
friends_df$Speaker <- str_replace_all(friends_df$Speaker, "Mnca", "Monica")

# Cleaning Title


friends_df$Title <- str_to_title(friends_df$Title)
friends_df$Title <- gsub("\\n", "", friends_df$Title)
friends_df$Title <- gsub(")", "", friends_df$Title)
friends_df$Title <- ifelse(str_detect(friends_df$Title, "The"), 
                           str_replace(friends_df$Title, ".*The\\s+", "The "),
                           NA)



# Distribute Cleaning column

friends_df[c(46963, 47205, 63931, 2605), "Script"] <- friends_df[c(46963, 47205, 63931, 2605), "Cleaning"]
#friends_df$Cleaning <- str_replace_all(friends_df$Cleaning, pattern, "")

friends_df <- friends_df %>% 
  filter(Script != "" | Speaker != "Commercial Break") %>% 
  select(-Action, -Cleaning)

#getwd()
#setwd("C:/Users/bluff/OneDrive - Burgundy School of Business/BSB/R Studio Projects/season2/friends")
#writexl::write_xlsx(x = friends_df, path = "friends_df.xlsx")



# ---------------------------------------- Text Analysis ----------------------------------

#-----episodes_per_Season-------------


episodes_per_season <- friends_df %>% 
  group_by(Season) %>% 
  summarise(Episode = n_distinct(Episode)) %>% 
  mutate(Season = factor(Season)) 

episodes_per_season$c<- rep('black', nrow(episodes_per_season))
episodes_per_season$c[episodes_per_season$Episode>24] <- 'firebrick1'
episodes_per_season$c[episodes_per_season$Episode<=24&episodes_per_season$Episode>=23] <- 'royalblue'
episodes_per_season$c[episodes_per_season$Episode<23] <- 'forestgreen'

ggplot(data=episodes_per_season,aes(y = Season, x =Episode)) +
  geom_col(fill=episodes_per_season$c) +
  labs(y = "Seasons", x = "Count of Episodes",
       title = "Episodes per season ",
       subtitle = "An observational study") +
  geom_text(aes(label = Episode), vjust = -0.3, size = 3, col = "Black") +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")

#----------------------------------------------------------------------------------------------

# Scripts per Episode
# Scripts per Episode

scripts_per_Season <- friends_df %>% 
  group_by(Episode) %>% 
  summarise(Script = n_distinct(Script))



f1 <- friends_df[,1:4]
f1episode <- f1%>%
  group_by(f1$Season,f1$Episode)%>%
  count()

f1episode <- cbind(Episode = 1:nrow(f1episode), f1episode) 

# Here we can get the number of total lines in each episode which can show the time of each episode.
f1episode$colour <- rep('black', nrow(f1episode))
f1episode$colour[f1episode$n>1000] <- 'firebrick1'
f1episode$colour[f1episode$n>=400&f1episode$n<=1000] <- 'royalblue'
f1episode$colour[f1episode$n<400] <- 'forestgreen'



ggplot(f1episode,aes(x=f1episode$Episode,y=f1episode$n))+
  geom_point(color=f1episode$colour)+
  labs(
    title = "Three types of episodes",
    subtitle = "Red is most lines included;Blue is the middle;Green are the mode",
    x=" Episode",
    y="Count of line of each script"
  )+  geom_label(
    label="Season2,Episode12,count=1065", 
    x=75,
    y=1000,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2"
  )+
  theme_classic()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank())


#----------------------------------------------------------------------------------------

# Define the main characters
main_characters <- c("Rachel", "Monica", "Phoebe", "Ross", "Chandler", "Joey")

# Filter the data frame by the main characters
friends_filtered <- friends_df %>%
  filter(Speaker %in% main_characters)

# Select the relevant columns
friends_selected <- friends_filtered %>%
  select(Season, Episode, Speaker, Script)

# Group the data frame by season, episode, script, and speaker, and combine the lines for each speaker
friends_grouped <- friends_selected %>%
  group_by(Season, Episode, Script, Speaker)


friends_grouped

#-------------------------------------------------------------------------------------------------------

#Wordcloud
used_words_df <- friends_df %>%
  unnest_tokens(word, Script, token = "ngrams", n= 3)

# Remove stop words
used_words_df <- used_words_df %>%
  anti_join(stop_words)    # %>%
#mutate(word = wordStem(word, language = "english"))

word_freq <- used_words_df %>%
  count(word, sort = TRUE) %>%
  na.omit()

# Filter by frequency and select top 100
word_freq_top50 <- word_freq %>%
  filter(n >= 5) %>%
  slice_max(n = 50, order_by = n)

wordcloud2(data = word_freq_top50, size = 0.5,
           color = "random-light", backgroundColor = "white",
           shape = "triangle", shuffle = FALSE, rotateRatio = 0.5,
           fontFamily = "Impact",
           figPath = NULL)

#---------------------------Sentiment score analysis using afinn lexicon-------------------------



# Preprocess the text data
friends_df_clean <- friends_df %>%
  mutate(Speaker = tolower(Speaker),
         Script = tolower(Script)) %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c(",", ".", "!", "?", ":", ";", "-", "_", "'", "\"", "(", ")", "[", "]", "{", "}", "<", ">"))

# Get the sentiment lexicon
afinn_lexicon <- get_sentiments("afinn")

# Join the lexicon with the text data
sentiments <- friends_df_clean %>%
  inner_join(afinn_lexicon, by = "word")

# Calculate the sentiment scores by season
sentiment_scores <- sentiments %>%
  group_by(Season) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(Episode = paste0("Season ", Season))


sentiment_scores$colour <- rep('black', nrow(sentiment_scores))
sentiment_scores$colour[sentiment_scores$sentiment>1400] <- 'firebrick1'
sentiment_scores$colour[sentiment_scores$sentiment<=1400&sentiment_scores$sentiment>=1100] <- 'royalblue'
sentiment_scores$colour[sentiment_scores$sentiment<1100] <- 'forestgreen'

# Plot the sentiment scores
ggplot(sentiment_scores, aes(x = Episode, y = sentiment,fill=sentiment_scores$colour)) +
  geom_bar(stat = "identity")+
  geom_text(label=sentiment_scores$sentiment)+
  labs( y = "Sentiment Score (afinn)", title = "Sentiment Analysis of Friends Series",
        subtitle = "Season 7 and 9 have the most sentiment score") +
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.ticks = element_blank())


#------------------------Sentiment analysis using "bing" lexicon -------------------------------


tidy_friends <- friends_df %>%
  select(Season, Episode, Speaker, Script) %>%
  mutate(Speaker = if_else(Speaker == "Opening Credits", NA_character_, Speaker))

tidy_friends <- tidy_friends %>%
  unnest_tokens(word, Script) %>%
  mutate(word = str_remove_all(word, "\r"))

bing_sentiment <- tidy_friends %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber = row_number()) %>%
  group_by(Season, Episode, Speaker, index = linenumber %/% 80) %>%
  summarise(sentiment = sum(sentiment == "positive") - sum(sentiment == "negative")) %>%
  ungroup()

bing_sentiment$colour <- rep('black', nrow(bing_sentiment))
bing_sentiment$colour[bing_sentiment$sentiment >= 0] <- 'firebrick1'
bing_sentiment$colour[bing_sentiment$sentiment < 0] <- 'royalblue'

s1 <- ggplot(bing_sentiment %>% filter(Season == "1"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(y = "Sentiment score",
       title = "Sentiment analysis of Friends TV show for season 1")


s2 <- ggplot(bing_sentiment %>% filter(Season == "2"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 2")

s3 <- ggplot(bing_sentiment %>% filter(Season == "3"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 3")

s4 <- ggplot(bing_sentiment %>% filter(Season == "4"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 4")

s5 <- ggplot(bing_sentiment %>% filter(Season == "5"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 5")

s6 <- ggplot(bing_sentiment %>% filter(Season == "6"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 6")

s7 <- ggplot(bing_sentiment %>% filter(Season == "7"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 7")

s8 <- ggplot(bing_sentiment %>% filter(Season == "8"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 7")


s9 <- ggplot(bing_sentiment %>% filter(Season == "9"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 7")

s10 <- ggplot(bing_sentiment %>% filter(Season == "10"), aes(index, sentiment, fill = colour)) +
  geom_col(show.legend = FALSE)  +
  labs(x = NULL, y = NULL, title = "Sentiment analysis of Friends TV show for season 7")

senti_80th <- plot_grid(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,ncol=2,nrow=5)

senti_80th

#---------------------------------------------------------------------------------------------------

# # load NRC lexicon
# nrc_lexicon <- get_sentiments("nrc")
# unique(nrc_lexicon$sentiment)
# 
# # prepare data
# tidy_friends <- friends_df %>%
#   select(Season, Episode, Speaker, Script) %>%
#   mutate(Speaker = if_else(Speaker == "Opening Credits", NA_character_, Speaker)) %>%
#   unnest_tokens(word, Script) %>%
#   anti_join(stop_words) %>%
#   inner_join(nrc_lexicon) %>%
#   filter(sentiment %in% c("anger", "anticipation","positive","negative", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
#   group_by(Season, sentiment) %>%
#   summarize(sentiment_score = sum(value))
# 
# # plot
# ggplot(tidy_friends, aes(x = Season, y = sentiment_score, fill = sentiment)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~sentiment, ncol = 2, scales = "free") +
#   labs(x = "Season", y = "Sentiment Score", fill = "Sentiment")


#------------------------------------- Sentiment analysis for NRC ----------------------------------------------------------


tidy_friends1 <- friends_df %>%
  select(Season, Episode, Speaker, Script) %>%
  mutate(Speaker = if_else(Speaker == "Opening Credits", NA_character_, Speaker))

tidy_friends1 <- tidy_friends1 %>%
  unnest_tokens(word, Script) %>%
  mutate(word = str_remove_all(word, "\r"))

nrc_sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")


nrc_sentiment <- tidy_friends %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(linenumber = row_number()) %>%
  group_by(Season, Episode, Speaker, sentiment) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(sentiment = factor(sentiment, levels = nrc_sentiments)) %>%
  group_by(Season, sentiment) %>%
  summarise(sentiment_score = sum(count)) %>%
  ungroup()


# plot for joy
joy <- ggplot(nrc_sentiment %>% filter(sentiment == "joy"), aes(x = Season, y = sentiment_score,group=1)) +
  geom_point(shape=23, fill="red", color="black", size=3) +geom_line(linetype = "dashed",color="red")+
  labs(title = "RED:positive BLUE:negative", y = "Sentiment score (joy)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# plot for surprise
surprise <- ggplot(nrc_sentiment %>% filter(sentiment == "surprise"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=23, fill="red", color="black", size=3) +geom_line(linetype = "dashed",color="red") +
  labs( y = "Sentiment score (surprise)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for sadness
sadness <- ggplot(nrc_sentiment %>% filter(sentiment == "sadness"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=21, fill="blue", color="black", size=3) +geom_line(linetype = "dashed",color="blue") +
  labs( y = "Sentiment score (sadness)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for trust
trust <- ggplot(nrc_sentiment %>% filter(sentiment == "trust"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=23, fill="red", color="black", size=3) +geom_line(linetype = "dashed",color="red") +
  labs( y = "Sentiment score (trust)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for anger
anger <- ggplot(nrc_sentiment %>% filter(sentiment == "anger"), aes(x = Season, y = sentiment_score,group=1)) +
  geom_point(shape=21, fill="blue", color="black", size=3) +geom_line(linetype = "dashed",color="blue") +
  labs( y = "Sentiment score (anger)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# plot for disgust
disgust <- ggplot(nrc_sentiment %>% filter(sentiment == "disgust"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=21, fill="blue", color="black", size=3) +geom_line(linetype = "dashed",color="blue") +
  labs( y = "Sentiment score (disgust)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for fear
fear <- ggplot(nrc_sentiment %>% filter(sentiment == "fear"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=21, fill="blue", color="black", size=3) +geom_line(linetype = "dashed",color="blue") +
  labs( y = "Sentiment score (fear)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for anticipation
anticipation <- ggplot(nrc_sentiment %>% filter(sentiment == "anticipation"), aes(x = Season, y = sentiment_score, group=1)) +
  geom_point(shape=23, fill="red", color="black", size=3) +geom_line(linetype = "dashed",color="red") +
  labs( y = "Sentiment score (anticipation)", x = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

all1 <- plot_grid(joy,surprise,trust,anticipation,sadness,anger,disgust,fear, labels=c("Joy","Surprise","Trust","Anticipation","Sadness","Anger","Disgust","Fear"),ncol = 4,nrow = 2)

all1
# all3
#---------------------NRC analysis by character-----------------------------------------------------------


tidy_friends2 <- friends_df %>%
  select(Season, Episode, Speaker, Script) %>%
  mutate(Speaker = if_else(Speaker == "Opening Credits", NA_character_, Speaker))

tidy_friends2 <- tidy_friends2 %>%
  unnest_tokens(word, Script) %>%
  mutate(word = str_remove_all(word, "\r"))

nrc_sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")

tidy_friends2 <- tidy_friends2 %>%
  filter(Speaker %in% c("Rachel", "Monica", "Phoebe", "Ross", "Chandler", "Joey"))

nrc_sentiment <- tidy_friends2 %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(linenumber = row_number()) %>%
  group_by(Season, Episode, Speaker, sentiment) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(sentiment = factor(sentiment, levels = nrc_sentiments)) %>%
  group_by(Speaker, sentiment) %>%
  summarise(sentiment_score = sum(count)) %>%
  ungroup() %>%
  group_by(Speaker) %>%
  mutate(total_score = sum(sentiment_score),
         sentiment_percentage = sentiment_score/total_score * 100) %>%
  ungroup()

# plot for joy
joy <- ggplot(nrc_sentiment %>% filter(sentiment == "joy"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkgreen")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (joy)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for surprise
surprise <- ggplot(nrc_sentiment %>% filter(sentiment == "surprise"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkgreen")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (surprise)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for sadness
sadness <- ggplot(nrc_sentiment %>% filter(sentiment == "sadness"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkblue")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (sadnss)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for trust
trust <- ggplot(nrc_sentiment %>% filter(sentiment == "trust"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkgreen")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (trust)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for anger
anger <- ggplot(nrc_sentiment %>% filter(sentiment == "anger"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkblue")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (anger)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# plot for disgust
disgust <- ggplot(nrc_sentiment %>% filter(sentiment == "disgust"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkblue")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (disgust)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for fear
fear <- ggplot(nrc_sentiment %>% filter(sentiment == "fear"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkblue")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (fear)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for anticipation
anticipation <- ggplot(nrc_sentiment %>% filter(sentiment == "anticipation"), aes(x = Speaker, y = sentiment_score,group=1)) +
  geom_point(color="red") +geom_line(linetype = "dotted",color="darkgreen")+
  labs(title = "Sentiment analysis of Friends TV show", y = "Sentiment score (anticipation)", x = "Speaker") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

chara_emotion <- plot_grid(joy,surprise,trust,anticipation,sadness,anger,disgust,fear, labels=c("Joy","Surprise","Trust","Anticipation","Sadness","Anger","Disgust","Fear"),ncol = 4,nrow = 2)
chara_emotion


#--------------------Stacked bar graph of number of lines spoken by character per season----------------


# Group the data by season and speaker, and count the number of lines
lines_by_speaker_season <- friends_df %>% 
  group_by(Speaker, Season) %>% 
  summarize(total_lines = n()) %>% 
  filter(Speaker != "Opening Credits") %>% 
  arrange(Season, desc(total_lines))

# Create a vector of main characters to use for the legend
main_characters <- c("Monica", "Joey", "Chandler", "Ross", "Rachel", "Phoebe")

# Filter the data to only include main characters
lines_by_main_character_season <- lines_by_speaker_season %>% 
  filter(Speaker %in% main_characters)

# Create a stacked bar chart
theme_set(theme_bw()) + theme(plot.width = 10)
as.factor( lines_by_main_character_season$Speaker)

data1=lines_by_main_character_season%>% filter(Speaker=="Chandler")
data2=lines_by_main_character_season %>% filter(Speaker=="Ross")
data3=lines_by_main_character_season %>% filter(Speaker=="Rachel")
data4=lines_by_main_character_season %>% filter(Speaker=="Phoebe")
data5=lines_by_main_character_season %>% filter(Speaker=="Monica")
data6=lines_by_main_character_season %>% filter(Speaker=="Joey")

Chandler <- ggplot(data1,aes(x=Season,y=total_lines,label=data1$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(title = "Lines Spoken by Charaters per Season",
       x=" ")+
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)

Ross<- ggplot(data2,aes(x=Season,y=total_lines,label=data2$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(x=" ",y=" ")+
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)

Rachel<- ggplot(data3,aes(x=Season,y=total_lines,label=data3$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(x=" ",y=" ")+
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)

Phoebe<- ggplot(data4,aes(x=Season,y=total_lines,label=data4$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+ 
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)

Monica<- ggplot(data5,aes(x=Season,y=total_lines,label=data5$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(y=" ")+
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)

Joey<- ggplot(data6,aes(x=Season,y=total_lines,label=data6$total_lines,group=1))+
  geom_text(hjust = 0, nudge_x = 0.05)+
  geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(y=" ")+
  scale_x_continuous(breaks=1:max(lines_by_main_character_season$Season), minor_breaks=NULL)


all2 <- plot_grid(Chandler,Ross,Rachel,Phoebe,Monica,Joey, labels=c("Chandler","Ross","Rachel","Phoebe","Monica","Joey"),ncol = 3,nrow = 2)

all2 

#----------------------------- Characters Most used 2 words -------------------------------------

#------- Filter data to only include lines spoken by Rachel----- 

# Filter for lines spoken by Rachel
rachel_lines <- friends_df %>% 
  filter(Speaker == "Rachel") %>% 
  select(Script)

# Tokenize words and filter stop words
rachel_words <- rachel_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
rachel_word_freq <- rachel_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(data = rachel_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")

------# Filter data to only include lines spoken by Joey------

joey_lines <- friends_df %>% 
  filter(Speaker == "Joey") %>% 
  select(Script)

# Tokenize words and filter stop words
joey_words <- joey_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
joey_word_freq <- joey_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(data = joey_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")



#----- Filter data to only include lines spoken by Ross----

ross_lines <- friends_df %>% 
  filter(Speaker == "Ross") %>% 
  select(Script)

# Tokenize words and filter stop words
ross_words <- ross_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
ross_word_freq <- ross_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(data = ross_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")



#---- Filter data to only include lines spoken by Chandler--------
chandler_lines <- friends_df %>% 
  filter(Speaker == "Chandler") %>% 
  select(Script)

# Tokenize words and filter stop words
chandler_words <- chandler_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
chandler_word_freq <- chandler_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(data = chandler_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")



#------- Filter data to only include lines spoken by Phoebe-----



phoebe_lines <- friends_df %>% 
  filter(Speaker == "Phoebe") %>% 
  select(Script)

# Tokenize words and filter stop words
phoebe_words <- phoebe_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
phoebe_word_freq <- phoebe_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(phoebe_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")


#------ Filter data to only include lines spoken by Monica------

monica_lines <- friends_df %>% 
  filter(Speaker == "Monica") %>% 
  select(Script)

# Tokenize words and filter stop words
monica_words <- monica_lines %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words)

# Count word frequency and select top 50
monica_word_freq <- monica_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(50)

# Create wordcloud
wordcloud2(monica_word_freq, size = 0.8, color = "random-light", backgroundColor = "white")

#-------------------------------------------------------------------------------------------------------

# Identify words with a sexist, homophobic and fatphobic connotation



sexist_words <- c("slut", "bitch","whore", "homo", "fat", "ugly", "stupid")

friends_words5 <- friends_selected %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words)

# Use of this words per character
friends_words5 %>%
  filter(word %in% sexist_words) %>%
  filter(Speaker %in% main_characters) %>%
  group_by(word, Speaker) %>%
  summarise(count = n()) %>%
  ggplot()+
  aes(x = word, y = count, fill = Speaker) +
  geom_col() +
  facet_wrap(~Speaker) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "") +
  labs(title = "Use of words with a sexist, homophobic and  fatphobic connotation",
       subtitle = "Total for all seasons  \n")


# ----------------------------------------------------------------------------------------------------

# Humour Analysis

library(readr)
library(tidyverse)
library(tidytext)
library(syuzhet)
library(wordcloud)

# Import Friends script data
friends_script <- friends_df

# Clean and preprocess text
friends_words <- friends_script %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words) %>%
  #mutate(word = wordStem(word)) %>%
  filter(!str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace_all(word, "[^[:alnum:]']", "")) %>%
  filter(!word %in% c("", "s")) 

# Analyze sentiment and humor
friends_sentiment <- friends_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarize(sentiment = sum(value)) %>%
  mutate(humor = ifelse(sentiment > -10, "humorous", "not humorous"))

# Create a word cloud of humorous words
# set.seed(123)
wordcloud(friends_words$word[friends_sentiment$humor == "humorous"],
          max.words = 75, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


chandler_script <- friends_df %>% filter(Speaker == "Chandler")

# Clean and preprocess text
chandler_words <- chandler_script %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words) %>%
  #mutate(word = wordStem(word)) %>%
  filter(!str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace_all(word, "[^[:alnum:]']", "")) %>%
  filter(!word %in% c("", "s")) 

# Analyze sentiment and humor
chandler_sentiment <- chandler_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarize(sentiment = sum(value)) %>%
  mutate(humor = ifelse(sentiment > -10, "humorous", "not humorous"))

# Create a word cloud of humorous words
set.seed(123)
wordcloud(chandler_words$word[friends_sentiment$humor == "humorous"],
          max.words = 75, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


# verbosity analysis

script_words <- strsplit(friends_df$Script, "\\s+")

script_words %>%
  map_dbl(length) %>%
  mean()

word_counts <- script_words %>% 
  map_dbl(length)

ggplot(data.frame(word_counts), aes(x = word_counts)) +
  geom_histogram()

sentences <- tibble(text = friends_df$Script) %>%
  unnest_tokens(sentence, text, token = "sentences")


sentence_lengths <- sentences %>%
  mutate(length = str_count(sentence, "\\w+"))


round(mean(na.omit(sentence_lengths$length)),2)

# Create a list of the six main characters
main_characters <- c("Monica", "Rachel", "Phoebe", "Ross", "Chandler", "Joey")

# Initialize an empty data frame to store the results
word_counts <- data.frame(Character = character(), 
                          Season_1 = integer(), 
                          Season_2 = integer(), 
                          Season_3 = integer(), 
                          Season_4 = integer(), 
                          Season_5 = integer(), 
                          Season_6 = integer(), 
                          Season_7 = integer(), 
                          Season_8 = integer(), 
                          Season_9 = integer(), 
                          Season_10 = integer())

# Loop over each character
for (i in 1:length(main_characters)) {
  character <- main_characters[i]
  
  # Loop over each season
  for (j in 1:10) {
    season <- j
    
    # Subset the data frame to get the lines spoken by the current character in the current season
    lines <- friends_df$Script[friends_df$Season == season & friends_df$Speaker == character]
    
    # Combine the lines into a single string and split into words
    words <- unlist(strsplit(paste(lines, collapse = " "), " "))
    
    # Count the number of words and store in the results data frame
    word_counts[i, j+1] <- length(words)
  }
}

# Assign character names to the first column of the results data frame
word_counts$Character <- main_characters

# View the resulting data frame
word_counts



plot_data <- gather(word_counts, key = "Season", value = "Word_Count", -Character)
ggplot(plot_data, aes(x = Character, y = Word_Count)) + 
  geom_boxplot() +
  xlab("Character") +
  ylab("Word Count") +
  ggtitle("Word Counts by Character") +
  theme_minimal()

library(pastecs)


# Calculate summary statistics by character
#summary_stats <- aggregate(Word_Count ~ Character, word_counts, 
 #                          function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x), range = max(x) - min(x)))

# Reshape the summary statistics data into a long format
#plot_data <- gather(summary_stats, key = "Statistic", value = "Value", -Character)

# Create a box plot with summary statistics
# ggplot(plot_data, aes(x = Character, y = Value, fill = Statistic)) +
#   geom_boxplot() +
#   xlab("Character") +
#   ylab("Word Count") +
#   ggtitle("Word Counts by Character") +
#   theme_minimal() +
#   scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
#   guides(fill = guide_legend(title = "Statistic", nrow = 1))


# cultural analysis of friends 

library(tidytext)

# words <- friends_df %>%
#   select(Script) %>%
#   unnest_tokens(word, Script)
# 
# 
# adjectives <- words %>%
#   inner_join(get_sentiments("nrc"), by = "word") %>%
#   filter(sentiment == "positive" & part_of_speech == "adjective")



# read in Friends script
friends_script <- friends_df

# create a tibble with one row per word
words_tibble <- friends_script %>%
  unnest_tokens(word, Script)

# identify the frequency of female objectifying words
female_objectification_words <- c("lesbian","hot","fat","stripper", "sexy", "babe", "bimbo", "chick", "fox", "hottie", "babe-a-licious", "smoking", "stunning", "fine", "gorgeous", "bod", "rack", "legs", "ass", "butt", "boobs", "breasts", "beautiful")
female_objectification_count <- words_tibble %>%
  filter(word %in% female_objectification_words) %>%
  count() %>%
  pull(n)

# identify the frequency of non-female objectifying words
non_female_objectification_words <- c("tall", "naked man", "handsome", "attractive", "good-looking", "dashing", "suave")
non_female_objectification_count <- words_tibble %>%
  filter(word %in% non_female_objectification_words) %>%
  count() %>%
  pull(n)

# calculate the ratio of female objectifying words to non-female objectifying words
objectification_ratio <- female_objectification_count / non_female_objectification_count


# identify the frequency of female objectifying words
female_objectification_words <- c("hot", "sexy", "babe", "bimbo", "chick", "fox", "hottie", "babe-a-licious", "smoking", "stunning", "fine", "gorgeous", "bod", "rack", "legs", "ass", "butt", "boobs", "breasts")
female_objectification_count <- words_tibble %>%
  filter(word %in% female_objectification_words) %>%
  count() %>%
  pull(n)

# identify the frequency of non-female objectifying words
non_female_objectification_words <- c("handsome", "attractive", "good-looking", "dashing", "suave")
non_female_objectification_count <- words_tibble %>%
  filter(word %in% non_female_objectification_words) %>%
  count() %>%
  pull(n)

# calculate the ratio of female objectifying words to non-female objectifying words
objectification_ratio <- female_objectification_count / non_female_objectification_count

# display the results
objectification_ratio


# create a tibble with one row per line of dialogue
dialogue_tibble <- friends_df %>%
  group_by(Episode, Season, Speaker) %>%
  summarise(dialogue = paste(Script, collapse = " ")) %>%
  ungroup()

# words objectifying women
f_objectification <- dialogue_tibble %>%
  filter(str_detect(dialogue, paste(female_objectification_words, collapse = "|", sep = "\\b"))) %>% 
  select(-dialogue)

# per season
tibble(f_objectification %>% 
  group_by(Season) %>%
  summarise(`Number of Words` = n())) %>% 
  arrange(-`Number of Words`)

# per character
tibble(f_objectification %>% 
         filter(Speaker == "Joey" | Speaker == "Ross" | Speaker == "Rachel" | Speaker == "Phoebe" | 
                Speaker == "Monica" | Speaker == "Chandler") %>% 
         group_by(Speaker) %>%
         summarise(`Number of Words` = n()) %>% 
         arrange(-`Number of Words`))

# words objectifying men
m_objectification <- dialogue_tibble %>%
  filter(str_detect(dialogue, paste(non_female_objectification_words, collapse = "|", sep = "\\b"))) %>% 
  select(-dialogue)

# per season
m_objectification %>% 
         group_by(Season) %>%
         summarise(`Number of Words` = n()) %>% 
  arrange(-`Number of Words`) %>% 
  ggplot2::ggplot(aes(x = Season, y = `Number of Words`))+
  geom_col() 
  

# per character
m_objectification %>% 
         filter(Speaker == "Joey" | Speaker == "Ross" | Speaker == "Rachel" | Speaker == "Phoebe" | 
                  Speaker == "Monica" | Speaker == "Chandler") %>% 
         group_by(Speaker) %>%
         summarise(`Number of Words` = n()) %>% 
         arrange(-`Number of Words`)


# ----------------------------------------------------------------------------------------------------



# verbosity analysis

script_words <- strsplit(friends_df$Script, "\\s+")

script_words %>%
  map_dbl(length) %>%
  mean()

word_counts <- script_words %>% 
  map_dbl(length)

ggplot(data.frame(word_counts), aes(x = word_counts)) +
  geom_histogram()

sentences <- tibble(text = friends_df$Script) %>%
  unnest_tokens(sentence, text, token = "sentences")


sentence_lengths <- sentences %>%
  mutate(length = str_count(sentence, "\\w+"))


round(mean(na.omit(sentence_lengths$length)),2)

# Create a list of the six main characters
main_characters <- c("Monica", "Rachel", "Phoebe", "Ross", "Chandler", "Joey")

# Initialize an empty data frame to store the results
word_counts <- data.frame(Character = character(), 
                          Season_1 = integer(), 
                          Season_2 = integer(), 
                          Season_3 = integer(), 
                          Season_4 = integer(), 
                          Season_5 = integer(), 
                          Season_6 = integer(), 
                          Season_7 = integer(), 
                          Season_8 = integer(), 
                          Season_9 = integer(), 
                          Season_10 = integer())

# Loop over each character
for (i in 1:length(main_characters)) {
  character <- main_characters[i]
  
  # Loop over each season
  for (j in 1:10) {
    season <- j
    
    # Subset the data frame to get the lines spoken by the current character in the current season
    lines <- friends_df$Script[friends_df$Season == season & friends_df$Speaker == character]
    
    # Combine the lines into a single string and split into words
    words <- unlist(strsplit(paste(lines, collapse = " "), " "))
    
    # Count the number of words and store in the results data frame
    word_counts[i, j+1] <- length(words)
  }
}

# Assign character names to the first column of the results data frame
word_counts$Character <- main_characters

# View the resulting data frame
word_counts



plot_data <- gather(word_counts, key = "Season", value = "Word_Count", -Character)
plot_data <- gather(word_counts, key = "Season", value = "Word_Count", -Character)
ggplotly(ggplot(plot_data, aes(x = Character, y = Word_Count, fill = Character)) + 
           geom_boxplot() +
           xlab("Character") +
           ylab("Word Count") +
           ggtitle("Word Counts by Character") +
           theme(panel.background = element_blank(),
                 axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), # adjusting margin of axis label from the axis values
                 axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
                 legend.position = "none", 
                 legend.background = element_blank(),
                 #legend.title = element_text(size = 10),
                 axis.line = element_line(linewidth = 1),
                 axis.title = element_text(size = 9),
                 axis.text = element_text(size = 9),
                 legend.text = element_text(size = 8),
                 #legend.spacing = unit(2, 'cm'),
                 plot.title = element_text(size = 14, hjust = 0.5, face = 'bold', family = "Verdana"),
                 plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", family = "Verdana"))+
           scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6")))



# words per dialogue of 6 main characters

friends_main_chars <- friends_df[friends_df$Speaker %in% main_characters,]
friends_main_chars <- friends_main_chars %>% 
                      filter(Script != '')

friends_main_chars$Words <- str_split(friends_main_chars$Script, "\\s+")

friends_main_chars$NumWords <- sapply(friends_main_chars$Words, length)

total_sentences <- friends_main_chars %>%
  group_by(Speaker) %>%
  summarize(TotalSentences = n()) %>%
  ungroup()

avg_words <- friends_main_chars %>%
  group_by(Speaker) %>%
  summarize(AverageWords = round(mean(na.omit(str_count(Script, "\\S+"))),2)) %>%
  ungroup()

min_max_words <- friends_main_chars %>%
  group_by(Speaker) %>%
  summarize(MinWords = min(na.omit(str_count(Script, "\\S+"))),
            MaxWords = max(na.omit(str_count(Script, "\\S+")))) %>%
  ungroup()

sd_words <- friends_main_chars %>%
  group_by(Speaker) %>%
  summarize(SD_Words = round(sd(na.omit(str_count(Script, "\\S+"))),2)) %>%
  ungroup()

# table showing the word analysis of 6 main characters
merge(total_sentences, avg_words, by = "Speaker") %>%
  merge(min_max_words, by = "Speaker") %>%
  merge(sd_words, by = "Speaker")


# boxplot of number of dialogues in each season

friends_main_chars <- friends_df[friends_df$Speaker %in% main_characters,]

friends_main_chars %>%
  group_by(Season, Speaker) %>%
  summarize(SentenceCount = n()) %>% select(Season, SentenceCount)

# range of sentences by characters 
sent_char <- friends_main_chars %>%
  group_by(Season, Speaker) %>%
  summarize(SentenceCount = n()) 

sent_char %>% 
ggplotly(ggplot(aes(x = Speaker, y = SentenceCount)) +
  geom_boxplot() +
  labs(title = "Count of Sentences in Dialogues by Characters",
       x = "Speaker",
       y = "Count of Sentences in Dialogues"))

# histogram showing the count of sentences per season
sentences <- friends_df %>%
  group_by(Season) %>%
  summarize(SentenceCount = n())

# histogram showing the count of sentences per season
ggplotly(friends_df %>%
  group_by(Season) %>%
  summarize(SentenceCount = n()) %>% 
  ggplot(aes(x = Season, y = SentenceCount, text = paste0("Season: ", {Season}, '<br>', #hoverinfo
                                                          #"Region: ", {color}, '<br>',
                                                          "Number of Sentences: ", {round(SentenceCount,2)})))+
  geom_col(fill = ifelse(sentences$SentenceCount == min(sentences$SentenceCount), yes = "#82EEFD", 
                         no = ifelse(sentences$SentenceCount > mean(sentences$SentenceCount), yes = "#0077B6",
                         no = "#ADD8E6"))) +
  labs(title = "Count of Sentences in Dialogues by Seasons",
       x = "Speaker",
       y = "Count of Sentences in Dialogues")+
  scale_x_continuous(breaks = 1:10)+
  geom_hline(yintercept = mean(sentences$SentenceCount), linetype = "dashed")+
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), #adjusting margins of x and y axis titles
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        axis.text.x = element_text(hjust=0.9, angle = 45), #text aligned at an angle
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        #legend.title = element_text(size = 10),
        axis.line = element_line(linewidth = 1),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 8),
        #legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold', family = "Verdana"),
        plot.subtitle = element_text(size = 8, hjust = 0.5, face = "italic", family = "Verdana")),
tooltip = "text")


  library(plotly)

#=========================================================================================
#                                  Humour and Objectification Analysis
# ========================================================================================
library(readr)
library(tidyverse)
library(tidytext)
library(syuzhet)
library(wordcloud)

# Import Friends script data
friends_script <- friends_df

# Clean and preprocess text
friends_words <- friends_script %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words) %>%
  #mutate(word = wordStem(word)) %>%
  filter(!str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace_all(word, "[^[:alnum:]']", "")) %>%
  filter(!word %in% c("", "s")) 

# Analyze sentiment and humor
friends_sentiment <- friends_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarize(sentiment = sum(value)) %>%
  mutate(humor = ifelse(sentiment > 10, "humorous", "not humorous"))

# word cloud of humorous words
friends_words <- friends_words %>%
  filter(word != "ross" | word != "monica" | word != "rachel" | word != "joey" | word != "chandler" | word != "phoebe" ) 


wordcloud(friends_words$word[friends_sentiment$humor == "humorous"],
          max.words = 75, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


chandler_script <- friends_df %>% filter(Speaker == "Chandler")

# Clean and preprocess text
chandler_words <- chandler_script %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words) %>%
  #mutate(word = wordStem(word)) %>%
  filter(!str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace_all(word, "[^[:alnum:]']", "")) %>%
  filter(!word %in% c("", "s")) 

# Analyze sentiment and humor
chandler_sentiment <- chandler_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarize(sentiment = sum(value)) %>%
  mutate(humor = ifelse(sentiment > -10, "humorous", "not humorous"))

# Create a word cloud of humorous words
set.seed(123)
wordcloud(chandler_words$word[friends_sentiment$humor == "humorous"],
          max.words = 75, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# ==========================
#       Objectification
# ==========================
friends_script <- friends_df

# create a tibble with one row per word
words_tibble <- friends_script %>%
  unnest_tokens(word, Script)

# identify the frequency of female objectifying words
female_objectification_words <- c("lesbian","hot","fat","stripper", "sexy", "babe", "bimbo", "chick", "fox", "hottie", "babe-a-licious", "smoking", "stunning", "fine", "gorgeous", "bod", "rack", "legs", "ass", "butt", "boobs", "breasts", "beautiful")
female_objectification_count <- words_tibble %>%
  filter(word %in% female_objectification_words) %>%
  count() %>%
  pull(n)

# identify the frequency of non-female objectifying words
non_female_objectification_words <- c("tall", "naked man", "handsome", "attractive", "good-looking", "dashing", "suave")
non_female_objectification_count <- words_tibble %>%
  filter(word %in% non_female_objectification_words) %>%
  count() %>%
  pull(n)

# calculate the ratio of female objectifying words to non-female objectifying words
objectification_ratio <- female_objectification_count / non_female_objectification_count


# identify the frequency of female objectifying words
female_objectification_words <- c("hot", "sexy", "babe", "bimbo", "chick", "fox", "hottie", "babe-a-licious", "smoking", "stunning", "fine", "gorgeous", "bod", "rack", "legs", "ass", "butt", "boobs", "breasts")
female_objectification_count <- words_tibble %>%
  filter(word %in% female_objectification_words) %>%
  count() %>%
  pull(n)

# identify the frequency of non-female objectifying words
non_female_objectification_words <- c("handsome", "attractive", "good-looking", "dashing", "suave")
non_female_objectification_count <- words_tibble %>%
  filter(word %in% non_female_objectification_words) %>%
  count() %>%
  pull(n)

# calculate the ratio of female objectifying words to non-female objectifying words
objectification_ratio <- female_objectification_count / non_female_objectification_count

# display the results
objectification_ratio


# create a tibble with one row per line of dialogue
dialogue_tibble <- friends_df %>%
  group_by(Episode, Season, Speaker) %>%
  summarise(dialogue = paste(Script, collapse = " ")) %>%
  ungroup()

# words objectifying women
f_objectification <- dialogue_tibble %>%
  filter(str_detect(dialogue, paste(female_objectification_words, collapse = "|", sep = "\\b"))) %>% 
  select(-dialogue)

# per season
tibble(f_objectification %>% 
         group_by(Season) %>%
         summarise(`Number of Words` = n())) %>% 
  arrange(-`Number of Words`)

# per character
tibble(f_objectification %>% 
         filter(Speaker == "Joey" | Speaker == "Ross" | Speaker == "Rachel" | Speaker == "Phoebe" | 
                  Speaker == "Monica" | Speaker == "Chandler") %>% 
         group_by(Speaker) %>%
         summarise(`Number of Words` = n()) %>% 
         arrange(-`Number of Words`))

# words objectifying men
m_objectification <- dialogue_tibble %>%
  filter(str_detect(dialogue, paste(non_female_objectification_words, collapse = "|", sep = "\\b"))) %>% 
  select(-dialogue)

# per season
m_objectification %>% 
  group_by(Season) %>%
  summarise(`Number of Words` = n()) %>% 
  arrange(-`Number of Words`) %>% 
  ggplot2::ggplot(aes(x = Season, y = `Number of Words`))+
  geom_col() 


# per character
m_objectification %>% 
  filter(Speaker == "Joey" | Speaker == "Ross" | Speaker == "Rachel" | Speaker == "Phoebe" | 
           Speaker == "Monica" | Speaker == "Chandler") %>% 
  group_by(Speaker) %>%
  summarise(`Number of Words` = n()) %>% 
  arrange(-`Number of Words`)


