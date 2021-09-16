#Competition 1 Script
#2021
#Enrico Erler

#libraries
require(tidyverse) # metapackage with lots of helpful functions
require(tidytext)

#Set WD
setwd("C:/Users/Enrico/Google Drive/Uni/Amsterdam/2nd year/Big Data Analytics/Competitions/1")

#Path content
directory_content = list.files(full.names = TRUE)
print(directory_content)


# Path to the transcripts directory with transcript .txt files
path_to_transcripts = "./transcripts" 

#Importing transcripts
transcript_files = list.files(path_to_transcripts, full.names = TRUE) 

#Get vector of IDs
vlogId = basename(transcript_files)
vlogId = gsub(pattern = ".txt$", replacement = "", vlogId)

#Read transcripts into data frame
transcripts_df = tibble(
  
  # vlogId connects each transcripts to a vlogger
  vlogId=vlogId,
  
  # Read the transcript text from all file and store as a string
  Text = map_chr(transcript_files, ~ paste(readLines(.x), collapse = "\\n")), 
  
  # `filename` keeps track of the specific video transcript
  filename = transcript_files
)



#Data import
#audiovisual_df <- read.csv2("YouTube-Personality-audiovisual_features.csv", header = T, sep = " ")
audiovisual_df <- read.csv2("YouTube-Personality-audiovisual_features.csv", header = T, sep = " ")
gender_df <- read.csv2("YouTube-Personality-gender.csv", header = T, sep = " ")
pers_df <- read.csv2("YouTube-Personality-Personality_impression_scores_train.csv", header = T, sep = " ")
vlogger_df <- left_join(gender_df,pers_df)


#Create word tokens (splits up the texts into single words)
transcript_features_df = unnest_tokens(tbl = transcripts_df, word ,Text , token = 'words') 
transcript_features_df <- transcript_features_df[-2]







######################
# Feature extraction #
######################



########################
###Convert Gender to binary
########################

for (i in 1:length(gender_df$gender))
{
  if (gender_df$gender[i] == "Male")
  { 
    gender_df$gender[i] <- 0  
  }
  else
  {
    gender_df$gender[i] <- 1
  }
}



########################
### Frequency of "i"
########################

#count num of "i" per vlogId
count_i <- transcript_features_df %>% filter(word == "i") %>% count(vlogId)

#count num of words per vlogId
count_words <- transcript_features_df %>% count(vlogId)

#join both counts
counts <- left_join(count_i, count_words, by = "vlogId")

#calculate frequency
counts <- counts %>% mutate(freq_i = n.x / n.y)

#Problem: We still missing the 3 rows with zero occurences
diff <- setdiff(count_words$vlogId, count_i$vlogId)
#VLOG201,304,352 have zero "i"

#Create a tibble with all missing vlogIds & counts
t <- count_words %>% filter(vlogId == diff)

#add those to the "counts" object which stores all cases
counts <- counts %>% add_row(vlogId = t$vlogId, n.x = 0, n.y = t$n, freq_i = 0)



########################
###Frequency of "we"
########################

#count num of "i" per vlogId
count_we <- transcript_features_df %>% filter(word == "we") %>% count(vlogId)

#join both counts
counts <- left_join(counts, count_we, by = "vlogId", na_matched = "na")

#Replace NA's with zeros
counts$n <- counts$n %>% replace_na(0)


#calculate frequency
counts <- counts %>% mutate(freq_we = n / n.y)



########################
### Average word length
########################

#create letter_count vector
letter_count <- vector(mode ="numeric", length = length(transcript_features_df$word))

#loop though all words, split it into letters & count the number of letters
for (i in 1:length(transcript_features_df$word))
{
letter_count[i] <- length(unlist(strsplit(transcript_features_df$word[i], split = "")))  
}

#add letter_count vector to df
transcript_features_df <- cbind(transcript_features_df,letter_count)

#calculate average word length per vlogId
avg_word_leng <- transcript_features_df %>% group_by(vlogId) %>% summarise(avg_word_length = mean(letter_count))

#add to "counts" df
counts <- left_join(counts, avg_word_leng, by = "vlogId")


########################
### Overall text sentiment
########################

#load in NRC data frame
load_nrc = function() {
  if (!file.exists('nrc.txt'))
    download.file("https://www.dropbox.com/s/yo5o476zk8j5ujg/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt?dl=1","nrc.txt")
  nrc = read.table("nrc.txt", col.names=c('word','sentiment','applies'), stringsAsFactors = FALSE)
  nrc %>% filter(applies==1) %>% select(-applies)
}

nrc = load_nrc()


#Merge duplicate words into one row, and paste all sentiments into one column
nrc <- nrc %>% group_by(word) %>% summarise(sentiment = paste(sentiment, collapse = " "))


# Add columns for each sentiment, giving a "1" if sentiment there, "0" if not
trust_vector <- vector(mode = "numeric", length = length(nrc$word))
fear_vector <- vector(mode = "numeric", length = length(nrc$word))
negative_vector <- vector(mode = "numeric", length = length(nrc$word))
sadness_vector <- vector(mode = "numeric", length = length(nrc$word))
anger_vector <- vector(mode = "numeric", length = length(nrc$word))
surprise_vector <- vector(mode = "numeric", length = length(nrc$word))
positive_vector <- vector(mode = "numeric", length = length(nrc$word))
disgust_vector <- vector(mode = "numeric", length = length(nrc$word))
joy_vector <- vector(mode = "numeric", length = length(nrc$word))
anticipation_vector <- vector(mode = "numeric", length = length(nrc$word))

#add vectors to nrc data frame
nrc <- cbind(nrc,
        positive_vector,
        negative_vector,
        trust_vector,
        fear_vector,
        sadness_vector,
        anger_vector,
        surprise_vector,
        disgust_vector,
        joy_vector,
        anticipation_vector)

for (i in 1:length(nrc$word))
{
  
any(unlist(strsplit(nrc$sentiment[i], split = " ")))  
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "positive"))
  {
    nrc$positive_vector[i] <- 1
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "negative"))
  {
    nrc$negative_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "trust"))
  {
    nrc$trust_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "fear"))
  {
    nrc$fear_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "sadness"))
  {
    nrc$sadness_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "anger"))
  {
    nrc$anger_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "surprise"))
  {
    nrc$surprise_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "disgust"))
  {
    nrc$disgust_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "joy"))
  {
    nrc$joy_vector[i] <- 1 
  }
  
  if (any(unlist(strsplit(nrc$sentiment[i], split = " ")) == "anticipation"))
  {
    nrc$anticipation_vector[i] <- 1 
  }
}

#get rid of sentiment string (no longer needed)
nrc <- nrc[-2]

#Join nrc scores into word-by-word transcript df
transcript_features_df <- left_join(transcript_features_df,nrc, by = "word")

#Replace NA's with zeros
#We are not sure whether this should be done at the moment
for (i in 4:13)
{
  transcript_features_df[[i]] <- transcript_features_df[[i]] %>% replace_na(0)
}
 







