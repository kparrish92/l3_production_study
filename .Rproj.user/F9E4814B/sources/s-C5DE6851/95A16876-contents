# 01 helpers


# function to add a single word given xmin and xmax of textgrid dfs 
add_word = function(x_min, x_max, df)
{
  library(dplyr)
  # subset
  df <- df %>% filter(x_min <= xmin & x_max >= xmax)
  # mutate
  df$word <- df$text[1]
  # bind
  return(df)  
}  

thisRun = 1

# using 'add_word' function, apply it to all words in df 

add_words <- function(df)
{
  new_df <- character()
  # subset words 
  word_df <-  df %>% 
    dplyr::filter(tier_name == "ORT-MAU")
  # setup loop 
  for(thisRun in 1:nrow(word_df))
  {
    df_n <- add_word(word_df$xmin[thisRun], word_df$xmax[thisRun], df) %>% 
      mutate(duration = word_df$xmax[thisRun] - word_df$xmin[thisRun])
    new_df <- rbind(new_df, df_n)  
  }
  return(new_df)
}

# function to calculate phonotactic probability of a word given a corpus in summary form.
# function assumes that one column is named "word" and the other "token freq"

# current version uses orthography rather than a phonetic transcription for the purpose of L3 research

# df for testing
#corpus <- data.frame(word= c("blik", "blep", "dwik", "mup"), token_freq = c("22", "107", "3", "57")) # create group_by() df #
#word <- "twik"

biphon_prob <- function(word, corpus)
{  
  library(tidyverse)
  library(stringr)
  npos <- nchar(word) - 1
  df <- matrix(nrow = npos, ncol = 3)
  # find number of biphone positions 
  
  corpus$token_freq = as.numeric(corpus$token_freq)
  word_seg_df <- str_split(word, pattern = "")[[1]] 
  
  for(thisRun in 1:npos){
    str <- str_c(word_seg_df[thisRun], word_seg_df[thisRun + 1])
    corpus$truth <- str_detect(corpus$word, str) 
    corpus_filt <- corpus %>% 
      filter(truth == "TRUE")
    container <- matrix(nrow = nrow(corpus_filt), ncol = 2) %>% 
      as.data.frame()# container of output from loop 
    
    for(thisRun2 in 1:nrow(corpus_filt)){
      container[thisRun2, 1] = log(corpus_filt$token_freq[thisRun2]) # column 1 of container per run 
      container[thisRun2, 2] = thisRun2 # column 2 of container per run 
      
      df_denom <- corpus %>% 
        filter(nchar(word) > thisRun)
      
      container_denom1 <- matrix(nrow = nrow(df_denom), ncol = 2) %>% 
        as.data.frame()# container of output from loop 
      
      for(i in 1:nrow(df_denom)){
        container_denom1[i, 1] = log(corpus$token_freq[i]) # column 1 of container per run 
        container_denom1[i, 2] = i # column 2 of container per run 
      }
      
    }
    
    df[is.na(df)] <- 0
    df[thisRun, 1] = sum(container$V1)
    df[thisRun, 2] = sum(container_denom1$V1)
    df[thisRun, 3] = thisRun
  }  
  
  log_df <- df %>% 
    as.data.frame() %>% 
    mutate(part = V1/V2)
  
  log_prob = sum(log_df$part)/nrow(log_df)
  return(log_prob)
}


