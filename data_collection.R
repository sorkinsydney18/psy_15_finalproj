library(rtweet)
library(tidytext)
library(tidyverse)
library(ggmap)
library(syuzhet)


register_google(key = google_maps_key)


#Step 1: gather tweets from each location

###########US

us <- search_tweets("coronavirus OR COVID", 
                        include_rts = FALSE, 
                        geocode = lookup_coords("usa", apikey = google_maps_key), 
                    lang = "en",
                        n = 1000)
#clean tweets

us$stripped_text <- gsub("http.*","",  us$text)
us$stripped_text <- gsub("https.*","", us$stripped_text)


us_clean <- us %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words) %>% 
  select(status_id, text, location, word)

#sentiment analysis 

us_clean1 <- us_clean %>% 
  mutate(sentiment = get_sentiment(word), method = "syuzhet") %>% 
  group_by(status_id) %>% 
  mutate(twt_polarity = sum(sentiment)) %>% 
  ungroup()

avg_polarity <- mean(us_clean1$sentiment)


#################France

UK <- search_tweets("coronavirus OR COVID", 
                    include_rts = FALSE, 
                    geocode = lookup_coords("United Kingdom", apikey = google_maps_key), 
                    lang = "en",
                    n = 1000)
#clean tweets

UK$stripped_text <- gsub("http.*","",  UK$text)
UK$stripped_text <- gsub("https.*","", UK$stripped_text)


UK_clean <- UK %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words) %>% 
  select(status_id, lang, text, location, word)

#sentiment analysis 

UK_clean1 <- UK_clean %>% 
  mutate(sentiment = get_sentiment(word), method = "syuzhet") %>% 
  group_by(status_id) %>% 
  mutate(twt_polarity = sum(sentiment)) 

avg_polarity_UK <- mean(UK_clean1$sentiment)

###############China

china <- search_tweets("coronavirus OR COVID", 
                    include_rts = FALSE, 
                    geocode = lookup_coords("China", apikey = google_maps_key), 
                    lang = "en",
                    n = 1000)
#clean tweets

china$stripped_text <- gsub("http.*","",  china$text)
china$stripped_text <- gsub("https.*","", china$stripped_text)


china_clean <- china %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words) %>% 
  select(status_id, lang, text, location, word)

#sentiment analysis 

china_clean1 <- china_clean %>% 
  mutate(sentiment = get_sentiment(word), method = "syuzhet") %>% 
  group_by(status_id) %>% 
  mutate(twt_polarity = sum(sentiment)) 

avg_polarity_china <- mean(china_clean1$sentiment)

#################Japan

japan <- search_tweets("coronavirus OR COVID", 
                       include_rts = FALSE, 
                       geocode = lookup_coords("Japan", apikey = google_maps_key), 
                       lang = "en",
                       n = 1000)
#clean tweets

japan$stripped_text <- gsub("http.*","",  japan$text)
japan$stripped_text <- gsub("https.*","", japan$stripped_text)


japan_clean <- japan %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words) %>% 
  select(status_id, lang, text, location, word)

#sentiment analysis 

japan_clean1 <- japan_clean %>% 
  mutate(sentiment = get_sentiment(word), method = "syuzhet") %>% 
  group_by(status_id) %>% 
  mutate(twt_polarity = sum(sentiment)) 

avg_polarity_japan <- mean(japan_clean1$sentiment)

