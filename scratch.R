library(rtweet)
library(tidytext)
library(tidyverse)
library(ggmap)
library(syuzhet)


GOOGLE_MAPS_KEY <- "AIzaSyDabUPNzUcgC808hOwM3_QYVYW5UTijsnI"
register_google(key = GOOGLE_MAPS_KEY)
getOption("ggmap")

data("stop_words")

covid_tweets <- search_tweets(q = "coronavirus", n = 10, include_rts = FALSE)

covid_tweets$stripped_text <- gsub("http.*","",  covid_tweets$text)
covid_tweets$stripped_text <- gsub("https.*","", covid_tweets$stripped_text)


cleaned_tweets <- covid_tweets %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words)

cleaned_tweets %>%
  count(location, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

cleaned_tweets %>% 
  mutate(location_clean = ifelse(location == "", NA, location))%>% 
  na.omit() %>% 

  count(location_clean, sort = TRUE) %>%
  mutate(location_clean = reorder(location_clean, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location_clean, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")



stuff <- cleaned_tweets %>% 
  select(text, quoted_location,location)
  
china1 <- search_tweets("coronavirus",
  geocode = lookup_coords("China", apikey = GOOGLE_MAPS_KEY), n = 10)



japan <- search_tweets("coronavirus", n = 10, lang = "ja")
china <- search_tweets("coronavirus", n = 10, lang = "zh-cn", country = "china")



#locating users

covid_tweets <- search_tweets(q = "coronavirus", n = 100, include_rts = FALSE)
userinfo <- lookup_users(covid_tweets$screen_name) %>% 
  filter(location != "") %>%
  mutate_geocode(location)

##sentiment analysis using syuzhet


covid_tweets <- search_tweets(q = "coronavirus", n = 10, include_rts = FALSE, lang = "jap")

covid_tweets$stripped_text <- gsub("http.*","",  covid_tweets$text)
covid_tweets$stripped_text <- gsub("https.*","", covid_tweets$stripped_text)


cleaned_tweets <- covid_tweets %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words) %>% 
  mutate(sentiment = get_sentiment(word), method = "afinn") %>% 
  select(status_id, word, sentiment)
  
  #sentiment analysis 
sentiment <- get_sentiment(cleaned_tweets$word, method = "bing", lexicon = TRUE)
    


