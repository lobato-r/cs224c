library(tidyverse)
library(text2vec)
library(uwot)
library(tidytext)
library(furrr)



# data = read_rds("data/nested_2015-2019.rds")
# data =
#   data %>% 
#   filter(score > 5) 

data =  read_rds("data/nested_2015-2019_score5.rds")

scores_p = data %>% 
  group_by(subreddit) %>% 
  summarise(high_score = quantile(score, 0.75))

data_smol =
  data %>% 
  mutate(p = runif(nrow(data))) %>% 
  filter(p >= 0.75) %>% 
  select(-p)
# data_smol %>% write_rds("data/nested_smol.rds")

data %>% 
  mutate(n_comments = map_int(comments, nrow)) %>% 
  group_by(subreddit) %>%
  summarise(
    n_posts = n(),
    n_comments = sum(n_comments)
  ) %>% 
  arrange(desc(n_posts)) %>% 
  write_csv("data/n_subreddit.csv")


clean_text = function(text_df){
  text_df %>% 
    pull(body) %>% 
    str_c(collapse = " ") %>% 
    clean_functions() %>% 
    return()
}

clean_functions = function(text){
  text %>% 
    str_to_lower() %>% 
    str_remove_all(pattern = "\\[removed\\]") %>% 
    str_remove_all(pattern = "\\[deleted\\]") %>% 
    str_remove_all(pattern = "r/") %>% 
    str_replace_all(pattern = "\n", "") %>% 
    str_remove_all(pattern = "@\\w+") %>% 
    # str_remove_all(pattern = "(https?:\\/\\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w\.-]*)") %>% 
    str_remove_all(pattern = "https?://\\.+") %>% 
    str_remove_all("\\d+\\w*\\d*") %>% 
    str_remove_all("#\\w+") %>% 
    str_remove_all("[^\x01-\x7F]") %>% 
    str_remove_all("[[:punct:]]") %>% 
    # str_remove_all(pattern = "\\s+$") %>% 
    return()
}


plan(multisession, workers = 2)

wrap_comments = function(data){
  data %>% 
    select(body, score_comment) %>% 
    mutate(body = map_chr(body, clean_functions)) %>% 
    return()
}

data_tidy =
  data_smol %>%
  unnest(comments) %>% 
  select(-c(created_utc, post_created_utc, author_comment)) 


data_clean =
  data_tidy %>% 
  filter(score_comment > 5) %>% 
  # mutate(title  =  furrr::future_map_chr(title, clean_functions)) %>% 
  # mutate(selftext  =  furrr::future_map_chr(selftext, clean_functions)) %>% 
  mutate(body  =  map_chr(body, clean_functions)) 


data_clean %>% 
  write_rds("data/smol.rds")


data_tidy =
  data_smol %>%
  unnest(comments) %>% 
  select(-c(created_utc, post_created_utc, author_comment)) 


data_clean =
  data %>% 
  mutate(title  =  map_chr(title, clean_functions)) %>%
  mutate(selftext  =  map_chr(selftext, clean_functions)) %>%
  unnest(comments) %>% 
  mutate(body  =  map_chr(body, clean_functions)) %>% 
  nest(comments = c(body, score_comment, author_comment, created_utc))


data_clean %>% 
  write_rds("data/clean_2015-2019_score5.rds")


