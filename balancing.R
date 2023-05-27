library(tidyverse)


data = read_rds("data/clean_2015-2019_score5.rds")

data %>% 
  count(subreddit)

data_balanced = 
  data %>% 
  filter(subreddit != "datingadviceformen") 

data_balanced = 
  data_balanced %>% 
  mutate(sampler = TRUE) %>% 
  mutate(p = runif(nrow(data_balanced))) %>% 
  mutate(sampler = ifelse(.$subreddit == "relationships", .$p < 0.33, .$sampler)) %>% 
  filter(sampler) %>% 
  mutate(
    sampler = NULL,
    p = NULL
  )

data_balanced %>% 
  count(subreddit) %>% 
  mutate(freq = n/nrow(data_balanced))

data_balanced = 
  data_balanced %>% 
  mutate(sampler = TRUE) %>% 
  mutate(p = runif(nrow(data_balanced))) %>% 
  mutate(sampler = ifelse(.$subreddit %in% c("ForeverAlone", "relationships", "relationship_advice"), .$p < 0.5, .$sampler)) %>% 
  filter(sampler) %>% 
  mutate(
    sampler = NULL,
    p = NULL
  )

data_balanced %>% 
  count(subreddit) %>% 
  mutate(freq = n/nrow(data_balanced))

data_balanced %>% 
  write_rds("data/balanced_score5.rds")
