library(bigrquery)
library(tidyverse)


con <- dbConnect(
  bigrquery::bigquery(),
  project = "cs224c",
  dataset = "",
  billing = ""
)
con 

dbListTables(con)


projectid <- "cs224c"



sql6  = "select subreddit, title, selftext, score, post_created_utc, created_utc, body, author_comment, score_comment
from `cs224c.reddit_dating_2019.2015-2019`,
 UNNEST(comments)
where 
(body is not null
and created_utc is not null
and title is not null
and selftext is not null 
and score is not null
and subreddit is not null
and post_created_utc is not null
and author_comment is not null
and score_comment is not null
and score > 1
and score_comment >1
) "

df6 <- bq_project_query(projectid, sql6) %>% 
  bq_table_download() %>% 
  nest(comments = c(body, created_utc, author_comment, score_comment)) 


write_rds(df6, "data/nested_2015-2019.rds")
