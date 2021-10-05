#################################################
# Super sophisticated economics Nobel predictor #
#################################################

# Grab all names from the EJMR Nobel prediction thread and sort them by by
# frequency. This is of course the most educated guess of who is going to win.
# It works with the website design of 2020, 2021.

# Thore Petersen
# Original version October 2020, updated October 2021

library(rvest)
library(stopwords)
library(stringr)
library(thoremisc) # devtools::install_github("thorepet/thoremisc")


# Define functions --------------------------------------------------------

grab_sites <- function(stump, n_pages) {
  # Scrape HTML of all pages in EJMR thread. First page of the thread is given
  # as stump.
  
  urls <- as.list(paste0(stump, "/page/", 2:n_pages))
  sites <- lapply(urls, read_html)
  
  return(sites)
}

grab_posts <- function(site) {
  # Grab post-object from scraped HTML.
  
  posts <- site %>%
    html_nodes(xpath = "//div[@class='threadpost']")
  
  return(posts)
}

grab_text <- function(post) {
  # Grab post text from post objects.
  
  t <- post %>%
    html_node(xpath = ".//p") %>%
    html_text
  
  return(t)
}


# Scrape ------------------------------------------------------------------

# # 2020
# url <- "https://www.econjobrumors.com/topic/official-economics-nobel-prize-2020-predictions-thread"

# 2021, there are two active threads
url <- c(
  "https://www.econjobrumors.com/topic/2021-economics-nobel-prize",
  "https://www.econjobrumors.com/topic/official-economics-nobel-prize-2021-predictions-thread"
)

texts <- character(0)
for(u in url) {
  # grab HTML of first page
  site <- read_html(u)
  
  # extract total number of pages
  n_pages <- site %>%
    html_nodes(xpath = "//a[@class='page-numbers']") %>%
    html_text() %>%
    as.numeric %>%
    max
  
  # grab all thread pages' HMTL 
  sites <- grab_sites(u, n_pages)
  sites <- c(list(site), sites)
  
  # extract all posts and their text
  posts <- lapply(sites, grab_posts)
  posts <- lapply(posts, grab_text)
  texts <- c(texts, unlist(posts))
}


# Text analysis -----------------------------------------------------------

t <- str_split(texts, "\\s", simplify = TRUE)
t <- string_clean(t)
t <- t[!is.na(t)]
# split again after cleaning to capture some hidden names
t <- str_split(t, "\\s", simplify = TRUE)

s <- stopwords(language = "en", source = "snowball")

# remove stopwords
t <- t[!(t %in% s)]

# common irrelevant words
remove_patterns <- c(
  "nobel", "prize\\w*", "committee\\w*", "econ\\w*", "list\\w*", "year\\w*", 
  "cit[ae]\\w*", "contribut\\w*", "field\\w*", "think\\w*", "going\\w*", 
  "time\\w*", "more", "john", "best", "choice", "deserve\\w*", "give\\w*",
  "google", "like\\w*", "make\\w*", "maybe", "paper\\w*", "know\\w*", "many",
  "guy\\w*", "research\\w*", "last", "trade", "good", "just", "probably", 
  "also", "public", "candidate", "debate", "decade", "discipline", "fact", 
  "general", "influen\\w*", "outside", "point", "social", "strong\\w*", 
  "article\\w*", "book\\w*", "chapter\\w*", "consider\\w*", "does\\w*",
  "contender\\w*", "great\\w*", "journal\\w*", "publish\\w*", "science\\w*",
  "need\\w*", "people", "\\w*educate\\w*", "award\\w*", "care\\w*", 
  "alternative\\w*", "already", "believe\\w*", "activit\\w*", "whoever",
  "guess\\w*", "demand", "empiric\\w*", "firm\\w*", "industr\\w*", "production",
  "discuss\\w*", "obligat\\w*", "break\\w*", "top", "two", "win\\w*", "range", 
  "get\\w*", "one", "another", "betting", "banned", "corrupt\\w*", "fingers",
  "expectation\\w*", "may", "number", "author\\w*", "weight\\w*", "impact\\w*",
  "old", "three", "weird", "never", "comb\\w*", "use\\w*", "hug", "factor\\w*",
  "werden", "complete\\w*", "coming", "distinct\\w*", "dude\\w*", "else", 
  "except", "experiment\\w*", "four", "idea", "hard", "laureate", "less", 
  "heard", "new", "next", "nobody", "obvious\\w*", "rank\\w*", "https", "won",
  "first", "now", "theor\\w*", "business", "development", "invent\\w*", 
  "start\\w*", "account\\w*", "credib\\w*", "wait\\w*", "suggest\\w*", 
  "retir\\w*", "profess\\w*", "possib\\w*", "phd", "other\\w*", "candidate\\w*",
  "public\\w*", "let", "male\\w*", "mean\\w*", "postpone", "pointless", "repec",
  "ready", "crazy", "interest\\w*", "recursive", "score\\w*", "second", "see",
  "simple"
)
remove_patterns <- paste0("\\b", remove_patterns, "\\b", collapse = "|")
t <- str_remove_all(t, remove_patterns)

t <- str_remove_all(t, "\\d+")
t <- string_redund_ws(t)
t <- t[str_length(t) > 2]

# make it a contingency table
predictions <- as.data.frame(table(t))
colnames(predictions) <- c("probably_name", "n")
predictions <- predictions[order(predictions$n, decreasing = TRUE), ]

View(predictions)
