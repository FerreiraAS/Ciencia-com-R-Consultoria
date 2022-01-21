# Getting Altmetrics Data Using R
# by Markus Wust, Alison Blaine, and Erica Hayes
# adapted by Arthur de Sá Ferreira

#### Section 1. Setting up Your R Session #####

# Step 1. Install the following packages if not already installed. Otherwise, skip this step.
require("rAltmetric")
require("plyr")
require("tidyverse")

# Step 2. Load the libraries into your R Session.
library("rAltmetric")
library("plyr")
library("tidyverse")

# Step 6. Create a function to test whether the doi leads to an actual article.
# If no data is returned, assign the value NA.
getArticleData <- function(x) {
  articleData <-
    try(altmetrics(doi = x), silent = TRUE)
  # looks up altmetrics for the doi
  if (class(articleData) == 'try-error') {
    # if error is returned, give it the value NA
    return(NA)
  }
  articleData
}

# Step 7. Get the raw_metrics data from every doi in the dois_list dataset.
raw_metrics <-
  lapply(dois_list, function(x)
    getArticleData(x))  # apply getArticleFunction to dois_list data.

# Step 8. Notice that there are lots of dois with NA values. Now write a function to remove all those dois that have NA values (because they have no data).
# This code comes from this source on github : https://gist.github.com/rhochreiter/7029236
na.omit.list <- function(y) {
  return(y[!sapply(y, function(x)
    all(is.na(x)))])
}

no_altmetric_dois_list <- dois_list[is.na(raw_metrics)]

# Step 9. You haven't actually removed the values yet. Now remove those NA values by passing the raw_metrics
# data into the function just created, na.omit.list().
raw_metrics <- na.omit.list(raw_metrics)

# Step 10. Now use ldply() to apply the altmetric_data function to get the actual data and return results as a data frame.
metric_data <- ldply(raw_metrics, altmetric_data)

#### Section 3. Data Cleaning, Subsetting and Filtering ####

# Step 11. Specify a list of the columns you want for your analysis. (issns1 -> print; issns2 -> online)
columns_to_grab <-
  c(
    "title",
    "doi",
    "url",
    "score",
    "journal",
    "issns1",
    "issns2",
    "issns",
    "cited_by_fbwalls_count",
    "cited_by_posts_count",
    "cited_by_policies_count",
    "cited_by_wikipedia_count",
    "cited_by_feeds_count",
    "cited_by_gplus_count",
    "cited_by_msm_count",
    "cited_by_tweeters_count",
    "cited_by_accounts_count",
    "last_updated"
  )
columns_to_grab <-
  c("title",
    "doi",
    "url",
    "score",
    "journal",
    "issns1",
    "issns2",
    "issns",
    "last_updated")

# Step 12. Create a data subset only including columns specified in Step 9.
subset_data <- select(metric_data, one_of(columns_to_grab))

doi_reshaped_data <- subset_data

# # Step 13. Clean up and rename social media categories.
# doi_reshaped_data <- subset_data %>%
#  gather(cited_by, times, cited_by_fbwalls_count:cited_by_accounts_count) %>% # collapses range of columns into two
#  mutate(cited_by = gsub("_count", "", cited_by)) %>%
#  mutate(cited_by = gsub("cited_by_", "", cited_by)) %>%
#  mutate(cited_by = gsub("tweeters", "Twitter", cited_by)) %>%
#  mutate(cited_by = gsub("fbwalls", "Facebook", cited_by)) %>%
#  mutate(cited_by = gsub("gplus", "Google+", cited_by)) %>%
#  mutate(cited_by = gsub("feeds", "Bloggers", cited_by)) %>%
#  mutate(cited_by = gsub("msm", "News Outlets", cited_by)) %>%
#  mutate(cited_by = gsub("posts", "Posts", cited_by)) %>%
#  mutate(cited_by = gsub("accounts", "Total", cited_by)) %>%
#  mutate(cited_by = gsub("policies", "Policies", cited_by)) %>%
#  mutate(cited_by = gsub("wikipedia", "Wikipedia", cited_by)) %>%
#  mutate(times = as.numeric(times))

year_publ <-
  as.Date(as.POSIXct(as.numeric(metric_data$last_updated), origin = "1970-01-01"))

author.names <- c()
for (k in 1:dim(metric_data)[1]) {
  names <-
    paste(
      na.omit(metric_data$authors1[k]),
      na.omit(metric_data$authors2[k]),
      na.omit(metric_data$authors3[k]),
      na.omit(metric_data$authors4[k]),
      na.omit(metric_data$authors5[k]),
      na.omit(metric_data$authors6[k]),
      na.omit(metric_data$authors7[k]),
      na.omit(metric_data$authors8[k]),
      na.omit(metric_data$authors9[k]),
      na.omit(metric_data$authors10[k]),
      na.omit(metric_data$authors11[k]),
      na.omit(metric_data$authors12[k]),
      na.omit(metric_data$authors13[k]),
      na.omit(metric_data$authors14[k]),
      na.omit(metric_data$authors15[k]),
      sep = ", ",
      collapse = ""
    )
  names <- gsub(", , ", "", names)
  author.names <- c(author.names, names)
}

# convert score to integer
doi_reshaped_data$score <-
  ceiling(as.numeric(doi_reshaped_data$score))
doi_reshaped_data$last_updated <-
  as.numeric(format(year_publ, "%Y"))
doi_reshaped_data$author.names <- author.names

# merge at least one ISSN to each journal to search for in the CSV provided by SCImago
# initialize with lastest ISSN vector
issn <- doi_reshaped_data$issns
# try replacing with first issn
issn[is.na(issn)] <- doi_reshaped_data$issns1[is.na(issn)]
# try replacing with second issn
issn[is.na(issn)] <- doi_reshaped_data$issns2[is.na(issn)]

doi_reshaped_data$issn <- issn
