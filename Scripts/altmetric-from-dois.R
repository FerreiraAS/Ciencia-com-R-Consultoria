# Getting Altmetrics Data Using R
# by Markus Wust, Alison Blaine, and Erica Hayes
# adapted by Arthur de Sá Ferreira

# initialize output vectores
doi_reshaped_data <- c()
doi_sort <- c()

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

no_altmetric_dois_list <- dois_list[is.na(raw_metrics)]

# Step 8. Notice that there are lots of dois with NA values. Now write a function to remove all those dois that have NA values (because they have no data).
# This code comes from this source on github : https://gist.github.com/rhochreiter/7029236
na.omit.list <- function(y) {
  return(y[!sapply(y, function(x)
    all(is.na(x)))])
}

# Step 9. You haven't actually removed the values yet. Now remove those NA values by passing the raw_metrics
# data into the function just created, na.omit.list().
raw_metrics <- na.omit.list(raw_metrics)

if (!is_empty(raw_metrics)) {
  # Step 10. Now use ldply() to apply the altmetric_data function to get the actual data and return results as a data frame.
  metric_data <- ldply(raw_metrics, altmetric_data)
  
  #### Section 3. Data Cleaning, Subsetting and Filtering ####
  # Step 11. Specify a list of the columns you want for your analysis. (issns1 -> print; issns2 -> online)
  columns_to_grab <-
    c(
      "title",
      "doi",
      "url",
      "issns",
      "journal",
      "issns1",
      "issns2",
      "published_on",
      "last_updated",
      "cited_by_posts_count",
      "cited_by_tweeters_count",
      "cited_by_fbwalls_count",
      "cited_by_accounts_count",
      "cited_by_videos_count",
      "cited_by_msm_count",
      "cited_by_feeds_count",
      "cited_by_patents_count",
      "score"
    )
  
  # Step 12. Create a data subset only including columns specified in Step 9.
  subset_data <- select(metric_data, one_of(columns_to_grab))
  
  # Step 13. Clean up and rename social media categories.
  doi_reshaped_data <- subset_data
  
  # https://en.wikipedia.org/wiki/Unix_time
  # The Unix epoch is 00:00:00 UTC on 1 January 1970 (an arbitrary date)
  if(!is.null(doi_reshaped_data$published_on)){
    year_publ <-
      format(as.Date(
        as.POSIXct(as.numeric(doi_reshaped_data$published_on), origin = "1970-01-01")
      ), format = "%Y")
  } else {
    year_publ <-
      format(as.Date(
        as.POSIXct(as.numeric(doi_reshaped_data$last_updated), origin = "1970-01-01")
      ), format = "%Y")
  }
  doi_reshaped_data$published_on <- year_publ
  
  author.names <- c()
  for (k in 1:dim(doi_reshaped_data)[1]) {
    names <-
      # remove the comma and reorder the author name (name-surname)
      c(
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors1[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors2[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors3[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors4[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors5[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors6[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors7[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors8[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors9[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors10[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors11[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors12[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors13[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors14[k])
        ),
        sub(
          '(.*)\\,\\s+(.*)',
          '\\2 \\1',
          na.omit(metric_data$authors15[k])
        )
      )
    # collapse the authors' names vector into an atomic vector string
    names <- paste(unique(names[names != ""]), collapse = ", ")
    author.names <- c(author.names, names)
  }
  
  # filling with output data

  # convert score to integer
  doi_reshaped_data$score <-
    ceiling(as.numeric(doi_reshaped_data$score))
  doi_reshaped_data$author.names <- author.names
  
  # merge at least one ISSN to each journal to search for in the CSV provided by SCImago
  issns <- matrix(NA, nrow = dim(doi_reshaped_data)[1])
  # initialize with latest ISSN vector if available
  if(length(doi_reshaped_data$issns) != 0){
    issns <- doi_reshaped_data$issns
  }
  # try replacing with first issn (print)
  if(length(doi_reshaped_data$issns1) != 0){
    issns[is.na(issns)] <- doi_reshaped_data$issns1[is.na(issns)]
  }
  # try replacing with second issn (online)
  if(length(doi_reshaped_data$issns2) != 0){
    issns[is.na(issns)] <- doi_reshaped_data$issns2[is.na(issns)]
  }  
  doi_reshaped_data$issn <- issns
  
  # remove duplicate entries
  doi_unique <-
    doi_reshaped_data[!duplicated(doi_reshaped_data$doi),]
  
  # sort columns by title
  doi_sort <-
    doi_unique[order(doi_unique$title),]
}