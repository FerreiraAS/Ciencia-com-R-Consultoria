# read SCImago csv file (download from https://www.scimagojr.com)
scimago <-
  read.csv(
    "SJR/scimagojr 2021.csv",
    header = TRUE,
    sep = ";",
    quote = "\"",
    dec = ",",
    fill = TRUE
  )
