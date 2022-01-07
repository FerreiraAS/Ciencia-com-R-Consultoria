# Required metadata for content tracking
# Source: https://help.altmetric.com/support/solutions/articles/6000240582-required-metadata-for-content-tracking

if(length(dois_2_meta) !=0){
  for(j in 1:length(dois_2_meta)){
    if(!is.na(dois_2_meta[j])){
      cat('<head>')
      cat('<meta name="citation_doi" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="Doi" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="DC.Identifier" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="DC.DOI" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="DC.Identifier.DOI" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="DOIs" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="bepress_citation_doi" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('<meta name="rft_id" content="doi:', dois_2_meta[j], '"/>', sep = "")
      cat('</head>')
    }
  } 
}