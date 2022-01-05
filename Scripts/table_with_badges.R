# read packages
library("usethis")
library("anytime")
library("httpuv")
library("lubridate")
library("janitor")
library("rcrossref")
library("dplyr")

# start table
cat("<table style=\"width:100%\">\n    <tr>\n      <th>Artigos (n = ", dim(doi_sort)[1] + ifelse(length(my_dois_works) != 0, dim(my_dois_works)[1], 
	0), ") e Impactos (Altmetric^1^, Dimensions^2^, PlumX^3^, SJR^4^, Qualis^5^) \n\n </th>\n    </tr>", sep = "")

# Add badges and links
for (i in 1:dim(doi_sort)[1]) {
	# add bibliography info
	cat("<tr><td valign=top>")
	cat("<br>")
	cat(paste0("[**", doi_sort$title[i], "**](", doi_sort$url[i], "){target=\"_blank\"}", "<br>"))
	cat(doi_sort$author.names[i])
	cat(paste0("<br>", paste0(doi_sort$published_on[i], "&nbsp; - &nbsp;")))
	cat(paste0("*", doi_sort$journal[i], "*", "<br>"))
	# initialize the DIV element for the badges
	cat("<div style=\"vertical-align: middle;\">")
	# add Altmetric badge
	cat("<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" class=\"altmetric-embed\" data-badge-type=\"donut\" data-badge-popover=\"right\" data-doi=\"", 
		doi_sort$doi[i], "\"></a>", sep = "")
	# add Dimensions badge
	cat("<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" data-legend=\"hover-right\" class=\"__dimensions_badge_embed__\" data-doi=\"", 
		doi_sort$doi[i], "\" data-style=\"small_circle\"></a>", sep = "")
	# add PlumX badge
	cat("<style>.PlumX-Popup{display: inline-block; float: left;}</style>")
	cat("<style>.ppp-container{display: inline-block; float: left;}</style>")
	cat("<style>.ppp-medium{display: inline-block; float: left;}</style>")
	cat("<style>.plx-print{display: inline-block; float: left;}</style>")
	cat("<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.5em 0.3em 0.5em 0.3em;\" class=\"plumx-plum-print-popup\" href=\"https://plu.mx/plum/a/?doi=", 
		doi_sort$doi[i], "\" data-popup=\"right\" data-size=\"medium\" data-site=\"plum\"></a>", sep = "")
	# add SJR
	SJR_id <- scimago[grep(gsub("-", "", doi_sort$issn[i]), scimago$Issn), 2]
	cat("<a target=\"_blank\" href=\"https://www.scimagojr.com/journalsearch.php?q=", SJR_id, "&tip=sid&clean=0\" style=\"border-radius:10%; margin:0.1em 0.3em 0.1em 0.3em; padding:0.5em 0.3em 0.5em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; font-size:1.1em; color:white; background-color:rgb(216,124,78);\"> SJR <br>", 
		sep = "")
	SJR <- scimago[grep(gsub("-", "", doi_sort$issn[i]), scimago$Issn), 6]
	cat(paste0(ifelse(length(SJR) != 0 | all(!is.na(SJR)), SJR, "?"), "</a>"), sep = "")
	# add QUALIS
	cat("<a style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; font-size:1.1em; color:black;\"> Qualis <br>", 
		sep = "")
	WebQualis <- qualis[match(doi_sort$issn[i], qualis$ISSN), 3]
	cat(paste0(ifelse(length(WebQualis) != 0 | all(!is.na(WebQualis)), WebQualis, "?"), "</a>"), sep = "")
	cat("</div>")
	cat("</tr>")
}

# print table with DOI but no Altmetric (== NA) (donut ?)

if (length(my_dois_works) != 0) {
	for (i in 1:dim(my_dois_works)[1]) {
		# add bibliography info
		cat("<tr><td valign=top>")
		cat("<br>")
		cat(paste0("[**", my_dois_works$title[i], "**](https://doi.org/", my_dois_works$doi[i], "){target=\"_blank\"}", "<br>"))
		cat(paste(my_dois_works$author[[i]]$given, my_dois_works$author[[i]]$family, collapse = ", "))
		cat(paste0("<br>", paste0(format(as.POSIXct(my_dois_works$deposited[i]), format = "%Y"), "&nbsp; - &nbsp;")))
		cat(paste0("*", my_dois_works$container.title[i], "*", "<br>"))

		# initialize the DIV element for the badges
		cat("<div style=\"vertical-align: middle;\">")
		# add Altmetric badge
		cat("<img style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" src=\"https://badges.altmetric.com/?score=?&types=????????\">", 
			sep = "")
		# add Dimensions badge
		cat("<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" data-legend=\"hover-right\" class=\"__dimensions_badge_embed__\" data-doi=\"", 
			my_dois_works$doi[i], "\" data-style=\"small_circle\"></a>", sep = "")
		# add PlumX badge
		cat("<style>.PlumX-Popup{display: inline-block; float: left;}</style>")
		cat("<style>.ppp-container{display: inline-block; float: left;}</style>")
		cat("<style>.ppp-medium{display: inline-block; float: left;}</style>")
		cat("<style>.plx-print{display: inline-block; float: left;}</style>")
		cat("<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.5em 0.3em 0.5em 0.3em;\" class=\"plumx-plum-print-popup\" href=\"https://plu.mx/plum/a/?doi=", 
			my_dois_works$doi[i], "\" data-popup=\"right\" data-size=\"medium\" data-site=\"plum\"></a>", sep = "")
		# add SJR
		SJR_id <- scimago[grep(gsub("-", "", substr(my_dois_works$issn[i], 1, 9)), scimago$Issn), 2]
		cat("<a target=\"_blank\" href=\"https://www.scimagojr.com/journalsearch.php?q=", SJR_id, "&tip=sid&clean=0\" style=\"border-radius:10%; margin:0.1em 0.3em 0.1em 0.3em; padding:0.5em 0.3em 0.5em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; font-size:1.1em; color:white; background-color:rgb(216,124,78);\"> SJR <br>", 
			sep = "")
		SJR <- scimago[grep(gsub("-", "", substr(my_dois_works$issn[i], 1, 9)), scimago$Issn), 6]
		cat(paste0(ifelse(length(SJR) != 0 | all(!is.na(SJR)), SJR, "?"), "</a>"), sep = "")

		# add QUALIS
		cat("<a style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; font-size:1.1em; color:black;\">  Qualis <br>", 
			sep = "")
		WebQualis <- qualis$ESTRATO[match(my_dois_works$issn[i], qualis$ISSN)]
		cat(paste0(ifelse(length(WebQualis) != 0 | all(!is.na(WebQualis)), WebQualis, "?"), "</a>"), sep = "")
		cat("</div>")
		cat("</td></tr>")
	}
}
# end table
cat("</table>")
