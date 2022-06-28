table.with.badges <-
  function(show.Altmetric = NULL,
           show.Dimensions = NULL,
           show.PlumX = NULL,
           show.SJR = NULL,
           show.Qualis = NULL,
           doi_unique,
           my_dois_works,
           scimago,
           qualis) {
    cat(
      "<style>.PlumX-Popup{display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;}</style>"
    )
    cat(
      "<style>.ppp-container ppp-medium{display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;}</style>"
    )
    cat(
      "<style>.plx-wrapping-print-link{display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;}</style>"
    )
    cat(
      "<style>.plx-print{display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;}</style>"
    )
    
    # start table
    cat(
      "<table class=\"tb\" style=\"width:100%; font-size: 16px !important;\">\n    <tr>\n      <th>Produtos (n = ",
      max(dim(doi_unique)[1], 0) + max(dim(my_dois_works)[1], 0),
      ") e Impactos (Altmetric^1^, Dimensions^2^, PlumX^3^, SJR^4^, Qualis^5^, Open Access^6^) </th>\n    </tr>",
      sep = ""
    )
    
    # print table with DOI and Altmetric
    if (max(dim(doi_unique)[1], 0) != 0) {
      for (ix in 1:dim(doi_unique)[1]) {
        # add bibliography info
        cat("<tr><td valign=top>")
        cat("<br>")
        cat(
          paste0(
            "[**",
            doi_unique$title[ix],
            "**](",
            doi_unique$url[ix],
            "){target=\"_blank\"}",
            "<br>"
          )
        )
        cat(doi_unique$author.names[ix])
        cat(paste0(
          "<br>",
          paste0(doi_unique$published_on[ix], "&nbsp; - &nbsp;")
        ))
        cat(paste0("*", doi_unique$journal[ix], "*", "<br>"))
        # initialize the DIV element for the badges
        cat("<div style=\"vertical-align: middle; display: inline-block;\">")
        
        # add Altmetric badge
        if (show.Altmetric == TRUE) {
          cat(
            "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" class=\"altmetric-embed\" data-badge-type=\"donut\" data-badge-popover=\"right\" data-doi=\"",
            doi_unique$doi[ix],
            "\"></a>",
            sep = ""
          )
        }
        
        # add Dimensions badge
        if (show.Dimensions == TRUE) {
          cat(
            "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" data-legend=\"hover-right\" class=\"__dimensions_badge_embed__\" data-doi=\"",
            doi_unique$doi[ix],
            "\" data-style=\"small_circle\"></a>",
            sep = ""
          )
        }
        
        # add PlumX badge
        if (show.PlumX == TRUE) {
          cat(
            "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.5em 0.3em 0.5em 0.3em;\" class=\"plumx-plum-print-popup\" href=\"https://plu.mx/plum/a/?doi=",
            doi_unique$doi[ix],
            "\" data-popup=\"right\" data-size=\"medium\" data-site=\"plum\"></a>",
            sep = ""
          )
        }
        
        # add SJR
        if (show.SJR == TRUE) {
          SJR_id <-
            scimago[grep(gsub("-", "", doi_unique$issn[ix]), scimago$Issn), 2][1]
          SJR <-
            scimago[grep(gsub("-", "", doi_unique$issn[ix]), scimago$Issn), 6][1]
          cat(
            "<a target=\"_blank\" href=\"https://www.scimagojr.com/journalsearch.php?q=",
            SJR_id,
            "&tip=sid&clean=0\" style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; color:white; border-color:rgb(216,124,78); background-color:rgb(216,124,78);\"> SJR <br>",
            sep = ""
          )
          cat(paste0(ifelse(
            identical(SJR, numeric(0)) |
              all(is.na(SJR)), "?", SJR
          ), "</a>"), sep = "")
        }
        
        # add QUALIS
        if (show.Qualis == TRUE) {
          cat(
            "<a style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; color:black;\"> Qualis <br>",
            sep = ""
          )
          WebQualis <-
            qualis[match(doi_unique$issn[ix], qualis$ISSN), 3]
          cat(paste0(ifelse(
            identical(WebQualis, numeric(0)) |
              all(is.na(WebQualis)),
            "?",
            WebQualis
          ), "</a>"), sep = "")
        }
        
        # add OPEN ACESS badge
        tryCatch(
          expr = {
            my_doi_oa <-
              roadoi::oadoi_fetch(dois = doi_unique$doi[ix], email = "arthur_sf@icloud.com")
            if (my_doi_oa$is_oa) {
              cat(
                "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.1em 0.3em 0.1em 0.3em;\" href=\"",
                doi_unique$url[ix],
                "\" target=\"_blank\">",
                "<img height=\"64px;\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/2/25/Open_Access_logo_PLoS_white.svg/256px-Open_Access_logo_PLoS_white.svg.png\">",
                "</a>",
                sep = ""
              )
            }
          },
          error = function(e) {
            
          }
        )
        
        cat("</div>")
        cat("</tr>")
      }
    }
    
    # print table with DOI but no Altmetric (== NA; donut ?)
    if (max(dim(my_dois_works)[1], 0) != 0) {
      for (ix in 1:dim(my_dois_works)[1]) {
        # add bibliography info
        cat("<tr><td valign=top>")
        cat("<br>")
        cat(
          paste0(
            "[**",
            my_dois_works$title[ix],
            "**](https://doi.org/",
            my_dois_works$doi[ix],
            "){target=\"_blank\"}",
            "<br>"
          )
        )
        cat(
          paste(
            my_dois_works$author[[ix]]$given,
            my_dois_works$author[[ix]]$family,
            collapse = ", "
          )
        )
        cat(paste0("<br>",
                   paste0(
                     ifelse(
                       !is.na(my_dois_works$issued[ix]),
                       substr(my_dois_works$issued[ix], 1, 4),
                       substr(my_dois_works$created[ix], 1, 4)
                     ),
                     "&nbsp; - &nbsp;"
                   )))
        cat(paste0("*", my_dois_works$container.title[ix], "*", "<br>"))
        
        # initialize the DIV element for the badges
        cat("<div style=\"vertical-align: middle; display: inline-block;\">")
        
        # add Altmetric badge
        if (show.Altmetric == TRUE) {
          cat(
            "<img style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" src=\"https://badges.altmetric.com/?score=?&types=????????\">",
            sep = ""
          )
        }
        
        # add Dimensions badge
        if (show.Dimensions == TRUE) {
          cat(
            "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em;\" data-legend=\"hover-right\" class=\"__dimensions_badge_embed__\" data-doi=\"",
            my_dois_works$doi[ix],
            "\" data-style=\"small_circle\"></a>",
            sep = ""
          )
        }
        
        # add PlumX badge
        if (show.PlumX == TRUE) {
          cat(
            "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em;\" class=\"plumx-plum-print-popup\" href=\"https://plu.mx/plum/a/?doi=",
            my_dois_works$doi[ix],
            "\" data-popup=\"right\" data-size=\"medium\" data-site=\"plum\"></a>",
            sep = ""
          )
        }
        
        # add SJR
        if (show.SJR == TRUE) {
          SJR_id <-
            scimago[grep(gsub("-", "", substr(my_dois_works$issn[ix], 1, 9)), scimago$Issn), 2][1]
          SJR <-
            scimago[grep(gsub("-", "", substr(my_dois_works$issn[ix], 1, 9)), scimago$Issn), 6][1]
          cat(
            "<a target=\"_blank\" href=\"https://www.scimagojr.com/journalsearch.php?q=",
            SJR_id,
            "&tip=sid&clean=0\" style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; color:white; border-color:rgb(216,124,78); background-color:rgb(216,124,78);\"> SJR <br>",
            sep = ""
          )
          cat(paste0(ifelse(
            identical(SJR, numeric(0)) |
              all(is.na(SJR)), "?", SJR
          ), "</a>"), sep = "")
        }
        
        # add QUALIS
        if (show.Qualis == TRUE) {
          cat(
            "<a style=\"border-radius:10%; border-style: solid; margin:0.1em 0.3em 0.1em 0.3em; padding:0.4em 0.3em 0.4em 0.3em; text-decoration:none; text-align: center; display:inline-block; float:left; color:black;\">  Qualis <br>",
            sep = ""
          )
          WebQualis <-
            qualis$ESTRATO[match(my_dois_works$issn[ix], qualis$ISSN)]
          cat(paste0(ifelse(
            identical(WebQualis, numeric(0)) |
              all(is.na(WebQualis)),
            "?",
            WebQualis
          ), "</a>"), sep = "")
        }
        
        # add OPEN ACESS badge
        tryCatch(
          expr = {
            my_doi_oa <-
              roadoi::oadoi_fetch(dois = my_dois_works$doi[ix], email = "arthur_sf@icloud.com")
            if (my_doi_oa$is_oa) {
              cat(
                "<a style=\"display: inline-block; float: left; margin:0.1em 0.3em 0.1em 0.3em; padding:0.1em 0.3em 0.1em 0.3em;\" href=\"",
                paste0("https://doi.org/", my_dois_works$doi[ix]),
                "\" target=\"_blank\">",
                "<img height=\"64px;\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/2/25/Open_Access_logo_PLoS_white.svg/256px-Open_Access_logo_PLoS_white.svg.png\">",
                "</a>",
                sep = ""
              )
            }
          },
          error = function(e) {
            
          }
        )
        
        cat("</div>")
        cat("</td></tr>")
      }
    }
    
    # end table
    cat("</table>")
    cat('<br>')
    cat('*Fontes:*', sep = "")
    cat('^1^ [**Altmetric**](https://www.altmetric.com)', sep = "")
    cat(', ', sep = "")
    cat('^2^ [**Dimensions**](https://www.dimensions.ai)', sep = "")
    cat(', ', sep = "")
    cat('^3^ [**PlumX**](https://plu.mx)', sep = "")
    cat(', ', sep = "")
    cat('^4^ [**SCImago**](https://www.scimagojr.com)', sep = "")
    cat(', ', sep = "")
    cat('^5^ [**WebQualis**](https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf)', sep = "")
    cat(', ', sep = "")
    cat('^6^ [**DOAJ**](https://doaj.org)', sep = "")
    cat('<br>')
    cat('<br>')
    cat('<br><a style="float:right" href="#top"><b>Início &nbsp;</b>⬆️</a><br>')
  }
cat('<br>')
