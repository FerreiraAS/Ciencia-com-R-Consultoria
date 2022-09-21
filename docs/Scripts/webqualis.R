# read QUALIS xlsx file (download from https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf)
qualis <-
  data.frame(
    read_excel(
      file.path(
       "Qualis",
       "relatorio_producao_intelectual_Qualis_Periódicos.xlsx"
      ),
      sheet = 1,
      col_types = c("text")
    )
  )