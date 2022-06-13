# read QUALIS xlsx file (download from https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf)
qualis <-
  data.frame(read_excel(
    "Qualis/QUALIS Referência 2017-2018.xlsx",
    sheet = 1,
    col_types = c("text")
  )
)