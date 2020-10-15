#' Label categorical variables from PNADCovid19 datasets
#'
#' @import survey readr dplyr magrittr readxl
#' @param data_pnadcovid19 A tibble of PNAD-covid19 data read with \code{read_pnadcovid19} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: \url{ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/Dicionario_e_input.zip}
#' @return  A tibble with the data provided from PNADc survey and its categorical variables as factors with labels.
#' @examples
#' input_path <- pnadcovid19_example("input_example.txt")
#' data_path <- pnadcovid19_example("exampledata.txt")
#' dictionary.path <- pnadcovid19_example("dictionaryexample.xls")
#' pnadc.df <- read_pnadcovid19(data_path, input_path, vars="VD4002")
#' pnadc.df <- pnadcovid19_labeller(pnadc.df,dictionary.path)
#'
#' @export

pnadcovid19_labeller <- function(data_pnadcovid19,dictionary.file){
  require(magrittr)
  require(dplyr)
  dictionary <- readxl::read_excel(dictionary.file)
  X__3=X__6=X__7=NULL
  colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
  dictionary %<>% subset(!is.na(X__5))
  codcurrent <- dictionary$X__2
  for(i in 1:dim(dictionary)[1]){
    if(is.na(dictionary$X__2[i])){
      dictionary$X__2[i] <- codcurrent
    }
    else{
      codcurrent <- dictionary$X__2[i]
    }
  }
  notlabel <- c("Ano", "UPA", "Estrato", "V1008","V1031","V1030", "V1012","V1013",
                "A001B1","A001B2","A001","A001B3","A002","C0051","C0052","C0053",
                "C007E1","C007E2","C008","C009","C01011","C01012","C01021","C01022","C011A11",
                "C011A12","C011A21","C011A22","D0013","D0023","D0033","D0043","D0053","D0063","D0073",
                "F0021","F0022","F006",
                "V1014", "V1016", "posest", "V2003", "V2008", "V20081",
                "V20082", "V40081", "V40082", "V40083", "V4010", "V4013",
                "V4041", "V4044","V4075A1","VD4031","VD4035",
                "V401511","V401512","V40161","V40162","V40163","V401711",
                "V40181","V40182","V40183")
  vars <- names(data_pnadcovid19)
  #varsc <- vars[sapply(data_pnadcovid19, class) == "integer"]
  varsf <- setdiff(vars, notlabel)
  for (i in 1:length(varsf)) {
    if (varsf[i] %in% (dictionary$X__2)) {
      data_pnadcovid19[varsf[i]] <- factor(as.numeric(unlist(data_pnadcovid19[varsf[i]])),
                                     levels = suppressWarnings(
                                       as.numeric(unlist(dictionary %>%
                                              subset(X__2 == varsf[i]) %>%
                                              select(X__5)))),
                                     labels = unlist(dictionary %>%
                                              subset(X__2 == varsf[i]) %>%
                                              select(X__6)))
    }
  }
  return(data_pnadcovid19)
}
