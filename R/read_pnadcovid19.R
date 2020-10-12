#' Read PNAD-COVID19 microdata
#' @import readr dplyr magrittr
#' @param microdata A text file containing microdata from PNAD-COVID19 survey. The file must be downloaded from \url{ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/}
#' @param vars Character vector of the name of the variables you want to keep for analysys. \code{default} is to keep all variables
#' @return A tibble with the survey design variables and selected variables.
#' @examples
#' input_path <- pnadcovid19_example("input_example.xlsx")
#' data_path <- pnadcovid19_example("exampledata.txt")
#' pnadc.df <- read_pnadcovid19(data_path,input_path,vars="VD4002")
#' @export

read_pnadcovid19 <- function(microdata,vars=NULL) {

    ########## reading data
    data_pnadcovid19 <- suppressWarnings(readr::read_csv(microdata))
    
    input <- names(data_pnadcovid19)  
  ########## creating columns specification
  if(!is.null(vars)){
    if(any(!(vars %in% input))) {
      missvar=vars[!(vars %in% input)]
      warning(paste("Variables", paste(missvar,collapse = ", "), "not present in dataset\n"))
      }
    data_pnadcovid19 %<>% select("UPA","Estrato","V1027","posest","V1029","V1031","V1030",vars)
  }
  
  return(data_pnadcovid19)
}
