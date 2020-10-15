#' Create pnadcovid19 survey object utilizing its sampling design for analysis with survey package
#'
#' @import survey readr dplyr magrittr
#' @param data_pnadcovid19 A tibble of PNAD-COVID19 data read with \code{read_pnadcovid19} function.
#' @return An object of class \code{survey.design} with the data from PNAD-COVID19 survey and its sample design.
#' @examples
#'
#' #Using data read from disk
#' input_path <- pnadcovid19_example("input_example.txt")
#' data_path <- pnadcovid19_example("exampledata.txt")
#' pnadcovid19.df <- read_pnadcovid19(data_path, input_path, vars="VD4002")
#' dictionary.path <- pnadcovid19_example("dictionaryexample.xls")
#' pnadcovid19.df <- pnadcovid19_labeller(pnadcovid19.df,dictionary.path)
#' \dontrun{
#' pnadcovid19.svy <- pnadcovid19_design(pnadcovid19.df)
#' #Calculating unemployment rate
#' survey::svymean(~VD4002, pnadcovid19.svy, na.rm=TRUE)}
#'
#' #Downloading data
#' \dontrun{
#' pnadcovid19.df2<- get_pnadc(2,2020,vars="VD4002")
#' pnadcovid19.df2 <- pnadcovid19_labeller(pnadcovid19.df2,dictionary.path)
#' pnadcovid19.svy2 <- pnadcovid19_design(pnadcovid19.df2)
#' #Calculating unemployment rate
#' survey::svymean(~VD4002, pnadcovid19.svy2, na.rm=TRUE)}
#' @export


pnadcovid19_design <- function(data_pnadcovid19) {
  options(survey.lonely.psu="adjust")
  data_pre <- survey::svydesign(ids     = ~UPA,
                              strata  = ~Estrato,
                              data    = data_pnadcovid19,
                              weights = ~V1031,
                              nest    = T)
########## defining total for poststratification
popc.types <- data.frame(posest = as.character(unique(data_pnadcovid19$posest)),
                         Freq   = as.numeric(unique(data_pnadcovid19$V1030)))
popc.types <- (popc.types[order(popc.types$posest),])
########## creating final desing object w/ poststratification
data_pos <- survey::postStratify(design     = data_pre,
                                 strata     = ~posest,
                                 population = popc.types)

return(data_pos)

}
