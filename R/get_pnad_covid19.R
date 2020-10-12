#' Download, label and create design object for PNAD COVID-19 microdata
#' @description Core function of package. with this function only, the user can download a PNADc microdata from a year or month and get a design object ready to use with \code{survey} package functions.
#' @import  readr dplyr magrittr RCurl utils timeDate
#' @param year The year of the data to be downloaded. Must be a number between 2020 and current year. Vector not accepted.
#' @param month The month of the year of the data to be downloaded. Must be number from 1 to 12. Vector not accepted. If \code{NULL}, \code{interview} number must be provided.
#' @param interview The interview number of the data to be downloaded. Must be number from 1 to 5. Vector not accepted. Using this option will get annual data. If \code{NULL}, monthly data will be downloaded instead.
#' @param vars Character vector of the name of the variables you want to keep for analysys. \code{default} is to keep all variables
#' @param labels \code{logical}. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary. Not available for annual data.
#' @param savedir Directory for dowloading data. \code{default} is to use a temporary directory.
#' @param design \code{logical}. If \code{TRUE}, \code{get_pnadcovid19} will return a object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @return An object of class \code{survey.design} with the data from PNADc survey and its sample design or a tibble with the survey design variables and selected variables.
#' @examples
#' \dontrun{
#' pnadcovid19.svy <- get_pnadcovid19(4,2020)
#' pnadcovid19.svy2 <- get_pnadcovid19(6,2020,vars=c("VD4001","VD4002"))
#' survey::svymean(~VD4002, pnadcovid19.svy2, na.rm=TRUE)
#' }
#'
#' @export
#'
get_pnadcovid19 <- function (year, month = NULL, vars = NULL, interview = NULL,
                       labels = T, design = T, savedir = tempdir())
{
  if (is.null(month) & is.null(interview))
    stop("Month or Interview number must be provided.")
  if (!is.null(month) & !is.null(interview))
    stop("Must be provided ONLY month number OR interview number.")
  if (year < 2020)
    stop("Year must be greater or equal to 2020.")
  if (year > timeDate::getRmetricsOptions("currentYear"))
    stop("Year can't be greater than current year.")
  if (!is.null(month)) {
    if (month > 12 | month < 1)
      stop("month must be a integer from 1 to 12.")
    ftpdir = ("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/")
    ftpdata <- paste0(ftpdir,"Dados", "/")
    #ftpdata <- ftpdir
    datayear <- gsub(".*ftp.*ftp.*:\\d+ ","",unlist(strsplit(RCurl::getURL(ftpdata, dirlistonly = FALSE),"\n")))
    dataname <- datayear[datayear == paste0("PNAD_COVID_",
                                                           sprintf("%02d",month), year,".zip")]
    if (length(dataname) == 0) {
      stop("Data unavailable for selected month/year")
    }
    docfiles = gsub(".*ftp.*ftp.*:\\d+ ","", unlist(strsplit(RCurl::getURL(paste0(ftpdir,"Documentacao/"), dirlistonly = F), "\n")))
    inputfile = docfiles[1+which(datayear == paste0("PNAD_COVID_",sprintf("%02d",month),year,".zip"))]
    utils::download.file(paste0(ftpdir, "Documentacao/",
                                inputfile), paste0(savedir,"/",inputfile))
    # utils::unzip(paste0(savedir, dataname), exdir = savedir)
    input_txt <- paste0("PNAD_COVID_",sprintf("%02d",month),year,".csv")
    utils::download.file(paste0(ftpdata, dataname), paste0(savedir,
                                                           "/", dataname))
    utils::unzip(paste0(savedir, "/", dataname), exdir = savedir)
    microdataname <- paste0("PNAD_COVID_",sprintf("%02d",month),
                            year)
    tmpfiles <- dir(savedir,".csv")
    print(tmpfiles)
    microdatadir <- paste0(savedir,"/",tmpfiles[substr(tmpfiles, 1, 12) == microdataname])
    data_pnadcovid19 <- read.csv(paste0(savedir,"/",input_txt))
    
    if (labels == T) {
      dicnames <- dir(savedir, pattern = paste0("Dicionario_PNAD_COVID_",sprintf("%02d",month),year,".xls"))
      print(dicnames)
      dicfile <- paste0(savedir, "/", dicnames[1])
      data_pnadcovid19 <- pnadcovid19_labeller(data_pnadcovid19, dicfile)
    }
  }
  if (!is.null(interview)) {
    if (interview > 5 | interview < 1)
      stop("interview must be a integer from 1 to 5.")
    ftpdir = ("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/")
    ftpdata <- paste0(ftpdir, "Dados/")
    datayear <- unlist(strsplit(gsub("\r\n","\n",RCurl::getURL(ftpdata, dirlistonly = TRUE)),
                                "\n"))
    yrint_list <- regmatches(datayear, gregexpr("[[:digit:]]+",
                                                datayear))
    dataname = NULL
    for (i in 1:length(datayear)) {
      if (as.numeric(yrint_list[[i]])[1] == year & as.numeric(yrint_list[[i]])[2] ==
          interview) {
        dataname = datayear[i]
      }
    }
    if (length(dataname) == 0) {
      stop("Data unavailable for selected interview/year")
    }
    docfiles <- unlist(strsplit(gsub("\r\n","\n",RCurl::getURL(paste0(ftpdir,
                                                                      "Documentacao/"), dirlistonly = F)),
                                "\n"))
    if (year < 2015) {
      input_pre <- paste0("Input_PNADC_", interview, "_visita_2012_a_2014")
    }
    else {
      input_pre <- paste0("Input_PNADC_", interview, "_visita_",
                          year)
    }
    input_txt = docfiles[which(startsWith(docfiles, input_pre))]
    utils::download.file(paste0(ftpdata, dataname), paste0(savedir,
                                                           "/", dataname))
    utils::download.file(paste0(ftpdir, "Documentacao/",
                                input_txt), paste0(savedir, "/", input_txt))
    utils::unzip(paste0(savedir, "/", dataname), exdir = savedir)
    microdataname <- paste0(savedir, "/", dataname)
    data_pnadcovid19 <- read_pnadc(microdataname, paste0(savedir,
                                                   "/", input_txt), vars = vars)
    if(labels==T){
       dicnames <- unlist(strsplit(RCurl::getURL(paste0(ftpdir,"Documentacao/"), dirlistonly = TRUE),"\r\n"))
       dicnames <- dicnames[unlist(substr(dicnames,1,3))=="dic"]
       matches <- regmatches(dicnames, gregexpr("[[:digit:]]+", dicnames))
       for(i in 1:length(dicnames)){
         if(as.numeric(matches[[i]])[1]==interview){
           if(length(matches[[i]])==3){
             if(as.numeric(matches[[i]])[2]==year){
               dicfile <- paste0(savedir,"/Dic_",interview,year,".xls")
               utils::download.file(paste0(ftpdir,"Documentacao/",dicnames[i]),dicfile)
             }
           }
           if(length(matches[[i]])>3){
             if(as.numeric(matches[[i]])[2]<=year & as.numeric(matches[[i]])[3]>=year){
               dicfile <- paste0(savedir,"/Dic_",interview,year,".xls")
               utils::download.file(paste0(ftpdir,"Documentacao/",dicnames[i]),dicfile)
             }
           }
        }
      }
    data_pnadcovid19 <- pnadcovid19_labeller(data_pnadcovid19,dicfile)
    }
  }
if (design == TRUE) {
    data_pnadcovid19 = pnadcovid19_design(data_pnadcovid19)
  }
  return(data_pnadcovid19)
}
