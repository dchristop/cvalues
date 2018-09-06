#' @title Read data and create map of variables
#' @description
#' Read World Values Survey data in R, SPSS and STATA format and create variable map
#' @param rin Rdata file
#' @param spssin SPSS sav file
#' @param statain STATA dta file
#' @return NULL
#' @importFrom openxlsx write.xlsx
#' @importFrom foreign read.spss
#' @importFrom readstata13 read.dta13
#' @examples 
#' #Supposed that you have downloaded from
#' # http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp next raw data files:
#' #"WV6_Data_R_v_2016_01_01.rdata","WV6_Data_spss_v_2016_01_01.sav","WV6_Stata_v_2016_01_01.dta"
#; #then you can execute next commands:
#' #read_data_create_map(rin="WV6_Data_R_v_2016_01_01.rdata",
#' #spssin = "WV6_Data_spss_v_2016_01_01.sav",statain="WV6_Stata_v_2016_01_01.dta")
#' #Or for just the R data use next:
#' #read_data_create_map(rin="WV6_Data_R_v_2016_01_01.rdata")
#' @export
read_data_create_map <-
function(rin=NULL,spssin=NULL,statain=NULL){
  #Function to read data, create map for all variable answers and store data frames as rds files
  #Read data sets in Rdata (R), sav (SPSS) and dta (STATA) format, under request
  #
  ######Check arguments:
  #
  if(all(is.null(c(rin,spssin,statain)))){
    stop('You must provide at least one data set as input data.')
  }
  #
  if(!(is.null(rin))){useR=TRUE}else{useR=FALSE}
  #
  #
  if(!(is.null(spssin))){useSPSS=TRUE}else{useSPSS=FALSE}
  #
  #
  if(!(is.null(statain))){useSTATA=TRUE}else{useSTATA=FALSE}
  #
  #
  ###################################
  ################# Work with R data
  ###################################
  #
  if(useR){ 
  #
  cat('R','\n')
  cat('------','\n')
  T1=Sys.time()
  #
  ######################Read Rdata
  #
  yy=load(rin)
  names(yy)=yy
  yin=yy[names(yy)!=".Traceback"]
  dr=eval(parse(text=yin))
  #
  cat(paste0('Data file "',rin,'" was read'),'\n')
  #
  ###################################################################
  ############### Create a map of possible answers for all variables:
  ###################################################################
  #
  #Create list:
  drlist=as.list(dr)
  vnamesall=names(drlist)
  vmaprall=mapply(function(x,y){tx=as.data.frame.table(table(x));colnames(tx)=c(y,'Freq');return(tx)},drlist,vnamesall,SIMPLIFY = FALSE)
  #Save in excel:
  write.xlsx(vmaprall,'MapOfAllVariables_R.xlsx')
  #Save in rds:
  saveRDS(vmaprall,'mapr.rds')
  cat(paste0('Map of all possible answers for all variables was stored in files "MapOfAllVariables_R.xlsx" and "mapr.rds"'),'\n')
  #
  #########################################
  # Save data frame as a compressed file:
  #########################################
  #
  saveRDS(dr,'dr.rds')
  #
  cat(paste0('Data frame with dimensions ',dim(dr)[1],' x ',dim(dr)[2], ' was stored as compressed file "dr.rds"'),'\n')
  #
  #OK R
  #
  T2=Sys.time();print(as.POSIXlt(T2, "GMT")-as.POSIXlt(T1, "GMT"),quote=F);
  cat(" ","\n")
  #
  }
  #
  ###################################
  ################# Process SPSS data
  ###################################
  #
  if(useSPSS){
    #
    cat('SPSS','\n')
    cat('------','\n')
    T1=Sys.time()
    #
    ######################Read SPSS
    #
    ds=suppressWarnings(read.spss(spssin, to.data.frame=TRUE,use.missings=FALSE))
    #
    cat(paste0('Data file "',spssin,'" was read'),'\n')
    #
    ###################################################################
    ############### Create a map of possible answers for all variables:
    ###################################################################
    #
    #Create list:
    dslist=as.list(ds)
    vnames=names(dslist)
    #All available factor levels:
    vmapsall=mapply(function(x,y){tx=as.data.frame.table(table(x));colnames(tx)=c(y,'Freq');return(tx)},dslist,vnames,SIMPLIFY = FALSE)
    #Save in excel:
    write.xlsx(vmapsall,'MapOfAllVariables_SPSS.xlsx')
    #Save in rds:
    saveRDS(vmapsall,'mapspss.rds')
    cat(paste0('Map of all possible answers for all variables was stored in files "MapOfAllVariables_SPSS.xlsx" and "mapspss.rds"'),'\n')
    #
    #########################################
    # Save data frame as a compressed file:
    #########################################
    #
    saveRDS(ds,'ds.rds')
    #
    cat(paste0('Data frame with dimensions ',dim(ds)[1],' x ',dim(ds)[2], ' was stored as compressed file "ds.rds"'),'\n')
    #
    #OK SPSS
    #
    T2=Sys.time();print(as.POSIXlt(T2, "GMT")-as.POSIXlt(T1, "GMT"),quote=F);
    cat(" ","\n")
    #
  }
  #
  #
  ######################################
  ################# Work with STATA data
  ######################################
  #
  if(useSTATA){
    #
    cat('STATA','\n')
    cat('------','\n')
    T1=Sys.time()
    #
    ######################Read stata
    #
    dt =suppressWarnings(read.dta13(statain,missing.type=TRUE))
    #
    cat(paste0('Data file "',statain,'" was read'),'\n')
    #
    ###################################################################
    ############### Create a map of possible answers for all variables:
    ###################################################################
    #
    #Create list:
    dtlist=as.list(dt)
    vnames=names(dtlist)
    #All available factor levels:
    vmaptall=mapply(function(x,y){tx=as.data.frame.table(table(x));colnames(tx)=c(y,'Freq');return(tx)},dtlist,vnames,SIMPLIFY = FALSE)
    #Save in excel:
    write.xlsx(vmaptall,'MapOfAllVariables_STATA.xlsx')
    #Save in rds:
    saveRDS(vmaptall,'mapstata.rds')
    cat(paste0('Map of all possible answers for all variables was stored in files "MapOfAllVariables_STATA.xlsx" and "mapstata.rds"'),'\n')
    #
    #########################################
    # Save data frame as a compressed file:
    #########################################
    #
    saveRDS(dt,'dt.rds')
    #
    cat(paste0('Data frame with dimensions ',dim(dt)[1],' x ',dim(dt)[2], ' was stored as compressed file "dt.rds"'),'\n')
    #
    #OK STATA
    #
    T2=Sys.time();print(as.POSIXlt(T2, "GMT")-as.POSIXlt(T1, "GMT"),quote=F);
    #
  }
  #
}
