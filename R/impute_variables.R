#' @title Impute requested R variables using package 'mice' in parallel mode
#' @description
#' Read excel file of description for imputing variables, read missing values data frame, impute and save results
#' @param inxlfile Excel file of variables to be imputed
#' @param df R rds file of data with missing values to be imputed
#' @param ncores the number of logical cores that should be used (apply for multicore computers)
#' @return NULL
#' @importFrom readxl read_excel
#' @import parallel
#' @import foreach
#' @import iterators
#' @import doParallel
#' @import lattice
#' @import mice
#' @export
#' @examples
#' #impute_variables_R(inxlfile,df)
impute_variables <-
function(inxlfile,df,ncores=NULL){
  #Function to impute variables given by excel file 'inxlfile'
  #
  ######Check arguments:
  #
  if(all(is.null(c(inxlfile,df)))){
    stop('You must use two files for input.')
  }
  #
  #Check for number of cores
  #
  if(is.null(ncores)){ncores=detectCores()}
  #
  if(ncores>detectCores()){ncores=detectCores()}
  #
  ##########################################################################################################
  ######Read chosen variables and methods for imputation
  ##########################################################################################################
  #
  #Data frame of variables:
  dvars=as.data.frame(read_excel(inxlfile));
  vars=dvars$name
  varsnames=dvars[dvars$name%in%vars,"rename"]
  #Keep only required variables:
  dw=df[,vars]
  colnames(dw)=varsnames
  # Convert variable to factors...
  dvars$factor=sapply(dvars$type,function(x){ifelse(x!="numeric",{return(TRUE)},{return(FALSE)})});
  #
  dvars[dvars$factor,'rename']
  dw[,dvars[dvars$factor,'rename']] <- lapply(dw[,dvars[dvars$factor,'rename']] , factor)
  #Check the percentage of missing values
  yy=apply(dw,2,function(x){100*sum(is.na(x))/length(x)})
  #Use of 'rule of thumb': 1 imputation per average each % of missing values
  nimps=floor(mean(yy,na.rm = T))
  cat(paste0('The rule of thumb criterion: "1 imputation per each 1% of average missing values" suggests ',nimps,' imputations'),'\n')
  # Create vector of proper imputation methods for mice:
  vmethod=dvars$method;vmethod
  #Function for a single imputation:
  fmice=function(i,df,method){mice(df,m=1,method = method)}
  #Do Parallel:
  #Check availability of cores to satisfy the 'rule of thumb' criterion:
  #
  if(ncores==1){
    warning('It seems that there exist only 1 core, so parallel imputation cannot be applied','\n')
    nused=1
  }else if(ncores>nimps){
    nused=nimps
    cat(paste0('There will be used ',nused,' cores for parallel mice imputation'),'\n')
    cat(paste0('The rule of thumb criterion for choosing number of imputations is satisfied'),'\n')
  }else{
    nused=ncores
    cat(paste0('There will be used ',nused,' cores for parallel mice imputation'),'\n')
    cat(paste0('The rule of thumb criterion for choosing number of imputations is not satisfied'),'\n')
  }
  #
  i=NULL
  cl <- makeCluster(nused);registerDoParallel(cl);
  t1=Sys.time();
  m22=foreach(i=1:nused,.combine=ibind,.packages = c('mice')) %dopar% {fmice(i,df=dw,method=vmethod)};
  t2=Sys.time();print(as.POSIXlt(t2, "GMT")-as.POSIXlt(t1, "GMT"),quote=F);#
  stopCluster(cl);
  #
  #Save output:
  #
  fname=paste0('dimp',nused,'.rds')
  saveRDS(m22,fname)
  #
  cat(paste0(dim(dw)[2],' variables were imputed using "mice" with ',nused,' imputations'),'\n')
  cat(paste0('Data frame of imputed variables has been stored in file "',fname,'" as a "mids" object'),'\n')
  #
  }
