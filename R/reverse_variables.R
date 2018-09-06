#' @title Reverse requested R variables
#' @description
#' Read excel file of description for reversing variables
#' @param inxlfile Excel file of variables
#' @param dr R rds file of data
#' @param validansR Valid answers file
#' @return NULL
#' @examples
#' #reverse_variables(inxlfile,dr,validansR=validr)
#' @importFrom readxl read_excel
#' @importFrom plotrix rescale
#' @export
reverse_variables <-
function(inxlfile,dr,validansR){
  #Function to reverse and rescale chosen variables from excel file 'inxlfile'
  #
  ######Check arguments:
  #
  if(all(is.null(c(inxlfile,dr,validansR)))){
    stop('You must use three files for input.')
  }
  #
  #
  ##########################################################################################################
  ######Read chosen variables and instructions for reversing and rescaling
  ##########################################################################################################
  #
  dvars=as.data.frame(read_excel('in3.xlsx'),stringsAsFactors=FALSE)
  dvars$factor=as.logical(dvars$factor)
  dvars$reverse=as.logical(dvars$reverse)
  vall=dvars$name
  #All processes variables
  vallr=names(validansR)
  #Factors:
  vars=dvars[dvars$factor,"name"]
  #Find factor variables to be reversed
  vrev=dvars[dvars$reverse,"name"]
  #Find variables to be rescaled
  vch=dvars[!is.na(dvars$rescale),"name"]
  #To be reversed and to be rescaled:
  vars2=c(vrev,vch)
  #Find range for
  vranges=matrix(as.integer(unlist(strsplit(dvars[dvars$name%in%vch,"rescale" ],";"))),ncol=2,byrow = T)
  rownames(vranges)=vch
  colnames(vranges)=c('MIN','MAX')
  #
  #Create data frame of valid answers only:
  #
  #
  dvlist=lapply(vallr, function(vname,validans,df){
    out=df[,vname]
    ii=df[,vname]%in%validans[[vname]][,vname]
    out[ii]=df[ii,vname]
    out[!ii]=NA
    return(out)
  },validans=validansR,df=dr)
  names(dvlist)=vallr
  dv=do.call(cbind,dvlist)
  #
  #
  #Function for reversing values:
  #
  frevcol=function(cn){
    xmin=min(cn,na.rm=TRUE)
    xmax=max(cn,na.rm=TRUE)
    fx=function(x,xmin,xmax){xmax+xmin-x}
    yy=fx(cn,xmin,xmax)
    return(yy)
  }
  #
  #Reverse them:
  #
  if(length(vrev)==1){
    dv[,vrev]=frevcol(dv[,vrev])
  }else {
    dv[,vrev]=apply(dv[,vrev],2,frevcol)
  }
  #
  #
  #Use rescale form 'plotrix' package for those that must be rescaled:
  #
  if(length(vch)==1){
    dv[,vch]=rescale(dv[,vch],vranges)
  }else {
    yy=sapply(vch, function(vn,df,ranges){
      xrange=as.integer(ranges[vn,])
      out=rescale(df[,vn],xrange)
      return(out)
    },df=dv[,vch],ranges=vranges)
    dv[,vch]=yy
  }
  #
  #Store both reversed, rescaled and untouched variables:
  #
  dwr=dr[,vall]
  dwr[,vallr]=dv[,vallr]
  dwr[,vrev]=dv[,vrev]
  dwr[,vch]=dv[,vch]
  #
  #Save working data frame dwr:
  #
  saveRDS(dwr,'dwr.rds')
  #OK
  #
  cat(paste0(length(vall),' variables were chosen'),'\n')
  cat(paste0(length(vrev),' variables were reversed'),'\n')
  cat(paste0(length(vch),' variables were rescaled'),'\n')
  cat(paste0('Data frame of valid answers after reverses and rescales has been stored in file "dwr.rds"'),'\n')
  #
}
