#' @title Read variables from excel file, process and save
#' @description
#' Read user specified variables with instructions, create piecharts, barplots and save frequencies of answers
#' @param inxlfile Excel input file
#' @param vmapr  Map of R variables
#' @param vmapspss Map of SPSS variables
#' @param vmapstata Map of STATA variables
#' @param dr  File of R variables
#' @param ds File of SPSS variables
#' @param dt File of STATA variables
#' @return NULL
#' @importFrom readxl read_excel
#' @importFrom plotrix pie3D
#' @examples
#' #choose_variables_describe(inxlfile,vmapr,vmapspss,vmapstata,dr,ds,dt)
#' @export
choose_variables_describe <-
function(inxlfile,vmapr=NULL,vmapspss=NULL,vmapstata=NULL,dr=NULL,ds=NULL,dt=NULL){
  #Function to read chosen variables from excel file 'inxlfile' and describe them
  #
  ######Check arguments:
  #
  if(all(is.null(c(vmapr,vmapspss,vmapstata)))){
    stop('You must use at least one variable map as input.')
  }
  #
  if(!(is.null(vmapr))){useR=TRUE}else{useR=FALSE}
  if(!(is.null(dr))){histR=TRUE}else{histR=FALSE}
  #
  #
  if(!(is.null(vmapspss))){useSPSS=TRUE}else{useSPSS=FALSE}
  if(!(is.null(ds))){histS=TRUE}else{histS=FALSE}
  #
  #
  if(!(is.null(vmapstata))){useSTATA=TRUE}else{useSTATA=FALSE}
  if(!(is.null(dt))){histT=TRUE}else{histT=FALSE}
  #
  ##########################################################################################################
  ######Read chosen variables and instruction for reading their proper values, except NA, non applicable etc
  ##########################################################################################################
  #
  dvars=as.data.frame(read_excel(inxlfile),stringsAsFactors=FALSE)
  vars=dvars[dvars$factor & dvars$countlevels,"name"]
  varnames=dvars[dvars$factor & dvars$countlevels,"rename"]
  #
  ###################################
  ################# Work with R data
  ###################################
  #
  #
  if(useR){
    #
    cat('R','\n')
    cat('------','\n')
    #Create frequencies of answers in R:
    vansr=lapply(vars,function(vname,vmapr,dvars){
      dh=vmapr[[vname]]
      ii=as.integer(unlist(strsplit(dvars[dvars$name==vname,"rowsr"  ],";")))-1
      dh[ii[1]:ii[2],]
    },vmapr,dvars)
    #
    names(vansr)=vars
    #
    #Store valid answers for all variables:
    saveRDS(vansr,'validansR.rds')
    #
    #Pies for  R:
    #
    fnamer='PieChartsOfVariables_R.pdf'
    pdf(fnamer,onefile = TRUE,paper='a4r',width = 0, height = 0)
    yy=mapply(function(vname,vnewname,dans){
      dd=dans[[vname]]
      labs=dd[,vname]
      vals = dd[,'Freq']
      pcs = paste0("(",round(vals/sum(vals)*100),"%)")
      labs = paste(labs, pcs)
      # pie3D(vals,labels=labs,explode=0.1,main=paste("\n \n  \n  \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
      pie3D(vals,labels=labs,explode=0.1,main=paste(" \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
    },
    vars,varnames,MoreArgs=list(dans=vansr))
    dev.off()
    #
    cat(paste0(length(vars),' variables were chosen, frequency tables were read and pies have been printed in file "',fnamer,'"'),'\n')
    #
    #Bar Plots for R:
    #
    if(histR){
      #
      hnamer='BarPlotsOfVariables_R.pdf'
      pdf(hnamer,onefile = TRUE,paper='a4r',width = 0, height = 0)
      yy=mapply(function(vname,vnewname,dans,df){
        dd=dans[[vname]]
        dh=df[df[,vname]%in%dd[,vname],vname]
        vplot=table(dh)
        if(dim(dd)[1]<3){colr=c('purple','red')}else{colr=brewer.pal(dim(dd)[1],'Spectral')}
        x <- barplot(vplot, space=0,xlab="",ylab = 'Frequency',main=paste("Bar Plot of \n",vname,":",vnewname),
                     # las=2, cex.names = 1.5,
                     col=colr,
                     # legend.text = names(vplot),
                     # args.legend = list(x= 7, y = 0, xpd = T, horiz = T, bg = "white", bty ="o", box.lwd = 0),
                     # xaxt="n"
                     )
        # labs <- names(vplot)
        # yd=max(vplot)/100
        # text(cex=1.8, x=x,y=-yd,labs, xpd=TRUE, srt=45, pos=2)
        box()
        #
      },
      vars,varnames,MoreArgs=list(dans=vansr,df=dr))
      dev.off()
      #
      cat(paste0('Box Plots  have been printed in file "',hnamer,'"'),'\n')
      cat(' ','\n')
      #
    }
  }
  #
  #OK R
  #
  #
  ###################################
  ################# Process SPSS data
  ###################################
  #
  if(useSPSS){
    #
    cat('SPSS','\n')
    cat('------','\n')
    #Create frequencies of answers in SPSS:
    vanss=lapply(vars,function(vname,vmapspss,dvars){
      dh=vmapspss[[vname]]
      ii=as.integer(unlist(strsplit(dvars[dvars$name==vname,"rowsother"  ],";")))-1
      dh[ii[1]:ii[2],]
    },vmapspss,dvars)
    #
    names(vanss)=vars
    #
    #Store valid answers for all variables:
    saveRDS(vanss,'validansSPSS.rds')
    #
    #Pies for SPSS:
    #
    fnames='PieChartsOfVariables_SPSS.pdf'
    pdf(fnames,onefile = TRUE,paper='a4r',width = 0, height = 0)
    par(mar = c(1, 3, 1, 1))
    yy=mapply(function(vname,vnewname,dans){
      dd=dans[[vname]]
      labs=dd[,vname]
      vals = dd[,'Freq']
      pcs = paste0("(",round(vals/sum(vals)*100),"%)")
      labs = paste(labs, pcs)
      # pie3D(vals,labels=labs,explode=0.1,main=paste("\n \n  \n  \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
      pie3D(vals,labels=labs,explode=0.1,main=paste(" \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
    },
    vars,varnames,MoreArgs=list(dans=vanss))
    dev.off()
    #
    cat(paste0(length(vars),' variables were chosen, frequency tables were read and pies have been printed in file "',fnames,'"'),'\n')
    #
    #
    #Bar Plots for SPSS:
    #
    if(histS){
      #
      hnames='BarPlotsOfVariables_SPSS.pdf'
      pdf(hnames,onefile = TRUE,paper='a4r',width = 0, height = 0)
      yy=mapply(function(vname,vnewname,dans,df){
        dd=dans[[vname]]
        dh=df[df[,vname]%in%dd[,vname],vname]
        dtab=table(dh)
        vplot=dtab[dtab!=0]
        if(dim(dd)[1]<3){colr=c('purple','red')}else{colr=brewer.pal(dim(dd)[1],'Spectral')}
        x <- barplot(vplot, space=0,ylab = 'Frequency',main=paste("Bar Plot of \n",vname,":",vnewname),
                     las=2, cex.names = 0.9,col=colr,
                     # legend.text = names(vplot),
                     # args.legend = list(x= 7, y = 0, xpd = T, horiz = T, bg = "white", bty ="o", box.lwd = 0),
                     xaxt="n")
        labs <- names(vplot)
        yd=max(vplot)/100
        text(cex=0.8, x=x,y=-yd,labs, xpd=TRUE, srt=45, pos=2)
        box()
        #
      },
      vars,varnames,MoreArgs=list(dans=vanss,df=ds))
      dev.off()
      #
      cat(paste0('Box Plots  have been printed in file "',hnames,'"'),'\n')
      cat(' ','\n')
      #
  }
  }
  #OK SPSS
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
    #Create frequencies of answers in STATA:
    vanst=lapply(vars,function(vname,vmapstata,dvars){
      dh=vmapstata[[vname]]
      ii=as.integer(unlist(strsplit(dvars[dvars$name==vname,"rowsother"  ],";")))-1
      dh[ii[1]:ii[2],]
    },vmapstata,dvars)
    #
    names(vanst)=vars
    #
    #Store valid answers for all variables:
    saveRDS(vanst,'validansSTATA.rds')
    #
    #Pies for STATA:
    #
    fnamet='PieChartsOfVariables_STATA.pdf'
    pdf(fnamet,onefile = TRUE,paper='a4r',width = 0, height = 0)
    par(mar = c(1, 3, 1, 1))
    yy=mapply(function(vname,vnewname,dans){
      dd=dans[[vname]]
      labs=dd[,vname]
      vals = dd[,'Freq']
      pcs = paste0("(",round(vals/sum(vals)*100),"%)")
      labs = paste(labs, pcs)
      # pie3D(vals,labels=labs,explode=0.1,main=paste("\n \n  \n  \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
      pie3D(vals,labels=labs,explode=0.1,main=paste(" \n Pie Chart of \n",vname,":",vnewname),labelcex=0.75,mar=c(1,1,1,1))
    },
    vars,varnames,MoreArgs=list(dans=vanst))
    dev.off()
    #
    cat(paste0(length(vars),' variables were chosen, frequency tables were read and pies have been printed in file "',fnamet,'"'),'\n')
    #
    #
    #Bar Plots for STATA:
    #
    if(histT){
      #
      hnamet='BarPlotsOfVariables_STATA.pdf'
      pdf(hnamet,onefile = TRUE,paper='a4r',width = 0, height = 0)
      yy=mapply(function(vname,vnewname,dans,df){
        dd=dans[[vname]]
        dh=df[df[,vname]%in%dd[,vname],vname]
        dtab=table(dh)
        vplot=dtab[dtab!=0]
        if(dim(dd)[1]<3){colr=c('purple','red')}else{colr=brewer.pal(dim(dd)[1],'Spectral')}
        x <- barplot(vplot, space=0,ylab = 'Frequency',main=paste("Bar Plot of \n",vname,":",vnewname),
                     las=2, cex.names = 0.9,col=colr,
                     # legend.text = names(vplot),
                     # args.legend = list(x= 7, y = 0, xpd = T, horiz = T, bg = "white", bty ="o", box.lwd = 0),
                     xaxt="n")
        labs <- names(vplot)
        yd=max(vplot)/100
        text(cex=0.8, x=x,y=-yd,labs, xpd=TRUE, srt=45, pos=2)
        box()
        #
      },
      vars,varnames,MoreArgs=list(dans=vanst,df=dt))
      dev.off()
      #
      cat(paste0('Box Plots  have been printed in file "',hnamet,'"'),'\n')
      cat(' ','\n')
      #
    }
    #
  }
  #
  #OK STATA
  #OK ALL
}
