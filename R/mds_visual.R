#' @title Visualize several MDS variables
#' @description
#' Read configuration inputs and create 3d plots using packages rgl, plot3D, plotrgl3d
#' Create 2d maps for all variables
#' Create Google Maps and Google Earth Maps for specific variables
#' @param mdsconfigsin The excel file of MDS configurations for all countries
#' @param mdsregressin The excel file of biplot regressions for all countries
#' @param countrymixin The excel file with country mixtures
#' @param a_names The character name of initially created archetype names
#' @param anamesorder The character name of finally chosen name order for archetypes
#' @param countrydata The excel file with country data to be used as input
#' @param plotrgl Logical entry set to TRUE if rgl plots will be created
#' @param plot3d Logical entry set to TRUE if plot3D plots will be created
#' @param plotrgl3d Logical entry set to TRUE if plotrgl3d plots will be created
#' @param plotworld3d Logical entry set to TRUE if rotatable 3d projected world map plots will be created
#' @param plotworld2d Logical entry set to TRUE if 2d world maps will be created
#' @param plotgoogle Logical entry set to TRUE if Google maps will be created
#' @param plotnatearth Logical entry set to TRUE if 'naturalearth' maps will be created
#' @param showiso2 Logical entry set to TRUE if ISOAlpha2 labels will be used
#' @param showiso3 Logical entry set to TRUE if ISOAlpha3 labels will be used
#' @param plotcrop Logical entry set to TRUE if knitr::plot_crop command will be used for pdf's
#' @param trimplot Logical entry set to TRUE if magick::trimplot command will be used for png's
#' @param maxradius Numeric entry for the radius of pies to be plotted in Google Maps (default=100000m)
#' @param col.ellipse The color of the inner ellipsoid of concentration
#' @param col.ellipse.special The color of the outer ellipsoid of concentration
#' @param usepositive Logical entry set to TRUE in rotatable 3d projected world map plots will be created if vertical lines are only above globe
#' @param brewerpal The Brewer pallette to be used for 2d maps
#' @param sagapal The SAGA pallette to be used for Google Maps
#' @param col.bubble The color for bubble plots
#' @importFrom readxl read_excel
#' @import countrycode
#' @importFrom openxlsx write.xlsx
#' @import rgl
#' @import sp
#' @importFrom matlib vectors3d
#' @importFrom knitr plot_crop
#' @import rmarkdown
#' @importFrom magick image_read image_trim image_write
#' @import plot3D
#' @importFrom plot3Drgl plotrgl
#' @importFrom maps map
#' @importFrom rworldmap joinCountryData2Map mapCountryData addMapLegend
#' @importFrom classInt classIntervals
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plotKML SAGA_pal kml
#' @import plotGoogleMaps
#' @importFrom rnaturalearth countries
#'@examples
#'\dontrun{
#' #mds_visual(mdsconfigsin,mdsregressin,countrymixin,a_names,anamesorder,
#' #countrydata,showiso2=TRUE,showiso3=FALSE,plotcrop=TRUE,trimplot=TRUE,
#' #plotrgl=T,plot3d=T,plotrgl3d=T,plotworld3d=T,plotworld2d=T,plotgoogle=T,plotnatearth=T,
#' #col.ellipse='lightsteelblue',col.ellipse.special='lightcoral',usepositive=TRUE,
#' #brewerpal='RdPu',sagapal=1,col.bubble='#FF5A0088',maxradius=1e5)
#'}
#' @return NULL
#' @export
mds_visual=function(mdsconfigsin,mdsregressin,countrymixin,a_names,anamesorder,countrydata,
                    plotrgl=TRUE,plot3d=TRUE,plotrgl3d=TRUE,plotworld3d=TRUE,plotworld2d=TRUE,plotgoogle=TRUE,plotnatearth=TRUE,
                    showiso2=TRUE,showiso3=FALSE,plotcrop=FALSE,trimplot=TRUE,maxradius=1e5,
                    col.ellipse='lightsteelblue',col.ellipse.special='lightcoral',usepositive=TRUE,
                    brewerpal='RdPu',sagapal=1,col.bubble='#FF5A0088'){
  #Begin report
  sink('VisualMDSreport.txt', split = TRUE)
  T1=Sys.time()
  cat('Read necessary data inputs:','\n')
  #Read mds configurations for all countries
  configs=as.data.frame(read_excel(mdsconfigsin,col_names = TRUE))
  if(showiso2){rownames(configs)=configs$ISOAlpha2}
  if(showiso3){rownames(configs)=countrycode(configs$ISOAlpha2,'iso2c', 'iso3c')}
  # rownames(configs)=configs$ISOAlpha2
  ndim=dim(configs)[2]-1
  dh=configs[,2:(dim(configs)[2])]
  dm3=as.matrix(dh)
  #Read biplot regressions for all countries
  df=as.data.frame(read_excel(mdsregressin))
  #Include only statistical significant regressions
  df$include=as.logical(df$include)
  #Create ata frame of statistical significant regressions
  dg=df[df$include,]
  rownames(dg)=dg$Variable
  #
  ####################################### Prepare data frame dp for plots:
  #Number of archetypes:
  n_arches=length(a_names)
  #Input for country mixtures:
  dinput=as.data.frame(read_excel(countrymixin))
  colnames(dinput)=c('Country',a_names,"HH","NHH","MI")
  dinput[,c("HH","NHH","MI")] <- NULL
  country_data <- as.data.frame(read_excel(countrydata))
  dinput <- merge(dinput,country_data,by="Country",all.x = FALSE,sort=FALSE) # keep countries in mixture table/input order
  dinput=dinput[,setdiff(colnames(dinput),'WVS_Code')]
  #Create country code ISOAlpha3
  country_data$ISOAlpha3=countrycode(country_data$ISOAlpha2, 'iso2c', 'iso3c')
  # #Read mds configurations for all countries
  # configs=readRDS('configs_MDS_exclude_FALSE_dmetric_standard_D3.rds')
  if(showiso2){dnext <- merge(dinput,configs,by="ISOAlpha2",sort=FALSE)}
  if(showiso3){dnext <- merge(dinput,configs,by="ISOAlpha3",sort=FALSE)}
  # head(dnext)
  # varnames=names(dinput)[9:dim(dinput)[2]];varnames
  #Find variable names
  varnames=names(dnext)[3:dim(dnext)[2]]
  #
  #Use Country names, iso codes and their centroid coordinates:
  #
  dcys=wcentroids
  #
  #Read Country mixtures read:
  cy60=as.data.frame(read_excel(countrymixin),stringsAsFactors=FALSE)
  colnames(cy60) <- c("name",a_names,"HH","NHH","MI")
  head(cy60)
  names(cy60)
  cy60$ISOAlpha2=countrycode(cy60$name,'country.name','iso2c')
  cy60$ISOAlpha3=countrycode(cy60$name,'country.name','iso3c')
  head(cy60)
  if(showiso2){rownames(cy60)=cy60$ISOAlpha2}
  if(showiso3){rownames(cy60)=cy60$ISOAlpha3}
  head(cy60)
  cyorder=cy60$name
  cyorder
  cyorder2=cy60$ISOAlpha2
  cyorder3=cy60$ISOAlpha3
  #Define data frame of special countries only:
  if(showiso2){dcys60=dcys[dcys$ISOAlpha2%in%cy60$ISOAlpha2,]}
  if(showiso3){dcys60=dcys[dcys$ISOAlpha3%in%cy60$ISOAlpha3,]}
  head(dcys60)
  #############################
  #Keep order by country names:
  #############################
  #
  if(showiso2){
    target2 <- cyorder2
    dcys60=dcys60[match(target2, dcys60$ISOAlpha2),]
    #Keep only full countries
    dcys60=dcys60[dcys60$ISOAlpha2%in%dnext$ISOAlpha2,]
    rownames(dcys60)=dcys60$ISOAlpha2
    }
  #
  if(showiso3){
    target3 <- cyorder3
    dcys60=dcys60[match(target3, dcys60$ISOAlpha3),]
    #Keep only full countries
    dcys60=dcys60[dcys60$ISOAlpha3%in%dnext$ISOAlpha3,]
    rownames(dcys60)=dcys60$ISOAlpha3
    }
  #
  head(dcys60)
  ###############
  #OK fixed order
  ###############
  #
  if(showiso2){
  dcys60[,c(a_names,'HH','NHH','MI')]=cy60[cy60$ISOAlpha2%in%dcys60$ISOAlpha2,c(a_names,'HH','NHH','MI')]
  }
  #
  if(showiso3){
    dcys60[,c(a_names,'HH','NHH','MI')]=cy60[cy60$ISOAlpha3%in%dcys60$ISOAlpha3,c(a_names,'HH','NHH','MI')]
  }
  #
  varnames2=setdiff(varnames,a_names)
  # varnames2[grep("&",varnames2)]=gsub("&","and",varnames2[grep("&",varnames2)])
  varnames2
  #
  dcys60[,varnames2]=dnext[,varnames2]
  head(dcys60)
  #
  #Consolidate more:
  #
  data_desc=dcys60[, c('longitude','latitude','ISOAlpha2','ISOAlpha3','name',a_names,'HH','NHH','MI',varnames2) ]
  colnames(data_desc)=c('long','lat','ISOAlpha2','ISOAlpha3','name',a_names,'HH','NHH','MI',varnames2)
  head(data_desc)
  #Create data frame for plots now:
  #
  dp=data_desc[ ,c(c('name','ISOAlpha2','ISOAlpha3','long','lat'),
                   setdiff(colnames(data_desc),c('name','ISOAlpha2','ISOAlpha3','long','lat'))) ]
  #
  head(dp)
  #Re order variables for better visualization:
  dp=dp[,c('name','ISOAlpha2','ISOAlpha3','long','lat',paste0("D",1:3),setdiff(colnames(dp),c('name','ISOAlpha2','ISOAlpha3','long','lat',paste0("D",1:3))))]
  head(dp)
  #Change existing &s to and in variable names:
  names(dp)[grep("&",names(dp))]=gsub("&","and",names(dp)[grep("&",names(dp))])
  head(dp)
  #
  #Store dp:
  # WriteXLS('dp','FullCountryDataFrameForPlots.xlsx',row.names = TRUE)
  write.xlsx(dp,'FullCountryDataFrameForPlots.xlsx',row.names=TRUE,col.names=TRUE,sheetName='CountryDataPlot')
  saveRDS(dp,'FullCountryDataFrameForPlots.rds')
  #
  cat(paste0('Data frame of country variables has been stored in files \n "FullCountryDataFrameForPlots.xlsx" \n "FullCountryDataFrameForPlots.rds" '),'\n')
  ######### Find the first highest norms (>0.80) countries from MDS 3 dimemsion analysis:
  #
  cnorms=apply(dm3,1,norm,'2')
  cbnorms=data.frame(cbind(sort(cnorms,decreasing = T)))
  colnames(cbnorms)='norm'
  cbnorms
  #Choose the countries with norm greater than the Q3 of all
  spids=which(cbnorms$norm>quantile(cbnorms$norm,probs=3/4))
  specialcys=rownames(cbnorms)[spids]
  cat("Countries that seem to be close to the boundary are:","\n")
  if(showiso2){
    d1=dp[dp$ISOAlpha2%in%specialcys,"name"]
    dh=data.frame("country"=d1,"norm"=rep(NA,length(d1)))
    dh[spids,"norm"]=cbnorms[spids,]
    print(dh)
    }
  if(showiso3){
    d1=dp[dp$ISOAlpha3%in%specialcys,"name"]
    dh=data.frame("country"=d1,"norm"=rep(NA,length(d1)))
    dh[spids,"norm"]=cbnorms[spids,]
    print(dh)
    }
  #
  #ok
  #
  ################################################
  ##########################OK ALL REQUIRED READY
  ################################################
  #
  #Function for initialize rgl plots:
  #
  rgl_init2 <- function(theta=15,phi=20,zoom=0.5) {
    open3d(windowRect=c(0,0,1800,1800))
    rgl.viewpoint(theta, phi,zoom)
  }
  #
  #rgl other functions:
  #
  rgl_add_axes <- function(x, y, z, axis.col = "grey",
                           xlab = "", ylab="", zlab="", show.plane = TRUE,
                           show.bbox = FALSE, bbox.col = c("#333377","black")){
    lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
    # Add axes
    xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
    rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)

    # Add a point at the end of each axes to specify the direction
    axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0),
                  c(0, 0, zlim[2]))
    rgl.points(axes, color = axis.col, size = 3)

    # Add axis labels
    rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
              adj = c(0.5, -0.8), size = 2)

    # Add plane
    if(show.plane) {
      xlim <- xlim/1.1; zlim <- zlim /1.1
      rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
                 z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
    }
    # Add bounding box decoration
    if(show.bbox){
      rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
               emission=bbox.col[1], specular=bbox.col[1], shininess=5,
               xlen = 3, ylen = 3, zlen = 3)
    }
  }
  #
  #plot3D functions:
  #
  scatter3D_fancy <- function(x, y, z,..., colvar = z)
  {
    panelfirst <- function(pmat) {
      XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
      scatter2D(XY$x, XY$y, colvar = colvar, pch = ".",
                cex = 2, add = TRUE, colkey = FALSE)

      XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
      scatter2D(XY$x, XY$y, colvar = colvar, pch = ".",
                cex = 2, add = TRUE, colkey = FALSE)
    }
    scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
              colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
  }
  #
  #
  ###########
  ###########
  #
  ##Create vector of archetypes coordinates:
  ii=1:n_arches
  dd=dg[ii,2:4]
  ddm=as.matrix(dd)
  rownames(ddm)=a_names
  colnames(ddm)=paste0("D",1:3)
  cat('MDS coordinates of adchetypes are:','\n')
  print(ddm)
  #
  #Create basis orthonormal vectors:
  b3=diag(3);rownames(b3)=paste0("D",1:3);b3
  #ok
  #Create data frame of special chosen countries in coordinates:
  dspecials=dm3[rownames(dm3)%in%specialcys,]
  cat('MDS coordinates of boundary countries are:','\n')
  print(dspecials)
  #All countries except the special ones:
  dm33=dm3[!rownames(dm3)%in%specialcys,  ]
  #
  ##Normalized vectors:
  daunity=as.matrix(apply(ddm,2,function(x){y=cbind(x);y/norm(y,'2')}))
  rownames(daunity)=rownames(ddm)
  daunity
  #
  #
  ######################################################
  ######## End of data reading and initial processinge
  ######################################################
  #
  if(plotrgl)
    {
    #
    cat('Create, save and trim (if it is available) RGL plots:','\n')
    #
  ##########################################################################################################
  ########################################## BEGIN OF RGL PACKAGE PLOTS
  ##########################################################################################################
  #
  ########
  ################# Begin Plot 1:
  ########
  #
  rgl_init2()
  #
  text3d(dm3,texts=rownames(dm3),cex=1,font=2)
  #
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25, headlength = 0.05)
  #
  rgl_add_axes(dm3[,1], dm3[,2], dm3[,3],show.plane =FALSE)
  #
  #Print in various formats:
  #
  fname1="PLOTRGL_ALL_COUNTRIES_1"
  #In pdf:
  rgl.postscript(paste0(fname1,".pdf"), fmt="pdf")
  #crop white spaces if plotcrop:
  #
  if(plotcrop){plot_crop(paste0(fname1,".pdf"))}
  #
  #Create ellipses of concentration at level of 1%:
  #
  ellips <- ellipse3d(cov(dm3),centre=c(apply(dm3,2,mean)), level = 0.99)
  #
  shade3d(ellips,
          col = col.ellipse,
          # color='cyan',
          # color='lightgray',
          alpha = 0.1, lit = FALSE)
  # wire3d(ellips, col = "#D95F02",  lit = FALSE)
  wire3d(ellips,alpha = 0.1,
         col = col.ellipse,
         # col = 'lightgray',
         # col='cyan',
         lit = FALSE)
  #
  title3d('Countries in 3D MDS coordinate system',col='black',font=2)
  #
  rgl.snapshot(filename = paste0(fname1,".png"))
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname1,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  rgl.close()
  #
  if(trimplot){
    plotname=paste0(fname1,".png")
    img=image_read(plotname)
    img2=image_trim(img)
    image_write(img2, path = plotname, format = "png")
  }
  #
  #
  ########### End plot 1
  #
  ########### Begin plot 2
  #
  rgl_init2()
  plot3d(dm3[,1], dm3[,2], dm3[,3], col="gray",box = TRUE,type ="s", radius = 0.02,xlab="D1",ylab="D2",zlab="D3")
  #
  # rgl.viewpoint(theta=15, phi=20,zoom=0.80)
  text3d(1.10*dm3,texts=rownames(dm3),cex=1,font=2,col='blue')
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25, headlength = 0.05)
  title3d('Countries in 3D MDS coordinate system',col='black',font=2)
  #
  #
  #Print in various formats:
  #
  fname2="PLOTRGL_ALL_COUNTRIES_2"
  #
  #In pdf:
  rgl.postscript(paste0(fname2,".pdf"), fmt="pdf")
  # crop white spaces if plotcrop:
  #
  if(plotcrop){plot_crop(paste0(fname2,".pdf"))}
  #
  #Create ellipses of concentration at level of 1%:
  #
  ellips <- ellipse3d(cov(dm3),centre=c(apply(dm3,2,mean)), level = 0.99)
  #
  shade3d(ellips, col = col.ellipse,alpha = 0.1, lit = FALSE)
  #
  wire3d(ellips,alpha = 0.1,col = col.ellipse,lit = FALSE)
  #
  #
  rgl.snapshot(filename = paste0(fname2,".png"))
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname2,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  rgl.close()
  #
  if(trimplot){
  plotname=paste0(fname2,".png")
  img=image_read(plotname)
  img2=image_trim(img)
  image_write(img2, path = plotname, format = "png")
  }
  #
  ########### End plot 2
  #
  #
  ########### Begin plot 3
  #
  # All except special countries:
  #
  rgl_init2()
  dm33=dm3[!rownames(dm3)%in%specialcys,  ]
  rgl.spheres(dm33, radius = 0.04, color = "cyan")
  text3d(1.15*dm33,texts=rownames(dm33),cex=1,col='blue',font=2)
  #Create ellipses of concentration at level of 1%:
  ellips <- ellipse3d(cov(dm33),centre=c(apply(dm33,2,mean)), level = 0.99)
  shade3d(ellips, col = col.ellipse,alpha = 0.1, lit = FALSE)
  wire3d(ellips,alpha = 0.1,col = col.ellipse,lit = FALSE,add=TRUE)
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25, headlength = 0.05)
  rgl_add_axes(b3[,1], b3[,2], b3[,3],show.plane =FALSE)
  title3d('Countries with small vector norm in 3D MDS coordinate system',col='black',font=2)
  #Print in various formats:
  #
  fname3="PLOTRGL_ALL_EXCEPT_SPECIAL_COUNTRIES_3"
  #
  #png:
  rgl.snapshot(filename = paste0(fname3,".png"))
  #
  #Rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname3,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  #
  rgl.close()
  #
  #
  if(trimplot)
  {plotname=paste0(fname3,".png")
  img=image_read(plotname)
  img2=image_trim(img)
  image_write(img2, path = plotname, format = "png")
  }
  #
  #
  ############# End plot 3
  #
  ############# Begin Plot 4
  #
  ###### special countries plot:
  #
  rgl_init2()
  #
  dspecials=dm3[rownames(dm3)%in%specialcys,]
  #
  plot3d(dspecials[,1], dspecials[,2], dspecials[,3], col="red4",box = FALSE,type ="s", radius = 0.05,xlab="",ylab="",zlab="")
  text3d(1.16*dspecials,texts=rownames(dspecials),cex=0.75,font=2,col='red')
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25, headlength = 0.05)
  rgl_add_axes(b3[,1], b3[,2], b3[,3],show.plane =FALSE)
  #
  #Simple Archetypal vectors
  vectors3d(ddm[1:n_arches,], color=rep('black',n_arches), lwd=2, radius=1/25, headlength = 0.10,cex.lab=0.75,ref.length=0.35)
  U <- par3d("userMatrix");U;par3d(userMatrix = rotate3d(U, -pi/16, 0,1,0)) # Rotate about model's y axis
  aspect3d(1.10, 1, 1)
  #
  #Print in various formats:
  #
  fname4="PLOTRGL_SPECIAL_COUNTRIES_4"
  #
  #png:
  #
  rgl.snapshot(filename = paste0(fname4,".png"))
  #
  #pdf:
  #
  rgl.postscript(paste0(fname4,".pdf"), fmt="pdf")
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname4,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  #
  rgl.close()
  #
  #
  if(trimplot)
  {
    plotname=paste0(fname4,".png")
    img=image_read(plotname)
    img2=image_trim(img)
    image_write(img2, path = plotname, format = "png")
  }
  #
  #
  ############# End Plot 4
  #
  #
  ############# Begin Plot 5
  #
  #
  rgl_init2()
  #
  plot3d(dspecials[,1], dspecials[,2], dspecials[,3], col="red4",box = FALSE,type ="s", radius = 0.05,xlab="",ylab="",zlab="")
  #
  ellips_specs <- ellipse3d(cov(dspecials),centre=c(apply(dspecials,2,mean)), level = 0.99)
  plot3d(ellips_specs, col = "blue", alpha = 0.1, add =TRUE, box =TRUE)
  shade3d(ellips_specs, col = col.ellipse, alpha = 0.1, lit = FALSE,add=TRUE)
  wire3d(ellips_specs, col = col.ellipse,  lit = FALSE,add=TRUE)
  text3d(1.15*dspecials,texts=rownames(dspecials),cex=1,col='red',font=2)
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25)
  rgl_add_axes(b3[,1], b3[,2], b3[,3],show.plane =FALSE)
  #
  #
  #Simple Archetypal vectors
  vectors3d(ddm[1:n_arches,], color=rep('black',n_arches), lwd=2, radius=1/25, headlength = 0.10,cex.lab=0.75,ref.length=0.35,font=2)
  U <- par3d("userMatrix");U;par3d(userMatrix = rotate3d(U, -pi/16, 0,1,0)) # Rotate about model's y axis
  aspect3d(1.10, 1, 1)
  #
  title3d(sub='Countries with largest vector norm in 3D MDS coordinate system',col='black',font=2)
  #
  #Print in various formats:
  #
  fname5="PLOTRGL_SPECIAL_COUNTRIES_5"
  #
  #png:
  #
  rgl.snapshot(filename = paste0(fname5,".png"))
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname5,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  #
  rgl.close()
  #
  #
  if(trimplot)
  {
    plotname=paste0(fname5,".png")
    img=image_read(plotname)
    img2=image_trim(img)
    image_write(img2, path = plotname, format = "png")
  }
  #
  #
  ############# End Plot 5
  #
  #
  ############# Begin Plot 6
  #
  #
  rgl_init2()
  #
  plot3d(dspecials[,1], dspecials[,2], dspecials[,3], col="red4",box = FALSE,type ="s", radius = 0.05,xlab="",ylab="",zlab="")
  #
  dspecials=dm3[rownames(dm3)%in%specialcys,]
  plot3d(dspecials[,1], dspecials[,2], dspecials[,3], col="red4",box = FALSE,type ="s", radius = 0.05,xlab="",ylab="",zlab="",add=TRUE)
  text3d(1.16*dspecials,texts=rownames(dspecials),cex=0.75,font=2,col='red')
  ellips_specs <- ellipse3d(cov(dspecials),centre=c(apply(dspecials,2,mean)), level = 0.99)
  plot3d(ellips_specs, col = col.ellipse.special, alpha = 0.1, add =TRUE, box =TRUE)
  shade3d(ellips_specs, col = col.ellipse.special, alpha = 0.1, lit = FALSE,add=TRUE)
  wire3d(ellips_specs, col = col.ellipse.special,  lit = FALSE,add=TRUE)
  #Normalized vectors:
  #
  vectors3d(daunity[1:n_arches,], color=rep('darkblue',n_arches), lwd=4, radius=1/25,ref.length=0.25)
  #
  title3d(main='Countries with largest vector norm in 3D MDS coordinate system',sub='Normalized Archetypes',col='black',font=2)
  #
  #Print in various formats:
  #
  fname6="PLOTRGL_SPECIAL_COUNTRIES_NORMALIZED_ARCHETYPES_6"
  #
  #png:
  #
  rgl.snapshot(filename = paste0(fname6,".png"))
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname6,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  #
  rgl.close()
  #
  #Trim plots using 'magick' package:
  #
  if(trimplot)
  {
    plotname=paste0(fname6,".png")
    img=image_read(plotname)
    img2=image_trim(img)
    image_write(img2, path = plotname, format = "png")
  }
  #
  #
  ############# End Plot 6
  #
  #
  ############# Begin Plot 7
  #
  ####both special and non special countries plot
  #
  # All except special countries first
  #
  rgl_init2()
  dm33=dm3[!rownames(dm3)%in%specialcys,  ]
  rgl.spheres(dm33, radius = 0.04, color = "cyan")
  text3d(1.15*dm33,texts=rownames(dm33),cex=1,col='blue',font=2)
  #Create ellipses of concentration at level of 1%:
  ellips <- ellipse3d(cov(dm33),centre=c(apply(dm33,2,mean)), level = 0.99)
  shade3d(ellips, col = col.ellipse,alpha = 0.1, lit = FALSE,add=TRUE)
  wire3d(ellips,alpha = 0.1,col = col.ellipse,lit = FALSE,add=TRUE)
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25, headlength = 0.05)
  rgl_add_axes(b3[,1], b3[,2], b3[,3],show.plane =FALSE)
  #
  # Special countries now:
  #
  dspecials=dm3[rownames(dm3)%in%specialcys,]
  plot3d(dspecials[,1], dspecials[,2], dspecials[,3], col="red4",box = FALSE,type ="s", radius = 0.05,xlab="",ylab="",zlab="",add=TRUE)
  text3d(1.16*dspecials,texts=rownames(dspecials),cex=0.75,font=2,col='red')
  ellips_specs <- ellipse3d(cov(dspecials),centre=c(apply(dspecials,2,mean)), level = 0.99)
  plot3d(ellips_specs, col = col.ellipse.special, alpha = 0.1, add =TRUE, box =TRUE)
  shade3d(ellips_specs, col = col.ellipse.special, alpha = 0.1, lit = FALSE,add=TRUE)
  wire3d(ellips_specs, col = col.ellipse.special,  lit = FALSE,add=TRUE)
  #
  #
  title3d(main='Ellipse of concentration for boundary countries  covers all countries',sub='',col='black',font=2)
  #
  #
  #Print in various formats:
  #
  fname7="PLOTRGL_SPECIAL_OR_NOT_COUNTRIES_7"
  #
  #png:
  #
  rgl.snapshot(filename = paste0(fname7,".png"))
  #
  #Save in rotatable Java HTML format:
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0(fname7,'.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  #
  rgl.close()
  #
  #Trim plots using magick package:
  #
  if(trimplot)
  {
    img=image_read(paste0(fname7,".png"))
    img2=image_trim(img)
    image_write(img2, path = paste0(fname7,".png"), format = "png")
  }
  #
  ############# End Plot 7
  #
  ##########################################################################################################
  ########################################## END OF RGL PACKAGE PLOTS
  ##########################################################################################################
  #
  }
  #
  if(plot3d)
    {
    #
    cat('Create, save and trim (if it is available) plot3D plots:','\n')
    #
  ##########################################################################################################
  ########################################## BEGIN OF PLOT3D PACKAGE PLOTS
  ##########################################################################################################
  #
  ########################################
  ############## plot3D package plots
  ########################################
  #
  ####################All countries
  #
  #Create coordinates for basic vectors:
  #
  x0=rep(0,dim(ddm)[1]);y0=x0;z0=x0
  x1=ddm[,1];y1=ddm[,2];z1=ddm[,3]
  #unit vectors:
  xu0=rep(0,dim(ddm)[1]);yu0=xu0;zu0=xu0
  xu1=daunity[,1];yu1=daunity[,2];zu1=daunity[,3]
  #
  b3=diag(3);rownames(b3)=paste0("D",1:3);b3
  bx0=rep(0,dim(b3)[1]);by0=bx0;bz0=bx0
  bx1=b3[,1];by1=b3[,2];bz1=b3[,3]
  #
  #
  ####################
  #PLOT 3D all cases:
  ####################
  #
  #
  ############# Begin 3D Plot 1
  #
  gname='PLOT3D_COUNTRIES_1'
  gnamepdf=paste0(gname,'.pdf')
  gnamepng=paste0(gname,'.png')
  #
  pdf(gnamepdf,width = 0,height = 0,paper='a4r',onefile = FALSE)
  #
  # plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
           main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  scatter3D(dm3[,1], dm3[,2], dm3[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',add=TRUE,type='p',pch=20,
            cex=0.5)
  #
  sx=1;sy=1;sz=1
  text3D(sx*dm3[,1], sy*dm3[,2], sz*dm3[,3],  labels = rownames(dm3),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='blue',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  plotdev()
  dev.off()
  #
  png(gnamepng,units='px', width=1600, height=1600, res=300)
  #
  plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
            main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  scatter3D(dm3[,1], dm3[,2], dm3[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',add=TRUE,type='p',pch=20,
            cex=0.5)
  #
  sx=1;sy=1;sz=1
  text3D(sx*dm3[,1], sy*dm3[,2], sz*dm3[,3],  labels = rownames(dm3),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='blue',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #
  #Trim png plots using magick package and pdf plots using knitr::plot_crop function
  #
  if(plotcrop){plot_crop(gnamepdf,quiet = TRUE)}
  #
  if(trimplot){gnamepng2=paste0(gname,'_b.png');img=image_read(gnamepng);img2=image_trim(img);image_write(img2, path = gnamepng2, format = "png")}
  #
  #
  ############# End 3D Plot 1
  #
  #
  ############# Begin 3D Plot 2
  #
  gname='PLOT3D_SPECIAL_COUNTRIES_2'
  gnamepdf=paste0(gname,'.pdf')
  gnamepng=paste0(gname,'.png')
  #
  pdf(gnamepdf,width = 0,height = 0,paper='a4r',onefile = FALSE)
  plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
           main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',add=TRUE,type='p',pch=20,
            cex=0.5)
  #
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='red',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #
  png(gnamepng,units='px', width=1600, height=1600, res=300)
  plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
           main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',add=TRUE,type='p',pch=20,
            cex=0.5)
  #
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='red',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #Trim png plots using magick package and pdf plots using knitr::plot_crop function
  if(plotcrop){plot_crop(gnamepdf,quiet = TRUE)}
  if(trimplot){gnamepng2=paste0(gname,'_b.png');img=image_read(gnamepng);img2=image_trim(img);image_write(img2, path = gnamepng2, format = "png")}
  #
  #
  ############# End 3D Plot 2
  #
  #
  ############# Begin 3D Plot 3
  #
  #Normalized archetypes
  #
  gname='PLOT3D_SPECIAL_COUNTRIES_NORMALIZED_ARCHETYPES_3'
  gnamepdf=paste0(gname,'.pdf')
  gnamepng=paste0(gname,'.png')
  #
  pdf(gnamepdf,width = 0,height = 0,paper='a4r',onefile = FALSE)
  plotdev()
  #Plot countries
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',type='p',pch=20,
            theta=20,phi=15, cex=0.5,bty='g',ticktype = "detailed",add=FALSE)
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='red',cex=0.7,theta=25,phi=15,font=2)
  #
  # Plot normalized archetype arrows:
  arrows3D(xu0, yu0, zu0, xu1, yu1, zu1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
           main = "\n Normalized Archetypes", bty ="g",ticktype = "detailed",type="cone",add=TRUE)
  #Plot archetype labels:
  text3D(xu1, yu1, zu1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue',
           xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,bty ="g",ticktype = "detailed")
  text3D(bx1, by1, 0.95*bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #ok
  #
  png(gnamepng,units='px', width=1600, height=1600, res=300)
  plotdev()
  #Plot countries
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',type='p',pch=20,
            theta=20,phi=15, cex=0.5,bty='g',ticktype = "detailed",add=FALSE)
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='red',cex=0.7,theta=25,phi=15,font=2)
  #
  # Plot normalized archetype arrows:
  arrows3D(xu0, yu0, zu0, xu1, yu1, zu1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,
           main = "\n Normalized Archetypes", bty ="g",ticktype = "detailed",type="cone",add=TRUE)
  #Plot archetype labels:
  text3D(xu1, yu1, zu1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue',
           xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,bty ="g",ticktype = "detailed")
  text3D(bx1, by1, 0.95*bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #Trim png plots using magick package and pdf plots using knitr::plot_crop function
  if(plotcrop){plot_crop(gnamepdf,quiet = TRUE)}
  if(trimplot){gnamepng2=paste0(gname,'_b.png');img=image_read(gnamepng);img2=image_trim(img);image_write(img2, path = gnamepng2, format = "png")}
  #
  #
  ############# End 3D Plot 3
  #
  #
  ############# Begin 3D Plot 4
  #
  gname='PLOT3D_SPECIAL_OR_NOT_COUNTRIES_4'
  gnamepdf=paste0(gname,'.pdf')
  gnamepng=paste0(gname,'.png')
  #
  pdf(gnamepdf,width = 0,height = 0,paper='a4r',onefile = FALSE)
  plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #Plot non special countries
  scatter3D(dm33[,1], dm33[,2], dm33[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='blue',add=TRUE,type='p',pch=20,cex=0.5)
  #Labels of non special countries
  sx=1;sy=1;sz=1
  text3D(sx*dm33[,1], sy*dm33[,2], sz*dm33[,3],  labels = rownames(dm33),xlab="D1",ylab="D2",zlab="D3",add=TRUE,col='blue',cex=0.5,theta=25,phi=15,font=2)
  #Plot special countries
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='red',add=TRUE,type='p',pch=20,cex=0.5)
  #Labels of special countries
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",add=TRUE,col='red',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #ok
  #
  png(gnamepng,units='px', width=1600, height=1600, res=300)
  plotdev()
  # Plot archetype arrows:
  arrows3D(x0, y0, z0, x1, y1, z1,colvar=NULL,lwd = 2, d = 3,xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15,main = "Archetypes", bty ="g",ticktype = "detailed",type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE,cex=0.75)
  #Plot non special countries
  scatter3D(dm33[,1], dm33[,2], dm33[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='blue',add=TRUE,type='p',pch=20,cex=0.5)
  #Labels of non special countries
  sx=1;sy=1;sz=1
  text3D(sx*dm33[,1], sy*dm33[,2], sz*dm33[,3],  labels = rownames(dm33),xlab="D1",ylab="D2",zlab="D3",add=TRUE,col='blue',cex=0.5,theta=25,phi=15,font=2)
  #Plot special countries
  scatter3D(dspecials[,1],dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='red',add=TRUE,type='p',pch=20,cex=0.5)
  #Labels of special countries
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",add=TRUE,col='red',cex=0.5,theta=25,phi=15,font=2)
  #Plot basis vectors
  arrows3D(bx0, by0, bz0, bx1, by1, bz1,colvar=NULL,lwd = 2, d = 3,type="cone",add=TRUE, col='lightsteelblue')
  text3D(bx1, by1, bz1,  labels = rownames(b3), add = TRUE,cex=0.5,col='lightsteelblue')
  #
  dev.off()
  #
  #Trim png plots using magick package and pdf plots using knitr::plot_crop function
  if(plotcrop){plot_crop(gnamepdf,quiet = TRUE)}
  if(trimplot){gnamepng2=paste0(gname,'_b.png');img=image_read(gnamepng);img2=image_trim(img);image_write(img2, path = gnamepng2, format = "png")}
  #
  #
  ############# End 3D Plot 4
  #
  ########################
  #
  if(plotrgl3d)
   {
    #
    cat('Create and save rotatable RGL plots from plot3D package:','\n')
    #
  ######################################################
  ##############Plot in rotatable format: plot3D to rgl
  ######################################################
  #
  #
  ############# Begin 3DRGL Plot 1
  #
  #Plot archetypes
  arrows3D(x0, y0, z0, x1, y1, z1,
           colvar=NULL,lwd = 2, d = 3,main = "Archetypes", bty ="g",
           ticktype = "detailed",xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15, type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE)
  #Plot cases
  scatter3D(dm3[,1], dm3[,2], dm3[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='black',add=TRUE)
  #Plot cases labels
  sx=1;sy=1;sz=1
  text3D(sx*dm3[,1], sy*dm3[,2], sz*dm3[,3],  labels = rownames(dm3),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='blue',theta=25,phi=15,font=2)
  #
  plotrgl(lighting = TRUE)
  #Plot dimension vectors
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25,add=TRUE)
  #
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0('PLOT3DRGL_ALL_CASES_1.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  rgl.close()
  dev.off()
  #
  #ok rotatable all cases
  #
  ############# End 3DRGL Plot 1
  #
  #
  ############# Begin 3DRGL Plot 2
  #

  #Plot archetypes
  arrows3D(x0, y0, z0, x1, y1, z1,
           colvar=NULL,lwd = 2, d = 3,main = "Archetypes", bty ="g",
           ticktype = "detailed",xlab='D1',ylab='D2',zlab='D3',theta=20,phi=15, type="cone")
  #Plot archetype labels:
  text3D(x1, y1, z1,  labels = rownames(ddm), add = TRUE)
  #Plot special cases
  scatter3D(dspecials[,1], dspecials[,2], dspecials[,3],xlab="D1",ylab="D2",zlab="D3",colvar=NULL,col='red',add=TRUE)
  #Plot cases labels
  sx=1;sy=1;sz=1
  text3D(sx*dspecials[,1], sy*dspecials[,2], sz*dspecials[,3],  labels = rownames(dspecials),xlab="D1",ylab="D2",zlab="D3",
         add=TRUE,col='red',theta=25,phi=15,font=2)
  #
  plotrgl(lighting = TRUE)
  #Plot dimension vectors
  vectors3d(b3, color=rep('darkgreen',3), lwd=2, radius=1/25,add=TRUE)
  #
  filename <- writeWebGL(dir = file.path(getwd()),filename=paste0('PLOT3DRGL_SPECIAL_CASES_2.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
  #
  rgl.close()
  dev.off()
  #
  #ok rotatable special cases
  #
  #
  ############# End 3DRGL Plot 2
  #
  }
  #
  }
  #
  if(plotworld3d)
  {
    #
    cat('Create and save rotatabe 3d RGL plots for all variables and world countries:','\n')
    #
  ##################################################
  ############## Plots over a world map using plot3D:
  ###################################################
  #
  #Get world coordinates:
  #
  #World data
  # data(worldMapEnv)
  yy=map('world',plot=FALSE)
  X=yy$x
  Y=yy$y
  Z=rep(0,length(X))
  #Choose variables for plot
  varplots=setdiff(colnames(dp),c('name','ISOAlpha2','ISOAlpha3','long','lat'))
  cat('Variables that will be mapped over world map are:','\n')
  print(varplots)
  #
  #
  ####################################### Plot variables
  #
  varplot=NULL
  #
  cl <- makeCluster(detectCores());registerDoParallel(cl);
  #
  m2=foreach(varplot=varplots,.packages = c('rgl','plot3D','plot3Drgl'))%dopar%{
    cat(varplot,'\n')
    dw=dp[,c('long','lat',varplot)]
    #Find summary:
    vs=summary(dw[,varplot])
    print(vs)
    #
    #
    if(usepositive)
     {
      #
      ##########################################
      #Option 1: plot positive only above globe:
      ##########################################
      #check positive values
    if( vs['Min.']>0){
      zmin= vs['Min.']
      zplot= dw[,varplot]-vs['Min.']
    }else{
      zplot=dw[,varplot]
    }
    }else{
      #
      #####################################################
      #Option 2: plot above globe values greater than mean:
      #####################################################
      #
      zplot= dw[,varplot]-vs['Mean']
      #
    }
    #
    #Plot variable:
    scatter3D(dw$long,dw$lat,
              # dw[,varplot],
              zplot,
              xlab="Easting",ylab="Northing",zlab=varplot,
              # dp$long,dp$lat,dp$Happiness,
              # add=TRUE,
              clab=varplot,
              type='h',
              colvar=dw[,varplot],
              colkey = list(side = 1, length = 0.3)
    )
    #Plot world:
    if(usepositive){
    scatter3D(X,Y,Z,cex=0.1,colvar = NULL,add=TRUE,ticktype = "detailed")
      }else{ZM=rep(vs['Mean'],length(X));scatter3D(X,Y,ZM,cex=0.1,colvar = NULL,add=TRUE,ticktype = "detailed")}
    #Plot country names:
    text3D(dw$long,dw$lat,
           # dw[,varplot],
           zplot,
           # dp$long,dp$lat,dp$Happiness,
           labels = rownames(dw),
           add = TRUE,
           clab=varplot,
           # colkey = FALSE,
           colvar=dw[,varplot],
           colkey = list(side = 1, length = 0.3),
           cex = 0.9)
    #Send to rgl (no colkey acceptable!):
    plotrgl()
    title3d(main=paste0('Mean ( ',varplot,' ) = ',round(vs['Mean'],digits=2)))
    filename <- writeWebGL(dir = file.path(getwd()),filename=paste0('World_',varplot,'_3D_Mean.html'),font='Arial', width = 1920, height=1080,reuse = TRUE)
    rgl.close()
    Sys.sleep(1)
    #
  }
  #
  stopCluster(cl);
  #
  #OK
  #
  ##################################################
  ##############:Plots over a world map using plot3D
  ##################################################
  #
  }
  #
  if(plotworld2d)
   {
    #
    cat('Create and save 2d plots for all variables and world countries:','\n')
    #
  ###################################################
  ############# Plot 2D World Maps for all variables:
  ###################################################
  #
  # pals=c('Blues','BuGn','BuPu','GnBu','Greens','Greys','Oranges','OrRd','PuBu','PuBuGn','PuRd','Purples','RdPu','Reds','YlGn','YlGnBu','YlOrBr','YlOrRd')
  #
  cl <- makeCluster(detectCores());registerDoParallel(cl);
  #
  m2=foreach(varname=varplots,.packages = c('rworldmap','sp','RColorBrewer','classInt','magick'))%dopar%{
    #
    fname=paste0(varname,'_WorldMap.png')
    #
    png(filename =fname, width = 1600, height = 900, units = "px", pointsize = 12,
        bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"))
    #
    #
    if(showiso2){
      dmap=dp[,c("ISOAlpha2",varname)]
      wmap <-joinCountryData2Map(dmap, joinCode = "ISO2",nameJoinColumn = "ISOAlpha2")
    }
    if(showiso3){
      dmap=dp[,c("ISOAlpha3",varname)]
      wmap <- joinCountryData2Map(dmap, joinCode = "ISO3",nameJoinColumn = "ISOAlpha3")
    }
    #
    par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
    #Find number of colors and breaks:
    ntables=table(dmap[,varname])
    if(length(ntables)>7){nbreaks=7}else{nbreaks=length(ntables)}
    classints <- classIntervals( wmap[[varname]], n=nbreaks, style="jenks")
    catmethod = unique(classints[["brks"]])
    #Set colors:
    colourpalette <- brewer.pal(7,brewerpal)
    #calling mapCountryData
    #
    mapcases <-mapCountryData( wmap
                                 , nameColumnToPlot=varname
                                 , addLegend=FALSE
                                 , catMethod = catmethod
                                 , colourPalette = colourpalette
                                 , missingCountryCol = gray(.8)
    )
    #
    #Add legend:
    #
    do.call( addMapLegend
             , c( mapcases
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ) )

    #
    dev.off()
    #Trim if asked and available
    if(trimplot){
      img=image_read(fname)
      img2=image_trim(img)
      image_write(img2, path = fname, format = "png")
      Sys.sleep(1)
      #
    }
    #ok
  }
  #
  stopCluster(cl)
  #
  # OK
  #
  ###################################################
  #############:Plot 2D World Maps for all variables
  ###################################################
  #
  }
  #
  if(plotgoogle)
  {
    #
    cat('Create and save Google Maps for all variables and world countries:','\n')
    #
  ######################################################
  ######################################################
  ############### plot google maps files:
  ######################################################
  ######################################################
  #
  googlevars=setdiff(colnames(dp),c("name","ISOAlpha2","ISOAlpha3","long","lat"))
  cat('Variables that will be google plotted are:','\n')
  cat(googlevars,'\n')
  #
  dloc=dp
  #Round variables that will be mapped:
  dloc[,googlevars]=apply(dloc[,googlevars],2,function(x){round(x,digits=2)})
  coordinates(dloc)=c("long","lat")
  proj4string(dloc)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  dloc_ll<-spTransform(dloc,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  #
  # data("SAGA_pal")
  #Create directory for Google Maps
  oldwd=getwd()
  #
  dirgoogle=paste0(getwd(),'/googlemaps/')
  dir.create(dirgoogle,showWarnings =FALSE)
  #Work in this directory:
  setwd(dirgoogle)
  #
  for(varn in googlevars){
    # cat(varn,'\n')
    obj=dloc_ll[varn]
    #
    fname=paste0('GoogleMap_',varn,'.html')
    mp <- plotGoogleMaps(obj, filename=fname, zcol=varn, add=FALSE, colPalette=SAGA_pal[[sagapal]],openMap=FALSE)
  }
  #Return to initial directory:
  setwd(oldwd)
  #
  #
  #Plot pies in google map and kml files for archetype mixtures:
  #
  legcolors=c('blue','orange','green','brown','steelblue')
  # legcolors=as.character(brewer.pal(length(anamesorder),brewerpal))
  sapply(legcolors, color.id)
  names(legcolors)=anamesorder
  acolors=legcolors
  mcolors=t(col2rgb(acolors))
  acolors=apply(mcolors,1,function(x){rgb(x[1], x[2], x[3], maxColorValue = 255, alpha = 85)})
  names(acolors)=anamesorder
  #
  duse=dp[,c('ISOAlpha2','long','lat',anamesorder)]
  coordinates(duse) = ~long+lat
  proj4string(duse) = '+proj=longlat +ellps=WGS84 +datum=WGS84'
  duse <- spTransform(duse, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  m<-segmentGoogleMaps(duse, zcol=anamesorder,
                       do.sqrt=FALSE,
                       fillOpacity=0.50,
                       max.radius = maxradius,
                       filename='CountryMixtures.htm',
                       colPalette=acolors, strokeColor='black',openMap=FALSE)
  CountryMixtures<-pieSP(duse,zcol=anamesorder, max.radius=maxradius)
  CountryMixtures$pie=rep(anamesorder,dim(dp)[1])
  # plotKML(pies,'CountryMixtures',col='pie',open.kml = FALSE,plot.labpt=TRUE,LabelScale=0.75,points_names=pnames)
  #
  kml(CountryMixtures, colour = pie, plot.labpt = TRUE,
      labels = pie, kmz = TRUE, balloon=TRUE,
      alpha=0.75,LabelScale=0.75)


  #
  ######################################
  ##############End plotgoogle files
  ######################################
  #
  }
  #
  if(plotnatearth)
  {
    #
    cat('Create, save a nd trime (if available) country mixtures for all archetypes and countries:','\n')
    #
  ###################### Natural Earth maps
  #
  #Function for plot World
  #
  plotnatworld=function(continents=NULL,cy2not='Antarctica'){
    world <- ne_countries(continent=continents)
    world <- world[world$name != c(cy2not),]
    #
    grid.lines.mj <- gridlines(world,easts = seq(-180,180,by=30), norths = seq(-90,90,by=30))
    grid.lines.mi <- gridlines(world,easts = seq(-165,195,by=15), norths = seq(-90,90,by=15))
    world <- spTransform(world, CRS("+proj=wintri"))
    grid.lines.mj <- spTransform(grid.lines.mj,CRS("+proj=wintri"))
    grid.lines.mi <- spTransform(grid.lines.mi,CRS("+proj=wintri"))
    par(mar = c(8, 0.1, 0.1, 0.1))
    plot(as(world, 'Spatial'), expandBB=c(0,0,0.05,0.05))
    plot(grid.lines.mi, col=grey(0.95), add=T)
    plot(grid.lines.mj, col=grey(0.875), add=T)
    plot(world, add=TRUE, border=grey(0.2), col=grey(0.975))
    return(world$iso_a2)
  }
  #
  #Plot variables in bubble format:
  #Remove special character $ if existing:
  #
  googlevars=setdiff(colnames(dp),c("name","ISOAlpha2","ISOAlpha3","long","lat"))
  cat('Variables that will be google plotted are:','\n')
  cat(googlevars,'\n')
  #
  bubblevars=googlevars
  bubblevars=gsub('$', 'Dollars', bubblevars, fixed = TRUE)
  #Create auxiliary data frame 'dh':
  dh=dp[,c("long","lat",googlevars)]
  colnames(dh)=c("long","lat",bubblevars)
  #
  #Plot all variables
  #
  for(varname in bubblevars){
  #
  png(paste0(varname,'_2.png'),width = 1920,height = 1080)
  #
  cy2=plotnatworld()
  #
  wdata=dh[,c("long","lat",varname)]
  coordinates(wdata) = ~long+lat
  proj4string(wdata) = '+init=epsg:4326'
  wdata <- spTransform(wdata, CRS("+proj=wintri"))
  # text(labels(grid.lines.mj, side=1:2, labelCRS = CRS("+init=epsg:4326")), col = grey(.6), offset=0.3)
  #
  vreal=eval(parse(text=paste0('wdata$',varname)))
  vmin=min(vreal,na.rm=TRUE)
  vmax=max(vreal,na.rm = TRUE)
  t01=function(x,xmin,xmax){(x-xmin)/(xmax-xmin)}
  invt01=function(y,xmin,xmax){y*(xmax-xmin)+xmin}
  vplot=10*t01(vreal,vmin,vmax)
  #
  plot(wdata, add=TRUE, col=col.bubble, pch=20,cex=vplot)
  #
  v=seq(min(vplot,na.rm = T),max(vplot,na.rm = T),length.out = 6)
  vshow=round(as.double(sort(v[v>0])),digits=2)
  vshow
  iv=invt01(vshow/10,vmin,vmax)
  iv
  #
  if(vmax>180){
    fiv=format(iv, scientific=T,digits=2)
    vlegend=fiv}else{
      vlegend=round(iv,digits=2)
      }
  #
  #
  legend(bty="n",'bottom',legend = vlegend, pch = 20,pt.cex = vshow,text.col ='black',
    box.col=grey(0.9),col = '#FF5A0088',title=varname,horiz =T)
  #
  dev.off()
  #
  }
  #
  #ok
  #
  #######
  ############### Plot all archetypes country mixtures in one map for all countries
  #######
  #
  #
  pdf('CountryMixtures.pdf',width = 0,height = 0,paper='a4r')
  #
  # png('amix1.png',units='px', width=1600, height=1600, res=300)
  #
  cy2=plotnatworld()
  legcolors=c('blue','orange','green','brown','steelblue')
  # legcolors=as.character(brewer.pal(length(anamesorder),brewerpal))
  sapply(legcolors, color.id)
  names(legcolors)=anamesorder
  acolors=legcolors
  mcolors=t(col2rgb(acolors))
  acolors=apply(mcolors,1,function(x){rgb(x[1], x[2], x[3],maxColorValue =  255, alpha = 85)})
  names(acolors)=anamesorder
  for(varname in anamesorder){
      cola=acolors[varname]
      wdata=dh[,c("long","lat",varname)]
      # wdata=dh[cy,c("long","lat",varname)]
      coordinates(wdata) = ~long+lat
      proj4string(wdata) = '+init=epsg:4326'
      wdata <- spTransform(wdata, CRS("+proj=wintri"))
      #
      vreal=eval(parse(text=paste0('wdata$',varname)))
      summary(vreal)
      vmin=min(vreal,na.rm=TRUE)
      vmax=max(vreal,na.rm = TRUE)
      t01=function(x,xmin,xmax){(x-xmin)/(xmax-xmin)}
      invt01=function(y,xmin,xmax){y*(xmax-xmin)+xmin}
      vplot=10*t01(vreal,vmin,vmax)
      summary(vplot)
      plot(wdata, add=TRUE, col=cola, pch=20,cex=vplot)
    }
  #
  # legend('bottom', legend=anamesorder, horiz =T,bty='n',xjust=0.5,yjust=0,cex=0.7,pch=15,col=legcolors,text.col=legcolors)
  # legend('top', legend=anamesorder, horiz =T,bty='n',xjust=0.5,yjust=1,cex=0.7,pch=15,col=legcolors,text.col=legcolors)
  legend('left', legend=anamesorder, horiz =F,bty='n',xjust=0.5,yjust=0,cex=1.2,col=legcolors,text.col=legcolors)
  # dev.off()
  #
  vvmin=min(as.vector(dp[,anamesorder]))
  vvmax=max(as.vector(dp[,anamesorder]))
  vlegend=round(seq(vvmin,vvmax,length.out = 1+length(anamesorder)),digits=2)
  vlegend=vlegend[vlegend>0]
  vshow=10*t01(vlegend,vvmin,vvmax)
  summary(vshow)
  chlegend=paste0(100*vlegend,"%")
  #
  legend(bty="n",'top',legend = chlegend, pch = 20,pt.cex = vshow,text.col ='black', box.col=grey(0.9),col='gray',
    title='Country Mixtures', horiz =T)
  #
  dev.off()
  #
  #
  if(plotcrop){plot_crop('CountryMixtures.pdf')}
  #
  }
  #End report
  T2=Sys.time();print(as.POSIXlt(T2, "GMT")-as.POSIXlt(T1, "GMT"),quote=F)
  sink()
  #End all actions
}
#eof
