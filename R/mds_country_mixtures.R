#' @title Create MDS configuration for countries
#' @description
#' Read country data and parameters in order ot create MDS configuration
#' @param n_arches The integer number of archetypes used
#' @param ndim The integer dimension of MDS configuration
#' @param countrydata The excel file with country data to be used as input
#' @param exclude Logical entry set to FALSE
#' @param dmetric The distance metric to be used
#' @param usebest Logical entry set to FALSE if Torgerson will be used
#' @param perm Logical entry set to TRUE if permutation will be performed
#' @param jack Logical entry set to TRUE if jackknife will be performed
#' @param use_indices Logical entry set to TRUE if country indices will be used
#' @return NULL
#' @importFrom readxl read_excel
#' @importFrom tibble rownames_to_column
#' @importFrom plotrix color.id
#' @import smacof
#' @importFrom robCompositions aDist
#' @importFrom openxlsx write.xlsx
#' @examples
#' #mds_country_mixtures(n_arches,ndim,countrydata)
#' @export
mds_country_mixtures=function(n_arches,ndim=3,countrydata,exclude=FALSE,dmetric="standard",usebest=FALSE,perm=TRUE,jack=TRUE,use_indices=TRUE){
# mapping country mixtures
# this version, switch for altered index numbers if "excluded"
# reading tables from multisheet Excel
run_title <- paste0("MDS_exclude_",exclude,"_dmetric_",dmetric,"_D",ndim)
sink(file=paste0(run_title,".txt"),split=T)
cat("settings are","\n")
cat("archetypes = ",n_arches,"dimensions = ",ndim,"\n")
cat("exclude = ",exclude,"dmetric = ",dmetric,"\n")
cat("use best = ",usebest,"\n")
if(dmetric=="Aitchison"){ cat("Aitchison distance metric used","\n") }else{ cat("Euclidean distance metric used", "\n")}
#
if(dmetric=="standard"){ dmethod <- "euclidean"}
#
if(exclude){ sheetname <- "Median" }else{sheetname <- "Full"}
cat("reading sheet:",sheetname,"\n")
dinput <- as.data.frame(read_excel(paste0("Kappa_",n_arches,"_country_tables.xlsx"),sheet = sheetname))
dinput[,c("HH","NHH","MI")] <- NULL
colnames(dinput) [1] <- "Country"
print(head(dinput),quote=F)
if (exclude){dinput$Median_Exclude <- NULL}# drop them if present
a_names <- colnames(dinput[2:(n_arches+1)]) # read archetypes given names
# get country data
country_data <- as.data.frame(read_excel(countrydata))
dinput <- merge(dinput,country_data,by="Country",all.x = FALSE,sort=FALSE) # keep countries in mixture table/input order
#Remove WVS_Code
dinput=dinput[,setdiff(colnames(dinput),'WVS_Code')]
####
if(dmetric=="standard"){dissim <- dist(dinput[,a_names],diag=TRUE,method = dmethod)}
if(dmetric=="Aitchison"){dissim <- aDist(dinput[,a_names])}
attr(dissim,"Labels") <- dinput[,(n_arches+2)] # watch this, it is the ISO code column
res <- smacofSym(dissim,ndim = ndim,type="ratio")
cat(paste0("overall stress: ",res$stress),"\n")
cat("stress by country: ","\n")
print(res$spp,quote=F)
cat("random starting configurations","\n")
stressvec <- NULL
set.seed(1234567)
fitran <- vector(mode ="list",length=20)
for (k in 1:30) {
  fitran[[k]] <- smacofSym(dissim,ndim=ndim,init="random",type = "ratio")
  stressvec[k] <- fitran[[k]]$stress
}
cat("stress for random starts","\n")
print(stressvec,quote=F)
best <- which.min(stressvec)
cat(paste0("minimum of ",stressvec[best]," found at ",best),"\n")
if(stressvec[best] < res$stress) {
      temp <- round((1 - (stressvec[best]/res$stress))*100,2)
      cat(paste0("random solution is ",temp," % better than classical solution"),"\n")
      if(usebest) {
        cat("random solution is used hereafter","\n")
        res <- fitran[[best]]
        } else cat("however, set to use classical solution", "\n")
} else cat("classical solution is best and used hereafter","\n")
#####
pdf(file=paste0("plots_",run_title,".pdf"),paper="a4r",height=9,width=11)
#
if(perm) {
cat("permutation test of solution:","\n")
res.perm <- permtest(res,nrep = 30,verbose = F)
print(res.perm,quote=F)
hist(res.perm$stressvec,xlab="Stress Values", main="Permutations")
}
#######
if(jack) {
jackfit <- jackknife(res)
cat("jacknife test of stability:","\n")
print(jackfit,quote=F)
}
#####
# output configurations at this point
cat("writing final configurations to .rds & Excel","\n")
configs <- as.data.frame(res$conf)
dnames <- colnames(configs)
configs <- rownames_to_column(configs,var = "ISOAlpha2")
#Ssve configurations:
saveRDS(configs,paste0("configs_",run_title,".rds"))
# WriteXLS(configs,paste0("configurations_",run_title,".xlsx"),row.names = FALSE)
write.xlsx(configs,paste0("configurations_",run_title,".xlsx"),row.names=FALSE,col.names=TRUE,sheetName='configs')
########
cat("biplots of archetypes start:","\n")
x <- biplotmds(res,dinput[,a_names])
print(summary(x))
vs <- 0.7
lim_x <- c(-1.5,1.5)
lim_y <- c(-1.5,1.5)
plot(res,plot.type = "stressplot")
plot(x,plot.dim = c(1,2),main="Country Mixtures + Archetypes",vecscale=vs)
plot(x,plot.dim = c(1,3),main="Country Mixtures + Archetypes",vecscale=vs)
plot(x,plot.dim = c(2,3),main="Country Mixtures + Archetypes",vecscale=vs)
#
if(use_indices) {
  # merge configurations and dinput
  dnext <- merge(dinput,configs,by="ISOAlpha2",sort=FALSE)
  # build table of biplot regressions
  # "dinput" has archetype mixtures, plus indices
  # for plots of indices we need to do one by one, also this allows to use maximum numbers of observations for each index
  table_biplot <- as.data.frame(matrix(nrow=length(c(a_names,9:(ncol(dnext)-3))),ncol=17))
  colnames(table_biplot) <- c("Variable",paste0("coef_D",1:3),paste0("t_D",1:3),paste0("t_prob_D",1:3),
                              "R_Square","F","df1","df2","F_prob","test","include")
  jj <- 1
  X <- as.matrix(dnext[,dnames]) #used in all cases, order enforced
  for (ii in c(3:7,9:50)) {
    a_biplot <- summary(lm(scale(dnext[,ii]) ~ -1 + X))
    table_biplot[jj,"Variable"] <- colnames(dnext) [ii]
    table_biplot[jj,"coef_D1"] <- a_biplot$coefficients[1,"Estimate"]
    table_biplot[jj,"coef_D2"] <- a_biplot$coefficients[2,"Estimate"]
    table_biplot[jj,"coef_D3"] <- a_biplot$coefficients[3,"Estimate"]
    table_biplot[jj,"t_D1"] <- a_biplot$coefficients[1,"t value"]
    table_biplot[jj,"t_prob_D1"] <- a_biplot$coefficients[1,"Pr(>|t|)"]
    table_biplot[jj,"t_D2"] <- a_biplot$coefficients[2,"t value"]
    table_biplot[jj,"t_prob_D2"] <- a_biplot$coefficients[2,"Pr(>|t|)"]
    table_biplot[jj,"t_D3"] <- a_biplot$coefficients[3,"t value"]
    table_biplot[jj,"t_prob_D3"] <- a_biplot$coefficients[3,"Pr(>|t|)"]
    table_biplot[jj,"R_Square"] <- a_biplot$r.squared
    table_biplot[jj,"F"] <- a_biplot$fstatistic["value"]
    table_biplot[jj,"df1"] <- a_biplot$fstatistic["numdf"]
    table_biplot[jj,"df2"] <- a_biplot$fstatistic["dendf"]
    table_biplot[jj,"F_prob"] <- pf(a_biplot$fstatistic["value"],
                                    a_biplot$fstatistic["numdf"],a_biplot$fstatistic["dendf"],lower.tail = F)
    if(table_biplot[jj,"F_prob"] < 0.001) {
      table_biplot[jj,"test"] <- "p<0.001"
      table_biplot[jj,"include"] <- TRUE
      } else {
      if(table_biplot[jj,"F_prob"] < 0.005) {
        table_biplot[jj,"test"] <- "p<0.005"
        table_biplot[jj,"include"] <- TRUE
      } else {
          table_biplot[jj,"test"] <- "ns"
          table_biplot[jj,"include"] <- FALSE
          }}
    jj <- jj + 1
    }
  table_biplot[,c(2:12,15)] <- round(table_biplot[,c(2:12,15)],3)
  # WriteXLS(table_biplot,paste0("biplot_regressions_",run_title,".xlsx"))
  write.xlsx(table_biplot,paste0("biplot_regressions_",run_title,".xlsx"),row.names=FALSE,col.names=TRUE,sheetName='table_biplot')
}
#
dev.off()
sink(file=NULL)
#eof
}
