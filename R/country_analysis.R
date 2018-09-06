#' @title Create country mixtures for archetypes and indices HH, NH, MI
#' @description
#' Read processed data frame, read variables, archetype data frame, variable category and 
#' create country mixtures for archetypes plus indices HH, NH, MI
#' @param wavedatain The rds file with columns the World Values to be used
#' @param varnames R The vector of names of values to be used
#' @param archetypesin The rds file with columns the final archetype found
#' @param varcat The character name of values category (default='Schwartz')
#' @param exclude Logical entry set to FALSE
#' @return NULL
#' @importFrom DescTools Herfindahl
#' @importFrom openxlsx write.xlsx
#' @export
#' @examples 
#' #country_analysis(wavedatain,varnames,archetypesin)
country_analysis <-
function(wavedatain,varnames,archetypesin,varcat='Schwartz',exclude=FALSE){
# country mixture analysis
# now using Wave 6 complete case data and boot averages for archetypes
# this version generates 3 country proportion tables
# 1) based on full data
# 2) based on excluding those > median of distance to nearest archetype
# 3) based on exclduing those closer to "middle" (global average) than nearest archetype
#
# exclude <- FALSE
# library(DescTools)
# library(WriteXLS)
sink(file=paste0(varcat,"_country_analysis_report.txt"),split = T)
cat("Country mixture analysis","\n")
t1=Sys.time()
wavedata <- readRDS(wavedatain)
cat(paste0("summary of input respondent data for ",varcat),"\n")
print(t(apply(wavedata[,varnames],2,summary)),quote=F)
cat("respondents by country","\n")
print(table(wavedata$cyname),quote=F)
cat("Averages of varnames variables are:","\n")
Middle <- apply(wavedata[,varnames],2,mean,na.rm=T)
print(Middle,quote=F)
#######
archetypes <- readRDS(archetypesin) #watch this, can be a list or a simple df
#archetypes <- archetypes[["BestFit_Archetypes"]]
cat("Input archetypes to use","\n")
print(archetypes,quote=F,row.names = F)
####
kappa <- nrow(archetypes)
cat(paste0("Kappa is : ",kappa),"\n")
#
#transpose
t_data <- as.matrix(t(wavedata[,varnames]))
t_arches <- as.matrix(t(archetypes[,varnames]))
###
cat("adding in global average to transposed archetypes","\n")
t_arches <- cbind(t_arches,Middle)
# find nearest archetype
fdistimin=function(XCOL,XC){
  #function that returns the position of minimum distance from the archetypes XC
  ds=colSums((XCOL-XC)^2);return(which.min(ds))}
# distance to nearest archetype
fdistimin2=function(XCOL,XC){
  #function that returns the minimum distance from the archetypes XC
  ds=sqrt(colSums((XCOL-XC)^2));return(min(ds))}
# all k distances returned
fdistall=function(XCOL,XC){
  #function that returns all the k distances from the k archetypes XC
  ds=sqrt(colSums((XCOL-XC)^2));return(ds)}
#
# apply these
nearestA <- apply(t_data,2,fdistimin,XC=t_arches[,1:kappa]) # nearest ignoring global average
nearestA_d <- apply(t_data,2,fdistimin2,XC=t_arches[,1:kappa])
all_d <- apply(t_data,2,fdistall,XC=t_arches) # this includes global average
all_d <- t(all_d) #others are simple vectors, this matrix
colnames(all_d) <- paste0("d_",colnames(all_d))
###
wavedata <- cbind(wavedata,nearestA,nearestA_d,all_d)
#
dist_med <- aggregate(wavedata$nearestA_d,list(A=wavedata$nearestA),median)
colnames(dist_med) [which(colnames(dist_med)=="x")] <- "med_dist"
cat("medians of distances to nearest archetype, by archetype","\n")
print(dist_med,quote = F)
wavedata <- merge(wavedata,dist_med,by.x="nearestA",by.y="A",all.x = T,sort=F)
wavedata[,"Assign_med_d"] <- ifelse(wavedata[,"nearestA_d"] <= wavedata[,"med_dist"],TRUE,FALSE)
wavedata[,"Archetype_med_d"] <- ifelse(wavedata[,"Assign_med_d"],wavedata[,"nearestA"],0)
wavedata[,"Assign_middle"] <- ifelse(wavedata[,"nearestA_d"] <= wavedata[,"d_Middle"],TRUE,FALSE)
wavedata[,"Archetype_middle"] <- ifelse(wavedata[,"Assign_middle"],wavedata[,"nearestA"],0)
#Save processed file
saveRDS(wavedata,paste0("data_nearest.rds"))
#
tab_out <- vector(mode="list",length=3)
names(tab_out) <- c("Full","Median","Middle")
# tablulate full data
tab1 <- table(wavedata$cyname,wavedata$nearestA)
tab2 <- round(prop.table(tab1,margin=1),5)
out <- as.data.frame.matrix(tab2)
colnames(out)  <- row.names(archetypes)
tab_out[[1]] <- out
# tabulate median exclusion
tab1 <- table(wavedata$cyname,wavedata$Archetype_med_d)
tab2 <- round(prop.table(tab1,margin=1),5)
out <- as.data.frame.matrix(tab2)
colnames(out)  <- c("Median_Exclude",row.names(archetypes))
tab_out[[2]] <- out
# tabulate global average exclusion
tab1 <- table(wavedata$cyname,wavedata$Archetype_middle)
tab2 <- round(prop.table(tab1,margin=1),5)
out <- as.data.frame.matrix(tab2)
colnames(out)  <- c("Global_Exclude",row.names(archetypes))
tab_out[[3]] <- out
for (kk in 1:3) {
# herfindahl index
# max is 1 but here we have floor
t_col <- ncol(tab_out[[kk]])
floor <- Herfindahl(rep(1/t_col,t_col))
tab_out[[kk]] [,"HH"] <- round(apply(tab_out[[kk]],1,Herfindahl),2)
# normalize
tab_out[[kk]] [,"NHH"] <- round(((tab_out[[kk]] [,"HH"] - floor)/(1 - floor)),2)
# Sunil's index
tab_out[[kk]] [,"MI"] <- 1 - tab_out[[kk]] [,"NHH"]
print(paste0("Table: ",names(tab_out[kk])),quote=F)
print(round(tab_out[[kk]],2),quote=F)
print(summary(tab_out[[kk]]),quote=F)
temp <- as.data.frame(matrix(nrow=nrow(tab_out[[kk]]),ncol=2))
temp[,1] <- paste0(row.names(tab_out[[kk]]))
temp[,2] <- tab_out[[kk]] [,"MI"]
print(paste0("Table: ",names(tab_out[kk]),"--countries ordered by decreasing MI"),quote=F)
print(temp[order(temp[,2],decreasing = T),],quote=F)
}
# WriteXLS(tab_out,paste0("Kappa_",kappa,"_country_tables.xlsx"),row.names = TRUE)#deprecated due to Perl installation needed
write.xlsx(tab_out,paste0("Kappa_",kappa,"_country_tables.xlsx"),creator='David F. Midgley',row.names=TRUE,col.names=TRUE)
#
sink(file=NULL)
#eof
}
