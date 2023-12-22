####################################
# This script gives an overview of the linear size spectrum function used in the manuscript
# Code written by Valentin Marin // https://doi.org/10.1016/j.ecolind.2022.109833
####################################

# Load package
library(dplyr)

# Load functions
#List of parameters to include in this function:
##' @param X numeric individual body size, either length, weight or volume
##' @param min.size minimum starting point to be considered
##' @param max.size maximum size to be considered
##' @param sc number of size classes considered
##' @param base classification of the size classes following a geometric series
##' @param nis number of increased size in the base form. Usually should be 1
##' @param accum.size if smallest individuals can be considered (yes) or not (no)
##' @return the parameters of the size spectra, number of size classes, number of intermediate and extreme empty size classes, and a frequency table

# binned size spectra function
size.spectra <- function(X,min.size,max.size,sc,base,nis,accum.size)
{
  if (!is.numeric(X)){
    stop("This function only works for numeric inputs")
  }
  
  #First part: Create frequency table with all information of each size class
  sc.less <- sc - 1 
  width <- c(base^seq(1,sc,by=nis) - base^seq(0,sc.less,by=nis)) # create width of the size classes following a geometric series
  midpoint <-  (base^seq(1,sc,by=nis) - base^seq(0,sc.less,by=1))/2 +  base^seq(0,sc.less,by=nis)
  low.bound <- width[c(1:(sc))]# create lower boundary of each size class
  upp.bound <- width[c(2:(sc+1))]# create upper boundary of each size class
  fre.table <- data.frame(size.classes=c(1:sc ),low.bound=low.bound,upp.bound=upp.bound,midpoint=midpoint,width=width)#create final frequency table
  select.max.sc <- which.min(abs(fre.table$low.bound - max.size))#select closest value to the maximum size class
  fre.table <- fre.table[c(1:select.max.sc),] 
  fre.table$upp.bound[dim(fre.table)[1]] <- Inf
  class <- c(base^seq(0,select.max.sc,by=nis))
  class[length(class)] <-  Inf
  value <-length(class)  - 1 
  if(accum.size == "yes"){#if yes, one can accumulate individuals in the smallest size class
    select.min.sc <-  which.min(abs(fre.table$upp.bound - min.size))
    class[select.min.sc] <- - Inf
    class <- class[c(select.min.sc:length(class))]
    fre.table <- fre.table[c(select.min.sc:dim(fre.table)[1]),] 
    fre.table$low.bound[1] <- -Inf
    fre.table$size.classes <- c(1:dim(fre.table)[1])
    value <- length(class)  - 1 
  } 
  data.abund <- data.frame(values=X,size.classes=cut(X, class,label=c(1:value)))
  data.abund.sum <- aggregate(data.abund$values~data.abund$size.classes,FUN=length)
  colnames(data.abund.sum ) <- c("size.classes","values")
  fre.table  <- merge(fre.table,data.abund.sum ,by="size.classes",all.x = T)
  
  fre.table$norm.abun <- fre.table$values / fre.table$width
  
  #Second part: calculate number of intermediate and extreme empty size classes
  vector <-  fre.table$norm.abun
  vector[which(vector == 0)] <- NA
  idx <- which(!is.na(vector))
  int.empt.size.class <- length(setdiff(min(idx):max(idx),idx))#count the number of empty intermediate size classes
  extreme.empt.size.class <- length(which(is.na(vector))) - int.empt.size.class #count the number of empty extreme size classes
  
  #Third part: compute size spectra parameters
  if( length(which(is.na(fre.table$norm.abun)))==0){
    model <- lm(log2(fre.table$norm.abun) ~ scale(log2(fre.table$midpoint),center=T, scale=F)) #ordinary-least-square linear regression 
  } else{
    model <- lm(log2(fre.table$norm.abun[-which(is.na(fre.table$norm.abun))])~scale(log2(fre.table$midpoint[-which(is.na(fre.table$norm.abun))]),center=T, scale=F)) #ordinary-least-square linear regression 
  }
  output <- list(
    NASS.intercept = round(summary(model)$coefficients[1,1],3),
    NASS.slope = round(summary(model)$coefficients[2,1],3),
    NASS.adj.R.squared =round(summary(model)$adj.r.squared,3),
    NASS.p.value = round(summary(model)$coefficients[2,4],4),
    number.size.classes = dim(fre.table)[1] -  int.empt.size.class - extreme.empt.size.class,
    int.empt.size.class = int.empt.size.class,
    extreme.empt.size.class  = extreme.empt.size.class,
    frequency.table = fre.table
  )
  return(output)
}
