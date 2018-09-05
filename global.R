library(shiny)
library(DT)
require(readxl)
require(dplyr)
require(magrittr)
require(reshape2)
require(RColorBrewer)
require(ggpubr)
require(ggplot2)
require(knitr)
require(kableExtra)
require(htmltools)
require(plotly)
require(shinyjs)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm),
                         min  = min(xx[[col]],na.rm=na.rm),
                         max  = max(xx[[col]],na.rm=na.rm),
                         range = max(xx[[col]],na.rm=na.rm)-min(xx[[col]],na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- plyr::rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}

grps_cols = c("gray0", "red","dark green","yellow","purple","blue","grey",
              "chartreuse1","light blue","palevioletred1","salmon4","peachpuff","gray48",
              "greenyellow","orange","salmon","purple2","lightgoldenrod1","brown4","darkorchid4")

vecSummary=function(v){
    mv=sprintf('%.2f',mean(v))
    n=length(v)
    sem=sprintf('%.2f',sd(v)/sqrt(n))
    paste0(mv,' +/- ',sem,'(',n,')')
}

#convert a matrix a vector
matrix2vec=function(m){
    vec=as.vector(m)
    names(vec)=as.vector(sapply(colnames(m),paste,rownames(m),sep=" - "))
    vec
}

twoSampleTest=function(v1,v2){
    v1=v1[!is.na(v1)]
    v2=v2[!is.na(v2)]
    if(n_distinct(v1)<2 | n_distinct(v2)<2) return(NA)
    pb=bartlett.test(list(v1,v2))$p.val #bartlett test 
    pval=NA
    if(pb<0.05){
        #pval=round(wilcox.test(v1,v2)$p.val,3)
        pval=sprintf("%.3g",wilcox.test(v1,v2)$p.val)
    }else{
        pval=sprintf("%.3g",t.test(v1,v2)$p.val)
    }
    return(as.numeric(pval))
}

mytheme <- theme(plot.title=element_text(face="bold.italic",size=16, color="brown",hjust=0.5),
                 axis.title.x=element_text(face="bold.italic",size=14, color="brown"),
                 axis.text=element_text(face="bold", size=10,color="darkblue"),
                 axis.title.y=element_text(face="bold",size=14, color="brown"),
                 strip.text = element_text(face="bold", size=12,color="brown"),
                 panel.background=element_rect(fill="white",color="darkblue"),
                 panel.grid.major.y=element_line(color="grey",linetype=1),
                 panel.grid.minor.y=element_line(color="grey",linetype=2),
                 panel.grid.minor.x=element_blank(),
                 legend.position="top")