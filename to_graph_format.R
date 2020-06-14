#######################################################################################
require(dplyr)
require(data.table)
require(stringr)
require(readxl)
require(openxlsx)
require(stringdist)
require(tidyr)
library(data.table)
require(mgcv)
#######################################################################################
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}

'%!in%' <- function(x,y)!('%in%'(x,y))

#######################################################################################
dir_i <- "E:/Dropbox/Dropbox/Libro_Springer"

#cancer_list <- read.csv(paste0(dir_i,"/","AT_GEN_LIST_CH.csv"))
path_list <- read.xlsx(paste0(dir_i,"/","Datos Grafo.xlsx"),"TAB_ST",na.strings = "")
families <- unique(na.omit(path_list$FAMILIA))
IUCN <- unique(na.omit(path_list$CATEGORIA_IUCN))
markers <- unique(na.omit(path_list$MARCADORES))
gene_obj <- unique(na.omit(path_list$OBJETIVO_PRINCIPAL))


path_df_iucn  <- lapply(1:length(gene_obj),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$OBJETIVO_PRINCIPAL==gene_obj[[i]])
  ######################################
  IUCN_subset <- unique(x_sub2$CATEGORIA_IUCN)
  x_df <- as.data.frame(t(combn(IUCN_subset,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% IUCN_subset),]
  x_df <- x_df[which(x_df$target  %in% IUCN_subset),]
  if(nrow(x_df>0)){
    
  x_df$feature <- as.character(gene_obj[[i]])
  x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
  x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
  x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
  x_df$feedID3 <- NULL
   return(x_df)
} else {
  cat("no data","\n")
}

  })

#path_df = path_df[-which(sapply(path_df, is.null))]

path_df_iucn <- do.call(rbind,path_df_iucn)
path_df_iucn <- path_df_iucn[complete.cases(path_df_iucn$source),]
write.table(path_df_iucn,paste0(dir_i,"/","GRAPHS/","IUCN_list.tsv"),na = "",row.names = F,quote = F,sep="\t")


##################################################################################################################


path_df_marker  <- lapply(1:length(gene_obj),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$OBJETIVO_PRINCIPAL==gene_obj[[i]])
  ######################################
  marker_subset <- unique(x_sub2$MARCADORES)
  x_df <- as.data.frame(t(combn(marker_subset,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% marker_subset),]
  x_df <- x_df[which(x_df$target  %in% marker_subset),]
  if(nrow(x_df>0)){
    
    x_df$feature <- as.character(gene_obj[[i]])
    x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
    x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
    x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
    x_df$feedID3 <- NULL
    return(x_df)
  } else {
    cat("no data","\n")
  }
  
})



path_df_marker <- do.call(rbind,path_df_marker)
path_df_marker <- path_df_marker[complete.cases(path_df_marker$source),]
write.table(path_df_marker,paste0(dir_i,"/","GRAPHS/","MARKER_list.tsv"),na = "",row.names = F,quote = F,sep="\t")

##################################################################################################################

path_df_family  <- lapply(1:length(gene_obj),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$OBJETIVO_PRINCIPAL==gene_obj[[i]])
  family_subset <- unique(x_sub2$FAMILIA)
  x_df <- as.data.frame(t(combn(family_subset,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% family_subset),]
  x_df <- x_df[which(x_df$target  %in% family_subset),]
  if(nrow(x_df>0)){
    
    x_df$feature <- as.character(gene_obj[[i]])
    x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
    x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
    x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
    x_df$feedID3 <- NULL
    return(x_df)
  } else {
    cat("no data","\n")
  }
  
})



path_df_family <- do.call(rbind,path_df_family)
path_df_family <- path_df_family[complete.cases(path_df_family$source),]
write.table(path_df_family,paste0(dir_i,"/","GRAPHS/","FAMILY_list.tsv"),na = "",row.names = F,quote = F,sep="\t")



###################################################################################


path_df_marker_inv  <- lapply(1:length(markers),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$MARCADORES==markers[[i]])
  ######################################
  marker_subset_inv <- unique(x_sub2$OBJETIVO_PRINCIPAL)
  if(length(marker_subset_inv)>1){
  x_df <- as.data.frame(t(combn(marker_subset_inv,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% marker_subset_inv),]
  x_df <- x_df[which(x_df$target  %in% marker_subset_inv),]
  
  if(nrow(x_df>0)){
    
    x_df$feature <- as.character(markers[[i]])
    x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
    x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
    x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
    x_df$feedID3 <- NULL
    
  
    return(x_df)
  } else {
    cat("no data","\n")
  }
  } else {
    x_df <- data.frame(source=marker_subset_inv,target=marker_subset_inv,feature=as.character(markers[[i]]))
    
  }
  
})



path_df_marker_inv <- do.call(rbind,path_df_marker_inv)
path_df_marker_inv <- path_df_marker_inv[complete.cases(path_df_marker_inv$source),]
write.table(path_df_marker_inv,paste0(dir_i,"/","GRAPHS/","MARKER_list_inv.tsv"),na = "",row.names = F,quote = F,sep="\t")

##################################################################################################################











path_df2  <- lapply(1:length(IUCN),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$CATEGORIA.IUCN==IUCN[[i]])
  #######################################
  studies <- unique(x_sub2$ESTUDIO)
  x_df <- as.data.frame(t(combn(studies,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% studies),]
  x_df <- x_df[which(x_df$target  %in% studies),]
  if(nrow(x_df>0)){
  x_df$feature <- as.character(IUCN[[i]])
  x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
  x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
  x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
  x_df$feedID3 <- NULL
  return(x_df)
  } else {
    cat("no data","\n")
  }
})

#path_df = path_df[-which(sapply(path_df, is.null))]
path_df2 <- do.call(rbind,path_df2)
path_df2 <- path_df2[complete.cases(path_df2$source),]
write.table(path_df2,paste0(dir_i,"/","IUCN_edge_list.tsv"),na = "",row.names = F,quote = F,sep="\t")

##################################################################################################################
path_df3  <- lapply(1:length(markers),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$MARCADORES==markers[[i]])
  #######################################
  studies <- unique(x_sub2$ESTUDIO)
  x_df <- as.data.frame(t(combn(studies,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% studies),]
  x_df <- x_df[which(x_df$target  %in% studies),]
  if(nrow(x_df>0)){
    x_df$feature <- as.character(markers[[i]])
    x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
    x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
    x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
    x_df$feedID3 <- NULL
    return(x_df)
  } else {
    cat("no data","\n")
  }
})

#path_df = path_df[-which(sapply(path_df, is.null))]
path_df3 <- do.call(rbind,path_df3)
path_df3 <- path_df3[complete.cases(path_df3$source),]
write.table(path_df3,paste0(dir_i,"/","markers_edge_list.tsv"),na = "",row.names = F,quote = F,sep="\t")

##################################################################################################################

path_df4  <- lapply(1:length(gene_obj),function(i){
  cat(i,"\n")
  x_sub2 <- subset(path_list,path_list$OBJETIVO.PRINCIPAL==gene_obj[[i]])
  #######################################
  studies <- unique(x_sub2$ESTUDIO)
  x_df <- as.data.frame(t(combn(studies,2)))
  colnames(x_df) <- c("source","target")
  x_df <- x_df[which(x_df$source  %in% studies),]
  x_df <- x_df[which(x_df$target  %in% studies),]
  if(nrow(x_df>0)){
    x_df$feature <- as.character(gene_obj[[i]])
    x_df <- x_df[!(as.character(x_df$source) == as.character(x_df$target)),]
    x_df$feedID3 <- apply(x_df, 1, function(x) paste(sort(c(x[1], x[2])), collapse = '-'))
    x_df <- x_df[!duplicated(x_df[,c('feedID3')]),]
    x_df$feedID3 <- NULL
    return(x_df)
  } else {
    cat("no data","\n")
  }
})

#path_df = path_df[-which(sapply(path_df, is.null))]
path_df4 <- do.call(rbind,path_df4)
path_df4 <- path_df4[complete.cases(path_df4$source),]
write.table(path_df4,paste0(dir_i,"/","obj_edge_list.tsv"),na = "",row.names = F,quote = F,sep="\t")

