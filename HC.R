#Task 1 : Implementation of hierarchical agglomerative clustering

#euclidean distance
eucDist <- function(a,b){
  return((sum((a-b)^2))^0.5)
}

#calculating distance matrix
hClustering <- function(clusterList, m, method){
  distanceMatrix <- matrix(Inf, nrow=length(clusterList),ncol=length(clusterList))
  for(i in 1:length(clusterList)){
    i_index_vector <- c()
    for(j in 1:length(clusterList)){
      j_index_vector = c()
      if(i<j){
        for(k in 1:length(clusterList[[i]])){
          a = clusterList[[i]]
          for(n in length(a[[k]])){
            i_index_vector <- c(i_index_vector, a[[k]][n])
          }
        }
        for(k in 1:length(clusterList[[j]])){
          a = clusterList[[j]]
          for(n in length(a[[k]])){
            j_index_vector <- c(j_index_vector, a[[k]][n])
          }
        }
        if(method=='centroid'){
          if(length(i_index_vector)>1){
            colm_i <- colMeans(m[i_index_vector,])
          }
          else{
            colm_i <- m[i_index_vector,]
          }
          if(length(j_index_vector)>1){
            colm_j <- colMeans(m[j_index_vector,])
          }
          else{
            colm_j <- m[j_index_vector,]
          }
          dist = eucDist(colm_i,colm_j)
          distanceMatrix[i,j] <- dist
        }
        else{
          dist = c()
          for(i_value in i_index_vector){
            for(j_value in j_index_vector){
              if(i_value!=j_value){
                i_values <- m[i_value,]
                j_values <- m[j_value,]
                d = eucDist(i_values,j_values)
                dist <- c(dist, d)
              }
            }
          }
          if(method=='single'){
            one_dist <- min(dist)
          }
          else if(method =='complete'){
            one_dist <- max(dist)
          }
          else if(method=='average'){
            one_dist <- mean(dist)
          }
          distanceMatrix[i,j] <- one_dist
        }
      }
    }
  }
  return (distanceMatrix)
  
}
#main method for hierarchical agglomerative clustering
makeCluster <- function(m,labels,method){
  clusterList <- list()
  for(i in 1:nrow(m)){
    clusterList[[i]]<-list(i) 
  }
  for(n in 1:nrow(m)-1){
    output <- sprintf("---------------------------------- Level %s --------------------------------- ", n)
    print(output)
    if(method == 'single'){
      distanceMatrix <- hClustering(clusterList,m,'single')
    }
    else if(method == 'complete'){
      distanceMatrix <- hClustering(clusterList,m,'complete')
    }
    else if(method == 'average'){
      distanceMatrix <- hClustering(clusterList,m,'average')
    }
    else if(method == 'centroid'){
      distanceMatrix <- hClustering(clusterList,m,'centroid')
    }
    cluster_val <- which(distanceMatrix == min(distanceMatrix),arr.ind = TRUE)[1,]
    outputLabels <- c()
    if(n<nrow(m)-1){
      height <- distanceMatrix[cluster_val[1],cluster_val[2]]
      mergedto <- clusterList[[cluster_val[1]]]
      mergedfrom <- clusterList[[cluster_val[2]]]
      mer1 = paste(unlist(mergedto), collapse = ',')
      mer2 = paste(unlist(mergedfrom), collapse = ',')
      label1 <- paste(labels[unlist(mergedto),], collapse = ',')
      label2 <- paste(labels[unlist(mergedfrom),], collapse = ',')
      a = sprintf("[%s] to [%s], height = %f, Labels : %s to %s ",mer1,mer2,height,label1,label2)
      print(a)
    }
    if(n==nrow(m)-1){
      mergedto <- clusterList[[cluster_val[1]]]
      mergedfrom <- clusterList[[cluster_val[2]]]
      finaloutout <- sprintf("After Mergings final Cluster : [%s]",paste(unlist(mergedto), collapse = ','))
      outputLabels <- labels[unlist(mergedto),]
      print(finaloutout)
      print(outputLabels)
    }
    if(max(cluster_val)>1){
      if((length(clusterList[[cluster_val[1]]][[1]])==1) && (length(clusterList[[cluster_val[2]]][[1]])==1)){
        clusterList[[cluster_val[1]]][[1]] <- c(clusterList[[cluster_val[1]]][[1]],clusterList[[cluster_val[2]]][[1]])
        if(length(clusterList)!=1){
          clusterList[[cluster_val[2]]] <- NULL
        }
      }
      else{
        clusterList[[cluster_val[1]]] <- c(clusterList[[cluster_val[1]]],clusterList[[cluster_val[2]]])
        if(length(clusterList)!=1){
          clusterList[[cluster_val[2]]] <- NULL
        }
      }
      
    }
  }
  return(clusterList) 
}

#Task 2 : importing NCI Microarray dataset
NCI <- read.table("/Users/shalinipriya/Downloads/Data Analysis/Assignment 3/nci.data.txt", header=F, na.strings="?")
NCI <- t(NCI)
NCI_Labels <- read.table("/Users/shalinipriya/Downloads/Data Analysis/Assignment 3/label.txt", header=F)

#Applying the four linkages of hierarchical agglomerative clustering to NCI dataset
#calling the main method for hierarchical agglomerative clustering for NCI dataset for Single linkage
clusterList <- makeCluster(NCI,NCI_Labels,'single')
#calling the main method for hierarchical agglomerative clustering for NCI dataset for Complete linkage
clusterList <- makeCluster(NCI,NCI_Labels,'complete')
#calling the main method for hierarchical agglomerative clustering for NCI dataset for Average linkage
clusterList <- makeCluster(NCI,NCI_Labels,'average')
#calling the main method for hierarchical agglomerative clustering for NCI dataset for Centroid linkage
clusterList <- makeCluster(NCI,NCI_Labels,'centroid')


#Task 4 - Applying K-means for NCI datset for different values of K
km.out <- kmeans(NCI, 4, nstart=20)
stringofKeeamsCluster <- paste(unlist(km.out$cluster), collapse = ',')
kmeansop <- sprintf("K Mean Clustering algorithm clusters with 4 folds: %s",stringofKeeamsCluster)
print(kmeansop)

km.out <- kmeans(NCI, 20, nstart=20)
stringofKeeamsCluster <- paste(unlist(km.out$cluster), collapse = ',')
kmeansop <- sprintf("K Mean Clustering algorithm clusters with 20 folds: %s",stringofKeeamsCluster)
print(kmeansop)

km.out <- kmeans(NCI, 50, nstart=20)
stringofKeeamsCluster <- paste(unlist(km.out$cluster), collapse = ',')
kmeansop <- sprintf("K Mean Clustering algorithm clusters with 50 folds: %s",stringofKeeamsCluster)
print(kmeansop)
