source("CAH/A2R.R")

analysisData <- reactiveValues(cah = NA, clusters = NA)


runCAH <- function(df, columns, userDistance , userMethod) {
  cahDF <- df[,columns]
  cah <- hclust(dist(cahDF, method = userDistance), method = userMethod)
  return(cah)
}

plotProfiles <- function(df, columns, clusters, Cpal, scale){
  nClusters <- length(unique(clusters))
  df$CLUSCSP <- factor(clusters, levels = 1:nClusters, labels = paste('Cluster', 1:nClusters, sep = " "))
  
  plotDF <- df[, columns]
  
  if (scale){plotDF <- sapply(X = plotDF, FUN = function(x) {scale(x, center = TRUE, scale = TRUE)})}
  
  clusProfile <- aggregate(plotDF,
                           by = list(df$CLUSCSP),
                           mean)
  
  colnames(clusProfile)[1] <- "CLUSTER"
  clusLong <- melt(clusProfile, id.vars = "CLUSTER")
  ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value, fill = CLUSTER),
             stat = "identity") +
    scale_fill_manual(values = Cpal) +
    facet_wrap(~ CLUSTER) +
    coord_flip() + theme_bw()
}