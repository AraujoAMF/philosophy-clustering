library(data.table)
library(ggplot2)
library(plotly)

preprocessed_data_path <- "/home/allan/Documentos/gutenberg_vis/selected_books"
images_path <- "/home/allan/Documentos/gutenberg_vis/images"

dtm_tsne_2 <- readRDS(file.path(preprocessed_data_path, "dtm_tsne_2.RDS"))

kmeans_result <-lapply(2:50, function(x) {
  cat(sprintf("Runnig the %d/50 step\n", x))
  set.seed(0)
  kmeans_result <- kmeans(dtm_tsne_2[, .(tsne_x, tsne_y)], centers = x, iter.max = 1000)
  data.table(clusters = x, tot_withinss = kmeans_result[["tot.withinss"]])
})
kmeans_result <- rbindlist(kmeans_result)
g_kmeans <- ggplot(kmeans_result) + geom_line(aes(x = clusters, y = tot_withinss)) +
  scale_x_continuous(breaks = 1:50) + 
  xlab("Number of clusters")+ ylab("Total Within Clusters Sum of Squares") + 
  ggtitle("Total Within Clusters Sum of Squares by Cluster")

ggsave(file.path(images_path, "kmeans_withness.png"), width = 8*((1+sqrt(5))/2),height = 8)

#We'll going to choose 12 clusters
set.seed(0)
kmeans_model <- kmeans(dtm_tsne_2[, .(tsne_x, tsne_y)], centers = 12, iter.max = 1000)

dtm_tsne_2[, cluster := kmeans_model[["cluster"]]]
dtm_tsne_2[, cluster := as.factor(cluster)]


g <- ggplot(dtm_tsne_2) + geom_text(aes(x = tsne_x, y = tsne_y, label = author, col  = cluster, text = title)) + 
  theme(legend.position="none")+
  ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d Clustered with 12-means", 2))
ggsave(file.path(images_path, "philosophy_clustered.png"), width = 8*((1+sqrt(5))/2),height = 8)
ggplotly(g)

saveRDS(dtm_tsne_2, file.path(preprocessed_data_path, "dtm_tsne_2_clustered.RDS"))
saveRDS(g, file.path(preprocessed_data_path, "g_final.RDS"))


