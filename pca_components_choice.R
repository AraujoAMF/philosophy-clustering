library(data.table)
library(ggplot2)
library(scales)
preprocessed_data_path <- "/home/allan/Documentos/gutenberg_vis/selected_books"
images_path <- "/home/allan/Documentos/gutenberg_vis/images"
#read the preprocessed data
dtm <- readRDS(file.path(preprocessed_data_path, "dtm.RDS"))
books_names  <-  readRDS(file.path(preprocessed_data_path, "books_names.RDS"))
books_author <-  readRDS(file.path(preprocessed_data_path, "books_author.RDS"))

which(apply(dtm, 2, var)==0)
#100229 
#the word take don't have importance in our document term frequency and we'll remove it
dtm <- dtm[, -100229]

#let's compute the pca manually because 50 dimensions may not be enough 
dtm_pca <- prcomp(dtm, scale. = TRUE)

#analyze the explained variance for each component
variances <- apply(dtm_pca[['x']], 2, var)  
proportions <- vars / sum(variances)
proportions <- cumsum(proportions)

cum_explained_variance <- data.table(component = 1:177, proportions)

g_var <- ggplot(cum_explained_variance) + geom_line(aes(x = component, y  = proportions)) +
  scale_y_continuous(breaks = seq(0,1,0.05), labels = percent) + 
  xlab("PCA component") + ylab("Cumulative Proportion of Variance") + 
  ggtitle("Explained Cumulative Proportion of Variance by PCA Component")

ggsave(file.path(images_path, "pca_proportions.png"), width = 8*((1+sqrt(5))/2),height = 8)
cum_explained_variance[proportions > 0.8][1]
#in order to get 80% of our variance we need at least 82 components

saveRDS(dtm_pca[["x"]][, 1:82], file.path(preprocessed_data_path, "dtm_red.RDS"))
