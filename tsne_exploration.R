library(data.table)
library(ggplot2)
library(plotly)
library(Rtsne)
library(scales)
preprocessed_data_path <- "/home/allan/Documentos/gutenberg_vis/selected_books"
images_path <- "/home/allan/Documentos/gutenberg_vis/images"

#read the preprocessed data
dtm <- readRDS(file.path(preprocessed_data_path, "dtm_red.RDS"))
books_names  <-  readRDS(file.path(preprocessed_data_path, "books_names.RDS"))
books_author <-  readRDS(file.path(preprocessed_data_path, "books_author.RDS"))

#function to plot the result of the tsne

plot_tsne <- function(tsne_model,perplexity, books_names, books_author ) {
  dtm_tsne <- data.table(title = books_names, author = books_author,
                         tsne_x = tsne_model[["Y"]][, 1], tsne_y = tsne_model[["Y"]][, 2])
  g <- ggplot(dtm_tsne) + geom_point(aes(x = tsne_x, y = tsne_y)) + 
    theme(legend.position="none")+
    ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d", perplexity))
  g
}

plot_tsne_text <- function(tsne_model,perplexity, books_names, books_author ) {
  dtm_tsne <- data.table(title = books_names, author = books_author,
                         tsne_x = tsne_model[["Y"]][, 1], tsne_y = tsne_model[["Y"]][, 2])
  #the text parameter work if we want to convert it into interactive throgh ggplotly
  g <- ggplot(dtm_tsne) + geom_point(aes(x = tsne_x, y = tsne_y, label = author, col  = author, text = title)) + 
  theme(legend.position="none")+
    ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d", perplexity))
  g
}



#we going to run a simple tsne_model with the default perplexity to set a optimal number of iteration
set.seed(0)
tsne_model_0 = Rtsne(dtm, pca = FALSE, max_iter = 5000)    

costs_dt <- data.table(step = 1:(5000/50), itercosts = tsne_model_0[["itercosts"]])

g_costs <- ggplot(costs_dt) + geom_line(aes(x = step, y = itercosts)) + 
  xlab("50 x iteration") + ylab("K-L Divergence") + ggtitle("K-L Divergence for each 50 iterations")
ggsave(file.path(images_path, "itercosts.png"), width = 8*((1+sqrt(5))/2),height = 8)
#we going to choose the minimal
costs_dt[itercosts == min(itercosts)]
#and this happen with 25*50 = 1250 steps
#Now we going to try differenst perplexities to find a suitable one

lapply(1:58, function(x) {
  cat(sprintf("Running the %d/58 model\n", x))
  set.seed(0)
  tsne_model = Rtsne(dtm, perplexity = x, pca = FALSE, max_iter = 1250)    
  g <- plot_tsne(tsne_model ,perplexity = x, books_names, books_author )
  ggsave( sprintf("%s/perplexity_%d.png", images_path, x), width = 8*((1+sqrt(5))/2),height = 8)
  })

#perplexity choice 2 and 3 are strongs , after 5 there is a really detached cluster

set.seed(0)
tsne_model_2 = Rtsne(dtm, perplexity = 2, pca = FALSE, max_iter = 1250)  
set.seed(0)
tsne_model_3 = Rtsne(dtm, perplexity = 3, pca = FALSE, max_iter = 1250)  
set.seed(0)
tsne_model_5 = Rtsne(dtm, perplexity = 5, pca = FALSE, max_iter = 1250)  


dtm_tsne_2 <- data.table(title = books_names, author = books_author,
                       tsne_x = tsne_model_2[["Y"]][, 1], tsne_y = tsne_model_2[["Y"]][, 2])

dtm_tsne_3 <- data.table(title = books_names, author = books_author,
                         tsne_x = tsne_model_3[["Y"]][, 1], tsne_y = tsne_model_3[["Y"]][, 2])

dtm_tsne_5 <- data.table(title = books_names, author = books_author,
                         tsne_x = tsne_model_5[["Y"]][, 1], tsne_y = tsne_model_5[["Y"]][, 2])
#the text parameter work if we want to convert it into interactive throgh ggplotly
g_2 <- ggplot(dtm_tsne_2) + geom_text(aes(x = tsne_x, y = tsne_y, label = author, col  = author, text = title)) + 
  theme(legend.position="none")+
  ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d", 2))

g_3 <- ggplot(dtm_tsne_3) + geom_text(aes(x = tsne_x, y = tsne_y, label = author, col  = author, text = title)) + 
  theme(legend.position="none")+
  ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d", 3))

g_5 <- ggplot(dtm_tsne_5) + geom_text(aes(x = tsne_x, y = tsne_y, label = author, col  = author, text = title)) + 
  theme(legend.position="none")+
  ggtitle(sprintf("Philosophy books in tSNE dimensions - Perplexity = %d", 5))


ggplotly(g_2)
ggplotly(g_3)
ggplotly(g_5)

#Our choice will be 2 

saveRDS(dtm_tsne_2, file.path(preprocessed_data_path, "dtm_tsne_2.RDS"))
ggsave( sprintf("%s/text_perplexity_%d.png", images_path, 2), width = 8*((1+sqrt(5))/2),height = 8, plot = g_2)
ggsave( sprintf("%s/text_perplexity_%d.png", images_path, 3), width = 8*((1+sqrt(5))/2),height = 8, plot = g_3)
ggsave( sprintf("%s/text_perplexity_%d.png", images_path, 5), width = 8*((1+sqrt(5))/2),height = 8, plot = g_5)


saveRDS(g_2, file.path(preprocessed_data_path, "g_2.RDS"))
saveRDS(g_3, file.path(preprocessed_data_path, "g_3.RDS"))
saveRDS(g_5, file.path(preprocessed_data_path, "g_5.RDS"))
