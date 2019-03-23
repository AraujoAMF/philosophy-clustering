library(data.table)
library(stringr)
library(tm)
library(qdap)
library(gutenbergr)
library(textstem)

preprocessed_data_path <- "/home/allan/Documentos/gutenberg_vis/selected_books"


gutenberg_base <- readRDS(file.path(preprocessed_data_path, "gutenberg_base.RDS"))
raw_text <- readRDS(file.path(preprocessed_data_path, "philosophy.RDS"))
selected_books <- readRDS(file.path(preprocessed_data_path, "selected_books.RDS"))

#function to preprocess every document on our raw_text
preprocess_gutenberg <- function(raw_text) {
  
  
  index <- raw_text[, gutenberg_id][1]
  text1 <- raw_text[, text]
  #remove special x=character to avoid bugs
  text1 <- gsub("[^\x20-\x7E]", "", text1)
  text1 <- tolower(text1)
  text1 <-  iconv(text1, to="ASCII//TRANSLIT")
  text1 <- replace_contraction(text1, sent.cap = FALSE)
  text1 <- removePunctuation(text1)
  text1 <- removeWords(text1, stopwords("en"))
  text1 <- Trim(text1)
  text1 <- text1[text1 != ""]
  #lemmatization need to be done before the number replacing
  #lemmatize_strings("thousandth")
  text1 <- lemmatize_strings(text1)
  text1 <- replace_number(text1)
  text1 <- paste(text1, collapse = " ")
  
  #books's metadata
  #author <- gutenberg_base[gutenberg_id == index, author]
  #author <- gsub("^(.*?),.*", "\\1", author)
  #title <- gutenberg_base[gutenberg_id == index, title]
  text1
  
}

preprocessed_text <- lapply(seq_along(raw_text), function(x) {
  
  cat(paste(x, "\n"))
  text1 <- preprocess_gutenberg(raw_text[[x]] )
  
})

saveRDS(preprocessed_text, file.path(preprocessed_data_path, "preprocessed_text.RDS"))

corpus <- VCorpus(VectorSource(preprocessed_text))
saveRDS(corpus, file.path(preprocessed_data_path, "philosophy_corpus.RDS"))

#get to books metadata from the books for plotting
books_names <- selected_books[, title]
books_author <- selected_books[, author]
#regex to get everything before the comma
#the author is the way Surname,Name
books_author <- gsub("^(.*?),.*", "\\1", books_author)

saveRDS(books_names, file.path(preprocessed_data_path, "books_names.RDS"))
saveRDS(books_author, file.path(preprocessed_data_path, "books_author.RDS"))



#make the inverse term frequency matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- weightTfIdf(dtm)
dtm <- as.matrix(dtm)
rownames(dtm) <- books_names
saveRDS(dtm, file.path(preprocessed_data_path, "dtm.RDS"))
