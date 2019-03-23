library(data.table)
library(gutenbergr)

data_file_path <- "/home/allan/Documentos/gutenberg_vis/selected_books"
gutenberg_base <- data.table(gutenberg_works())
#
selected_authors <- c(44, 46, 93, 355, 473, 563, 779, 998, 1133, 1156, 1286, 1426, 
                      1440, 1497, 1705, 2161, 2447, 2747, 3648, 7168, 7489, 7913)


philosophy_books <- gutenberg_base[gutenberg_author_id %in% selected_authors, unique(gutenberg_id)]
selected_books <- gutenberg_base[gutenberg_id %in% philosophy_books]
saveRDS(selected_books, file.path(data_file_path, "selected_books.RDS"))

raw_text <- lapply(philosophy_books, function(x) {
  raw_text <-try( data.table(gutenberg_download(x)))
})

saveRDS(raw_text, file.path(data_file_path, "philosophy.RDS"))
saveRDS(gutenberg_base, file.path(data_file_path, "gutenberg_base.RDS"))



