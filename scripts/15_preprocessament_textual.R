# ==============================================================================
# PREPROCESSAMENT TEXTUAL
# ==============================================================================
# Entrada : ../data/reviews.csv  +  ../data/dataset_cure_python.rds
# Sortida : ../data/dtm_sample1000.rds         (matriu DTM TF, 1000 reviews x termes)
#           ../data/reviews_ca_sample1000.rds  (metadades alineades)
#           ../data/corpus_preprocessat.rds    (VCorpus)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LLIBRERIES
# ------------------------------------------------------------------------------
library(tm)
library(SnowballC)
library(dplyr)
library(cld3)

# ------------------------------------------------------------------------------
# 2. CÀRREGA I FILTRATGE
# ------------------------------------------------------------------------------
reviews <- read.csv("../data/reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

cat("Dimensions originals:", nrow(reviews), "reviews x", ncol(reviews), "columnes\n")

# Eliminem reviews buides
reviews <- reviews %>%
  filter(!is.na(comments) & trimws(comments) != "")

# Filtre: només listings presents al dataset preprocessat
listings_prep_ids <- readRDS("../data/dataset_cure_python.rds")$id
reviews <- reviews %>%
  filter(listing_id %in% listings_prep_ids)

cat("Reviews de listings al dataset preprocessat:", nrow(reviews), "\n")

# Detecció d'idioma
reviews$lang <- detect_language(reviews$comments)

# Filtre lingüístic: només anglès
reviews <- reviews %>%
  filter(lang == "en")

cat("Reviews en anglès:", nrow(reviews), "\n")
cat("Listings únics:", length(unique(reviews$listing_id)), "\n")

# ------------------------------------------------------------------------------
# 3. MOSTREIG ALEATORI (1.000 reviews)
# ------------------------------------------------------------------------------
# Criteris:
#   - Longitud mínima de 300 caràcters
#   - Un autor diferent per review (reviewer_id únic)
#   - Un listing diferent per review (listing_id únic)

MIN_CHARS <- 300

reviews_pool <- reviews %>%
  mutate(n_chars = nchar(comments)) %>%
  filter(n_chars >= MIN_CHARS) %>%
  distinct(reviewer_id, .keep_all = TRUE) %>%
  distinct(listing_id,  .keep_all = TRUE)

cat("Mostra vàlida (autors i listings únics, >=", MIN_CHARS, "car.):",
    nrow(reviews_pool), "reviews\n")

set.seed(42)
reviews_sample <- reviews_pool %>%
  slice_sample(n = 1000)

cat("Mostra final:", nrow(reviews_sample), "reviews\n")
cat("Listings únics a la mostra:", length(unique(reviews_sample$listing_id)), "\n")
cat("Autors únics a la mostra:", length(unique(reviews_sample$reviewer_id)), "\n")
cat("Longitud mitjana (car.):", round(mean(reviews_sample$n_chars)), "\n")

# ------------------------------------------------------------------------------
# 4. CREACIÓ DEL CORPUS
# ------------------------------------------------------------------------------

corpus <- VCorpus(VectorSource(reviews_sample$comments),
                  readerControl = list(reader   = readPlain,
                                       language = "en",
                                       load     = TRUE))

cat("Corpus creat amb", length(corpus), "documents\n")

# ------------------------------------------------------------------------------
# 5. PREPROCESSAT (homologació del text)
# ------------------------------------------------------------------------------
# - Minúscules
# - Eliminem: URLs i símbols no alfabètics (números, puntuació, emojis...)
# - Eliminem stopwords i paraules freqüents no discriminants
# - Stemming: redueix cada paraula a l'arrel morfològica
# - Eliminem espais múltiples generats pels passos anteriors

corpus <- tm_map(corpus, content_transformer(tolower))

removeSpecial <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, removeSpecial, "http[[:alnum:][:punct:]]*")
corpus <- tm_map(corpus, removeSpecial, "[^[:alpha:][:space:]]")

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)

custom_stopwords <- c(stopwords("english"),
                      "airbnb", "place", "stay", "stayed", "just",
                      "will", "also", "get", "got", "really",
                      "can", "one", "two", "even", "make", "made",
                      "ive", "dont", "didnt", "wasnt", "couldnt",
                      "wont", "wouldnt", "havent", "hadnt", "thats")
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, stripWhitespace)

cat("Preprocessat completat.\n")

# ------------------------------------------------------------------------------
# 6. CONSTRUCCIÓ DE LA DTM — mètode TF
# ------------------------------------------------------------------------------
# Files = reviews (1.000), Columnes = termes

dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            weighting   = weightTf,
                            wordLengths = c(3, Inf)
                          ))

cat("DTM inicial:", nrow(dtm), "documents x", ncol(dtm), "termes\n")

# sparse=0.95 conserva termes presents en >=50 reviews (5%)
dtm <- removeSparseTerms(dtm, sparse = 0.95)

cat("DTM filtrada (sparse=0.95):", nrow(dtm), "documents x", ncol(dtm), "termes\n")

# Eliminem reviews sense cap terme vàlid
dtm.matrix <- as.matrix(dtm)
keep_rows  <- rowSums(dtm.matrix) > 0
dtm.matrix <- dtm.matrix[keep_rows, ]
reviews_ca <- reviews_sample[keep_rows, ]

cat("DTM final:", nrow(dtm.matrix), "reviews x", ncol(dtm.matrix), "termes\n")

# ------------------------------------------------------------------------------
# 7. DESAR
# ------------------------------------------------------------------------------
# dtm_sample1000.rds        -> CA 
# reviews_ca_sample1000.rds -> CA + CA-GALT
# corpus_preprocessat.rds   -> LSA, LDA

saveRDS(dtm.matrix, "../data/dtm_sample1000.rds")
saveRDS(reviews_ca, "../data/reviews_ca_sample1000.rds")
saveRDS(corpus,     "../data/corpus_preprocessat.rds")

cat("\nDesat:\n")
cat("  ../data/dtm_sample1000.rds        —", nrow(dtm.matrix), "x", ncol(dtm.matrix), "\n")
cat("  ../data/reviews_ca_sample1000.rds —", nrow(reviews_ca), "reviews\n")
cat("  ../data/corpus_preprocessat.rds   —", length(corpus), "documents\n")