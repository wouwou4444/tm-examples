# install.packages("corrplot")
# install.packages("tm")
# install.packages("readtext")

library(tm)
library(corrplot)
setwd(dir = "~/../OneDrive/Coursera2/R/")
test_sentences <- readLines("./sentences.txt", encoding = "UTF-8")

text_sample <- c("serveur_ADBCRD01,nom1-nom2-nom3. test de differents sep, EMf:listener_ORAADB01_dg_DG01_12654",
                 "Bonjour autre FICHIER, demain, est Autre fichier(file.txt) avec des adresses IP 123.34.213.2 mais pas eulement. et inum ut corpus.",
                 "Encore un autre exemple, example, server123(12.153.123.54) et dg_vg87_spqs_psf")

test_sentences <- c(test_sentences, text_sample)
test_sentences <- paste(test_sentences, test_sentences[10:13])

my_vcorpus_fr <- VCorpus(VectorSource(test_sentences), readerControl = list(language = "fr"))

print(my_vcorpus_fr[[1]]$content)

my_vcorpus_fr_1 <- tm_map(my_vcorpus_fr, stripWhitespace)

my_vcorpus_fr_2 <- tm_map(my_vcorpus_fr_1, content_transformer(tolower))

my_vcorpus_fr_3 <- tm_map(my_vcorpus_fr_2, removeWords, stopwords("french"))
# my_vcorpus_fr_4 <- tm_map(my_vcorpus_fr_2, removePunctuation, preserve_intra_word_dashes = TRUE)


dtm_vcorpus_fr <- DocumentTermMatrix(my_vcorpus_fr_3)

dtm_vcorpus_fr_matrix <- (as.matrix(dtm_vcorpus_fr))

# inspect(dtm_vcorpus_fr)
 ####
kill_chars <- content_transformer (function(x, pattern) gsub(pattern, " ", x))
my_punct <- "[!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~]"
my_punct <- "[!\"#$%&'()*+,/:;<=>?@\\[\\]^`\\{|\\}~\\]"
my_punct <- "[],()!\"*#$+:;<=>?~@^|{}\\[/']"

# 

my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_3,FUN = kill_chars, my_punct)
my_punct_to_keep <- "[[:space:]][\\.\\-\\_]+"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "[\\.\\-\\_]+[[:space:]]"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "^[\\.\\-\\_]+"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "[\\.\\-\\_]+$"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)

my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5, stripWhitespace)
my_vcorpus_fr_6 <- my_vcorpus_fr_5
####
separator <- c("_",".", "-")
i <- 2**length(separator)
my_vcorpus_fr_7 <- VCorpus(VectorSource(character(length = length(my_vcorpus_fr_6))))

for (j in 1:(i-1)) {
  gexpr <- paste("[",paste(separator[(intToBits(j) == 1)[1:length(separator)]], collapse = ""),"]",sep = "")
  print(gexpr)
  temp_corpus <- tm_map(my_vcorpus_fr_6,FUN = kill_chars, gexpr)
  ttt <- mapply(function(X,Y) {
    # print("********")
    return(paste(X$content,Y$content,sep=" "))
    # print()
    # print("--------")
    
  },
  X= content(my_vcorpus_fr_7), 
  Y= content(temp_corpus))
  my_vcorpus_fr_7 <- VCorpus(VectorSource(ttt))
}

my_vcorpus_fr_7 <- tm_map(my_vcorpus_fr_7, stripWhitespace)


####


dtm_vcorpus_fr2 <- DocumentTermMatrix(my_vcorpus_fr_7)

dtm_vcorpus_fr_matrix2 <- (as.matrix(dtm_vcorpus_fr2))

# inspect(dtm_vcorpus_fr2)

mtd.norm <- as.matrix(removeSparseTerms(TermDocumentMatrix(my_vcorpus_fr_5),0.8))
# mtd.norm <- as.matrix((TermDocumentMatrix(my_vcorpus_fr_5)))
corrplot(mtd.norm, is.corr = FALSE)

mtd.norm2 <- as.matrix(removeSparseTerms(TermDocumentMatrix(my_vcorpus_fr_5,
                                         control=list(weighting=weightTfIdf)),0.8))
# mtd.norm2 <- as.matrix((TermDocumentMatrix(my_vcorpus_fr_5,
#                                                             control=list(weighting=weightTfIdf))))
corrplot(mtd.norm2, is.corr = FALSE)

## cosine similarity
install.packages("lsa")
library(lsa)
my_cosine <- cosine(mtd.norm2)

## Compute distance for Hierarchical clustering
my_dist <- dist(dtm_vcorpus_fr_matrix2, method = "euclidean")
my_dist
my_hc <- hclust(my_dist, method = "ward.D2")
plot(my_hc)

my_hc2 <- hclust(my_dist, method = "complete")
plot(my_hc2)
