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
my_punct <- "[],()!\"*#$+:;<=>?~@^|{}\\[]"
# my_vcorpus_fr_4 <- tm_map(my_vcorpus_fr_3,FUN = kill_chars, "'")
# my_vcorpus_fr_4 <- tm_map(my_vcorpus_fr_4,FUN = kill_chars, ",")
# my_vcorpus_fr_4 <- tm_map(my_vcorpus_fr_4,FUN = kill_chars, ":")
# my_vcorpus_fr_4 <- tm_map(my_vcorpus_fr_4,FUN = kill_chars, "(")
# 

my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_4,FUN = kill_chars, my_punct)
my_punct_to_keep <- "[[:space:]][\\.\\-\\_]+"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "[\\.\\-\\_]+[[:space:]]"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "^[\\.\\-\\_]+"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)
my_punct_to_keep <- "[\\.\\-\\_]+$"
my_vcorpus_fr_5 <- tm_map(my_vcorpus_fr_5,FUN = kill_chars, my_punct_to_keep)

dtm_vcorpus_fr2 <- DocumentTermMatrix(my_vcorpus_fr_5)

dtm_vcorpus_fr_matrix2 <- (as.matrix(dtm_vcorpus_fr2))

# inspect(dtm_vcorpus_fr2)

