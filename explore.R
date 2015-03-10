library(tm)
#bag <- readLines("final/en_US/en_US.blogs.txt", n=10, encoding="UTF-8")
# Need to explore Corpus(DirSource("final/en_US/"), readerControl = list(language="lat"))
#corpus <- Corpus(VectorSource(bag))
corpus <- Corpus(DirSource("final/en_US/"))
#rm(bag)

# Do some conversion on that thing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

