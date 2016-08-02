

library(tm)
library(stringi)
library(proxy)
library(RCurl)
library(utils)
#setInternet2(use = TRUE)

wiki <- "https://en.wikipedia.org/wiki/"
titles <- c("Integral", "Riemann_integral", "Riemann-Stieltjes_integral", "Derivative",
            "Limit_of_a_sequence", "Edvard_Munch", "Vincent_van_Gogh", "Jan_Matejko", "James_Joyce", "Donald_Trump",
            "Lev_Tolstoj", "Franz_Kafka", "J._R._R._Tolkien", "Britney_Spears", "Kim_Kardashian")
articles <- character(length(titles))

for (i in 1:length(titles)) {
  articles[i] <- stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
}

docs <- Corpus(VectorSource(articles))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

docsTDM <- TermDocumentMatrix(docs)
docsTDM <- removeSparseTerms(docsTDM, sparse = .9)
class(docsTDM)
docsTDM
docsdissim <- dist(t(as.matrix(docsTDM)), diag = FALSE, upper = TRUE)
docsdissim
docsdissim <- as.matrix(docsdissim)
rownames(docsdissim) <- titles
colnames(docsdissim) <- titles
docsdissim
plot(hclust(dist(docsdissim), method = "complete"))
