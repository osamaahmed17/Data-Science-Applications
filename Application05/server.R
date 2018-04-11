#Application 05

library(shiny)

shinyServer(function(input,output)
{
    #------------------------------------------------------------------
    #                     Frequency Bar Chart
    #------------------------------------------------------------------
    observeEvent(input$twt, 
    {
      output$plot1<-renderPlot({
        
        library(topicmodels)
        library(data.table)
        library(devtools)
        library(sentiment)
        library(tm)
        library(Rgraphviz)
        library(xts)
        library (twitteR)
        library (ggplot2)
        
        setup_twitter_oauth("675XOeFHdyhXWEna5pz2UKl6i", 
                            "1YWPcToZFIlKGSUypYrqtU3rdz0VA5qxPmLtwTBFKcWsxGuqjt", 
                            "1420820059-OPcIhx3niPyXgQXaxCZkVuHvhhYGPulVLutsYRM", 
                            "4XWrrfwWBlZYJfHtiQJJVHXEHtdsYEPixoN5sDW7TwquL")
        
        tweets <- userTimeline(input$text1, input$slider1)
        #tweets <- userTimeline("realDonaldTrump", input$slider1)
        #tweets<-searchTwitter("#psl2017",n=1000)
        
        (n.tweet <- length(tweets))
        tweets.df <- twListToDF(tweets)
        tweets.df[1, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
        writeLines(strwrap(tweets.df$text[1], 60)) #second argument of strwrap is for width of line
        #converting every element of our tweets dataframe as a text document and
        #making a corpus of each element in our tweets dataframe. corpus is made to work or do some computation over text documents
        myCorpus <- Corpus(VectorSource(tweets.df$text))
        #use tm_map function to apply transformation/mapping functions specially mapping on corpora. use content_transformer
        #function that creates a wraper to set and get the contents of text documents (character processing func)
        
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        
        #removeUTF<-tm_map(removeURL,function(x) iconv(enc2utf8(x),sub="byte"))
        #myCorpus <- tm_map(myCorpus, content_transformer(removeUTF))
        myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
        #writeLines(strwrap(tweets.df$text[1], 60))
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
        
        myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "ez")
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        myCorpus <- tm_map(myCorpus, stripWhitespace)
        myCorpusCopy <- myCorpus
        #if error occurs then use following commands
        #install.packages("SnowballC")
        #library(SnowballC)
        #getting stem words. i.e the word "friendship" contain stem word "friend" 
        myCorpus <- tm_map(myCorpus, stemDocument)
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        #completing the stem words using original data
        stemCompletion2 <- function(x, dictionary) {
          x <- unlist(strsplit(as.character(x), " "))
          x <- x[x != ""]
          x <- stemCompletion(x, dictionary=dictionary)
          x <- paste(x, sep="", collapse=" ")
          PlainTextDocument(stripWhitespace(x))
        }
        myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
        myCorpus <- Corpus(VectorSource(myCorpus)) # vectorsource interpret each element of the input vector as a document
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        
        #counting word frequencies
        wordFreq <- function(corpus, word) {
          results <- lapply(corpus,
                            function(x) { grep(as.character(x), pattern=word) }) #pattern=paste0("nn<",word))
          sum(unlist(results))
        }
        
        n.miner <- wordFreq(myCorpusCopy, "pak")
        n.mining <- wordFreq(myCorpusCopy, "pakistan")
        cat(n.miner, n.mining)
        
        replaceWord <- function(corpus, oldword, newword) {
          tm_map(corpus, content_transformer(gsub),
                 pattern=oldword, replacement=newword)
        }
        
        myCorpus <- replaceWord(myCorpus, "miner", "mining")
        myCorpus <- replaceWord(myCorpus, "ez", "is")
        myCorpus <- replaceWord(myCorpus, "scienc", "science")
        myCorpus <- replaceWord(myCorpus, "lahor", "lahore")
        tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))#unique terms used in every document
        tdm
        idx <- which(dimnames(tdm)$Terms %in% c("lala", "kings", "united"))
        as.matrix(tdm[idx, 1:100])
        #tweets.df$text[4] #extracting tweet to verify the matrix above (idx)
        
        term.freq <- rowSums(as.matrix(tdm))
        term.freq <- subset(term.freq, term.freq >= 5)
        df <- data.frame(term = names(term.freq), freq = term.freq)
        
        #if package does not installed then install the ggplot2 package.
        #install.packages(ggplot2)
        
        ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
          xlab("Terms") + ylab("Count") + coord_flip() +
          theme(axis.text=element_text(size=7))
      })
    })
    #------------------------------------------------------------------
    #                     Word Cloud
    #------------------------------------------------------------------
    
    observeEvent(input$twt, 
    {
      output$plot2 <- renderPlot(
      {
        
        library(ROAuth)
        library(tm)
        library(SnowballC)
        library(RColorBrewer)
        library(ggplot2)
        library(wordcloud)
        #source("http://bioconductor.org/biocLite.R")
        #biocLite("Rgraphviz")
        #the following two libraries can be used after biocLite installation
        library(graph)
        library(Rgraphviz)
        #############
        library(topicmodels)
        library(data.table)
        #install.packages("devtools")
        library(devtools)
        #install_github("okugami79/sentiment140")
        library(sentiment)
        library(tm)
        library(Rgraphviz)
        library(xts)
        
        setup_twitter_oauth("675XOeFHdyhXWEna5pz2UKl6i", 
                            "1YWPcToZFIlKGSUypYrqtU3rdz0VA5qxPmLtwTBFKcWsxGuqjt", 
                            "1420820059-OPcIhx3niPyXgQXaxCZkVuHvhhYGPulVLutsYRM", 
                            "4XWrrfwWBlZYJfHtiQJJVHXEHtdsYEPixoN5sDW7TwquL")
        
        tweets <- userTimeline("realDonaldTrump", n = 500)
        #tweets<-searchTwitter("#psl2017",n=1000)
        
        (n.tweet <- length(tweets))
        tweets.df <- twListToDF(tweets)
        tweets.df[1, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
        writeLines(strwrap(tweets.df$text[1], 60)) #second argument of strwrap is for width of line
        #converting every element of our tweets dataframe as a text document and
        #making a corpus of each element in our tweets dataframe. corpus is made to work or do some computation over text documents
        myCorpus <- Corpus(VectorSource(tweets.df$text))
        #use tm_map function to apply transformation/mapping functions specially mapping on corpora. use content_transformer
        #function that creates a wraper to set and get the contents of text documents (character processing func)
        
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        
        #removeUTF<-tm_map(removeURL,function(x) iconv(enc2utf8(x),sub="byte"))
        #myCorpus <- tm_map(myCorpus, content_transformer(removeUTF))
        myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
        #writeLines(strwrap(tweets.df$text[1], 60))
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
        
        myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "ez")
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        myCorpus <- tm_map(myCorpus, stripWhitespace)
        myCorpusCopy <- myCorpus
        #if error occurs then use following commands
        #install.packages("SnowballC")
        #library(SnowballC)
        #getting stem words. i.e the word "friendship" contain stem word "friend" 
        myCorpus <- tm_map(myCorpus, stemDocument)
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        #completing the stem words using original data
        stemCompletion2 <- function(x, dictionary) {
          x <- unlist(strsplit(as.character(x), " "))
          x <- x[x != ""]
          x <- stemCompletion(x, dictionary=dictionary)
          x <- paste(x, sep="", collapse=" ")
          PlainTextDocument(stripWhitespace(x))
        }
        myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
        myCorpus <- Corpus(VectorSource(myCorpus)) # vectorsource interpret each element of the input vector as a document
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        
        #counting word frequencies
        wordFreq <- function(corpus, word) {
          results <- lapply(corpus,
                            function(x) { grep(as.character(x), pattern=word) }) #pattern=paste0("nn<",word))
          sum(unlist(results))
        }
        
        n.miner <- wordFreq(myCorpusCopy, "pak")
        n.mining <- wordFreq(myCorpusCopy, "pakistan")
        cat(n.miner, n.mining)
        
        replaceWord <- function(corpus, oldword, newword) {
          tm_map(corpus, content_transformer(gsub),
                 pattern=oldword, replacement=newword)
        }
        
        myCorpus <- replaceWord(myCorpus, "miner", "mining")
        myCorpus <- replaceWord(myCorpus, "ez", "is")
        myCorpus <- replaceWord(myCorpus, "scienc", "science")
        myCorpus <- replaceWord(myCorpus, "lahor", "lahore")
        tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))#unique terms used in every document
        tdm
        idx <- which(dimnames(tdm)$Terms %in% c("lala", "kings", "united"))
        as.matrix(tdm[idx, 1:100])
        #tweets.df$text[4] #extracting tweet to verify the matrix above (idx)
        
        term.freq <- rowSums(as.matrix(tdm))
        term.freq <- subset(term.freq, term.freq >= 5)
        df <- data.frame(term = names(term.freq), freq = term.freq)
        
    
        m <- as.matrix(tdm)
        word.freq <- sort(rowSums(m), decreasing = T)
        #if error occurs then install the following package and load library
        # install.packages("RColorBrewer")
        #library(RColorBrewer)
        pal <- brewer.pal(9, "BuGn")[-(1:4)]
        #if error occurs then install the wordcloud library and then load it.
        library(wordcloud)
        
        wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)
      
      })
    })
    #------------------------------------------------------------------
    #                     Word Association Diagram
    #------------------------------------------------------------------
    
    observeEvent(input$twt,
    {  
      output$plot3 <- renderPlot(
      {
        setup_twitter_oauth("675XOeFHdyhXWEna5pz2UKl6i", 
                            "1YWPcToZFIlKGSUypYrqtU3rdz0VA5qxPmLtwTBFKcWsxGuqjt", 
                            "1420820059-OPcIhx3niPyXgQXaxCZkVuHvhhYGPulVLutsYRM", 
                            "4XWrrfwWBlZYJfHtiQJJVHXEHtdsYEPixoN5sDW7TwquL")
        
        tweets <- userTimeline("realDonaldTrump", n = 500)
        #tweets<-searchTwitter("#psl2017",n=1000)
        
        (n.tweet <- length(tweets))
        tweets.df <- twListToDF(tweets)
        tweets.df[1, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
        writeLines(strwrap(tweets.df$text[1], 60)) #second argument of strwrap is for width of line
        #converting every element of our tweets dataframe as a text document and
        #making a corpus of each element in our tweets dataframe. corpus is made to work or do some computation over text documents
        myCorpus <- Corpus(VectorSource(tweets.df$text))
        #use tm_map function to apply transformation/mapping functions specially mapping on corpora. use content_transformer
        #function that creates a wraper to set and get the contents of text documents (character processing func)
        
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        
        #removeUTF<-tm_map(removeURL,function(x) iconv(enc2utf8(x),sub="byte"))
        #myCorpus <- tm_map(myCorpus, content_transformer(removeUTF))
        myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
        #writeLines(strwrap(tweets.df$text[1], 60))
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
        
        myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "ez")
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        myCorpus <- tm_map(myCorpus, stripWhitespace)
        myCorpusCopy <- myCorpus
        #if error occurs then use following commands
        #install.packages("SnowballC")
        #library(SnowballC)
        #getting stem words. i.e the word "friendship" contain stem word "friend" 
        myCorpus <- tm_map(myCorpus, stemDocument)
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        #completing the stem words using original data
        stemCompletion2 <- function(x, dictionary) {
          x <- unlist(strsplit(as.character(x), " "))
          x <- x[x != ""]
          x <- stemCompletion(x, dictionary=dictionary)
          x <- paste(x, sep="", collapse=" ")
          PlainTextDocument(stripWhitespace(x))
        }
        myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
        myCorpus <- Corpus(VectorSource(myCorpus)) # vectorsource interpret each element of the input vector as a document
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        
        #counting word frequencies
        wordFreq <- function(corpus, word) {
          results <- lapply(corpus,
                            function(x) { grep(as.character(x), pattern=word) }) #pattern=paste0("nn<",word))
          sum(unlist(results))
        }
        
        n.miner <- wordFreq(myCorpusCopy, "pak")
        n.mining <- wordFreq(myCorpusCopy, "pakistan")
        cat(n.miner, n.mining)
        
        replaceWord <- function(corpus, oldword, newword) {
          tm_map(corpus, content_transformer(gsub),
                 pattern=oldword, replacement=newword)
        }
        
        myCorpus <- replaceWord(myCorpus, "miner", "mining")
        myCorpus <- replaceWord(myCorpus, "ez", "is")
        myCorpus <- replaceWord(myCorpus, "scienc", "science")
        myCorpus <- replaceWord(myCorpus, "lahor", "lahore")
        tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))#unique terms used in every document
        tdm
        idx <- which(dimnames(tdm)$Terms %in% c("lala", "kings", "united"))
        as.matrix(tdm[idx, 1:100])
        #tweets.df$text[4] #extracting tweet to verify the matrix above (idx)
        
        term.freq <- rowSums(as.matrix(tdm))
        term.freq <- subset(term.freq, term.freq >= 5)
        df <- data.frame(term = names(term.freq), freq = term.freq)
        
        m <- as.matrix(tdm)
        word.freq <- sort(rowSums(m), decreasing = T)
        #if error occurs then install the following package and load library
        # install.packages("RColorBrewer")
        #library(RColorBrewer)
        pal <- brewer.pal(9, "BuGn")[-(1:4)]
   
        # which words are associated with 'r'?
        findAssocs(tdm, "kings", 0.2) #if one is interested, see frequent pattern mining in data mining
        
        # which words are associated with 'psl'?
        findAssocs(tdm, "psl", 0.2)
        
        #
        #Install bioclite package.
        #source("http://bioconductor.org/biocLite.R")
        #biocLite("Rgraphviz")
        
        #library(graph)
        (freq.terms <- findFreqTerms(tdm, lowfreq = 5))
        plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)
      })
    })
  
    #------------------------------------------------------------------
    #                     Stack Diagram for Word Association
    #------------------------------------------------------------------
  
    observeEvent(input$twt,
    {  
      output$plot4 <- renderPlot({
          
        setup_twitter_oauth("675XOeFHdyhXWEna5pz2UKl6i", 
                            "1YWPcToZFIlKGSUypYrqtU3rdz0VA5qxPmLtwTBFKcWsxGuqjt", 
                            "1420820059-OPcIhx3niPyXgQXaxCZkVuHvhhYGPulVLutsYRM", 
                            "4XWrrfwWBlZYJfHtiQJJVHXEHtdsYEPixoN5sDW7TwquL")
        
        tweets <- userTimeline("realDonaldTrump", n = 500)
        #tweets<-searchTwitter("#psl2017",n=1000)
        
        (n.tweet <- length(tweets))
        tweets.df <- twListToDF(tweets)
        tweets.df[1, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
        writeLines(strwrap(tweets.df$text[1], 60)) #second argument of strwrap is for width of line
        #converting every element of our tweets dataframe as a text document and
        #making a corpus of each element in our tweets dataframe. corpus is made to work or do some computation over text documents
        myCorpus <- Corpus(VectorSource(tweets.df$text))
        #use tm_map function to apply transformation/mapping functions specially mapping on corpora. use content_transformer
        #function that creates a wraper to set and get the contents of text documents (character processing func)
        
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        
        #removeUTF<-tm_map(removeURL,function(x) iconv(enc2utf8(x),sub="byte"))
        #myCorpus <- tm_map(myCorpus, content_transformer(removeUTF))
        myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
        #writeLines(strwrap(tweets.df$text[1], 60))
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
        
        myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "ez")
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        myCorpus <- tm_map(myCorpus, stripWhitespace)
        myCorpusCopy <- myCorpus
        #if error occurs then use following commands
        #install.packages("SnowballC")
        #library(SnowballC)
        #getting stem words. i.e the word "friendship" contain stem word "friend" 
        myCorpus <- tm_map(myCorpus, stemDocument)
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        #completing the stem words using original data
        stemCompletion2 <- function(x, dictionary) {
          x <- unlist(strsplit(as.character(x), " "))
          x <- x[x != ""]
          x <- stemCompletion(x, dictionary=dictionary)
          x <- paste(x, sep="", collapse=" ")
          PlainTextDocument(stripWhitespace(x))
        }
        myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
        myCorpus <- Corpus(VectorSource(myCorpus)) # vectorsource interpret each element of the input vector as a document
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        
        #counting word frequencies
        wordFreq <- function(corpus, word) {
          results <- lapply(corpus,
                            function(x) { grep(as.character(x), pattern=word) }) #pattern=paste0("nn<",word))
          sum(unlist(results))
        }
        
        n.miner <- wordFreq(myCorpusCopy, "pak")
        n.mining <- wordFreq(myCorpusCopy, "pakistan")
        cat(n.miner, n.mining)
        
        replaceWord <- function(corpus, oldword, newword) {
          tm_map(corpus, content_transformer(gsub),
                 pattern=oldword, replacement=newword)
        }
        
        myCorpus <- replaceWord(myCorpus, "miner", "mining")
        myCorpus <- replaceWord(myCorpus, "ez", "is")
        myCorpus <- replaceWord(myCorpus, "scienc", "science")
        myCorpus <- replaceWord(myCorpus, "lahor", "lahore")
        tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))#unique terms used in every document
        tdm
        idx <- which(dimnames(tdm)$Terms %in% c("lala", "kings", "united"))
        as.matrix(tdm[idx, 1:100])
        #tweets.df$text[4] #extracting tweet to verify the matrix above (idx)
        
        term.freq <- rowSums(as.matrix(tdm))
        term.freq <- subset(term.freq, term.freq >= 5)
        df <- data.frame(term = names(term.freq), freq = term.freq)
        
        #if package does not installed then install the ggplot2 package.
        #install.packages(ggplot2)
  
        m <- as.matrix(tdm)
        word.freq <- sort(rowSums(m), decreasing = T)
        #if error occurs then install the following package and load library
        # install.packages("RColorBrewer")
        #library(RColorBrewer)
        pal <- brewer.pal(9, "BuGn")[-(1:4)]
        #if error occurs then install the wordcloud library and then load it.
        #install.packages("wordcloud")
        
        # which words are associated with 'r'?
        findAssocs(tdm, "kings", 0.2) #if one is interested, see frequent pattern mining in data mining
        
        # which words are associated with 'psl'?
        findAssocs(tdm, "psl", 0.2)
        
        #
        #Install bioclite package.
        #source("http://bioconductor.org/biocLite.R")
        #biocLite("Rgraphviz")
        
        #library(graph)
        (freq.terms <- findFreqTerms(tdm, lowfreq = 5))
        
        #dtm <- as.DocumentTermMatrix(tdm)
        #install.packages("topicmodels")
        
        
        #Error in LDA(dtm, k = 3) : 
        # Each row of the input matrix needs to contain at least one non-zero entry
        # if such error occrs then use the following command.
        rowTotals <- apply(tdm , 1, sum)
        tdm.new   <- tdm[rowTotals> 0, ]  # selecting rows having sum greater than 0
        lda <- LDA(tdm.new, k = 10)
        
        
        #lda <- LDA(dtm, k = 8) # find 8 topics
        term <- terms(lda, 7) # first 7 terms of every topic
        (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
        
        
        topics <- topics(lda) # 1st topic identified for every document (tweet)
        
        #if dimensions of dates and topics are not equal then use the subsets as the following
        topics <- data.frame(date=as.IDate(tweets.df$created[c(1:200,1)]), topic=topics[c(1:200,1)])#install.packages("xts")   #may be this pacakge requires.
        
        library(data.table)
        
        ggplot(topics, aes(date, fill = term[topic])) +
          geom_density(position = "stack")
      })
    })
  
    #------------------------------------------------------------------
    #                     Sentiment Analysis
    #------------------------------------------------------------------
  
    observeEvent(input$twt,
    {  
      output$plot5 <- renderPlot({
        
        setup_twitter_oauth("675XOeFHdyhXWEna5pz2UKl6i", 
                            "1YWPcToZFIlKGSUypYrqtU3rdz0VA5qxPmLtwTBFKcWsxGuqjt", 
                            "1420820059-OPcIhx3niPyXgQXaxCZkVuHvhhYGPulVLutsYRM", 
                            "4XWrrfwWBlZYJfHtiQJJVHXEHtdsYEPixoN5sDW7TwquL")
        
        tweets <- userTimeline("realDonaldTrump", n = 500)
        #tweets<-searchTwitter("#psl2017",n=1000)
        
        (n.tweet <- length(tweets))
        tweets.df <- twListToDF(tweets)
        tweets.df[1, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
        writeLines(strwrap(tweets.df$text[1], 60)) #second argument of strwrap is for width of line
        #converting every element of our tweets dataframe as a text document and
        #making a corpus of each element in our tweets dataframe. corpus is made to work or do some computation over text documents
        myCorpus <- Corpus(VectorSource(tweets.df$text))
        #use tm_map function to apply transformation/mapping functions specially mapping on corpora. use content_transformer
        #function that creates a wraper to set and get the contents of text documents (character processing func)
        
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        
        #removeUTF<-tm_map(removeURL,function(x) iconv(enc2utf8(x),sub="byte"))
        #myCorpus <- tm_map(myCorpus, content_transformer(removeUTF))
        myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
        #writeLines(strwrap(tweets.df$text[1], 60))
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
        
        myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "ez")
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        myCorpus <- tm_map(myCorpus, stripWhitespace)
        myCorpusCopy <- myCorpus
        #if error occurs then use following commands
        #install.packages("SnowballC")
        #library(SnowballC)
        #getting stem words. i.e the word "friendship" contain stem word "friend" 
        myCorpus <- tm_map(myCorpus, stemDocument)
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        #completing the stem words using original data
        stemCompletion2 <- function(x, dictionary) {
          x <- unlist(strsplit(as.character(x), " "))
          x <- x[x != ""]
          x <- stemCompletion(x, dictionary=dictionary)
          x <- paste(x, sep="", collapse=" ")
          PlainTextDocument(stripWhitespace(x))
        }
        myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
        myCorpus <- Corpus(VectorSource(myCorpus)) # vectorsource interpret each element of the input vector as a document
        writeLines(strwrap(myCorpus[[1]]$content, 60))
        
        #counting word frequencies
        wordFreq <- function(corpus, word) {
          results <- lapply(corpus,
                            function(x) { grep(as.character(x), pattern=word) }) #pattern=paste0("nn<",word))
          sum(unlist(results))
        }
        
        n.miner <- wordFreq(myCorpusCopy, "pak")
        n.mining <- wordFreq(myCorpusCopy, "pakistan")
        cat(n.miner, n.mining)
        
        replaceWord <- function(corpus, oldword, newword) {
          tm_map(corpus, content_transformer(gsub),
                 pattern=oldword, replacement=newword)
        }
        
        myCorpus <- replaceWord(myCorpus, "miner", "mining")
        myCorpus <- replaceWord(myCorpus, "ez", "is")
        myCorpus <- replaceWord(myCorpus, "scienc", "science")
        myCorpus <- replaceWord(myCorpus, "lahor", "lahore")
        tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))#unique terms used in every document
        tdm
        idx <- which(dimnames(tdm)$Terms %in% c("lala", "kings", "united"))
        as.matrix(tdm[idx, 1:100])
        #tweets.df$text[4] #extracting tweet to verify the matrix above (idx)
        
        term.freq <- rowSums(as.matrix(tdm))
        term.freq <- subset(term.freq, term.freq >= 5)
        df <- data.frame(term = names(term.freq), freq = term.freq)
  
        m <- as.matrix(tdm)
        word.freq <- sort(rowSums(m), decreasing = T)
        #if error occurs then install the following package and load library
        # install.packages("RColorBrewer")
        #library(RColorBrewer)
        pal <- brewer.pal(9, "BuGn")[-(1:4)]
        #if error occurs then install the wordcloud library and then load it.
        
        # which words are associated with 'r'?
        findAssocs(tdm, "kings", 0.2) #if one is interested, see frequent pattern mining in data mining
        
        # which words are associated with 'psl'?
        findAssocs(tdm, "psl", 0.2)
        
        #
        #Install bioclite package.
        #source("http://bioconductor.org/biocLite.R")
        #biocLite("Rgraphviz")
        
        #library(graph)
        (freq.terms <- findFreqTerms(tdm, lowfreq = 5))
        
        #dtm <- as.DocumentTermMatrix(tdm)
        #install.packages("topicmodels")
        
        
        #Error in LDA(dtm, k = 3) : 
        # Each row of the input matrix needs to contain at least one non-zero entry
        # if such error occrs then use the following command.
        rowTotals <- apply(tdm , 1, sum)
        tdm.new   <- tdm[rowTotals> 0, ]  # selecting rows having sum greater than 0
        lda <- LDA(tdm.new, k = 10)
        
        
        #lda <- LDA(dtm, k = 8) # find 8 topics
        term <- terms(lda, 7) # first 7 terms of every topic
        (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
        
        
        topics <- topics(lda) # 1st topic identified for every document (tweet)
        
        #if dimensions of dates and topics are not equal then use the subsets as the following
        topics <- data.frame(date=as.IDate(tweets.df$created[c(1:200,1)]), topic=topics[c(1:200,1)])#install.packages("xts")   #may be this pacakge requires.
        
        #install.packages("data.table")
        
        
        #some sentiment analysis
        #Sentiment Analysis
        #install.packages("devtools")
        
        sentiments <- sentiment(tweets.df$text)
        table(sentiments$polarity)
        sentiments$score <- 0
        sentiments$score[sentiments$polarity == "positive"] <- 1
        sentiments$score[sentiments$polarity == "negative"] <- -1
        sentiments$date <- as.IDate(tweets.df$created)
        result <- aggregate(score ~ date, data = sentiments, sum)
        plot(result, type = "l")
        
      })
    })
})



