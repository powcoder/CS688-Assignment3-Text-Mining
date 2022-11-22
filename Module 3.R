# Module 3 Code
install.packages("pdftools")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

### OPTIONAL:
### --- Example 1: Convert to text single pdf files that is contained in a single folder. ----
#set your working directory; You need to modify it to point to your working directory
setwd("C:/users/Michael Joner/Google Drive/BU CS 688/Instructor/3 Module/") 

exe.loc <- Sys.which("pdftotext") # location of "pdftotext.exe"
pdf.loc=file.path(getwd(),"PDF Files") # folder "PDF Files" with PDFs
myPDFfiles <- normalizePath(list.files(path = pdf.loc, 
                                       pattern = "pdf",  full.names = TRUE)) # Get the path (chr-vector) of PDF file names

# Convert single pdf file to text by placing "" around the chr-vector of PDF file name
system(paste(exe.loc, paste0('"', myPDFfiles[1], '"')), wait=FALSE)

# Convert to text several pdf files that are contained in a single folder.
# Use lapply with in line function to convert each PDF file indexed by "i" into a text file 
lapply(myPDFfiles, function(i) system(paste(exe.loc, paste0('"', i, '"')), wait = FALSE))

### --- Example 2: The recommended data analysis packages (install if needed) include: ----
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(dplyr) # Data preparation and pipes %>%.
library(ggplot2) # Plot word frequencies.
library(scales) # Common data analysis activities.
library(pdftools)

# tm - Readers and Sources
getReaders()
getSources()

### --- Example 2a: Extracting text content from text files and load them as Corpus. ----
system.file("texts",package="tm")  # where are the tm example files stored (MAKE SURE TM INSTALLED FIRST)

loremipsum <- system.file("texts", "loremipsum.txt", package = "tm") # Path to "loremipsum.txt"
ovid <- system.file("texts", "txt", "ovid_1.txt", package = "tm") # Path to "ovid.txt"
Docs.pth <- URISource(sprintf("file://%s", c(loremipsum, ovid))) # Specify Source
corpus.txt<-VCorpus(Docs.pth) # load them as Corpus 
inspect(corpus.txt) 

corpus.txt[[2]]$content[1:3] # Examine the first 3 lines of the "ovid.txt" corpus 

### --- Example 2b: Extracting file content from a PDF file and load them as Corpus. ----
pdf.loc <- system.file(file.path("doc", "tm.pdf"), package = "tm") # Find the path to "tm.pdf"
# pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = pdf.loc),
#                                                  language = "en",
#                                                  id = "id1")
cname<- URISource(pdf.loc) 
corpus.pdf <- Corpus(cname, readerControl=list(reader=readPDF))
inspect(corpus.pdf) 

corpus.pdf[[1]]$content[1]
corpus.pdf[[1]]$meta

### --- Example 3a: Processing - Transforming the Corpus  ----
### (continuation of Example 2a)
getTransformations() # The basic transforms available within tm package 

# Apply the function "tm_map()" and content_transformer() to the corpus

  # corpus.txt = same as used in Example
docs.tranf <- tm_map( corpus.txt, content_transformer(tolower)) # Conversion to Lowercase
corpus.txt[[2]]$content[1:3] # Original corpus with Uppercase
docs.tranf[[2]]$content[1:3] # Transformed Corpus all Lowercase

docs.tranf <- tm_map( docs.tranf, removePunctuation )
head(docs.tranf[[2]]$content,3) # lowercase AND no punctuation

### --- Example 3b: Processing - Transforming the Corpus  ----
# Transforms the "@" into " ".
email.texts <- c("Jon_Smith123@bu.edu",
                 "Subject: Speaker's Info", 
                 "I hope you enjoyed the lectures.",
                 "Here is address of the lecturer: ", "123 Main St. #25","Boston MA 02155") 
# Step 1: Create corpus
y <- data.frame(doc_id = 1:length(email.texts), text = email.texts)
corpus.txt <- VCorpus(DataframeSource(y))
  # alternative:
  # corpus.txt <- VCorpus(VectorSource(email.texts))

docs.tranf <- tm_map( corpus.txt, removePunctuation )

corpus.txt[[1]]$content[1] # "Jon_Smith123@bu.edu"
docs.tranf[[1]]$content[1]


# Step 2: "content transformer()" create a function to achieve the transformation
blank.chr <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs.tranf <- tm_map(corpus.txt, blank.chr, "@")

corpus.txt[[1]]$content[1] # "Jon_Smith123@bu.edu"
docs.tranf[[1]]$content[1] # "Jon_Smith123 bu.edu"

docs.tranf <- tm_map( corpus.txt, removeNumbers) # Remove numbers
corpus.txt[[1]]$content[1] # Original corpus with numbers
docs.tranf[[1]]$content[1] # Transformed Corpus without numbers

### --- Example 4: Replacing a word with another one  ----
### (continuation of Example 3b)
transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
docs.tranf <- tm_map(corpus.txt, transform.words, "Jon_Smith", "Jane_Doe")

corpus.txt[[1]]$content[1] # "Jon_Smith123@bu.edu"
docs.tranf[[1]]$content[1] # "Jane_Doe123@bu.edu" 

library(SnowballC) # Stemming 
docs.tranf <- tm_map(corpus.txt, removePunctuation) # remove punc
docs.tranf <- tm_map(docs.tranf, stemDocument) # Stem original corpus

corpus.txt[[2]]$content[1] # "Subject: Speaker's Info"
docs.tranf[[2]]$content[1] # "Subject: Speaker Info"

stopwords("english")
docs.tranf <- tm_map(corpus.txt, removeWords, stopwords("english"))
corpus.txt[[4]]$content[1] # "Here is address of the lecturer: "
docs.tranf[[4]]$content[1] # "Here  address   lecturer: "

# A better way #1 ???
stopwords("english")
docs.tranf <- tm_map(corpus.txt, content_transformer(tolower)) # make lowercase first, since the stopwords are all lowercase
docs.tranf <- tm_map(docs.tranf, removeWords, stopwords("english")) # remove stopwords with punctuation in them (contractions)
docs.tranf <- tm_map(docs.tranf, removePunctuation)
docs.tranf <- tm_map(docs.tranf, removeWords, stopwords("english")) # remove stopwords that were at end of sentences
### PROBLEM, sentences that end in contractions might not come out
### I can't!  ==>  can't doesn't get removed
corpus.txt[[4]]$content[1] # "Here is address of the lecturer: "
docs.tranf[[4]]$content[1] # "  address   lecturer: "

# A better way #2 ???
stopwords("english")
docs.tranf <- tm_map(corpus.txt, content_transformer(tolower)) # make lowercase first, since the stopwords are all lowercase
docs.tranf <- tm_map(docs.tranf, removePunctuation)
docs.tranf <- tm_map(docs.tranf, removeWords, removePunctuation(stopwords("english")))
### PROBLEM! Words like hell, ill, and id will be removed
corpus.txt[[4]]$content[1] # "Here is address of the lecturer: "
docs.tranf[[4]]$content[1] # "  address   lecturer: "

docs.tranf <- tm_map(corpus.txt, removeWords, c("address", "MA"))
corpus.txt[[4]]$content[1] # "Here is address of the lecturer: "
docs.tranf[[4]]$content[1] # "Here is  of the lecturer: "
corpus.txt[[6]]$content[1] # "Boston MA 02155"
docs.tranf[[6]]$content[1] # "Boston  02155"

## combining stopwords with custom words
## mywords <- c(stopwords("en"),"address","subject")
## tm_map(corpus.txt, removeWords, mywords)

### --- Example 5a: Creating a Document Term Matrix  ----
Docs.pth <- system.file(file.path("doc", "tm.pdf"), package = "tm") # Path to tm.pdf
Docs.corpus <- Corpus(URISource(Docs.pth),readerControl = list(reader = readPDF))
dtm <- DocumentTermMatrix(Docs.corpus) # Document Term Matrix
inspect(dtm)
freq <- colSums(as.matrix(dtm)) # Term frequencies
length(freq)
max(freq) # Max appearance frequency of a term 
findFreqTerms(dtm,max(freq)) # Find the term with max appearance ("the")
findFreqTerms(dtm,25)  # words that happen at least 25 times
ord <- order(freq) # Ordering the frequencies (ord contains the indices))
freq[ord[(length(ord)-99):length(ord)]] # Most frequent terms & their frequency 
freq[tail(ord,10)]
# ord <- rev(ord)  # same as ord <- order(freq, decreasing = TRUE)
# freq[head(ord,10)]
findFreqTerms(dtm, lowfreq=10) # List terms (alphabetically) with frequency higher than 10
findFreqTerms(dtm, lowfreq=17) # List terms (alphabetically) with frequency higher than 17
hist(freq, breaks=6)

# Create a Word Cloud Plot
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=5, colors=brewer.pal(6, "Dark2"))
# wordcloud(names(freq), freq, min.freq=5, colors=brewer.pal(6, "Pastel1"))

# freq <- colSums(...) is probably better than the following, but this can be useful
# (optional)
matdtm <- as.matrix(dtm) # Convert dtm to a matrix
colnames(matdtm)[1100:1131]
matdtm[1100:1131]
write.csv(matdtm, file=" dtm.csv") # Save matrix as CSV file

### 20Newsgroups.zip file - unzip to this folder:
system.file("texts",package="tm")

### --- Example 5b: Reading multiple documents  ----
### (this is closer to what you'll be doing on Assignment 3)
# load
Mac.Path.Loc <- system.file("texts","20Newsgroups","20news-bydate-train","comp.sys.mac.hardware",package="tm")
Mac.Files <- DirSource(Mac.Path.Loc)
# load all documents
# Mac.Corpus <- VCorpus(DirSource(Mac.Path.Loc), readerControl=list(reader=readPlain))
# load 50 documents
Mac.Corpus <- VCorpus(URISource(Mac.Files$filelist[1:50]), readerControl=list(reader=readPlain))
# How to load a second folder
# Space.Path.Loc <- system.file("texts","20Newsgroups","20news-bydate-train","sci.space",package="tm")
# Space.Files <- DirSource(Space.Path.Loc)
# Space.Corpus <- Corpus(URISource(Space.Files$filelist[1:50]), readerControl=list(reader=readPlain))
# Combined.Corpus <- c(Mac.Corpus,Space.Corpus)

# inspect first file
inspect(Mac.Corpus[[1]])

# preprocessing
Mac.Corpus.Proc <- tm_map(Mac.Corpus, content_transformer(tolower))
# inspect(Mac.Corpus.Proc[[1]])
Mac.Corpus.Proc <- tm_map(Mac.Corpus.Proc, removeWords, stopwords("english"))
Mac.Corpus.Proc <- tm_map(Mac.Corpus.Proc, removePunctuation)
# Mac.Corpus.Proc <- tm_map(Mac.Corpus.Proc, stemDocument)

# dtm
Mac.DTM <- DocumentTermMatrix(Mac.Corpus.Proc, control = list(
  wordLengths=c(3,20),  # words between 3 and 20 characters long
  bounds=list(global=c(20,Inf))  # only include words in DTM if they happen in 20 or more documents
))
inspect(Mac.DTM)

# (optional) how do I get binary? (1's if it happens, 0's if it doesn't)
Mac.DTM <- DocumentTermMatrix(Mac.Corpus.Proc, control = list(
  wordLengths=c(3,20),  # words between 3 and 20 characters long
  bounds=list(global=c(20,Inf)),  # only include words in DTM if they happen in 20 or more documents
  weighting=weightBin  # binary DTM instead of default term frequency DTM
))
inspect(Mac.DTM)

### Examples 6-8 removed for time considerations

### --- Example 9: Document kNN Text Classification (Step 6 in the Slide Deck)  ----
library("tm")
Doc1 <- "I spent 10K on my car. Compared to the prices of most cars in their class it was cheap. It is a red car so I like it and it has a lot of space."
Doc2 <- "I checked the car prices and I could not find a red car for under 10K. So the price was good even though it had a hole. I heard that it belonged to a movie star." 
Doc3 <- "I like the red color, so I would buy a red car even if the car's price is over 10K."
Doc4 <- "I don't like red cars. The insurance for red cars is higher regardless of the price and I would not spend more than 10K. I like black cars."
Doc5 <- "A red giant star can curve the space to form a black hole. In absence of stars the space is flat."
Doc6 <- "With exception of the stars the space is filled with blackness making the black holes even harder to see."
Doc7 <- "Our sun is a small star and it will not end as a black hole. It does not have enough mass to curve the space."
#Doc8 <- "Very few stars will end as black holes but still the space contains large number of black holes."
Doc8 <- "My flight is priced well."

doc <- c(Doc1,Doc2,Doc3,Doc4,Doc5,Doc6,Doc7,Doc8) # Merge all strings
corpus <- VCorpus(VectorSource(doc))

corpus.cars <- VCorpus(VectorSource(doc[1:4]))
# Preprocessing
corpusc2 <- tm_map(corpus.cars, content_transformer(tolower)) # Lowercase
corpusc3 <- tm_map(corpusc2, removeWords, stopwords("english"))
corpusc4 <- tm_map(corpusc3, removePunctuation) # Remove Punctuation
corpusc5 <- tm_map(corpusc4, removeWords, stopwords("english"))
corpusc6 <- tm_map(corpusc5, stemDocument, language = "english") # Perform Stemming
inspect(DocumentTermMatrix(corpusc6))
## BUT THIS DTM IS USELESS FOR KNN

# Preprocessing the combined corpus
corpus2 <- tm_map(corpus, content_transformer(tolower)) # Lowercase
corpus3 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus4 <- tm_map(corpus3, removePunctuation) # Remove Punctuation
corpus5 <- tm_map(corpus4, removeWords, stopwords("english"))
corpus6 <- tm_map(corpus5, stemDocument, language = "english") # Perform Stemming
dtm <- as.matrix(DocumentTermMatrix(corpus6)) # Document term matrix
dtm

# Text Classification
library(class) # Using kNN 
train.doc <- dtm[c(1,2,5,6),] # Dataset for which classification is already known
test.doc <- dtm[c(3,4,7,8),] # Dataset you are trying to classify
Tags <- factor(c(rep("cars",2), rep("space",2)))   # what is the CORRECT classification for the Training data?
## ESSENTIAL: length(Tags) MUST EQUAL nrow(train.doc)
### so you could do data.frame(Tags=Tags,train=train.doc)
## ESSENTIAL: ncol(train.doc) MUST EQUAL ncol(test.doc)
# TrainTags <- STUFF
# TestTags <- OTHER STUFF
# Tags - Correct answers for the training dataset (and also happens to be correct for the test dataset)
set.seed(4419)
prob.test<- knn(train.doc, test.doc, Tags, k = 2, prob=TRUE)
# k is the number of neighbors considered
# prob.test is a complicated object. It has length(prob.test) equal to the number of documents we are predicting

# Display Classification Results
a <- 1:length(prob.test)  # which document number - 1 through however many documents we predicted (test data, not train data)
b <- levels(prob.test)[prob.test]  # what is the classifier predicting for the ith test document?
c <- attributes(prob.test)$prob  # what percentage of the k nearest neighbors are the same type as what we are predicting for the ith test document?
d <- prob.test==Tags
    # in this case, the correct classifications for the test data is known, and
    # happens to be the same as the correct classifications for the train data!
    # So we can compare the predictions (in variable 'b' and also in 'prob.test') with the correct Tags
result <- data.frame(Doc=a, Predict=b,Prob=c, Correct=d)  # make a data frame of the a, b, c, d variables built above
result  # display the data frame
sum(c)/length(Tags) # Overall probability (not really a meaningful thing to compute)

sum(prob.test==Tags)/length(Tags) # Proportion of Documents Classified Correctly
sum(d)/length(Tags)  # does same thing

