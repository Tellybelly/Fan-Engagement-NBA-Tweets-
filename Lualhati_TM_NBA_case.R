## NBA Tweets Engagements
## Your bosses are interested in learning more about the commentary 
## for popular teams, athletes, or the league as a whole.
## Krystel Kim Lualhati
## March 07, 2022



# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

#### 
# set your working directory
getwd()
setwd("/Users/krystellualhati/Desktop/MsBA/8 - Text Analytics/Text-Mining-NLP - BikalPan")


# Load the following all libraries 
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(NLP) 
library(dplyr)
library(ggdendro)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)
#Options and Functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_All','C') #some tweets are in different language



# Loading the dataset
# Regular Season Before Suspension
# The entire schedule was released on August 12, 2019. The regular season began on October 22, 2019, 
# and was suspended on March 11, 2020, due to the COVID-19 pandemic.
a<- read.csv("Case/Case I/Data/A_Oct2019.csv")
b<- read.csv("Case/Case I/Data/B_Nov2019.csv")
c<- read.csv("Case/Case I/Data/C_Dec2019.csv")
e<- read.csv("Case/Case I/Data/E_Feb2020.csv")
f<- read.csv("Case/Case I/Data/F_Mar2020.csv")



# combining all the dataset using rbind 
# Objective: To identify emerging topics and top athletes
# during the beginning of the season

begin <- rbind(a,b,c,e,f)
nba<- slice_sample(begin, prop=0.15)

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# checking the dimension of the dataframe
dim(begin)


#Notable Players Candidates for Nike Endorsement
# Vince Carter, Luka Doncic, LeBron James, Zion Williamson, Kobe Bryant
# Vince Carter signed contract with Puma
# Luka Doncic signed a 5-Year Contract with Nike @luka7doncic
# LeBron James lifetime contract with Nike @KingJames
# Zion Williamson (rookie) highest guaranteed contracts with Nike @Zionwilliamson
# Kobe Bryant passed away around January of 2020

basicSubs <- function(x){
  x <- gsub('http\\S+\\s*','',x, ignore.case = TRUE)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)','',x, ignore.case = TRUE)
  x <- tolower(x)
  return(x)
}

txt <- basicSubs(nba$text)

#The objective is to examine the sentiment and polarity that will benefit
#marketing team's effort
#Lebron James
LebronTerms <- "Lebron|@KingJames|#LebronJames"
LJ<- grep(LebronTerms,txt, ignore.case = TRUE)



#Luka Doncic
LukaTerms <- "Doncic|@luka7doncic|#LukaDoncic"
Luka<- grep(LukaTerms,txt, ignore.case = TRUE)

#Luka Doncic
ZionTerms <- "Zion|@zionwilliamson|#Zionwilliamson"
Zion<- grep(ZionTerms,txt, ignore.case = TRUE)


lebron<-length(LJ)
luka<- length(Luka)
zion<-length(Zion)

# Organize term objects into a dataframe
termFreq <- data.frame(players = c(LebronTerms,LukaTerms,ZionTerms),
                      freq = c(lebron,luka,zion))

# Examine 
termFreq


# Plot a geom_bar with ggplot2 by filling in the correct data, adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq[-16,], aes(x = reorder(players, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")


####
stops <- c(stopwords('SMART'), 'lol', 'amp')

# Create a Clean Corpus Function
# add into the function removePunctuation
# add into the function removeNumbers
# add into the function stripWhitespace
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,stripWhitespace)
  return(corpus)
}



james<- grep(LebronTerms,nba$text, ignore.case = TRUE)
doncic<- grep(LukaTerms,nba$text, ignore.case = TRUE)
william<- grep(ZionTerms,nba$text, ignore.case = TRUE)

# Apply the VCorpus Function to a VectorSource of the original text object
#cleaning Lebron
cleanLebron <- VCorpus(VectorSource(nba$text[james]))
cleanLebron <- cleanCorpus(cleanLebron, stops)
#cleaningLuka
cleanLuka <- VCorpus(VectorSource(nba$text[doncic]))
cleanLuka <- cleanCorpus(cleanLuka, stops)


#cleaningZion
cleanZion <- VCorpus(VectorSource(nba$text[william]))
cleanZion <- cleanCorpus(cleanZion, stops)

#Combining 3 subjects 
cleanNike <- c(cleanLebron,cleanLuka,cleanZion)
cleanNike <- VCorpus((VectorSource(cleanNike)))


# Creating a TDM 
cleanNikeTDM <- TermDocumentMatrix(cleanNike)
cleanNikeTDMm <- as.matrix(cleanNikeTDM)
cleanNikeTDMm



# Get the most frequent terms
topTermsA <- colSums(cleanNikeTDMm)
topTermsB <- rowSums(cleanNikeTDMm)

# Add the terms
topTermsA <- data.frame(terms = colnames(cleanNikeTDMm), freq = topTermsA)
topTermsB <- data.frame(terms = rownames(cleanNikeTDMm), freq = topTermsB)

# Remove row attributes
rownames(topTermsA) <- NULL
rownames(topTermsB) <- NULL


# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]
head(exampleReOrder)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]



# Frequency Data Frame
tweetSums <- rowSums(cleanNikeTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)


# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 15
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 15) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

#ggplot(topWords, aes(x=word, y=frequency)) + 
  #geom_bar(stat="identity", fill='darkred') + 
  #coord_flip()+ theme_gdocs() +
  #geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

#help("findAssocs")


# Inspect word associations
# Top 1
associations<-findAssocs(cleanNikeTDM, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF


# Inspect word associations for using the second word with most frequency
don<-findAssocs(cleanNikeTDM, 'doncic', 0.30)
don

# Organize the word associations
assocDF1 <- data.frame(terms=names(don[[1]]),
                      value=unlist(don))
assocDF1$terms <- factor(assocDF1$terms, levels=assocDF1$terms)
rownames(assocD1F) <- NULL
assocDF1


# Top 5
zionasso<-findAssocs(cleanNikeTDM, 'zion', 0.30)
zionasso

# Organize the word associations
assocDF2 <- data.frame(terms=names(zionasso[[1]]),
                      value=unlist(zionasso))
assocDF2$terms <- factor(assocDF2$terms, levels=assocDF2$terms)
rownames(assocDF2) <- NULL
assocDF2


# Make a dot plot
#ggplot(assocDF2, aes(y=terms)) +
  #geom_point(aes(x=value), data=assocDF2, col='#c00c00') +
  #theme_gdocs() + 
  #geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 


######DENDROGRAMS######
# Reduce TDM
reducedTDM <- removeSparseTerms(cleanNikeTDM, sparse=0.95) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 





######WORDCLOUD########


# See a bi-gram
exampleTweet <- grep('lakers', rownames(cleanNikeTDMm))
cleanNikeTDMm[(exampleTweet-2):(exampleTweet),100:150]

# Get Row Sums & organize
nikeTDMv <- sort(rowSums(cleanNikeTDMm), decreasing = TRUE)
nikeDF   <- data.frame(word = names(nikeTDMv), freq = nikeTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

help("brewer.pal")

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(nikeDF$word,
          nikeDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

######COMMONALITY CLOUD########
# FYI

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
zioncloud <- paste(zion, collapse = ' ')
lukacloud <- paste(luka, collapse = ' ')



# Combine the subject documents into a corpus of *2* documents
rookie <- c(lukacloud, zioncloud)
rookie <- VCorpus((VectorSource(rookie)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
rookieTDM  <- TermDocumentMatrix(rookie, control = ctrl)
rookieTDMm <- as.matrix(rookieTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(rookieTDMm) <- c('doncic', 'zion')

# Examine
head(rookieTDMm)

# Make comparison cloud
comparison.cloud(rookieTDMm, 
                 max.words=30, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(rookieTDMm),"Dark2"),
                 scale=c(3,0.1))









