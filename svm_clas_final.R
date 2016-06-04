library('RTextTools')
library('twitteR')

#Preparing data
svmtweets <- tweets_as_frame[,c("tweet", "tourist")]

#svmtweets$tweet <- as.character(svmtweets$tweet)    
svmtweets$tourist <- factor(svmtweets$tourist)  

levels(svmtweets$tourist)
#[1] "FALSE" "TRUE" 

length(svmtweets$tourist)
#[1] 73663

table(svmtweets$tourist)
#FALSE  TRUE 
#13639 60024 

#Cleaning data. Here I dont remove numbers and punctuation, 
#I will do it later while creating the matrix dtm (to use cleaning function 
#for other algorithms just uncomment them)

#Cleaning function
clean.text <- function(svmtweets)
{
  svmtweets$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", svmtweets$tweet)
# svmtweets$tweet = gsub("[[:punct:]]", "", svmtweets$tweet)
  svmtweets$tweet = gsub("@\\w+", "", svmtweets$tweet)
  svmtweets$tweet = gsub("http\\w+", "", svmtweets$tweet)
  svmtweets$tweet = gsub("https\\w+", "", svmtweets$tweet)
# svmtweets$tweet = gsub("[[:digit:]]+", "", svmtweets$tweet)
  svmtweets$tweet = gsub("[ \t]{2,}", "", svmtweets$tweet)
  svmtweets$tweet = gsub("^ ", "", svmtweets$tweet)
  svmtweets$tweet = gsub(" $", "", svmtweets$tweet)
  svmtweets$tweet = tolower(svmtweets$tweet)
  return(smvtweets)
}

#Applying cleaning function
svmtweets$tweet = clean.text(svmtweets)

##Creating training Corpus
set.seed(2016)
train_out.data <- svmtweets$tourist
train_txt.data <- svmtweets$tweet

## Creating document-term matrix and container

#Term Document Matrix 
#for Russian use stemWords=TRUE
matrix <- create_matrix(train_txt.data,
                        language="english", #"russian", "french"
                        minWordLength=3, 
                        removeNumbers=TRUE,
                        stemWords=FALSE,
                        removePunctuation=TRUE,
                        weighting=weightTfIdf)

#Container and dividing dataset to train and test

train.num   <- round(nrow(svmtweets) / 2)
container <- create_container(matrix,t(train_out.data),
                              trainSize=1:train.num,
                              testSize=(train.num+1):nrow(svmtweets),
                              virgin=FALSE)

#Model building
svm.model       <- train_model(container, "SVM")

svm.result    <- classify_model(container, svm.model)

#Error rate calculating

N <- nrow(svmtweets)
tindex <- sample(N,train.num)
pt <- table(Truth=train_out.data[-tindex], Predictions=svm.result$SVM_LABEL)
cat('Error rate = ',100*(1-sum(diag(pt))/sum(pt)),'%')

#Error rate =  20.68638 %
summary(svm.result)
#SVM_LABEL        SVM_PROB     
#FALSE: 1261   Min.   :0.5000  
#TRUE :35570   1st Qu.:0.8201  
               #Median :0.8655  
               #Mean   :0.8444  
               #3rd Qu.:0.8977  
               #Max.   :1.0000  


