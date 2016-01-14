
## Initialization
The following R libraries are used in the data analysis process
```{r load_libraries , message=FALSE , warning=FALSE}

#library(ggplot2)       # plotting
library(R.utils)       # utility
library(quanteda)      # for tokenization
#library(RColorBrewer)  # wordcloud
library(plyr)          # data ordering
library(dplyr) 
library(reshape)
library(stringi)
library(caret)

```

```{r set_vars , echo = FALSE}

### Set options
options(scipen=999)          # force explicit expression of numbers vice scientific notation

### Assign director locations to variables 
twitter_location <- '/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/en_US/en_US.twitter.txt'
news_location <- '/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/en_US/en_US.news.txt'
blogs_location <- '/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/en_US/en_US.blogs.txt'
projDir <- '/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/en_US/'

directory <- '/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project'
setwd('//Users/mike.brown/Documents/Coursera_Data_Science')
getwd()
runApp(directory , display.mode = 'showcase')
runApp(server.R , display.mode = 'showcase')
```

```{r sample_files , message=FALSE , warning=FALSE}

sample_fraction <- 0.45

### Twitter
con <- file(twitter_location, "r") 
data.list <- as.list(readLines(con , n = -1 , warn = FALSE))
close(con)

### News
con <- file(news_location, "r") 
data.list <- readLines(con , n = -1 , warn = FALSE)
close(con)

### Blogs
con <- file(blogs_location, "r") 
data.list <- readLines(con , n = -1 , warn = FALSE)
close(con)

### Sample Data
data.sample <- unlist(sample(data.list , sample_fraction*length(data.list)))
dataCorpus_sample <- corpus(data.sample)
#dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removeNumbers = TRUE , removePunct = TRUE , removeTwitter = TRUE , removeSeparators = TRUE)

#clean up
rm(data.list)
rm(data.sample)

```

## Sample Data for Analysis
Create 1-gram, 2-grams 3-grams and 4-grams from the sampled data with Quanteda
```{r n_grams , message=FALSE , warning=FALSE}

### Data n-grams
#one_gram_dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removeNumbers = TRUE , removePunct = TRUE , removeTwitter = TRUE , removeSeparators = TRUE , ngram = 1)

two_gram_dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removeNumbers = TRUE , removePunct = TRUE , removeTwitter = TRUE , removeSeparators = TRUE , ngram = 2)

three_gram_dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removePunct = TRUE , removeSeparators = TRUE , removeNumbers = TRUE ,  removeTwitter = TRUE , ngram = 3)

four_gram_dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removePunct = TRUE , removeSeparators = TRUE , removeNumbers = TRUE ,  removeTwitter = TRUE , ngram = 4)

five_gram_dfm_data_sample <- dfm(dataCorpus_sample , toLower = TRUE , removePunct = TRUE , removeSeparators = TRUE , removeNumbers = TRUE ,  removeTwitter = TRUE , ngram = 5)

```


```{r Prediction_Method}
######### Form 2-gram data matrix

rows.2gram <- floor(two_gram_dfm_data_sample@Dim[2]/1)

feature_counts.2g <- as.data.frame(topfeatures(two_gram_dfm_data_sample , rows.2gram))
feature_counts.2g[,2] <- feature_counts.2g[,1]
feature_counts.2g[,1] <- rownames(feature_counts.2g)
feature_counts.2g[,3]  <- -999 
feature_counts.2g[,4] <- -999
#feature_counts.2g <- feature_counts.2g[which(feature_counts.2g[,2])>1,]
names(feature_counts.2g) <- c('bigram' , 'count' , 'leadingGram' , 'trailingGram')

feature_counts.2g.mat <- as.matrix(feature_counts.2g)

### Split words out in leading 1-gram and trailing 1-gram

for(i in 1:nrow(feature_counts.2g.mat)){
  gram <- unlist(strsplit(feature_counts.2g.mat[i,1] , '_'))
    if (length(gram) == 2){
        feature_counts.2g.mat[i,3] <- gram[1]
        feature_counts.2g.mat[i,4] <- gram[2]
        print(i)
    }
}

oracle.2df <- as.data.frame(feature_counts.2g.mat , stringsAsFactors = FALSE)
oracle.2df <- oracle.2df[which(oracle.2df[,3] != -999 ) , ] 
oracle.2df[,2] <- as.integer(oracle.2df[,2])
names(oracle.2df) <- c('bigram' , 'frequency' , 'leading_Gram' , 'trailing_Gram' )
oracle.2df <- oracle.2df[order(oracle.2df$leading_Gram , -oracle.2df$frequency ),]
oracle.2df <- as.matrix(oracle.2df)

oracle.2df[,5] <- 0
oracle.2df[1,5] <- 1


### Select highest value of each leading_Gram 
for(i in 2:nrow(oracle.2df)) {
    if(oracle.2df[i,3] != oracle.2df[i-1,3]){
    oracle.2df[i,5] <- 1
    print(i)
   }
}

oracle.2df.redux <- oracle.2df[which(oracle.2df[,5] == 1) , ]
oracle.2df.residual <- oracle.2df[which(oracle.2df[,5] == 0) , ]


oracle.2df.redux.final <- as.data.frame(oracle.2df.redux , stringsAsFactors = FALSE) 

oracle.2df.redux.final[,5] <- oracle.2df.redux.final[ , 2]
oracle.2df.redux.final <- oracle.2df.redux.final[ , -2]
oracle.2df.redux.final <- oracle.2df.redux.final[ , -1]
names(oracle.2df.redux.final) <- c('leading_Gram' , 'trailing_Gram' , 'frequency'  )

### get rid of leading grams with numbers:
oracle.2df.redux.final[,4] <- 1
for (i in 1:nrow(oracle.2df.redux.final)){
  if(grepl("[[:digit:]]+",oracle.2df.redux.final[i,1])){
      oracle.2df.redux.final[i,4] <- 0
      print(paste(oracle.2df.redux.final[i,] , i))
  }
}

oracle.2df.prod <- oracle.2df.redux.final[which(oracle.2df.redux.final$V4 == 1),]
oracle.2df.prod <- oracle.2df.prod[,-4]
oracle.2df.prod[,3] <- as.numeric(oracle.2df.prod[,3])

saveRDS(oracle.2df.prod  , "/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project/data/project.data.2df.rds")



######### Form 3-gram data matrix

rows.3gram <- floor(three_gram_dfm_data_sample@Dim[2]/1)

feature_counts.3g <- as.data.frame(topfeatures(three_gram_dfm_data_sample , rows.3gram))
feature_counts.3g[,2] <- feature_counts.3g[,1]
feature_counts.3g[,1] <- rownames(feature_counts.3g)
feature_counts.3g[,3]  <- -999 
feature_counts.3g[,4] <- -999
names(feature_counts.3g) <- c('trigram' , 'frequency' , 'leadingGram' , 'trailingGram')

feature_counts.3g <- feature_counts.3g[which(feature_counts.3g$frequency > 1) , ]

feature_counts.3g.mat <- as.matrix(feature_counts.3g)

### Split words out in 2-gram and trailing 1-gram

for(i in 1:nrow(feature_counts.3g)){
  gram <- unlist(strsplit(feature_counts.3g.mat[i,1] , '_'))
    if (length(gram) == 3){
        feature_counts.3g.mat[i,3] <- paste(gram[1] , gram[2])
        feature_counts.3g.mat[i,4] <- gram[3]
        print(i)
    }
}

oracle.3df <- as.data.frame(feature_counts.3g.mat , stringsAsFactors = FALSE)
oracle.3df <- oracle.3df[which(oracle.3df[,3] != -999 ) , ] 
oracle.3df[,2] <- as.integer(oracle.3df[,2])
oracle.3df <- oracle.3df[,-1]
oracle.3df[,4] <- oracle.3df[,1]
oracle.3df <- oracle.3df[,-1]
names(oracle.3df) <- c('leading_Gram' , 'trailing_Gram' , 'frequency' )

oracle.3df.redux <- oracle.3df
oracle.3df.redux[,4] <- 1

for (i in 1:nrow(oracle.3df.redux)){
  if(grepl("[[:digit:]]+",oracle.3df.redux[i,1])){
      oracle.3df.redux[i,4] <- 0
      print(paste(oracle.3df.redux[i,] , i))
  }
}

oracle.3df.prod <- oracle.3df.redux[which(oracle.3df.redux$V4 == 1),]
oracle.3df.prod <- oracle.3df.prod[,-4]
oracle.3df.prod <- oracle.3df.prod[which(oracle.3df.prod$frequency > 4) ,]


saveRDS(oracle.3df.prod , "/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project/data/project.data.3df.rds")


######### Form 4-gram data matrix

rows.4gram <- floor(four_gram_dfm_data_sample@Dim[2]/1)

feature_counts.4g <- as.data.frame(topfeatures(four_gram_dfm_data_sample , rows.4gram))
feature_counts.4g[,2] <- feature_counts.4g[,1]
feature_counts.4g[,1] <- rownames(feature_counts.4g)
feature_counts.4g[,3]  <- -999 
feature_counts.4g[,4] <- -999
feature_counts.4g <- feature_counts.4g[which(feature_counts.4g[,2]>1),]
names(feature_counts.4g) <- c('quadgram' , 'frequency' , 'leading_Gram' , 'trailing_Gram')

feature_counts.4g.mat <- as.matrix(feature_counts.4g)

### Split words out in leading 3-gram and trailing 1-gram

for(i in 1:nrow(feature_counts.4g.mat)){
  gram <- unlist(strsplit(feature_counts.4g.mat[i,1] , '_'))
    if (length(gram) == 4){
        feature_counts.4g.mat[i,3] <- paste(gram[1] , gram[2] ,  gram[3])
        feature_counts.4g.mat[i,4] <- gram[4]
        print(i)
    }
}

oracle.4df <- as.data.frame(feature_counts.4g.mat , stringsAsFactors = FALSE)
oracle.4df <- oracle.4df[which(oracle.4df[,3] != -999 ) , ] 
oracle.4df[,2] <- as.integer(oracle.4df[,2])
oracle.4df[,5] <- oracle.4df[,2]
oracle.4df <- oracle.4df[,-1]
oracle.4df <- oracle.4df[,-1]
names(oracle.4df) <- c('leading_Gram' , 'trailing_Gram' , 'frequency' )



### Remove any row with a number in the leading term
oracle.4df[,4] <- 1
for (i in 1:nrow(oracle.4df)){
  if(grepl("[[:digit:]]+",oracle.4df[i,1])){
      oracle.4df[i,4] <- 0
      print(paste(oracle.4df[i,] , i))
  }
}
oracle.4df <- oracle.4df[which(oracle.4df$V4 == 1) ,]
oracle.4df <- oracle.4df[,-4]

### Thin out the file by removing low frequency entries
oracle.4df <- oracle.4df[which(oracle.4df$frequency > 2) ,]

saveRDS(oracle.4df , "/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project/data/project.data.4df.rds")

######### Form 5-gram data matrix

rows.5gram <- floor(five_gram_dfm_data_sample@Dim[2]/1)

feature_counts.5g <- as.data.frame(topfeatures(five_gram_dfm_data_sample , rows.5gram))
feature_counts.5g[,2] <- feature_counts.5g[,1]
feature_counts.5g[,1] <- rownames(feature_counts.5g)
feature_counts.5g[,3]  <- -999 
feature_counts.5g[,4] <- -999
feature_counts.5g <- feature_counts.5g[which(feature_counts.5g[,2]>1),]
names(feature_counts.5g) <- c('pentagram' , 'count' , 'leading_Gram' , 'trailing_Gram')

feature_counts.5g.mat <- as.matrix(feature_counts.5g)

### Split words out in leading 4-gram and trailing 1-gram

for(i in 1:nrow(feature_counts.5g.mat)){
  gram <- unlist(strsplit(feature_counts.5g.mat[i,1] , '_'))
    if (length(gram) == 5){
        feature_counts.5g.mat[i,3] <- paste(gram[1] , gram[2] ,  gram[3] , gram[4])
        feature_counts.5g.mat[i,4] <- gram[5]
        print(i)
    }
}

oracle.5df <- as.data.frame(feature_counts.5g.mat , stringsAsFactors = FALSE)
oracle.5df <- oracle.5df[which(oracle.5df[,3] != -999 ) , ] 
oracle.5df[,2] <- as.integer(oracle.5df[,2])
oracle.5df <- oracle.5df[,-1]  
oracle.5df[,4] <- oracle.5df[,1] 
oracle.5df <- oracle.5df[,-1]  
names(oracle.5df) <- c( 'leading_Gram' , 'trailing_Gram' , 'frequency')

### Remove numbers from leading gram 
oracle.5df[,4] <- 1
for (i in 1:nrow(oracle.5df)){
  if(grepl("[[:digit:]]+",oracle.5df[i,1])){
      oracle.5df[i,4] <- 0
      print(paste(oracle.5df[i,] , i))
  }
}




oracle.5df <- oracle.5df[which(oracle.5df$V4 == 1) ,]
oracle.5df <- oracle.5df[,-4]


saveRDS(oracle.5df , "/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project/data/project.data.5df.rds")



###########  Testing
arrange(oracle[ which(oracle$leading_Bigram == 'case of' & oracle$count > 1) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'mean the' & oracle$count > 1) , ] , -count)
arrange(oracle[ which(oracle$leading_Bigram == 'me the' & (oracle$count > 1 & oracle$trailing_Unigram == 'happiest' |  oracle$trailing_Unigram == 'saddest' | oracle$trailing_Unigram == 'bluest')
) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'but the' & oracle$count > 1 & (oracle$count > 1 & oracle$trailing_Unigram == 'referees' |  oracle$trailing_Unigram == 'crowd' | oracle$trailing_Unigram == 'defense' |  oracle$trailing_Unigram == 'players')
) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'at the' & oracle$count > 4 & (oracle$count > 1 & oracle$trailing_Unigram == 'beach' |  oracle$trailing_Unigram == 'grocery' | oracle$trailing_Unigram == 'movies' |  oracle$trailing_Unigram == 'movies')) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'on my' & oracle$count > 1 & (oracle$count > 1 & oracle$trailing_Unigram == 'horse' |  oracle$trailing_Unigram == 'phone' | oracle$trailing_Unigram == 'motorcycle' |  oracle$trailing_Unigram == 'way')) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'quite some' & oracle$count > 1) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'his little' & (oracle$count > 1 & oracle$trailing_Unigram == 'eyes' |  oracle$trailing_Unigram == 'fingers' | oracle$trailing_Unigram == 'ears' |  oracle$trailing_Unigram == 'toes')) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'during the' & oracle$count > 1 & (oracle$count > 1 & oracle$trailing_Unigram == 'worse' |  oracle$trailing_Unigram == 'bad' | oracle$trailing_Unigram == 'sad' |  oracle$trailing_Unigram == 'hard')) , ], -count)
arrange(oracle[ which(oracle$leading_Bigram == 'must be' & oracle$count > 1 & (oracle$count > 1 & oracle$trailing_Unigram == 'asleep' |  oracle$trailing_Unigram == 'insane' | oracle$trailing_Unigram == 'callous' |  oracle$trailing_Unigram == 'insensitive')) , ], -count)

test <- readRDS("/Users/mike.brown/Documents/Coursera_Data_Science/Capstone/shiny/data_science_project/data/three_gram_oracle.rds")

```

