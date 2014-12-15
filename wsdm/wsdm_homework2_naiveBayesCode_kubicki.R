#
# Web Scale Data Mining and Processing
# Homework 2
#
# Author: Karol Kubicki


## 1. Choose train and test examples:

# I used following bash command to extract my test and training sets:
#
# cd .../train/pos
# ls | sort -R | head -n 4000 | while read file; do cp $file ../../../WSDM_H2_randomSampleOfDocuments/train/ ; done
# cd .../train/neg
# ls | sort -R | head -n 4000 | while read file; do cp $file ../../../WSDM_H2_randomSampleOfDocuments/train/ ; done
# cd .../test/pos
# ls | sort -R | head -n 1000 | while read file; do cp $file ../../../WSDM_H2_randomSampleOfDocuments/test/ ; done
# cd .../test/neg
# ls | sort -R | head -n 1000 | while read file; do cp $file ../../../WSDM_H2_randomSampleOfDocuments/test/ ; done

## 2. Read data:

pathTest<-"/home/user/Research/Classes/Rprogramming/data/WSDM_H2_randomSampleOfDocuments/test"
pathTrain<-"/home/user/Research/Classes/Rprogramming/data/WSDM_H2_randomSampleOfDocuments/train"

readFilesFromPath <- function(path){
  library(stringi)
  fnames_test <- list.files(path,
                            pattern= "*.txt", full.names=TRUE)
  names<-character(length(fnames_test))
  rating<- numeric(length(fnames_test))
  label<- numeric(length(fnames_test))
  body <- character(length(fnames_test))
  for(k in seq_along(fnames_test)){
      names[k]<- stri_extract_first_regex(fnames_test[k], "[01-9_]+(?=\\.txt)")
      body[k] <- readChar(fnames_test[k], file.info(fnames_test[k])$size)
      rating[k] <- as.numeric(stri_extract_first_regex(names[k], "(?<=[_])[01-9]+"))
      label[k] <- ifelse(rating[k]<5,0,1)
  }
  data.frame(names=names, body=body, label=label, stringsAsFactors = FALSE)
}

train <- readFilesFromPath(pathTrain)
test <- readFilesFromPath(pathTest)

## Generate class probabilities:
P_c0 <- sum(as.integer(train$label==0))/length(train$label)
P_c1 <- sum(as.integer(train$label==1))/length(train$label)

## Genarete dictionary with word probability for each class

generateDictionary <- function(){
   library(hash)
   # intialize hash tables for each class
   dict <-hash()
   dict_size <-0
   c0_size <-0
   c1_size <-0
   pb <- txtProgressBar(min=0, max= length(train$label), style=3)
  for(k in seq_along(train$body)){
     for(w in stri_extract_words(train$body[k])[[1]]){ # use only words of length greater then 3
         if (stri_length(w)<3) next
        # laplace correction is taken into account below!
         if(has.key(w,dict)) {# if the word is not new
            if(train$label[k]==0) {
               dict[[w]]<-dict[[w]]+c(1,0)
               c0_size<-c0_size+1
            }
            else {
               dict[[w]]<-dict[[w]]+c(0,1)
               c1_size<-c1_size+1
            }
         }
         else {# if the word is new
            dict_size <- dict_size+1
            if(train$label[k]==0) {
               dict[[w]]<-c(2,1)
               c0_size<-c0_size+2
               c1_size<-c1_size+1
            }
            else {
               dict[[w]]<-c(1,2)
               c0_size<-c0_size+1
               c1_size<-c1_size+2
            }
         }
     }
     setTxtProgressBar(pb, value = k)
  }

   list(dict=dict, sizes=c(dict_size, c0_size, c1_size))
}

dict <- generateDictionary()

# Predict classes in test set

checkAccuracy <- function(){

   predictions<- numeric(length(test$names))
   pb <- txtProgressBar(min=0, max= length(train$label), style=3)
   for (k in seq_along(test$body)){
      # predict class
      pr_c0 <- log(P_c0)
      pr_c1 <- log(P_c1)
      for (w in stri_extract_words(test$body[k])[[1]]){
         if(!has.key(w,dict$dict)) v<-c(dict$sizes[2],dict$sizes[3]) # it will make further logarithm equal to 0
         else v<- dict$dict[[w]]

         pr_c0 <- pr_c0 + log( v[1]/dict$sizes[2] )
         pr_c1 <- pr_c1 + log( v[2]/dict$sizes[3] )
      }
      if(pr_c0>=pr_c1) predictions[k] <- 0
      else predictions[k] <- 1
      setTxtProgressBar(pb, value = k)
   }
   predictions
}

predictions<- checkAccuracy()

sum(as.integer(test$label==predictions))/length(test$label)

#The result is: 0.8065 which is pretty nice :)
