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

pathTest<-"/home/user/Projects/ContentBigData/Dane/wsdm/WSDM_H2_randomSampleOfDocuments/test"
pathTrain<-"/home/user/Projects/ContentBigData/Dane/wsdm/WSDM_H2_randomSampleOfDocuments/train"

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
  data.frame(names=names, body=body, rating=rating, label=label, stringsAsFactors = FALSE)
}

train <- readFilesFromPath(pathTrain)
test <- readFilesFromPath(pathTest)

## Generate class probabilities:
## Genarete dictionary with word probability for each class



