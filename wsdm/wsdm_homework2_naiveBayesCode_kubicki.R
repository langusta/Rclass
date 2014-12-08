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


readFilesFromPath <- function(path){
  library(stringi)
  fnames_test <- list.files("/home/user/Projects/ContentBigData/Dane/wsdm/WSDM_H2_randomSampleOfDocuments/test",
                            pattern= "*.txt", full.names=TRUE)
  names<-character(length(fnames_test))
  rating<- numeric(length(fnames_test))
  label<- numeric(length(fnames_test))
  body <- character(length(fnames_test))
  for(k in seq_along(fnames_test)){
      names[k]<- stri_extract_first_regex(fnames_test[k], "[01-9_]+(?=\\.txt)")
      sprawdzam ssh
  }
}