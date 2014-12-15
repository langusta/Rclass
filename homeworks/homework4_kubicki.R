## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 4
##
## Student:       Kubicki Karol
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# All the blocks should be kept as-is,
# do not modify the structure of special "## ---" comments.
# This file will be pre-processed automatically.
#
# Solutions non confirming to this very template will not be checked.
#
# Exercise 04.05 and 04.06 -- I'll need separate .Rnw and .pdf files
#
# Before submitting your homework, clear RStudio's workspace
# (e.g. by calling `rm(list=ls(all=TRUE))`) and source() (CTRL+SHIFT+S)
# this script to make sure everything is OK with your code.


## ------------------------ Exercise 04.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# CSVToCSV2 converts csv file with file field separator "," and decimal separator "."
# into a csv file with later separato ";" and former separator ",".
#
# ARGUMENTS
# infname - single string, path to input file
# outfname - single string, path to output file
#
# RETURN VALUE
# function returns invisible(NULL)

## ---- Function ----

CSVToCSV2 <- function(infname, outfname){
   library(stringi)
   stopifnot(length(infname) == 1, length(outfname) == 1)
   stopifnot(is.character(infname), is.character(outfname))
   stopifnot(file.exists(infname))

   if (file.exists(outfname)) warning("CSVToCSV2: Output file will be overwritten!")

   input <- file(infname, open="r")
   output <- file(outfname, open="w")
   while (length(x <- readLines(input, n=1)) > 0){
      parts<-stri_split_fixed(x,'"')[[1]]
      # every second element of parts needs to be changed

      parts[seq(from=1, to= length(parts), by =2)] <- stri_replace_all_fixed(parts[seq(from=beg, to= length(parts), by =2)], ",", ";")
      parts[seq(from=1, to= length(parts), by =2)] <- stri_replace_all_fixed(parts[seq(from=beg, to= length(parts), by =2)], ".", ",")

      writeLines(paste(parts,collapse = '"'), output)

   }
   close(input)
   close(output)

   invisible(NULL)
}

## ---- Examples ----

library(testthat)
expect_error(CSVToCSV2(11,12)) # input file does not exist!

file <- tempfile()
write.csv(1:10,file = file)
file2 <- tempfile()
file.create(file2)
expect_warning(CSVToCSV2(file,file2),"CSVToCSV2: Output file will be overwritten!")

file <- tempfile()
write.csv(data.frame(a=c('"test".1,1', '"test".2,2'), b=c(0.1, 0.2)),
   file=file, row.names=FALSE)
file2 <- tempfile()
CSVToCSV2(file,file2)
expect_equal(readLines(file2), c('"a";"b"', '"""test"".1,1";0,1','"""test"".2,2";0,2'))

# Write some exemplary calls to your function here.
file <- tempfile()
write.csv(data.frame(a=c('"test".1,1', '"test".2,2'), b=c(0.1, 0.2)),
   file=file, row.names=FALSE)
file2 <- tempfile()
cat(readLines(file), sep="\n")
# "a","b"
# """test"".1,1",0.1
# """test"".2,2",0.2

CSVToCSV2(file,file2)
cat(readLines(file2), sep="\n")
# "a";"b"
# """test"".1,1";0,1
# """test"".2,2";0,2

## ------------------------ Exercise 04.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# template - processes each file with extension '.tpl'. It looks for all occurances of
# %name% pattern. If 'name' is a syntactically valid R name then %name% is substituted for its value
# from given data frame. Moreover there are following predefined names:
#   %filename% – base name of the file being processed,
#   %filepath% – full path to the file;
#   %curtime% – current time (hh:mm:ss);
#   %curdate% – current date (yyyy-mm-dd).
# Any other %name% than those mentioned above will not be changed and produce warning.
# Results are stored as '.txt' files. For each file to be overwritten, warning will be generated.
#
# ARGUMENTS
# dirname - single string, name of directory containing '.tpl' files,
# data - a data frame with two columns: key and value. Each key represents name to be substituted into its
#     corresponding value.
#
# RETURN VALUE
# function returns invisible(NULL)

## ---- Function ----

template <- function(dirname, data){
   library(stringi)
   stopifnot(is.character(dirname), length(dirname)==1)
   stopifnot(is.data.frame(data), length(data)==2)
   stopfifnot(names(data) %in% c("key", "value"))
   stopifnot(is.character(data[[1]]))
   stopifnot(is.character(data[[2]]))
   # check if all the keys are valid
   stopifnot( all(stri_detect_regex(data$key, "^([a-zA-Z]|[\\.](?![0-9]))[a-zA-Z0-9\\._]*$")) )

   # read names of files
   fnames <- list.files(dirname, pattern= "*.tpl", full.names=TRUE)

   for(k in seq_along(fnames)){# for each file
      body <- readLines(input)
      for(line in seq_along(body)){
         pos <- stri_locate_all_regex(body[line], "%([a-zA-Z]|[\\.](?![0-9]))[a-zA-Z0-9\\._]*%")[[1]]
         if (is.na(pos[1,1])) next
         for(found in 1:dim(pos)[1]){
            name <- substr(body[line], pos[found,1]+1, pos[found,2]-1)
            if ( any(data$key == name) ){

            }else if (data$key == "filename"){}
            else if (data$key == "filepath"){}
            else if (data$key == "curtime"){}
            else if (data$key == "curdate"){}
            else{ # warning!
            }
            #Sys.Date()
            #substr(Sys.time(),12,19)
            #basename( fnames[k])
            #dirname( fnames[k])
         }
      }

   }

}

## ---- Examples ----

# tests:
library(testthat)

dir <- tempdir()
f1 <- tempfile(fileext = ".tpl")
sink(f1)
cat("Plik: %filename%\nW katalogu: %filepath%\nala\nma\nkota %imie% \na\nkot %.1imie% \nma\nale\n%curtime%\n%curdate%\n")
sink()
cat(readLines(f1), sep="\n")

f2 <- tempfile(fileext = ".tpl")
f3 <- tempfile(fileext = ".tpl")


# examples:




## ------------------------ Exercise 04.03 ----------------------------

## ---- Documentation ----

# I don't know how to solve this exercise. :(

## ---- Function ----

# I don't know how to solve this exercise. :(

## ---- Examples ----

# I don't know how to solve this exercise. :(




## ------------------------ Exercise 04.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...

