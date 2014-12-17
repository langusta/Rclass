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
   stopifnot(names(data) %in% c("key", "value"))
   stopifnot(is.character(data[[1]]))
   stopifnot(is.character(data[[2]]))
   # check if all the keys are valid
   stopifnot( all(stri_detect_regex(data$key, "^([a-zA-Z]|[\\.](?![0-9]))[a-zA-Z0-9\\._]*$")) )

   # read names of files
   fnames <- list.files(dirname, pattern= "*.tpl", full.names=TRUE)

   for(k in seq_along(fnames)){# for each file
      body <- readLines(fnames[k])
      for(line in seq_along(body)){

         for(knum in seq_along(data$key)){
            body[line] <- stri_replace_all_fixed(body[line],
                           paste("%",data$key[knum],"%",sep=""),
                           data$value[knum]
                        )
         }
         body[line] <- stri_replace_all_fixed(body[line],"%filename%", basename( fnames[k]))
         body[line] <- stri_replace_all_fixed(body[line],"%filepath%", dirname( fnames[k]))
         body[line] <- stri_replace_all_fixed(body[line],"%curtime%", substr(Sys.time(),12,19))
         body[line] <- stri_replace_all_fixed(body[line],"%curdate%", as.character(Sys.Date()))

         pos <- stri_locate_all_regex(body[line], "%([a-zA-Z]|[\\.](?![0-9]))[a-zA-Z0-9\\._]*%")[[1]]

         if (!is.na(pos[1,1])){ # there are some strings of the form %name% left - rise warning
            warning("template: There are some unresolved %name% strings!")
         }
      }
      # write the output
      writeLines(body, stri_replace_last_fixed(fnames[k], "tpl", "txt"))
   }

}

## ---- Examples ----

# tests:
library(testthat)
expect_error(template(11,data.frame(key = "imie", value = "Jurek",stringsAsFactors = FALSE))) #wrong directory
expect_error(template(tempdir(),data.frame(asd = "imie", value = "Jurek",stringsAsFactors = FALSE))) #wrong column name in data frame
expect_error(template(tempdir(),data.frame(key = ".1imie", value = "Jurek",stringsAsFactors = FALSE))) #wrong key value


dir <- tempdir()
f1 <- tempfile(fileext = ".tpl")
sink(f1)
# in the following file - %.1imie% doesn't have valid R name, so it is not detected at all!
cat("Plik: %filename%\nW katalogu: %filepath%\nala\nma\nkota %imie% \na\nkot %.1imie% \nma\nale\n%curtime%\n%curdate%\n")
sink()
#cat(readLines(f1), sep="\n")
f2 <- tempfile(fileext = ".tpl")
sink(f2)
# in the following fille %nazwisko% should rise warning!
cat("Plik: %filename%\nW katalogu: %filepath%\nala\nma\nkota %imie% \na\nkot %nazwisko% \nma\nale\n%curdate%\n")
sink()
#cat(readLines(f2), sep="\n")

expect_warning(template(tempdir(),data.frame(key = "imie", value = "Jurek",stringsAsFactors = FALSE)),
      "template: There are some unresolved %name% strings!")
f1_out_body <- readLines(stri_replace_last_fixed(f1,"tpl","txt"))
f1_out_expect <- c(paste("Plik: ",basename(f1), sep=""),
      paste("W katalogu: ",dirname(f1), sep=""),
      "ala","ma", "kota Jurek ", "a", "kot %.1imie% ", "ma", "ale",
      substr(Sys.time(),12,19),
      as.character(Sys.Date()) )
expect_equal(f1_out_body, f1_out_expect)
f2_out_body <- readLines(stri_replace_last_fixed(f2,"tpl","txt"))
f2_out_expect <- c(paste("Plik: ",basename(f2), sep=""),
      paste("W katalogu: ",dirname(f2), sep=""),
      "ala","ma", "kota Jurek ", "a", "kot %nazwisko% ", "ma", "ale",
      as.character(Sys.Date()) )
expect_equal(f2_out_body, f2_out_expect)

# examples:

# look into tests above for an examples


## ------------------------ Exercise 04.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# BibTeX2data.frame - converts given BibTeX file int data frame. Each row represents one BibTeX entry.
# Colums:
#  type - type of BibTeX entry,
#  id - id of BibTeX entry
#  ... - a column for each field extracted from the file (fields are of the field=value form)
#
# ARGUMENTS
# filename - single string, name of the BibTeX file.
#
# RETURN VALUE
# a data frame with converted BibTeX file.

## ---- Function ----

BibTeX2data.frame <- function(filename){
   stopifnot(is.character(filename), length(filename)==1)

   input <- file(filename, open="r")

   while (length(line <- readLines(input, n=1)) > 0){


   }
   close(input)
}

## ---- Examples ----

# tests:
library(testthat)

#examples:



## ------------------------ Exercise 04.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...

