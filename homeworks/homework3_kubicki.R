## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 3
##
## Student:       YourSurnameHere YourNameHere
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# All the blocks should be kept as-is,
# do not modify the structure of special "## ---" comments.
# This file will be pre-processed automatically.
#
# Solutions non confirming to this very template will not be checked.
#
#
# Before submitting your homework, clear RStudio's workspace
# (e.g. by calling `rm(list=ls(all=TRUE))`) and source() (CTRL+SHIFT+S)
# this script to make sure everything is OK with your code.


## ------------------------ Exercise 03.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# eventdifftime computes difference in time between consecutive (input data is sorted first) events
# from input data frame.
#
# ARGUMENTS
# events - data frame in which each row represents time of some event.
#   It should have following columns:
#     h - an integer vector with elements in {0,1,...,23}
#     m - an integer vector with elements in {0,1,...,59}
#     s - an integer vector with elements in {0,1,...,59}
#
# RETURN VALUE
# A vector containing differences in seconds between every two consecutive
# events (input data is first sorted according to hours, then minutes, then seconds).

## ---- Function ----

eventdifftime <- function (events){
  stopifnot(is.data.frame(events))
  stopifnot(length(names(events))==3)
  stopifnot(all(sort(names(events))==c("h", "m", "s")))
  stopifnot( all(events$h>=0 & events$h<=23) )
  stopifnot( all(events$m>=0 & events$m<=59) )
  stopifnot( all(events$s>=0 & events$s<=59) )
  stopifnot(all(unlist(events)==as.integer(unlist(events))))

  events <- events[order(events$h, events$m, events$s),]
   n<-length(events$h)
  events$h[-1]*60*60+events$m[-1]*60+events$s[-1]-events$h[-n]*60*60-events$m[-n]*60-events$s[-n]
}

## ---- Examples ----

library(testthat)
expect_error(eventdifftime("czas"))
expect_error(eventdifftime(c(1,2,3)))
expect_error(eventdifftime(list(1,2,3)))
expect_error(eventdifftime(data.frame()))
expect_error(eventdifftime(data.frame(h=1,m=1,s=1, ala=1)))
expect_error(eventdifftime(data.frame(h=1,m=1,ala=1)))
expect_error( eventdifftime(data.frame(h=c(13,14,13), m=c(1,3,2), s=c(1,4,2.5))) )
expect_error( eventdifftime(data.frame(h=c(13,14,13), m=c(1,3.1,2), s=c(1,4,2))) )
expect_error( eventdifftime(data.frame(h=c(13.3,14,13), m=c(1,3,2), s=c(1,4,2))) )
expect_equivalent(eventdifftime(data.frame(h=1, m=1, s=1)), numeric(0))
expect_equivalent(eventdifftime(data.frame(h=c(13,14,13), m=c(1,3,2), s=c(1,4,2))), c(61,3662))
expect_equivalent(eventdifftime(data.frame(h=c(0,0,0,0,0,1,1,1,1,1), m=c(1,1,1,2,2,1,1,2,2,2), s=c(3,2,1,2,1,2,1,3,2,1))),
   c(1,1,58,1,3539,1,59,1,1))

# exemplary calls
eventdifftime(data.frame(h=c(13,14,13), m=c(1,3,2), s=c(1,4,2)))
eventdifftime(data.frame(h=c(0,0,0,0,0,1,1,1,1,1), m=c(1,1,1,2,2,1,1,2,2,2), s=c(3,2,1,2,1,2,1,3,2,1)))




## ------------------------ Exercise 03.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# uncomment removes comments from given source code chunk
#
# ARGUMENTS
# text - character vector, each string represents a single line of source code
# tags - 2-column character matrix - each row determines how to start and end a comment
#        if second entry in a row is NA, then the comment ends at the ned of the line
#
# RETURN VALUE
# a character vector with removed comments

## ---- Function ----

uncomment <- function (text, tags){
   stopifnot(is.character(text), is.character(tags))
   stopifnot(is.matrix(tags), dim(tags)[2]==2)
   stopifnot( !any(is.na(tags[,1]) ) )

   # get all the tags
   begs<-""
   ends<-""
   begs_na<-""
   for(k in seq_along(tags[,1])){
      if(is.na(tags[k,2])){
         begs_na[length(begs_na)+1]<-tags[k,1]
      }else{
         begs[length(begs)+1]<-tags[k,1]
         ends[length(ends)+1]<-tags[k,2]
      }
   }

   if(length(begs)==1) {no_double<-TRUE } else no_double<-FALSE
   if(length(begs_na)==1) {no_single<-TRUE } else no_single<-FALSE
   begs<-begs[-1]
   ends<-ends[-1]
   begs_na<-begs_na[-1]

   active<-FALSE
   beginning<-":)"
   beginning_na<-":)"
   ending<-":("
   for(k in seq_along(text)){
      # description below
      line <- text[k]
      if (active){ #if there is an open comment
         pos<-stri_locate_first_fixed(line, ending)[2]
         if( is.na(pos) ){
            text[k]<-""
            next
         }
         # remove comment
         active<-FALSE
         line <- substring(line, pos+1)
      }
      #look for comments:
      while(TRUE){
         if (no_double){pos_d<-(-1)}else{
            pos_v<-stri_locate_first_fixed(line, begs)[,1]
            if(all(is.na(pos_v))) {pos_d<-(-1)} else{
               pos_d<-min(pos_v,na.rm=TRUE)
               beginning<-begs[which(pos_v==pos_d)]
               ending<-ends[which(pos_v==pos_d)]
            }

         }
         if (no_single){pos_s<-(-1)}else{
            pos_v<-stri_locate_first_fixed(line, begs_na)[,1]
            if(all(is.na(pos_v))) {pos_s<-(-1)} else{
               pos_s<-min(pos_v,na.rm=TRUE)
               beginning_na<-begs_na[which(pos_v==pos_s)]
            }
         }


         if(pos_d==-1 && pos_s==-1) break

         if (pos_d==-1) pos_d<-pos_s+1

         if(pos_s!=-1 && pos_s<pos_d){
            line<-substring(line, 1, pos_s-1)
            break
         }else{
            pos_e<-stri_locate_first_fixed(line, ending)[2]
            if( is.na(pos_e) ){
               line<-substring(line,1,pos_d-1)
               active<-TRUE
               break
            }
            # remove comment
            line <- paste(substring(line, 1, pos_d-1),substring(line, pos_e+1), sep="")
         }
      }

      line<-stri_trim(line)
      text[k]<-line

      # description:
      #if you are inside comment search for terminating sign
         #search for ending
         #if ending found - del comment and set active<-FALSE
         #else - del whole line

      #while(TRUE)
         #search for first non-ending comment
         #search for first ending comment

         #if there is no comment break

         #if non-ending comment is earlier del till the end of line
         #else
            #search for ending
            #if found - del comment
            #else
               # set - inside comment (active<- TRUE)

   }
   #remove blank lines
   text[text!=""]
}

## ---- Examples ----

library(testthat)
expect_error(uncomment("ala ma kota"))
expect_error(uncomment("ala ma kota", matrix(c(1, NA),ncol=2) ))
expect_equivalent(uncomment("aha", matrix(c("#", NA),ncol=2) ), "aha")
expect_equivalent(uncomment("ala ma #kota", matrix(c("#", NA),ncol=2)), "ala ma")
expect_equivalent(uncomment("ala foma #kota", matrix(c("#","fo", NA,NA),ncol=2)), "ala")
expect_equivalent(uncomment("1 /* foo bar /* foo // bar */ 2", matrix(c("/*", "//", "*/", NA), ncol=2) ),
      "1  2")
expect_equivalent(uncomment("ala/* a*/ ma/* yhy */ kota /**/", matrix(c("/*","fo", "*/",NA),ncol=2)),
   "ala ma kota")
expect_equivalent(uncomment(c("1 /* foo bar /* foo // bar */ 2",
   "ala /* wcale",
   "nie */ ma kota",
   "jurek ma zyrafe //od listopada"
   ),
   matrix(c("/*", "//", "*/", NA), ncol=2) ),
   c("1  2", "ala", "ma kota", "jurek ma zyrafe")
)

# exemplary calls
uncomment("1 /* foo bar /* foo // bar */ 2", matrix(c("/*", "//", "*/", NA), ncol=2) )
uncomment(c("1 /* foo bar /* foo // bar */ 2",
   "ala /* wcale",
   "nie */ ma kota",
   "jurek ma zyrafe //od listopada"
   ),
   matrix(c("/*", "//", "*/", NA), ncol=2) )


## ------------------------ Exercise 03.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# findemails - extracts all well formed email addresses from each string in
# a given character vector
#
# ARGUMENTS
# text - a character vector
#
# RETURN VALUE
# a character vector with extracted, well formed email addresses

## ---- Function ----

findemails <- function(text){
   stopifnot(is.character(text))

   result<-character(0)
   for(k in seq_along(text)){
      result<-c(result, unlist(stri_extract_all_regex(text[k],
         "[a-zA-Z01-9][.\\w]*@[a-zA-Z01-9-]+([.][a-zA-Z01-9-]+)+")))
   }
   result[!is.na(result)]
}

## ---- Examples ----


library(testthat)
expect_error(findemails(1))
expect_equivalent(findemails(""),character(0))
expect_equivalent(findemails("k...a@m.c"),"k...a@m.c")
expect_equivalent(findemails("k...a@m."),character(0))
expect_equivalent(findemails("k...a@m"),character(0))
expect_equivalent(findemails("k.*.a@m.c"),"a@m.c")
expect_equivalent(findemails("k.@.a@m.c"),"a@m.c")
expect_equivalent(findemails("Jola lojalna <jl@google.com>"),"jl@google.com")
expect_equivalent(findemails(c("a@b@c", "Send comments to Grzegorz.Urlich@math.sk",
"ooo@e13.com, wu@ok.edu")),c("Grzegorz.Urlich@math.sk","ooo@e13.com","wu@ok.edu"))

# exemplary calls
findemails("Jola lojalna <jl@google.com>")
findemails("abra <12.._jl@goo-gle.com> kadavbra")
findemails(c("a@b@c", "Send comments to Grzegorz.Urlich@math.sk","ooo@e13.com, wu@ok.edu"))

## ------------------------ Exercise 03.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 03.05 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 03.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




