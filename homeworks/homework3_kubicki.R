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
  
  
  events
}

## ---- Examples ----

library(testthat)
expect_error(eventdifftime("czas"))
expect_error(eventdifftime(c(1,2,3)))
expect_error(eventdifftime(list(1,2,3)))
expect_error(eventdifftime(data.frame(h=1,m=1,s=1, ala=1)))
expect_error(eventdifftime(data.frame(h=1,m=1,ala=1)))
expect_equivalent(eventdifftime(data.frame(h=1, m=1, s=1)), c())
expect_equivalent(eventdifftime(data.frame(h=c(13,14,13), m=c(1,3,2), s=c(1,4,2))), c(61,3662))

# exemplary calls
eventdifftime(data.frame(h=c(13,14,13), m=c(1,3,2), s=c(1,4,2)))




## ------------------------ Exercise 03.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function determines which strings are not empty
#
# ARGUMENTS
# x - a character vector
#
# RETURN VALUE
# a character vector, !stri_empty(x)

## ---- Function ----

isnempty <- function(x) # Just an example, TO DO: DEL ME
{
   library("stringi")
   stopifnot(is.character(x))
   !stri_empty(x)
}

## ---- Examples ----

# Just a bunch of examples, TO DO: DEL ME
# tests:
# ...

# examples:
# ...



## ------------------------ Exercise 03.03 ----------------------------

## ---- Documentation ----

# I don't know how to solve this exercise. :(

## ---- Function ----

# I don't know how to solve this exercise. :(

## ---- Examples ----

# I don't know how to solve this exercise. :(




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




