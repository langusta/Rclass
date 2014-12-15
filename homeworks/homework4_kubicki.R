## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 4
##
## Student:       YourSurnameHere YourNameHere
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
# ... write at least one paragraph about what the function does ...
#
# ARGUMENTS
# ... document each of the function's arguments ...
#
# RETURN VALUE
# ... describe what kind of object the function returns ...

## ---- Function ----

# ... in this block, only functions' definitions are allowed,
# e.g. something like:
# fname1 <- function(....) { .... }
#
# Please document your code extensively.
#
# If you develop more than one solution to this exercise,
# put them all here, e.g. something like:
# fname2 <- function(....) { .... }
# fname3 <- function(....) { .... }
# ...

## ---- Examples ----

# Write at least 10 testthat tests here.
#
# Write some exemplary calls to your function here.




## ------------------------ Exercise 04.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function changes the sign of each element in a given numeric vector
#
# ARGUMENTS
# x - a numeric vector
#
# RETURN VALUE
# a numeric vector, -x

## ---- Function ----

chgsgn <- function(x) # Just an example, TO DO: DEL ME
{
   stopifnot(is.numeric(x))
   -x
}

## ---- Examples ----

# Just a bunch of examples, TO DO: DEL ME
# tests:
library(testthat)
expect_identical(chgsgn(numeric(0)), numeric(0))
expect_error(chgsgn(mean))
expect_equivalent(chgsgn(c(-1,2,3)), c(1,-2,-3))
expect_equivalent(chgsgn(c(-1,2,NA)), c(1,-2,NA))
test <- rnorm(10)
expect_equivalent(chgsgn(test), -test)
# ...

# examples:
chgsgn(rnorm(10)) # some random data



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

