## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 5
##
## Student:       Kubicki Karol
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


## ------------------------ Exercise 05.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# lcs() computes the length of the longest common subsequence
# of given two numeric vectors.
#
# ARGUMENTS
# v, w - numeric vectors
#
# RETURN VALUE
# n - integer, length of the longest common subsequence of two numeric vectors

## ---- Function ----



Rcpp::cppFunction('
   int lcs(const NumericVector v,
           const NumericVector w){
      int n = 0, l = 0;
      for(int i = 0; i<v.size() ; ++i){
         for(int j = 0; j<w.size() ; ++j){
            if( v(i) == w(j) ){
               int k = i + 1;
               ++j;
               ++l;
               while(k<v.size() && j<w.size()){
                  if(v(k) != w(j)) break;
                  ++k;++j;++l;
               }
               if(n < l) n = l;
               l=0;--j; // remember about ++ at the end of for loop!
            }
         }
         if (v.size() - i + 1 < n) break;
      }
      return n;
   }
')

## ---- Examples ----

library(testthat)
expect_error(lcs(c(1,2))) # there should be second argument
expect_error(lcs(c(1,2), c("a", "b"))) # input should be numeric
expect_error(lcs(c(), c(1,2,3)))
expect_equivalent(lcs(c(1,2,3,4), c(1,5,3,4,6,1)), 2)
expect_equivalent(lcs(       c(1,1,1,2,8),
                     c(1,5,3,4,6,1,1,2,90)),
                  3
   )
expect_equivalent(lcs(c(8), c(1,2,3)), 0)
expect_equivalent(lcs(integer(0), c(1,2,3)), 0)

# Write some exemplary calls to your function here.
lcs(1:10, 1:10)
lcs(c(1,2,3,4), c(1,5,3,4,6,1))
lcs(        c(1,1,1,2,8),
    c(1,5,3,4,6,1,1,2,90))


## ------------------------ Exercise 05.02 ----------------------------

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

# just an example, TO DO: DEL ME
Rcpp::cppFunction('
   NumericVector chgsgn(NumericVector x) {
      NumericVector y = Rcpp::clone(x);
      int n = y.size();
      for (int i=0; i<n; ++i)
         if (!NumericVector::is_na(y[i]))
            y[i] = -y[i];
      return y;
   }
')

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


## ------------------------ Exercise 05.03 ----------------------------

## ---- Documentation ----

# I don't know how to solve this exercise. :(

## ---- Function ----

# I don't know how to solve this exercise. :(

## ---- Examples ----

# I don't know how to solve this exercise. :(




## ------------------------ Exercise 05.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 05.05 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 05.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




