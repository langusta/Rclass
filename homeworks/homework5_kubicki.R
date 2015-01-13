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
# sortedmerge() merges two already sorted (nonincreasingly or nondecreasingly)
# numeric vectors into one, sorted vector.
#
# ARGUMENTS
# x, y - numeric vectors, both sorted nonincreasingly or nondecreasingly
#
# RETURN VALUE
# a sorted, numeric vector which is the result of merging input vectors

## ---- Function ----

# just an example, TO DO: DEL ME
Rcpp::sourceCpp(code='
#include<Rcpp.h>
using namespace Rcpp ;

int checkOrder(const NumericVector x){
  int tryb = 0;
  // tryb == -1 means that table is not sorted
  // tryb == 0 means that all elements are equal,
  // tryb == 1 means non decreasing order, 
  // tryb == 2 means non increasing order
  for(int i=1; i<x.size(); ++i){
    if(x[i]>x[i-1]) {
      if (tryb == 0) tryb = 1;
      else if (tryb == 2) return -1;
    }else if (x[i]<x[i-1]){
      if (tryb == 0) tryb = 2;
      else if (tryb == 1) return -1;
    }
  } 
  return tryb;
}

//[[Rcpp::export]]
NumericVector sortedmerge(const NumericVector x,
                          const NumericVector y) {
  NumericVector out (x.size()+y.size());
  int xtryb, ytryb; 
  
  // check if x and y are ordered
  xtryb = checkOrder(x);
  ytryb = checkOrder(y);

  if (xtryb == -1) stop("SORTEDMERGE: x is not sorted");
  if (ytryb == -1) stop("SORTEDMERGE: y is not sorted");
  if (xtryb >0 and ytryb >0 and xtryb!=ytryb) stop("SORTEDMERGE: x and y are in different orders!"); // both should be in the same order

  int tryb = (xtryb>ytryb)?xtryb:ytryb;

  for (int i=0, j=0; i<x.size() or j<y.size() ;){
    if (i<x.size() and j<y.size()){
      switch(tryb){
        case 0:
          out[i+j] = x[i]; ++i;
        break;
        case 1:
          if (x[i]<y[j]) {out[i+j] = x[i]; ++i;}
          else {out[i+j] = y[j]; ++j;}
        break;
        case 2:
          if (x[i]>y[j]) {out[i+j] = x[i]; ++i;}
          else {out[i+j] = y[j]; ++j;}
        break;
      }
    }else if (i>=x.size()){
      out[i+j] = y[j];
      ++j;
    }else{ // j>=y.size()
      out[i+j] = x[i];
      ++i;
    }
  }
     
  return out;
}
')

## ---- Examples ----

# tests:
library(testthat)
# expect_equivalent(checkOrder(c(4,3,2,2,1,2)),-1)
# expect_equivalent(checkOrder(c(1,2,3,3,4,4,2)),-1)
# expect_equivalent(checkOrder(c(4,3,2,2,1)),2)
# expect_equivalent(checkOrder(c(1,2,3,3)),1)
# expect_equivalent(checkOrder(c(1,1,1)),0)
# expect_equivalent(checkOrder(c(7)),0)
# expect_equivalent(checkOrder(integer(0)),0)
expect_error(sortedmerge(c(1,2),c(2,1))) # x and y in different orders
expect_error(sortedmerge(c(1,2),c(1,1,2,1))) # y is not sorted
expect_error(sortedmerge(c(2,1,2),c(1,1,0))) # x is not sorted
expect_equivalent(sortedmerge(integer(0),c(3,5,6)), c(3,5,6))
expect_equivalent(sortedmerge(c(6,5,3),integer(0)), c(6,5,3))
expect_equivalent(sortedmerge(c(1), integer(0)), c(1))
expect_equivalent(sortedmerge(integer(0), c(2)), c(2))
expect_equivalent(sortedmerge(c(1), c(2)), c(1,2))
expect_equivalent(sortedmerge(c(1,1,1), c(2,1,0)), c(2,1,1,1,1,0))
expect_equivalent(sortedmerge(c(1,1,1), c(2,2,2)), c(1,1,1,2,2,2))
expect_equivalent(sortedmerge(c(2,2,2), c(1,1,1)), c(2,2,2,1,1,1))
expect_equivalent(sortedmerge(c(1,4),c(3,5)), c(1,3,4,5))
expect_equivalent(sortedmerge(c(1,1,4,17),c(3,3,4,5)), c(1,1,3,3,4,4,5,17))
                  
# examples:
sortedmerge(c(1,4),c(3,5))

# when both input vectors are constant they are output to ouput vector in input order
sortedmerge(c(1,1,1), c(2,2,2))
sortedmerge(c(2,2,2), c(1,1,1))


## ------------------------ Exercise 05.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# naomit() removes all missing values from given numeric vector
#
# ARGUMENTS
# x - a numeric vector
#
# RETURN VALUE
# a numeric vector with missing values removed


## ---- Function ----

Rcpp::cppFunction('
NumericVector naomit(const NumericVector x){
  int l=0;
  for(int i=0; i<x.size(); ++i){
    if (!NumericVector::is_na(x[i])) ++l;
  }
  NumericVector out(l);
  l=0;
  for(int i=0; i<x.size(); ++i){
    if (!NumericVector::is_na(x[i])) out[l++]=x[i];
  }
  return out;
}
')

## ---- Examples ----

# tests
library(testthat)
expect_equivalent(naomit(c(1,2,3)), c(1,2,3))
expect_equivalent(naomit(c(1,NA,2,NA,3,NA)), c(1,2,3))
expect_equivalent(naomit(c(NA,NA,NA)), integer(0))
expect_equivalent(naomit(c(NA,NA,7,NA,NA)), 7)

# examples:
naomit(c(1,2,3))
naomit(c(1,NA,2,NA,3,NA))

## ------------------------ Exercise 05.04 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# sample2() generates random subvector of length k of input vector
#
# ARGUMENTS
# x - a numeric vector,
# k - length of random subvector to be generated, 0 <= k <= length(x)
#
# RETURN VALUE
# numeric vector with random subvector

## ---- Function ----

Rcpp::cppFunction('
NumericVector sample2(const NumericVector x, int k, int seed = -1){
  if(k>x.size()) stop("SAMPLE2 ERROR: k greater than length of x");
  if(k<0) stop("SAMPLE2 ERROR: negative k");

  NumericVector perm = Rcpp::clone(x);
  for(int i=0; i<k; ++i){
    int r = (int) Rf_runif(0.0, (perm.size()-i)*1.0);
    double swap = perm[i];
    perm[i] = perm[i+r];
    perm[i+r] = swap;
  }

  NumericVector out(k);
  for(int i=0; i<k; ++i) out[i] = perm[i];
  return out;
}
')

## ---- Examples ----

# tests
library(testthat)
expect_error(sample2(c(1,2), -1))
expect_error(sample2(c(1,2), 3))
expect_equivalent(sample2(integer(0), 0), numeric(0))
set.seed(123)
expect_equivalent(sample2(c(1,2), 2), c(1,2))
set.seed(123)
expect_equivalent(sample2(c(1,2,3,4,5), 2), c(2,5))

# examples:
sample2(c(1,2),2)
sample2(c(1,2,3,4,5), 2) 

## ------------------------ Exercise 05.05 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# randperm() generates random permutation of given numeric vector
#
# ARGUMENTS
# x - a numeric vector,
#
# RETURN VALUE
# numeric vector with random permutation

## ---- Function ----

Rcpp::cppFunction('
NumericVector randperm(const NumericVector x){
  NumericVector perm = Rcpp::clone(x);
  for(int i=0; i<perm.size(); ++i){
    int r = (int) Rf_runif(0.0, (perm.size()-i)*1.0);
    double swap = perm[i];
    perm[i] = perm[i+r];
    perm[i+r] = swap;
  }
  return perm;
}
')

## ---- Examples ----

# tests
library(testthat)
expect_equivalent(randperm(integer(0)), numeric(0))
set.seed(123)
expect_equivalent(randperm(1:3), c(1,3,2))
set.seed(123)
expect_equivalent(randperm(1:10), c(3,9,6,10,4,1,2,5,8,7))

# examples:
randperm(integer(0))
randperm(1:3)
randperm(1:10)

## ------------------------ Exercise 05.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




