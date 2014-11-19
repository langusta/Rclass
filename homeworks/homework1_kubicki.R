## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 1
##
## Student:       Kubicki Karol
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# All the blocks should be kept as-is,
# do not modify the structure of special "## ---" comments.
# This file will be pre-processed automatically.
#
# Solutions non conforming to this very template will not be checked.
#
#
# Before submitting your homework, clear RStudio's workspace
# (e.g. by calling `rm(list=ls(all=TRUE))`) and source() (CTRL+SHIFT+S)
# this script to make sure everything is OK with your code.


## ------------------------ Exercise 01.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# The function computes the arithmetic mean of its first argument x transformed in such a way
# that its k smallest or largest elements are substituted for the (k+1)th
# smallest or largest value in x, respectively.
#
# ARGUMENTS
# x – numeric vector of length n (for some n),
# k – a single natural number, k <= (n−1)/2
#
# RETURN VALUE
# k-Winsorized mean,

## ---- Function ----

winsor <- function(x, k=1){
   stopifnot(is.numeric(x), is.numeric(k))
   stopifnot(k>=0, k<=(sum(is.finite(x))-1)/2)
   #sort x
   x <- sort(x)
   # set first k elements to the value of (k+1)th
   x[0:k]<-x[k+1]
   #set the value of last k elements to the value of (k+1)th largest
   x[(length(x)-k+(k!=0)):length(x)]<-x[length(x)-k]
   #compute mean
   mean(x)
}

## ---- Examples ----

# For example, if x = (1, 2, . . . , 10),
# then its 3-winsorized mean (k=3) is equal to the mean of
# (4, 4, 4, 4, 5, 6, 7, 7, 7, 7), i.e. 5.5.
#

library(testthat)
expect_error(winsor(c(1,2,NA,4,5,6,7,NA),Inf))
expect_error(winsor(c(1,2,NA,4,5,6,7,NA),NA))
expect_error(winsor(c(1,2,NA,4,5,6,7,NA),-1))
expect_error(winsor(c(1,2,NA,4,5,6,7,NA),3))
expect_error(winsor(numeric(0),3))
expect_equivalent(winsor(c(1,2,NA,4,5,6,7,NA),2), 4.5)
expect_equivalent(winsor(1:10,3), 5.5)


# Write some exemplary calls to your function here.
winsor(1:10, 3)
winsor(sample(1:10, 10, replace=FALSE),3)
winsor(c(1,2,3),0) #the same as mean(c(1,2,3))


## ------------------------ Exercise 01.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function replaces every missing value in a given numerci vector for an arithmetic mean
# of its preceding and following element
# If a vector cannot be updated accordint to this rule - error is thrown.
#
# ARGUMENTS
# x - a numeric vector
#
# RETURN VALUE
# a numeric vector without NA values

## ---- Function ----

replacena <- function (x)
{
   stopifnot(is.numeric(x))
   stopifnot(sum(is.nan(x))==0)
   #stop if you cannot compute the average (NA at the beginning or ending)
   stopifnot(!is.na(x[1]), !is.na(x[length(x)]))
   #stop if there are more than one consecutive NA in given vector
   #( for every NA there should be a number before it)
   stopifnot(sum(is.na(x))==sum((is.na(x)-c(is.na(x)[-1], 0))==-1) )
   x[is.na(x)]=( x[ (is.na(x)-c(is.na(x)[-1], 0))==-1 ]
               + x[ (is.na(x)-c(0,is.na(x)[-length(x)]))==-1 ]
               )/2
   x
}

## ---- Examples ----
#For example,
# if (5, NA, 6, 2, 3, 5, 6, 4, NA, 2, NA, 5)
# is given, then the result should be (5, 5.5, 6, 2, 3, 5, 6, 4, 3, 2, 3.5, 5).

library(testthat)
expect_error(replacena(mean))
expect_error(replacena(c(1,NA,NA,4)))
expect_error(replacena(c(NA,2,3)))
expect_error(replacena(c(1,2,NA)))
expect_equivalent(replacena(c(1,NA,3,NA,5)), c(1,2,3,4,5))
expect_equivalent(replacena(c(1,NA,Inf,4,5,6)), c(1,Inf,Inf,4,5,6))

# examples:
replacena(c(1,2,3,4,5,6))
replacena(c(1,NA,3,4,NA,6))
replacena(c(1,NA,Inf,4,5,6))

## ------------------------ Exercise 01.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function returns vector of indices such that for every index i we have
# x_i==x_(i+1), where x_i are elements from input vector
#
# ARGUMENTS
# x - an atomic vector
#
# RETURN VALUE
# a numeric vector with desired indices

## ---- Function ----

neighboreq <- function(x)
{
   stopifnot(is.atomic(x))
   # compare x to itself without first element (and with NA at the end)
   which(x==c(x[-1],NA))
}


## ---- Examples ----

# If x == c(1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0), then
# the desired output is (1, 4, 5, 7, 9).

library(testthat)
expect_error(neighboreq(mean))
expect_error(neighboreq(list(1,2)))
expect_equivalent(neighboreq(integer(0)), integer(0))
expect_equivalent(neighboreq(1), integer(0))
expect_equivalent(neighboreq(c(1,2)), integer(0))
expect_equivalent(neighboreq(c(1,1)), 1)
expect_equivalent(neighboreq(c(1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0)), c(1,4,5,7,9))
expect_equivalent(neighboreq(c("ala", "ma", "ma", "kota")), 2)

# examples:
neighboreq(c(1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0))
neighboreq(c("ala", "ma", "ma", "kota"))

## ------------------------ Exercise 01.04 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function returns an integer vector with n (second argument) most significant
# decimal digits of its first argument x
# x_i==x_(i+1), where x_i are elements from input vector
#
# ARGUMENTS
# x – a single numeric value,
# n – a single natural number, n ≤ 16.
#
# RETURN VALUE
# a numeric vector with n most significant decimal digits

## ---- Function ----

decdig <- function (x=pi,n=10)
{
   stopifnot(is.numeric(x),length(x)==1)
   stopifnot(is.numeric(n), n==round(n), n>=1, n<=16)

   power <- floor(log10(x))
   #now floor(x/ 10^power) gives you the first digit of x
   #below - compute the powers of 10 to get all significant digits
   powers <- 10^seq(from=power,by=-1, length.out=n)
   floor(x/powers) %% 10
}


## ---- Examples ----

#if x == pi (a built-in R constant) and n == 10,
# the result should be (3, 1, 4, 1, 5, 9, 2, 6, 5, 3).

library(testthat)
expect_error(decdig(mean,2))
expect_error(decdig(12345.25,0))
expect_error(decdig(12345.25,17))
expect_error(decdig(12345.25,1.7))
expect_equivalent(decdig(12345.25,1), c(1))
expect_equivalent(decdig(12345.25,6), c(1,2,3,4,5,2))
expect_equivalent(decdig(pi,10), c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3))

# examples:
decdig(12345,10)
decdig(pi,10)



## ------------------------ Exercise 01.05 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function returns a vector with quantiles of input vector
# Let x (i) denote the ith smallest value in x, with assumption x (n+1) = x (n) .
# The quantile 2 of order p is given by x ( floor(h) ) + (h − floor(h) )(x ( floor(h) +1) − x ( floor(h) ) ),
# where h = (n − 1)p + 1.
# The function is vectorized w.r.t. p. For example, if x == c(1, 2, 3, 4, 5)
# and p == c(0.25, 1), then the result should be (2, 5).
#
# ARGUMENTS
# x – numeric vector,
# p – vector with orders of quantiles to be computed
#
# RETURN VALUE
# a numeric vector with quantiles of desired orders

## ---- Function ----

quantile <- function (x, p)
{
   stopifnot(is.numeric(x))
   stopifnot(is.numeric(p), p<=1, p>=0)

   x <- sort(x)
   h <- (length(x)-1)*p+1
   f_h <- floor(h)

   # unlist(lapply(f_h+1, min, length(x))) - returns a vector with each element of f_h+1
   # not greater than length(x)

   x[f_h]+(h-f_h)*(x[ unlist(lapply(f_h+1, min, length(x))) ]-x[f_h])
}

## ---- Examples ----

# if x == c(1, 2, 3, 4, 5) and p == c(0.25, 1),
# then the result is (2, 5).

library(testthat)
expect_error(quantile(mean,1))
expect_error(quantile(c(1,2,3),2))
expect_error(quantile(c("ala","ma","psa"),0.5))
expect_error(quantile(c(1,2,3),"ala jednak ma kota"))
expect_equivalent(quantile(c(1,2,3,4,5),c(0.25,1)), c(2,5))
expect_equivalent(quantile(c(1,2,3,4,5),c(0.25,0.5, 1)), c(2,3,5))
expect_equivalent(quantile(1:100, c(0.25, 0.5, 0.75)), c(25.75,50.50,75.25))

# examples:
quantile(c(1,2,3,4,5),c(0.25,1))
quantile(1:100, c(0.25, 0.5, 0.75))



## ------------------------ Exercise 01.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




