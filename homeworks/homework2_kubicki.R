## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 1
## Homework 2
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


## ------------------------ Exercise 02.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# The function returns a logical vector w of length n such that 
# w_l == TRUE iff (Exists(p) l in [i_p ; j_p ]). For example, 
# if n = 7, i == c(1, 4), j == c(1, 6), 
# then the result is (TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE).
#
# ARGUMENTS
# i - vector of integers,
# j - vector of integers of the same length as i,
# n - natural number
#
# following condition must be satisfied:
# For_all  ( l )  ( 1<= i_l <= j_l <= n AND i_l>j_(l-1) )
#
# RETURN VALUE
# w - vector of length n

## ---- Function ----

logiderle <- function(i,j,n) {
  # check if arguments satisfy excercise conditions
  stopifnot(is.numeric(i), is.numeric(j), is.numeric(n), length(n)==1)
  # check if i and j elements are integers
  stopifnot( all.equal(i, as.integer(i), check.attributes = FALSE),
             all.equal(j, as.integer(j), check.attributes = FALSE),
             n == as.integer(n) )
  stopifnot(length(i)==length(j))
  stopifnot(n>=1, n<Inf) # 1<=i<=j<=n yields n>=1
  stopifnot(i>=1, j>=i, n>=j)
  stopifnot(i[-1]>j[-length(j)])
  
  # init the output
  w <- rep(FALSE,n)
  # for each interval given by i and j add TRUE to w
  for( k in seq_along(i))
    w[i[k]:j[k]] <- TRUE
  w
}

## ---- Examples ----
library(testthat)
expect_error(logiderle(c(1,),  c(NA), 7))
expect_error(logiderle(c(1,),  c(2), Inf))
expect_error(logiderle(c(1,4),  c(1,3), 7))
expect_error(logiderle(c(1,2),  c(3,6), 7))
expect_error(logiderle(c(1,4),  c(1,6), 7.5))
expect_error(logiderle(c(1,4),  c(1,6.5), 7))
expect_error(logiderle(list(1,4),  c(1,6.5), 7))
expect_equivalent(logiderle(c(1),c(1), 1), TRUE)
expect_equivalent(logiderle(c(2),  c(3), 4), c(FALSE, TRUE, TRUE, FALSE))
expect_equivalent(logiderle(c(1,4),  c(1,6), 7), c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
expect_equivalent(logiderle(c(1,4,7),  c(2,5,8), 10), c(TRUE,TRUE, FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE))

# exemplary calls
logiderle(c(1),  c(1), 1)
logiderle(c(2),  c(3), 4)
logiderle(c(1,4),  c(1,6), 7)
logiderle(c(1,4,7),  c(2,5,8), 10)

## ------------------------ Exercise 02.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function gives an approximation of local minimum 
# of a given continous function on a given interval
#
# ARGUMENTS
# f() - a vectorized R function, f is continuous on [a,b]
# a - single numeric value,
# b - single numeric value, b > a
# eps - positive numeric value, default = 10^(-16)
# maxiter - maximal number of iterations, integer, default = 100
#
# RETURN VALUE
# named list with following components:
# par - approximate location of local minimum,
# value == f(par) - value of f() at calculated minimum,
# counts - numbered of considered iterations,
# convergence - 0 if the method converged in no more then maxiter iterations, 1 otherwise,
# message - always equal to NULL
#

## ---- Function ----

golden_ratio <- function(f, a, b, eps = 1e-16, maxiter = 100){
  
  stopifnot(is.function(f), is.finite(a), is.finite(b), is.finite(eps), is.finite(maxiter))
  stopifnot(length(a)==1, length(b)==1)
  stopifnot(a<b)  
  stopifnot(eps>0, maxiter>0)
  stopifnot(maxiter == as.integer(maxiter))
  
  #f <- match.fun(f)
  
  phi <- (sqrt(5)-1)/2
  convergence <- 1
  # main loop
  for (i in 1:maxiter){
    l <- b-phi*(b-a)
    p <- a+phi*(b-a)
    
    # check from which side should we shorten given interval
    if (f(l) > f(p) ) a <- l
    else b <- p
    
    # check if we have converged
    if (b-a < eps) {
      convergence <- 0
      break
    }
  }
  if (convergence == 1) warning ("golden_ratio did not converge within given parameters! You may want to use bigger eps or maxiter parameter.")
  list(
    par = (a+b)/2, 
    value = f((a+b)/2),
    counts = i,
    convergence = convergence,
    message = NULL
  )
}

## ---- Examples ----

# tests:
library(testthat)
expect_error(golden_ratio(sin, 0.5, 1, -0.5))
expect_error(golden_ratio(sin, 0.5, 1, maxiter=0))
expect_error(golden_ratio(sin, 0.5, -1))
expect_error(golden_ratio(sin, 0.5, NA))
expect_error(golden_ratio(sin, 0.5, NaN))
expect_error(golden_ratio(sin, 0.5, Inf))
suppressWarnings(expect_equivalent(golden_ratio(cos, 2.5, 3.5), list(pi, -1, 100, 1, NULL)))
expect_warning(golden_ratio(function(x) (x-1)^2, 0.5, 1.5), 'golden_ratio did not converge within given parameters! You may want to use bigger eps or maxiter parameter.')
expect_equivalent(golden_ratio(function(x) (x-1)^2, 0.5, 1.5, eps=1e-10), list(1, 0, 48, 0, NULL))
expect_equivalent(golden_ratio(function(x) (x-1)^2, 0.5, 1.5, eps=1e-10, maxiter=50), list(1, 0, 48, 0, NULL))

# examples:
golden_ratio(cos, 2.5, 3.5, eps=1e-12) 
suppressWarnings(golden_ratio(function(x) (x-1)^2, 0.5, 1.5)) # gives warning 
golden_ratio(function(x) (x-1)^2, 0.5, 1.5, eps=1e-10)
golden_ratio(function(x) (x-1)^2, 0.5, 1.5, eps= 1e-10, maxiter=50)


## ------------------------ Exercise 02.03 ----------------------------

## ---- Documentation ----
# DESCRIPTION
# This function generates a sample of elements of a given vector accordingly to given probabilities.
#
# ARGUMENTS
# n - length of the sample, single natural number,
# x - numeric vector with unique elements,
# p - numeric vector that represent probabilities, contains non-negative numbers that should (but don't have to) sum up to 1
#
# RETURN VALUE
#  vector of length n (with elements ampled from x)

## ---- Function ----

gendiscrete <- function (n, x, p){
  stopifnot(is.finite(n), is.finite(x), is.finite(p))
  stopifnot(n>0, n == as.integer(n) )
  # check if x elements are unique
  stopifnot(anyDuplicated(x)==0)
  stopifnot(length(x)==length(p))
  stopifnot(p>=0, sum(p)>0)
  
  if(sum(p)!=1) {
    warning('gendiscrete: p does not sum to 1, p will be normalized.')
    p<-p/sum(p)
  }
  
  # find cumulated probabilities
  p<- cumsum(p)
  # generate n random numbers from uniform distribution
  u<-runif(n) 
  out <- vector("numeric", n)
  for (i in seq_along(u)){
    # which(u[i]>c(0,p[-length(p)]) & u[i]<=p )  - return number of the interval of probabilities that u[i] falls into
    out[i] <- x[ which(u[i]>c(0,p[-length(p)]) & u[i]<=p ) ]
  }
  out
}

## ---- Examples ----

# tests:
library(testthat)
expect_error(gendiscrete(0,c(1:3),c(1,3,6)) )
expect_error(gendiscrete(0,c(1:3),c(-1,3,6)) )
expect_error(gendiscrete(0,c(1:3),c(0,0,0)) )
expect_error(gendiscrete(0,c(1,1:3),c(0,0,0)) )
expect_warning(gendiscrete(3,c(1:3),c(1,3,6) ) , 'gendiscrete: p does not sum to 1, p will be normalized.' )

# examples:
suppressWarnings(gendiscrete(3,c(1:3),c(1,3,6) ) ) # warning
gendiscrete(3,c(1:3),c(0.1,0.3,0.6) )
summary(factor(gendiscrete(100,c(1:3),c(0.1,0.3,0.6) )))/100


## ------------------------ Exercise 02.04 ----------------------------

## ---- Documentation ----
# DESCRIPTION
# This function generates a three column data frame from a given matrix.
# First column contains elements of the matrix, second and third column contain names of column and row
# of element from the first column
#
# ARGUMENTS
# mat - input matrix that has dimnames attribute set
# n - names of columns of output data frame
#
# RETURN VALUE
#  data frame with unwinded matrix
# it contains m*n rows and three columns, where (m,n) are dimensions of the matrix



## ---- Function ----

unwind <- function (mat, n){
  stopifnot(is.matrix(mat), !is.null(dimnames(mat)))
  stopifnot(is.character(n), length(n)==3)
  

  structure ( data.frame(
    list(as.vector(mat),
    as.vector( matrix(dimnames(mat)[[2]], nrow=dim(mat)[1], ncol=dim(mat)[2], byrow=TRUE ) ),
    as.vector( matrix(dimnames(mat)[[1]], nrow=dim(mat)[1], ncol=dim(mat)[2]) ) )
    ),
    names=n )
}

## ---- Examples ----

# tests:
library(testthat)
expect_error(unwind(x, 1) )
expect_error(unwind(x, c("a")) )
expect_error(unwind(c(1,2), c("a","b", "c")) )
expect_equivalent(unwind(WorldPhones, c("count", "where", "when"))[2,1], 60423 )
expect_equivalent(unwind(WorldPhones, c("count", "where", "when"))[2,2], factor("N.Amer") )
expect_equivalent(unwind(WorldPhones, c("count", "where", "when"))[2,3], factor(1956) )
expect_equivalent(unwind(WorldPhones, c("count", "where", "when"))[9,2], factor("Europe") )
expect_equivalent(unwind(WorldPhones, c("count", "where", "when"))[9,3], factor(1956) )

# examples:
WorldPhones
head(unwind(WorldPhones, c("count", "where", "when")))

## ------------------------ Exercise 02.05 ----------------------------

## ---- Documentation ----
# DESCRIPTION
# We say that a directed graph is immoral, 
# if there exist 2 vertices not joined (directly) 
# with any edge but having a common child.
# The function determines if a given graph is immoral.
#
# ARGUMENTS
# g - graph represented by square 0-1 matrix
#
# RETURN VALUE
#  TRUE - if given graph is immoral, FALSE otherwise.


## ---- Function ----

inquisition <- function (g){
  stopifnot(is.matrix(g), dim(g)[1]==dim(g)[2],dim(g)[1]>0 )
  stopifnot(sum(g*(1-g))==0)
  
  n<- dim(g)[1]
  if (n<3) return(FALSE)
  
  # for each two vertices
  for(a in 1:(n-1) ){
    for(b in (a+1):n){
      if (g[a,b]==0 && g[b,a]== 0){ # if a and b are not related check if they have common child
        for(c in 1:n){
          if (c==a || c==b) next
          # if both a and b point to the same vertex then they are immoral!
          if(g[a,c]==1 && g[b,c]==1) return(TRUE)
        }
      }
    }
  }
  
  FALSE
  
}

## ---- Examples ----

# tests:
library(testthat)
expect_error(inquisition(sin))
expect_error(inquisition(1:9))
expect_error(inquisition(matrix (1:9, nrow=3)))
expect_equivalent(inquisition(matrix (c(1,0,1,0), nrow=2)), FALSE)
expect_equivalent(inquisition(matrix(c(0,0,1,0,0,1,0,0,0), nrow=3, byrow=TRUE)), TRUE)

# examples:
(graph <- matrix(c(0,0,1,0,0,1,0,0,0), nrow=3, byrow=TRUE))
inquisition(graph)

## ------------------------ Exercise 02.06 ----------------------------

# not enough time :/



