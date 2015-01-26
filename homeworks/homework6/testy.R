
library(Rcpp)
sourceCpp("homeworks/homework6/wzor.cpp")

library(microbenchmark)
library(testthat)

expect_equal(mode(numeric(0)),NA_integer_)
expect_equal(mode(c(1)),1)
expect_equal(mode(c(1,2,3,3)),3)
expect_equal(mode(c(1,2,2,3,3,2)),2)


mode2 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
microbenchmark(mode(c(1:1000000,5)), mode2(c(1:1000000,5)))


expect_equal(simplify2array(c(1,3,5)),simplify2array2(c(1,3,5)))
expect_equal(simplify2array(list(1:3)),simplify2array2(list(1:3)))
expect_equal(simplify2array(list(1:3,3:5)),simplify2array2(list(1:3,3:5)))
expect_equal(simplify2array(list(1:3,3)),simplify2array2(list(1:3,3)))
expect_equal(simplify2array(list(1:3,"a")),simplify2array2(list(1:3,"a")))
expect_equal(simplify2array2(c()),numeric(0))
expect_equal(simplify2array2(list()),numeric(0))


library(gtools)
permutations(3, 3, 1:3)
permutations(6, 6, 1:6)
microbenchmark(perms(10), permutations(10,10,1:10))
