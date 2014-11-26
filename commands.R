
####  Pomoc i manual
?rep
?"rep" # for special characters, e.g. ?"=="
help("rep")

??replicate
help.search("replicate")
## Pakiety

install.packages("sos") # install a package (once)
library("sos") # zaladuj pakiet
findFunction("replicate")

####  DATA TYPES
x <- NULL
typeof(x) # (R internal) type of x
mode(x)
# mode
class(x) # class (may freely be changed by the user)

typeof(1)
typeof(1L)
mode(1)
mode(1L)

typeof(NA)
## [1] "logical"
typeof(NA_integer_)
## [1] "integer"
typeof(NA_real_)
## [1] "double"
typeof(NA_complex_)
## [1] "complex"
typeof(NA_character_)

c(sqrt(-1), 0^0)
## Warning: NaNs produced
## [1] NaN 1

c(1/0, Inf - Inf, log(0))
## [1] Inf NaN -Inf

typeof(NULL)
## [1] "NULL"
is.null(NULL)
## [1] TRUE
is.vector(NULL)
## [1] FALSE
is.atomic(NULL)
## [1] TRUE

length(NULL) # coerced to an empty vector
## [1] 0
identical(NULL, c()) # no type information here
## [1] TRUE
identical(NULL, logical())
## [1] FALSE

#return length of a string (number of code points)
nchar(c("ala", "ma", "kota"))
length(c("ala", "ma", "kota"))
length("ala")

#FLOATING POINTS ARYTHMETICS:
1e+34 + 1e-34 - 1e+34 - 1e-34 == 0 # big+small-big-small
## [1] FALSE
0.1 + 0.1 + 0.1 == 0.3
## [1] FALSE
print(0.1 + 0.1 + 0.1, digits = 22)
## [1] 0.3000000000000000444089
print(0.3, digits = 22)
## [1] 0.2999999999999999888978


#Constructing vectors
logical(3) # also: vector('logical', 3)
## [1] FALSE FALSE FALSE
integer(3) # also: vector('integer', 3) and so on.
## [1] 0 0 0
double(3)
## [1] 0 0 0
character(3)
## [1] "" "" ""

rep(c(TRUE, FALSE), 4)
rep(c(TRUE, FALSE),length.out = 5)
## [1] TRUE FALSE TRUE FALSE TRUE
rep(c(TRUE, FALSE),each = 3)
## [1] TRUE TRUE TRUE FALSE FALSE FALSE
rep(c(TRUE, FALSE),times = 3) # compare results
## [1] TRUE FALSE TRUE FALSE TRUE FALSE
rep(c(TRUE, FALSE), times = 3, each = 2)
## [1] TRUE TRUE FALSE FALSE TRUE TRUE FALSE FALSE
## [12] FALSE
rep(c(TRUE, FALSE), length.out = 8, each = 2)
##[1]  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE

seq(1,10, 2) # step=2, cf. ?seq
## [1] 1 3 5 7 9
seq(0,1, length.out = 6)
## [1] 0.0 0.2 0.4 0.6 0.8 1.0

####  TYPE CONVERSIONS
##type hierarchy:
typeof(c(TRUE, 1L, 1, 1 + (0+1i), "one"))
## [1] "character"
typeof(c(TRUE, 1L, 1, 1 + (0+1i)))
## [1] "complex"
typeof(c(TRUE, 1L, 1))
## [1] "double"
typeof(c(TRUE, 1L))
## [1] "integer"

as.numeric(c(TRUE, FALSE))
## [1] 1 0
as.logical(c(-2, -1, 0, 1, 2))
## [1] TRUE TRUE FALSE TRUE TRUE

as.double(as.character(0.1 + 0.1 + 0.1)) == 0.1 + 0.1 + 0.1
## [1] FALSE

as.integer(c(1.5, -1.5)) # truncates fractional part
## [1] 1 -1
as.double("3.1415") # parse string
## [1] 3.1415
as.character(c(TRUE, FALSE))
## [1] "TRUE" "FALSE"
as.logical(c("true", "false", "TRUE", "FALSE", "T", "F"))
## [1] TRUE FALSE TRUE FALSE TRUE FALSE

is.logical(TRUE)
## [1] TRUE
is.numeric(1L)
## [1] TRUE

is.nan(NaN)
is.na(NA)
is.finite(c(1, NA, NaN, Inf, -Inf))
## [1] TRUE FALSE FALSE FALSE FALSE

is.atomic(1:5) # atomic type
## [1] TRUE
is.vector(TRUE) # some vector (there are also ``generic'' ones)
## [1] TRUE
is.function(c(1, 5)) # definitely not a function
## [1] FALSE

####  ASSIGNMENT
#name <- value
#value -> name
#name = value # may be ambiguous in some contexts
#assign("name", value)
5 -> x
"<-"(x,5)

x <- 1:5 # suppress printing
(x <- 1:5) # force printing
## [1] 1 2 3 4 5

####  VECTOR OPERATIONS
# x^y  - exp
# x ** y - exp
# x %% y - division reminder
7%%3
# x %/% y - integer division
7 %/% 3

1 + c(1, 2, 3)
## [1] 2 3 4
c(1, -1) * 1:6
## [1] 1 -2 3 -4 5 -6
c(1, -1) * 1:7 # result OK, but a warning given
## Warning: longer object length is not a multiple of shorter object length
## [1] 1 -2 3 -4 5 -6 7

typeof(c(1L, 2L) + c(1, 2)) # integer + double
## [1] "double"
c(TRUE, FALSE) * 1:6 # TRUE -> 1, FALSE -> 0
## [1] 1 0 3 0 5 0
c("1", "2") + c("2", "3") # use as.numeric() here explicitly
## Error: non-numeric argument to binary operator

pmin(c(1, 2,3, 4), c(4, 1, 2, 3))
## [1] 1 1 2 3
pmax(c(1, 2,3, 4), c(4, 1, 2, 3))
## [1] 4 2 3 4

c(1, Inf)/c(0, Inf)

c(TRUE, TRUE, FALSE, FALSE) & c(TRUE, FALSE)
## [1] TRUE FALSE FALSE FALSE
c(TRUE, FALSE) | NA # Lukasiewicz's logic
## [1] TRUE NA
!c(0, 1, 2, 0) # coercion to logical
## [1] TRUE FALSE FALSE TRUE

v <- c(TRUE, FALSE, NA)
structure(outer(v, v, "&"), dimnames=rep(list(paste(v)), 2))


(x <- 10:20) # exemplary vector
## [1] 10 11 12 13 14 15 16 17 18 19 20
x[1] # first
## [1] 10
x[length(x)] # last
## [1] 20
x[c(1, length(x), 1)] # first, last, and first again
## [1] 10 20 10
x[1000] # no such element
## [1] NA
x[c(1.9, 2.1, 2.7)] # fractional part truncated
## [1] 10 11 11
x[-1]
# [1] 11 12 13 14 15 16 17 18 19 20
x[c(-(1:5), -1, -5)]
## [1] 15 16 17 18 19 20
x[c(1, -1)] # don't mix positive and negative indices
## Error: only 0’s may be mixed with negative subscripts
x[0] # empty vector
## integer(0)
x[c(-1, 0)] # 0 ignored
## [1] 11 12 13 14 15 16 17 18 19 20

(x <- 10:20) # exemplary vector
## [1] 10 11 12 13 14 15 16 17 18 19 20
x[c(TRUE, rep(FALSE, 9))]
## [1] 10 20
x[c(TRUE, FALSE)] # recycling rule
## [1] 10 12 14 16 18 20

(x <- sample(100:999, 10)) # random sample
## [1] 358 808 467 892 942 140 572 896 591 506
x[x > 500] # select only elements > 500
## [1] 808 892 942 572 896 591 506
x[x >= 250 & x <= 750]
## [1] 358 467 572 591 506

(x <- 1:6)
## [1] 1 2 3 4 5 6
x[1] <- 10; x
## [1] 10 2 3 4 5 6
x[x>3] <- (x[x>3])^2; x
## [1] 100 2 3 16 25 36
x[-c(1,length(x))] <- c(1, -1); x
## [1] 100 1 -1 1 -1 36

(x <- 1:6)
## [1] 1 2 3 4 5 6
x[length(x)+1]<- 7; x
## [1] 1 2 3 4 5 6 7
x[10] <- 10; x
## [1] 1 2 3 4 5 6 7 NA NA 10
## Above operation is done in linear time! (whole vector needs to be copied to different place)

####  Built-in Functions

## Vector functions
#abs(), sign(), floor(), ceiling(), round(), sqrt(), exp(), log(), sin(), cos(), etc.

## Aggregation
# sum(), prod(), mean(), median(), min(), max(), var(), sd(), quantile(), any(), all(), etc.
x <- c(1, 5, 4, NA, 3)
sum(x)
## [1] NA
sum(x[!is.na(x)]) # cf. sum(x, na.rm=TRUE)
## [1] 13
sum(is.na(x)) # how many missing values?
## [1] 1
any(is.na(x))
## [1] TRUE
all(x == 1:5)
## [1] FALSE

x <- c(4, 2, 3, 5, 1)
cumsum(x)
## [1] 4 6 9 14 15
cumprod(x)
## [1] 4 8 24 120 120
cummin(x)
## [1] 4 2 2 2 1
cummax(x)
## [1] 4 4 4 5 5
diff(x) # lag=1 by default, see ?diff
## [1] -2 1 2 -4

## Others
x <- c(40, 20, 30, 50, 20)
which(x > 3)
## [1] 1 2 3 4 5
which.min(x) # fater than which(x==min(x))[1]
## [1] 2
which.max(x)

x <- c(40, 20, 30, 50, 20)
sort(x) # stable sorting algorithm
## [1] 20 20 30 40 50
sort(x, decreasing = TRUE)
## [1] 50 40 30 20 20
# nonincreasing
order(x) # ordering permutation
## [1] 2 5 3 1 4
x[order(x)] # same as sort(x)
## [1] 20 20 30 40 50
sample(x) # random permutation
## [1] 20 20 30 50 40

#others - rank(), is.unsorted(), rev()

basket1 <- c("apples", "bananas", "apples")
basket2 <- c("bananas", "oranges", "cherries")
union(basket1, basket2)
## [1] "apples" "bananas" "oranges" "cherries"
intersect(basket1, basket2)
## [1] "bananas"
setdiff(basket1, basket2)
## [1] "apples"
is.element("pears", basket1)
## [1] FALSE
setequal(basket1, c("bananas", "apples"))
## [1] TRUE

#others - unique(), duplicated(), anyDuplicated(), tabulate()

paste(c("ala", "ma", "kota"), collapse=" ")
## "ala ma kota"
paste(c("a", "b"), 1:2)
## [1] "a 1" "b 2"
paste(c("a", "b"), 1, sep = "") # recycling rule, no separator
## [1] "a1" "b1"
paste(c("a", "b"), 1:2, sep = ":", collapse = ", ")
## [1] "a:1, b:2"
paste(c("a", "b"), c(1, NA))
## [1] "a 1" "b NA"

x <- c(1, 10, 100)
cat(x)
## 1 10 100
cat(x, sep = "\n")
## 1
## 10
## 100
cat(format(x), sep = "\n")
##   1
##  10
## 100

####  Lists

(L <- list(1:2, 11:13, 21:24))
## [[1]]
## [1] 1 2
##
## [[2]]
## [1] 11 12 13
##
## [[3]]
## [1] 21 22 23 24

str(L)
## List of 3
## $ : int [1:2] 1 2
## $ : int [1:3] 11 12 13
## $ : int [1:4] 21 22 23 24

typeof(L)
## [1] "list"
is.list(L)
## [1] TRUE
is.vector(L)
## [1] TRUE
length(L) # it's a vector
## [1] 3
is.atomic(L) # but not an atomic one
## [1] FALSE
is.recursive(L) # recursive type
## [1] TRUE

c(TRUE, 1, "one") # coercion
## [1] "TRUE" "1" "one"
list(TRUE, 1, "one") # 3 atomic objects
## [[1]]
## [1] TRUE
##
## [[2]]
## [1] 1
##
## [[3]]

list(1, list(2, 3))
## [[1]]
## [1] 1
##
## [[2]]
## [[2]][[1]]
## [1] 2
##
## [[2]][[2]]
## [1] 3

str(list(1, list(2, 3)))
## List of 2
## $ : num 1
## $ :List of 2
## ..$ : num 2
## ..$ : num 3

# Empty list:
vector("list", 3)
## [[1]]
## NULL
##
## [[2]]
## NULL
##
## [[3]]
## NULL

as.list(1:2)
## [[1]]
## [1] 1
##
## [[2]]
## [1] 2

## Subsets of lists

c(1, 2, 3)[2] # numeric vector of length 1
## [1] 2
list(1, 2, 3)[2] # list of length 1
## [[1]]
## [1] 2
list(1, 2, 3)[-c(1, 3)]
## [[1]]
## [1] 2
list(1, 2, 3)[c(TRUE, FALSE, FALSE)]
## [[1]]
## [1] 1

list(1, 2, 3)[[2]] # numeric vector
## [1] 2
c(1, 2, 3)[[2]] # also works on atomic vectors
## [1] 2
list(1, 2, 3)[[-1]] # only positive integers
## Error: attempt to select more than one element
list(1, 2, 3)[[c(TRUE, FALSE, FALSE)]] # only positive integers
## Error: recursive indexing failed at level 2

L <- list(1, list(2, 3))
L[[2]]
## [[1]]
## [1] 2
##
## [[2]]
## [1] 3
L[[c(2, 1)]] # the same as L[[2]][[1]]
## [1] 2
L[[c(2, 1, 2)]] # don't go too far
## Error: subscript out of bounds

L <- list(1, 2, 3)
L[[1]] <- 1:5
str(L)
## List of 3
## $ : int [1:5] 1 2 3 4 5
## $ : num 2
## $ : num 3

L[2:3] <- list(c(TRUE, FALSE), mean)
str(L)
## List of 3
## $ : int [1:5] 1 2 3 4 5
## $ : logi [1:2] TRUE FALSE
## $ :function (x, ...)

L <- list(1, 2, 3)
L[2:3] <- c(TRUE, FALSE)
str(L)
## List of 3
## $ : num 1
## $ : logi TRUE
## $ : logi FALSE

L[2:3] <- 10:15
## Warning: number of items to replace is not a multiple of replacement length
str(L)
## List of 3
## $ : num 1
## $ : int 10
## $ : int 11

L <- list(1, 2, 3)
L[[2]] <- NULL # different meaning
str(L)
## List of 2
## $ : num 1
## $ : num 3

L <- list(1, 2, 3)
L[2] <- list(NULL)
str(L)
## List of 3
## $ : num 1
## $ : NULL
## $ : num 3

## List operations

L1 <- list("a", 1)
L2 <- list(TRUE)
str(list(L1, L2)) # 2 sublists
## List of 2
## $ :List of 2
## ..$ : chr "a"
## ..$ : num 1
## $ :List of 1
## ..$ : logi TRUE

str(c(L1, L2)) # merge
## List of 3
## $ : chr "a"
## $ : num 1
## $ : logi TRUE

str(c(L1, L2, recursive = TRUE))
## chr [1:3] "a" "1" "TRUE"

unlist(list(1, list("two", list(FALSE))))
## [1] "1" "two" "FALSE"

rep(list(1:10, TRUE), 2)
## [[1]]
## [1] 1 2 3 4 5 6 7 8 9 10
##
## [[2]]
## [1] TRUE
##
## [[3]]
## [1] 1 2 3 4 5 6 7 8 9 10
##
## [[4]]
## [1] TRUE

## calculations on elements of a list:
lapply(list(c(1, 3, 2), c(3, 6, 2)), max)
## [[1]]
## [1] 3
##
## [[2]]
## [1] 6

lapply(list(1:5, 3), "-")
## [[1]]
## [1] -1 -2 -3 -4 -5
##
## [[2]]
## [1] -3

lapply(list(3.1415, c(1.23, 9.99)), round, digits = 1)
## [[1]]
## [1] 3.1
##
## [[2]]
## [1] 1.2 10.0

lapply(1:3, c, 10, 11, 12)
## [[1]]
## [1] 1 10 11 12
##
## [[2]]
## [1] 2 10 11 12
##
## [[3]]
## [1] 3 10 11 12

mapply("+", 1:3, 11:13, SIMPLIFY = FALSE)
## [[1]]
## [1] 12
##
## [[2]]
## [1] 14
##
## [[3]]
## [1] 16

mapply("*", list(1:3, 4:6), list(10, -1), SIMPLIFY = FALSE)
## [[1]]
## [1] 10 20 30
##
## [[2]]
## [1] -4 -5 -6

mapply(c,1:3, 11:13, 21:23, 31:33, SIMPLIFY = FALSE)
## [[1]]
## [1] 1 11 21 31
##
## [[2]]
## [1] 2 12 22 32
##
## [[3]]
## [1] 3 12 23 33

####  FUNCTIONS

## scope of variables:
# functions pass arguments by value
# so:
# - they don't change the value of given argument (they can change value of its copy)
# - their variables are local!
# Also:
# - you can give default values to variables,
# - arguments are only evaluated when needed! (LAZY evaluation)
#

f <- function(n) suppressWarnings(4*sum(c(1, -1)/(2*(0:n)+1)))

## Function without a name
(function(x) x^2)(1:5)

square <- function(x) x^2
square(1:5)
## [1] 1 4 9 16 25
is.function(square)
## [1] TRUE
is.atomic(square) # not an atomic type
## [1] FALSE
is.vector(square)
## [1] FALSE
is.recursive(square) # a recursive type
## [1] TRUE
typeof(square) # ``closure'' -> more on that later
## [1] "closure"
mode(square)
## [1] "function"

f <- function(...) {
   print(..1)
   print(..2)
}
f(1, 2, 3)
## [1] 1
## [1] 2
f(1) # sorry
## [1] 1
## Error: the ... list does not contain 2 elements

printvec <- function(x) {
   cat(x, sep = ", ")
   cat("\n")
   invisible(NULL) # return 'nothing', invisibly
}

remove_outliers3 <- function(x) {
   stopifnot(is.numeric(x))
   stopifnot(length(x) > 0, is.finite(x))
   Q13 <- quantile(x, c(0.25, 0.75))
   IQR <- diff(Q13)
   x[x >= Q13[1]-1.5*IQR & x <= Q13[2]+1.5*IQR]
}

## You can load packages inside functions:
random_string <- function() {
   library("stringi") # not installed -> error
   stri_rand_strings(1, 8, "[A-Za-z0-9#_!?$@%]")
}

#### PACKAGES

install.packages("packagename")

## to use the library:
library("packagename")

help(package = "pkgname")
example("function", package = "pkgname") # e.g. pie, graphics
demo(package = "pkgname") # e.g. graphics
vignette(package = "pkgname") # e.g. Rcpp

##Benchmarking:
library("microbenchmark") # attach the package
v <- rcauchy(10) # some random data
microbenchmark(sol1=remove_outliers1(v),
      sol2=remove_outliers2(v),
      sol3=remove_outliers3(v)
   )

# You can use a function without loading library like this:
microbenchmark::microbenchmark()

####  TESTING

#We should take care
# of:
# - vectorization,
# - recycling rule,
# - NA, Inf, NaN handling,
# - 0s, negative values, empty vectors,
# - FP arithmetic accuracy issues,
# - preservation of input object’s attributes.

fib <- function(x) {
stopifnot(is.numeric(x), is.finite(x))
# ...
}
fib(mean) # a proper behavior
## Error: default method not implemented for type ’closure’
fib("4") # OK
## Error: is.numeric(x) is not TRUE

## Testthat package:
# expect_true(x), expect_false(x), expect_is(x,class),
# expect_equal(x, expected), expect_equivalent(x, expected), expect_identical(x, expected),
# expect_output(x, regexp), expect_message(x), expect_warning(x), expect_error(x).

message("hahahaha")
## hahahaha
# can be suppressed by suppressMessages

warning("This is a warning.")
## Warning: This is a warning.
sqrt(-1)
## Warning: NaNs produced
## [1] NaN
1:2 + 1:3
## Warning: longer object length is not a multiple of shorter object length
## [1] 2 4 4

# warnings can be suppressed by suppressWarnings()

options(warn = 2)
# turns all warning into errors!

options(check.bounds = TRUE)
x <- 1:5
x[10] <- 10
## Warning: assignment outside vector/list limits (extending from 5 to 10)

stop("This is an error.")
## Error: This is an error.

## functions inside on.exit will be executed at the end regardless of errors
test <- function() {
   on.exit(print("C"))
   on.exit(print("D"), add = TRUE)
   print("A")
   stop("an error occurred")
   print("B")
}

test <- function(x) {
   tryCatch({
      sum(as.numeric(x))
   }, error = function(e) {
      NA
   })
}

test <- function(err) {
 tryCatch({
      if (err) stop("error")
      cat("good morning;")
   },
   error=function(e) {
      cat("an error occurred;")
   },
   finally={
      cat("this is the end;")
   })
   cat("goodbye\n")
}
test(FALSE)
## good morning;this is the end;goodbye
test(TRUE)
## an error occurred;this is the end;goodbye


####  Attributes

# we can 'invent' our own attributes
x <- (-5):5
attr(x, "color") <- "green"
attr(x, "which_positive") <- which(x > 0)
attr(x, "favorite_fun") <- exp
# above is equivalent to:
x <- structure((-5):5, color="green", which_positive=which(x > 0), favorite_fun=exp)

attr(x, "which_positive")
## [1] 7 8 9 10 11
attr(x, "favorite") # autocompletion
## function (x) .Primitive("exp")
attr(x, "no_such_attribute")
## NULL

x
## [1] -5 -4 -3 -2 -1 0 1 2 3 4 5
## attr(,"color")
## [1] "green"
## attr(,"which_positive")
## [1] 7 8 9 10 11
## attr(,"favorite_fun")
## function (x) .Primitive("exp")
str(x)
## atomic [1:11] -5 -4 -3 -2 -1 0 1 2 3 4 ...
## - attr(*, "color")= chr "green"
## - attr(*, "which_positive")= int [1:5] 7 8

# x is still an ordinary numeric vector
mode(x)
## [1] "numeric"
x[1]
## [1] -5
mean(x)
## [1] 0
x[attr(x, "which_positive")]
## [1] 1 2 3 4 5

# Deleting an attribute:
attr(x, "favorite_fun") <- NULL
x
## [1] -5 -4 -3 -2 -1 0 1 2
## attr(,"color")
## [1] "green"
## attr(,"which_positive")
## [1] 7 8 9 10 11

attributes(x) # returns a (named-see below) list
## $color
## [1] "green"
##
## $which_positive
## [1] 7 8 9 10 11

# some functions set attributes to provide information
x <- c(1, 2, NA, 4, NA)
na.omit(x)
## [1] 1 2 4
## attr(,"na.action")
## [1] 3 5
## attr(,"class")
## [1] "omit"

# Attributes with special meaning:
# comment, class, name

x <- 1:5
comment(x) <- "What a nice object!"
x # comment printing is suppressed
## [1] 1 2 3 4 5
attr(x, "comment")
## [1] "What a nice object!"
comment(x)
## [1] "What a nice object!"
comment(x) <- 10
## Error: attempt to set invalid ’comment’ attribute

x <- seq(0, 1, length.out = 5)
names(x) <- c("1st", "2nd", "3rd", "4th", "5th")
x
## 1st 2nd 3rd 4th 5th
## 0.00 0.25 0.50 0.75 1.00

structure(list(1:10, mean), names = c("vector", "function"))
## $vector
## [1] 1 2 3 4 5 6 7 8 9 10
##
## $`function`
## function (x, ...)
## UseMethod("mean")
## <bytecode: 0x3007df8>
## <environment: namespace:base>

structure(1:4, names = c("a", "b", "c", "d", "e"))
## Error: ’names’ attribute [5] must be the same length as the vector [4]
(x <- structure(1:4, names = c("a", "b", "c")))
## a b c <NA>
## 1 2 3 4
names(x)
## [1] "a" "b" "c" NA
x <- structure(1:4, names = c("a", "b", "c", "d"))
unname(x) # x <- unname(x) is equivalent to attr(x,'names')<-NULL
## [1] 1 2 3 4
names(x)[2] <- "zzz"
x
## a zzz c d
## 1  2  3 4

list(first=1:10, second=100:110)
## $first
## [1] 1 2 3 4 5 6 7 8 9 10
##
## $second
## [1] 100 101 102 103 104 105 106 107 108 109 110
c("1st"=1, "2nd"=2) # 1st = invalid syntactic name = use quotes
## 1st 2nd
## 1   2

x <- structure(1:4, names = c("a", "b", "c", "d"))
x["a"]
## a
## 1
x[c("a", "d", "a")]
## a d a
## 1 4 1

y <- list(a = 1, b = 2)
y["a"] # subset
## $a
## [1] 1
y[["a"]] # extract
## [1] 1

x <- c(one = 1, two = 2, one = 3)
x["one"] # first occurrence returned
## one
## 1
x[names(x) == "one"]
## one one
## 1 3

x <- structure(as.list(1:1000000), names=as.character(1:1000000))
#library("microbenchmark")
microbenchmark(x[[10000]], x[[100000]], x[[1000000]], unit="ms") # index - O(1)
microbenchmark(x[["10000"]], x[["100000"]], x[["1000000"]], unit="ms") # name - O(n)

x <- list(one = 1, `2nd` = 2)
x$one
## [1] 1
x$three <- 3 # adjust length
str(x)
## List of 3
## $ one : num 1
## $ 2nd : num 2
## $ three: num 3
x$"2nd" # 2nd - not a syntactic name - use quotes
## [1] 2

## Class attribute
x <- c("a", "b", "c")
class(x)
## [1] "character"
attr(x, "class")
## NULL

print
## function (x, ...)
## UseMethod("print")
## <bytecode: 0x2758508>
## <environment: namespace:base>
mean
## function (x, ...)
## UseMethod("mean")
## <bytecode: 0x3402980>
## <environment: namespace:base>

# Each function that makes a call to UseMethod() is called a generic function. Each generic function
# dispatches the control flow to another function, called method.
# Let f() be a generic function. Assume that we are calling it on an object of class classname.
# 1. If there exists a function named f.classname(), this is the routine to be evaluated on a given object.
# 2. Otherwise, f.default() will be called.

print.Pretty <- function(x, ...) {
   cat("PRETTY", paste(x, collapse = ", "), ":-) \n")
}
x <- 1:5
x
## [1] 1 2 3 4 5
class(x) <- "Pretty"
x
## PRETTY 1, 2, 3, 4, 5 :-)
print.default(x)
## [1] 1 2 3 4 5
## attr(,"class")
## [1] "Pretty"

test <- shapiro.test(rnorm(100))
test
##
## Shapiro-Wilk normality test
##
## data: rnorm(100)
## W = 0.9897, p-value = 0.6375
class(test)
## [1] "htest"
typeof(test)
## [1] "list"
str(unclass(test))
## List of 4
## $ statistic: Named num 0.99
## ..- attr(*, "names")= chr "W"
## $ p.value : num 0.637
## $ method : chr "Shapiro-Wilk normality test"
## $ data.name: chr "rnorm(100)"
test$p.value
## [1] 0.6374777
test[["method"]]
## [1] "Shapiro-Wilk normality test"

print.htest # inaccessible directly
## Error: object ’print.htest’ not found
getS3method("print", "htest") # here it is


####  Coumpound types - Factors, Matrices, Data Frames

## Factors:

factor(c("male", "female", "female", "male", "female"))
# [1] male   female female male   female
# Levels: female male

str(factor(c(1, 3, 1, 2, 5, 2)))
# Factor w/ 4 levels "1","2","3","5": 1 3 1 2 4 2

f <- factor(c(1, 3, 1, 2, 5, 2))
class(f)
## [1] "factor"
typeof(f)
## [1] "integer"
unclass(f)
## [1] 1 3 1 2 4 2
## attr(,"levels")
## [1] "1" "2" "3" "5"

attr(f, "levels")[as.integer(f)]
## [1] "1" "3" "1" "2" "5" "2"
as.character(f)
## [1] "1" "3" "1" "2" "5" "2"
as.integer(as.character(f)) # not the same as as.integer(f)
## [1] 1 3 1 2 5 2


test <- c(1L, 4L, 3L, 2L, 1L, 3L)
# levels(·) is the same as attr(·, "levels").
levels(test) <- c("one", "two", "three", "four")
class(test) <- "factor"
test
# [1] one   four  three two   one   three
# Levels: one two three four

f <- factor(c(1, 3, 1, 2, 5, 2))
is.factor(f)
## [1] TRUE
is.vector(f)
## [1] FALSE
is.atomic(f)
## [1] TRUE
is.integer(f)
## [1] FALSE
is.character(f)
## [1] FALSE

# order on factors:
f <- factor(c("one", "three", "two", "one"), levels=c("one", "two", "three"), ordered=TRUE)
sort(f)
# [1] one   one   two   three
# Levels: one < two < three
which(f > "one")
## [1] 2 3
f[f > "two"] # not a lexicographic order here
## [1] three
## Levels: one < two < three
max(f)
## [1] three
## Levels: one < two < three

# Contingency tables for factors:
table(factor(c("one", "three", "two", "one")))
##
#   one three   two
#     2     1     1

table(factor(c("male", "female", "male", "male", "female")),
      factor(c("low", "low", "high", "high", "high")))

(f <- factor(c(1, 2, 3, 5, 1), levels = 1:5))
## [1] 1 2 3 5 1
## Levels: 1 2 3 4 5
nlevels(f) # length(levels(f))
## [1] 5
(f <- droplevels(f))
## [1] 1 2 3 5 1
## Levels: 1 2 3 5
nlevels(f)
## [1] 4

# renaming levels:
levels(f) <- c("one", "two", "three", "five")
f
# [1] one   two   three five  one
# Levels: one two three five

height <- c(164, 182, 173, 194, 159)
gender <- c("m", "f", "m", "m", "f")
split(height, gender)
## $f
## [1] 182 159
##
## $m
## [1] 164 173 194
lapply(split(height, gender), mean) # avg height in each group
## $f
## [1] 170.5
##
## $m
## [1] 177

height <- c(164, 182, 173, 194, 159)
gender <- c("m", "f", "m", "m", "f")
tapply(height, gender, mean)
#     f     m
# 170.5 177.0

(x <- round(rnorm(10), 1))
## [1] -0.6 -0.2 1.6 0.1 0.1 1.7 0.5 -1.3 -0.7 -0.4
cut(x, c(-Inf, -1, 0, 1, Inf))
#  [1] (-Inf,-1] (-1,0]    (-Inf,-1] (-1,0]    (0,1]     (-Inf,-1] (-Inf,-1] (-Inf,-1] (0,1]     (1, Inf]
# Levels: (-Inf,-1] (-1,0] (0,1] (1, Inf]
cut(x, c(-Inf, -1, 0, 1, Inf), labels=c("very_small", "small", "large", "very_large"))
# [1] very_small small      very_small small      large      very_small very_small very_small large      very_large
# Levels: very_small small large very_large

## Matrices:

x <- 1:6
dim(x) <- c(2, 3) # or attr(x, 'dim') <- c(2, 3)
x
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

class(x) # vector + dim attr => implicit matrix class
## [1] "matrix"
is.matrix(x)
## [1] TRUE
is.numeric(x)
## [1] TRUE

# vector elements are read column-wise
x <- 1:6
dim(x) <- c(2, 3) # or attr(x, 'dim') <- c(2, 3)
x
dim(x) <- c(3, 2)
x

as.numeric(x) # drops the dim attribute
## [1] 1 2 3 4 5 6

# Matrices by default behave exactly the same as vectors:
x <- matrix(1:6, nrow=2, ncol=3) # == structure(1:6, dim=c(2, 3))
x^2
x*c(-1,2)
x*x

matrix(letters[1:4], ncol=2) # nrow auto-guessed
##      [,1] [,2]
## [1,] "a" "c"
## [2,] "b" "d"

# matrices of other kinds of elements:
structure(list(mean, sd, var, median), dim = c(2, 2))
structure(list(1:5, c(1, 5.4)), dim = c(1, 2))

## arrays:
structure(1:12, dim = c(2, 3, 2))
# , , 1
#
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
#
# , , 2
#
#      [,1] [,2] [,3]
# [1,]    7    9   11
# [2,]    8   10   12
?array

(x <- matrix(1:6, nrow=2, dimnames=list(c("r1", "r2"), c("c1", "c2", "c3"))))
#    c1 c2 c3
# r1  1  3  5
# r2  2  4  6
dim(x)
## [1] 2 3
str(dimnames(x))
## List of 2
## $ : chr [1:2] "r1" "r2"
## $ : chr [1:3] "c1" "c2" "c3"

(x <- matrix(letters[1:6], nrow = 2))
##      [,1] [,2] [,3]
## [1,] "a" "c" "e"
## [2,] "b" "d" "f"
x[1, 2] # 1st row, 2nd column
## [1] "c"
x[1, ] # 1st row
## [1] "a" "c" "e"
x[1:2, 2:3] # 1st and 2nd row, 2nd and 3rd column [select block]
#      [,1] [,2]
# [1,] "c"  "e"
# [2,] "d"  "f"

(x <- matrix(letters[1:6], nrow=2,
dimnames=list(c("r1", "r2"), c("c1", "c2", "c3"))))
##    c1  c2  c3
## r1 "a" "c" "e"
## r2 "b" "d" "f"
x[c("r2", "r1"), c("c3", "c1")]
##    c3  c1
## r2 "f" "b"
## r1 "e" "a"

(x <- matrix(letters[1:6], nrow=2))
##     [,1] [,2] [,3]
## [1,] "a" "c" "e"
## [2,] "b" "d" "f"
(y <- matrix(c(1, 3, 2, 1, 1, 1), byrow=TRUE, ncol=2))
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    1
# [3,]    1    1
x[y]
## [1] "e" "b" "a"

(a <- matrix(1:4, nrow = 2))
(b <- matrix(c(1, -1, -1, 1), nrow = 2))
# Matrix multiplication:
a %*% b # compare with a*b

# Adding new columns/rows:
(x <- matrix(1:6, nrow=2))
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
cbind(x, 1:2)
#      [,1] [,2] [,3] [,4]
# [1,]    1    3    5    1
# [2,]    2    4    6    2
rbind(x, 1:3)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
# [3,]    1    2    3
rbind(1:5, 11:15)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]   11   12   13   14   15

simplify2array(list(1, 2, 3)) # result = atomic vector
## [1] 1 2 3
simplify2array(list(1:2, 3:4, 5:6)) # result = matrix
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
simplify2array(list(1, 2:3)) # cannot simplify
## [[1]]
## [1] 1
##
## [[2]]
## [1] 2 3

sapply(list(1:10, 11:20, 21:30), mean)
## [1] 5.5 15.5 25.5
sapply(list(1:10, 11:20, 21:30), range)
#      [,1] [,2] [,3]
# [1,]    1   11   21
# [2,]   10   20   30

(x <- matrix(1:6, nrow = 2))
apply(x, 1, sum) # each row
## [1] 9 12
apply(x, 2, mean) # each column
## [1] 1.5 3.5 5.5

?rowSums
?colSums
?rowMeans
?colMeans

outer(c(TRUE, FALSE, NA), c(TRUE, FALSE, NA), "|")
##      [,1] [,2] [,3]
## [1,] TRUE TRUE  TRUE
## [2,] TRUE FALSE NA
## [3,] TRUE NA    NA
outer(c("a", "b"), 1:3, paste, sep = "")
##      [,1] [,2] [,3]
## [1,] "a1" "a2" "a3"
## [2,] "b1" "b2" "b3"

## Other:
# ?t, ?diag, ?upper.tri, ?lower.tri, ?isSymmetric,
# ?maxCol, ?aperm, ?norm, ?dist, ?det, ?eigen, ?qr, ?svd, ?chol, ?kappa, ?solve, ?lsfit

## Data Frames
# data frames are special kind of lists
# - lists such that every list element has the same length

data.frame(gender=c("m", "f", "m", "f"),
           height=c(185, 158, 191, 174))
# gender height
# 1      m    185
# 2      f    158
# 3      m    191
# 4      f    174

df <- data.frame(gender=c("m", "f", "m", "f"),
                 height=c(185, 158, 191, 174))
class(df)
## [1] "data.frame"
typeof(df)
## [1] "list"
is.list(df)
## [1] TRUE
is.data.frame(df)
## [1] TRUE
str(unclass(df))
## List of 2
## $ gender: Factor w/ 2 levels "f","m": 2 1 2 1
## $ height: num [1:4] 185 158 191 174
## - attr(*, "row.names")= int [1:4] 1 2 3 4

# Manual creation of data frame:
df <- list(c("m", "f", "m", "f"),
           c(185, 158, 191, 174))
names(df) <- c("gender", "height")
attr(df, "row.names") <- c("1", "2", "3", "4")
class(df) <- "data.frame"
df

(x <- structure(list(
  list(matrix(1:4, ncol=2),
       matrix(11:14, ncol=2)
  )
),
class="data.frame", names="c1", row.names=c("r1", "r2") 
))

(survey <- data.frame(
  gender = c("m", "m", "f", "m", "f", "f"),
  coffee = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE),
  time = c(23, 25, 31, 46, 24, 38),
  weight = c(69, 71, 58, 98, 63, 41)
))
# gender coffee time weight
# 1      m  FALSE   23     69
# 2      m   TRUE   25     71
# 3      f   TRUE   31     58
# 4      m  FALSE   46     98
# 5      f   TRUE   24     63
# 6      f  FALSE   38     41

survey$weight
## [1] 69 71 58 98 63 41
survey[["weight"]]
## [1] 69 71 58 98 63 41
survey[[4]]
## [1] 69 71 58 98 63 41

survey[c(1, 3)]
# gender time
# 1      m   23
# 2      m   25
# 3      f   31
# 4      m   46
# 5      f   24
# 6      f   38

attr(survey, "dim")
## NULL
dim(survey)
## [1] 6 4

survey[1, ] # 1st row

survey[, 4] # == survey[[4]] != survey[4]

survey[1:2, 3:4]
# time weight
# 1   23     69
# 2   25     71

as.matrix(survey)
# automatic conversion to the broadest type:
# gender coffee  time weight
# [1,] "m"    "FALSE" "23" "69"  
# [2,] "m"    " TRUE" "25" "71"  
# [3,] "f"    " TRUE" "31" "58"  
# [4,] "m"    "FALSE" "46" "98"  
# [5,] "f"    " TRUE" "24" "63"  
# [6,] "f"    "FALSE" "38" "41"

# when data frame is created, character data is automatically converted to factor
class(survey$gender)
## [1] "factor"
# the stringsAsFactors argument of data.frame() and stringsAsFactors option can change that

row.names(survey) <- c("Frank", "Avishai", "Ella", "John", "Ella", "Elis")
## Warning: non-unique value when setting ’row.names’: ’Ella’
## Error: duplicate ’row.names’ are not allowed
row.names(survey) <- c("Frank", "Avishai", "Ella", "John", "Nina", "Elis")

survey["Ella", ]
# gender coffee time weight
# Ella      f   TRUE   31     58

test <- data.frame(good.name=1:2, "1bad.name"=3:4)
test # auto-fix
#     good.name X1bad.name
# 1         1          3
# 2         2          4

survey[survey$gender == "m" & survey$weight > 70, ]
# gender coffee time weight
# Avishai      m   TRUE   25     71
# John         m  FALSE   46     98
subset(survey, gender == "m" & weight > 70)
# gender coffee time weight
# Avishai      m   TRUE   25     71
# John         m  FALSE   46     98
subset(survey, gender == "f", select = -weight)
# gender coffee time
# Ella      f   TRUE   31
# Nina      f   TRUE   24
# Elis      f  FALSE   38

# see also: ?with, ?within, ?transform.

cbind(survey, height = c(184, 159, 173, 162, 195, 178))
# gender coffee time weight height
# Frank        m  FALSE   23     69    184
# Avishai      m   TRUE   25     71    159
# Ella         f   TRUE   31     58    173
# John         m  FALSE   46     98    162
# Nina         f   TRUE   24     63    195
# Elis         f  FALSE   38     41    178
rbind(survey, data.frame(gender = "m", coffee = TRUE, time = 41, weight = 98,
                         row.names = "Steinar"))

sapply(survey, class) # class of each column (it's a list...)
# gender    coffee      time    weight 
# "factor" "logical" "numeric" "numeric" 
tapply(survey$time, survey$gender, mean) # avg time / each gender

# See also: ?aggregate, ?by, ?ave

survey[order(survey$gender, survey$time), ]
# gender coffee time weight
# Nina         f   TRUE   24     63
# Ella         f   TRUE   31     58
# Elis         f  FALSE   38     41
# Frank        m  FALSE   23     69
# Avishai      m   TRUE   25     71
# John         m  FALSE   46     98

# Data frame processing packages:
# Here is a list of “hot” data frame processing-related packages:
# • data.table
# • reshape2
# • plyr2 and dplyr
# • magrittr

# Time Series:

?ts

t <- ts(1:10, frequency = 4, start=c(1959,2))
class(t)
typeof(t)
is.vector(t)
is.atomic(t)
unclass(t)

####  Controlling program flow:

# Conditional execution: if
# Repetitive execution: for, while, repeat


sgn <- function(x) {
  stopifnot(is.numeric(x), length(x) == 1, is.finite(x))
  if (x > 0)
    cat("positive\n")
  else if (x < 0)
    cat("negative\n")
  else
    cat("zero\n")
}

# !
if (NA) cat("!")
# Błąd wif (NA) cat("!") : 
#   brakuje wartości tam, gdzie wymagane jest TRUE/FALSE

# careful - you cannot put if and else in different lines in console
if (TRUE) print(TRUE)
else print(FALSE)
# BŁĄD: nieoczekiwane 'else' in "else"
# but you can do this in body of a function

# Conditions are lazy evaluated!
FALSE || {cat("!"); TRUE}
## !
## [1] TRUE
TRUE || {cat("!"); TRUE}
## [1] TRUE

# return - return given value and stop processing
qs <- function(x) {
  stopifnot(is.atomic(x), is.vector(x))
  if (length(x) <= 1) # already sorted
    return(x)
  pivot <- sample(x, 1) # random element of x
  c(qs(x[x<pivot]), x[x==pivot], qs(x[x>pivot]))
}

## ifelse - vectorized version
x <- c(5, 3, 1, 2, 4)
ifelse(x < 3, -x, x^2)
## [1] 25 9 -1 -2 16
ifelse(x > 3, NA, x)
## [1] NA 3 1 2 NA

x <- c(-1, 0, 1, 2)
ifelse(x >= 0, sqrt(x), NA) # sqrt is evaluated on -1 anyway
## Warning: NaNs produced
## [1] NA 0.000000 1.000000 1.414214

# while (logical_condition) expression
sum_while <- function(x) {
  x <- as.numeric(x) # coercion not possible => error
  result <- 0
  i <- 1
  n <- length(x)
  while (i <= n) { # logical_condition depends on i
    result <- result + x[i]
    i <- i + 1 # i changes here
  }
  result # return value
}
sum_while(1:5)
## [1] 15
sum_while(c(1, 2, NA, 4, 5))
## [1] NA

i <- 0
while (TRUE) { # infinite loop?
  j <- 0
  while (TRUE) {
    j <- j+1
    if (j > i) break
    print(c(i, j))
  }
  i <- i+1
  if (i > 3) break
}
## [1] 1 1
## [1] 2 1
## [1] 2 2
## [1] 3 1
## [1] 3 2
## [1] 3 3

i <- 0
while (i < 6) {
  i <- i+1
  if (i %% 2 == 0) next
  print(i)
}
## [1] 1
## [1] 3
## [1] 5

# repeat expression
# is equivalent to
# while (TRUE) expression

# for (name in vector) expression

sum_for2 <- function(x) {
  x <- as.numeric(x) # coercion not possible => error
  result <- 0
  for (i in seq_along(x))
    result <- result+x[i]
  result # return value
}
sum_for2(1:5)

seq_along(numeric(0))
# integer(0)
1:length(numeric(0))
# [1] 1 0

lapply_for <- function(x, f, ...) {
  stopifnot(is.vector(x))
  f <- match.fun(f) # see ?match.fun
  result <- vector("list", length(x)) # preallocate
  for (i in seq_along(x)) result[[i]] <- f(x[[i]], ...)
  result
}
lapply_for(list(1:5, 2:6), "*", 2)


####  String processing

# R character vectors consist of character strings. Each string is a sequence of bytes.

test <- "R programming 2014"
charToRaw(test) # printed in HEX
## [1] 52 20 70 72 6f 67 72 61 6d 6d 69 6e 67 20 32 30 31 34
as.integer(charToRaw(test)) # now in DEC
## [1] 82 32 112 114 111 103 114 97 109 109 105 110 103 32
## [18] 52

test <- "R programming 2014"
rawToChar(rev(charToRaw(test)[3:13]))

library("stringi") # we will be using this package extensively
as.integer(stri_conv("ąśćżźółńę", "", "latin2", to_raw=TRUE)[[1]])
## [1] 177 182 230 191 188 243 179 241 234
as.integer(stri_conv("ąśćżźółńę", "", "cp1250", to_raw=TRUE)[[1]])
## [1] 185 156 230 191 159 243 179 241 234

x <- as.raw(192:207)
stri_conv(x, "latin1", "")
## [1] "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ"
stri_conv(x, "latin2", "")
## [1] "ŔÁÂĂÄĹĆÇČÉĘËĚÍÎĎ"

#Try to detect encoding:
stri_enc_detect(test)

stri_length("aą") # how many code points?
## [1] 2
stri_numbytes("aą") # how many bytes are used?
## [1] 3
charToRaw("aą") # bytes
## [1] 61 c4 85
stri_enc_toutf32("aą")[[1]] # 1 code point -> 1 integer
## [1] 97 261

# string normalizationL
stri_enc_toutf32("\u0105")[[1]]
## [1] 261
stri_enc_toutf32("a\u0328")[[1]]
## [1] 97 808
stri_enc_toutf32(stri_trans_nfc("a\u0328"))[[1]]
## [1] 261

# Code points may also exhibit a set of Binary Properties. These include:
# • ALPHABETIC – alphabetic character;
# • ASCII_HEX_DIGIT – a character matching [0-9A-Fa-f];
# • LOWERCASE ;
# • UPPERCASE ;
# • WHITE_SPACE – a space character or TAB or CR or LF or ZWSP or ZWNBSP; this is not the same as
# General Category Z.
# Note that a code point has only one General Category but may exhibit many binary properties.
stri_extract_all_charclass(" \t\n\r", "\\p{Z}")
## [[1]]
## [1] " "
stri_extract_all_charclass(" \t\n\r", "\\p{C}")
## [[1]]
## [1] "\t\n\r"
stri_extract_all_charclass(" \t\n\r", "\\p{WHITE_SPACE}")
## [[1]]
## [1] " \t\n\r"

rawToChar(as.raw(c(65, 66, 67, 0, 68, 69)))
## Error: embedded nul in string: ’ABC\0DE’

Sys.getlocale("LC_CTYPE")
## [1] "pl_PL.UTF-8"

Encoding("ąść")  # unknown == native
## [1] "unknown"


## Basic operations:

stri_paste("advanced", "R")
## [1] "advancedR"
stri_paste("advanced", "R", sep = " ")
## [1] "advanced R"
stri_paste(c("advanced", "R"), collapse = "")
## [1] "advancedR"
stri_flatten(c("advanced", "R"))
## [1] "advancedR"
stri_paste(c("a", "b"), 1:2, sep = ",", collapse = ";")
## [1] "a,1;b,2"
"a" %stri+% "b"
## [1] "ab"
stri_dup("a", 1:5)
## [1] "a" "aa" "aaa" "aaaa" "aaaaa"
x <- "abc123ąęś"
stri_sub(x, 1, 3)
## [1] "abc"
stri_sub(x, -3)
## [1] "ąęś"
stri_sub(x, -3, -2)
## [1] "ąę"
stri_sub(x, -3, length=1)
## [1] "ą"
stri_sub(x, -3) <- "AES"; x
## [1] "abc123AES"
stri_trim_both("\t abc\n ")
## [1] "abc"
cat(stri_pad_both(c("abc", "defghij"), 20), sep = "\n")
#       abc         
#     defghij 

stri_rand_shuffle("abcdefghi")
## [1] "fhdbgciea"
stri_rand_strings(3, 5, "[a-z]")
## [1] "olylr" "ocxgb" "iyxsq"

sprintf("Test #%d of %d", 1:3, 3)
## [1] "Test #1 of 3" "Test #2 of 3" "Test #3 of 3"
sprintf("%-20s%.4f", "data science", pi)
## [1] "data science      3.1416"
sprintf("SELECT * FROM %s WHERE id IN (%s)", "myTable", stri_paste(sample(1:9, 3), collapse=", "))
## [1] "SELECT * FROM myTable WHERE id IN (9, 6, 5)"

stri_trans_general("ß", "Name")
## [1] "\\N{LATIN SMALL LETTER SHARP S}"
stri_trans_general("groß© żółć", "Latin-ASCII")
## [1] "gross(C) zolc"


## Locale dependent operations

stri_cmp_lt("hladny", "chladny")
## [1] FALSE
stri_cmp_lt("hladny", "chladny", list(locale="sk_SK"))
## [1] TRUE
stri_cmp_eq("\u0105", "a\u0328") # normalization
## [1] FALSE
stri_sort(c("a", "b", "w", "z"))
## [1] "a" "b" "w" "z"
stri_sort(c("a", "b", "w", "z"), opts=list(locale="et_EE"))
## [1] "a" "b" "z" "w"

stri_extract_words("above-mentioned advanced, data; analysis!")
## [[1]]
## [1] "above" "mentioned" "advanced" "data" "analysis"
stri_split_boundaries("above-mentioned advanced, data; analysis!")
## [[1]]
## [1] "above-" "mentioned " "advanced, " "data; " "analysis!"

stri_trans_toupper("groß")
## [1] "GROSS"
stri_trans_totitle("have a nice day")
## [1] "Have A Nice Day"

# stringi currently gives access to 4 search engines:
# • regex – regular expressions (t.b.d. later)
# • fixed – very fast, locale-independent exact substring searching
# • coll – Collator-based, locale-dependent pattern searching (for natural language processing, slow)
# • charclass – search for code points from a specific character class (Binary properties, General cate-
#                                                                         gories, etc.)
# Basic search operations:
# • detect
# • count
# • extract_all, extract_first, extract_last
# • locate_all, locate_first, locate_last
# • replace_all, replace_first, replace_last
# • split

stri_detect_fixed("science", "en")
## [1] TRUE
stri_detect_fixed(c("data", "science"), "en")
## [1] FALSE TRUE
stri_detect_fixed("science", c("en", "em", NA))
## [1] TRUE FALSE NA
stri_detect_fixed(c("data", "science"), c("at", "en"))
## [1] TRUE TRUE

stri_detect_fixed("a\u0328", "\u0105")
## [1] FALSE
stri_detect_coll("a\u0328", "\u0105")
## [1] TRUE
stri_detect_coll(c("GROSS", "groß"), "gross")
## [1] FALSE FALSE
stri_detect_coll(c("GROSS", "groß"), "gross", list(strength=2))
## [1] TRUE FALSE
stri_detect_coll(c("GROSS", "groß"), "gross", list(strength=1))
## [1] TRUE TRUE

stri_count_fixed("a1a2a3", "a")
stri_count_charclass("a1b2ß3ą4","[a-z]")
#?"stringi-search-charclass"

stri_extract_all_charclass(c("123", "abc", "ąęś123abc"), "\\p{L}")
## [[1]]
## [1] NA
##
## [[2]]
## [1] "abc"
##
## [[3]]
## [1] "ąęś" "abc"
stri_extract_all_charclass("ąęś123abc", "\\p{L}", merge = FALSE)
## [[1]]
## [1] "ą" "ę" "ś" "a" "b" "c"
stri_extract_first_charclass(c("123", "abc", "12ąęś45"), "\\p{L}")
## [1] NA "a" "ą"
stri_locate_all_fixed(c("abababa", "ba"), "aba")
stri_locate_last_fixed(c("abababa", "ba"), "aba")

stri_replace_all_fixed("Python programming", "Python", "R")
## [1] "R programming"
stri_replace_all_fixed("Python", "Python", c("R", "C++"))
# [1] "R"   "C++"
stri_replace_all_charclass("data science ", "\\p{Z}", "")
## [1] "datascience"

stri_split_charclass("data science", "\\p{Z}")
stri_split_fixed("beware! dogs?", c("!", "! ", "e"))

choices <- c("fish", "burrito", "marmalade", "broccoli")
match(c("burrito", "bur", "fish"), choices)
## [1] 2 NA 1
c("burrito", "bur", "fish") %in% choices
## [1] TRUE FALSE TRUE
choices <- c("fish", "burrito", "marmalade", "broccoli")
pmatch("burrito", choices)
## [1] 2
pmatch("bur", choices)
## [1] 2
pmatch("b", choices) # ambiguous
## [1] NA

s <- c("nieżółty" , "nieżółtej", "żółtego", "zazółcił",
       "zaczerwienił" , "żyły" , "żółty", "żółtemu", "zażółconemu")
s[agrep("żółty" , s)]

dst <- as.numeric(adist(s, "żółty"))/stri_length("żółty")
names(dst) <- s
dst



