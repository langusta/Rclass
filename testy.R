
# Write a function Range(), which for a given numeric vector x returns a named list with the following
# components:
# • x – a copy of the input vector,
# • min – the minimum of x,
# • max – the maximum.
# The return value should have the class attribute set to "Range".
# Then write a method print.Range(), which echoes an object of class "Range" in a form resembling:

Range <- function(x) {
   x<-list(x=x, min=min(x), max=max(x))
   attr(x, "class")<- "Range"

}

print.Range <- function(x) {
   cat(" x = ", paste(x$x, collapse = ", "), "\n","min = ", x$min, "\n max = ", x$max, "\n" )
}


library(Rcpp)

cppFunction('
   int fib(int n, int f_1 = 1, int f_2 = 1){
      if (n <= 1) return f_2;
      else{
         fib(n - 1, f_2, f_1+f_2);
      }
   }
')

Rcpp::cppFunction("
int fib2(int n) {
if (n <= 1) return 1;
int last1 = 1;
int last2 = 1;
for (int i=2; i<=n; ++i) {
int last3 = last2;
last2 = last1;
last1 = last2+last3;
}
return last1;
}
")

Rcpp::cppFunction('
double sum2(NumericVector x) {
   int n = x.size(); // a method
   Rprintf("n %d\\n",
         n);
   double result = 0.0;
   for (int i=0; i<n; ++i)
      result += x[i]; // or x(i)
   return result;
}
')

