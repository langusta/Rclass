
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
