
values <- 0:9
prob <- c(40,100,15,5,2,1,1,1,1,1)
prob <- prob/sum(prob)
N <- 1000000

x<- numeric(0)
for (k in seq_along(values)){
 x <- c(x, rep(values[k], as.integer(prob[k]*N)) )
}
Nr <- length(x)

n <- 5000
l <- 1000
means <- numeric(l)
for (k in 1:l){
   means[k] <- mean(sample(x, n))
}

#hist(means)
qqnorm(means)
qqline(means)
dpois(1, mean(x))
