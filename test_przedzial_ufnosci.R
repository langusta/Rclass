
values <- c(0:9,20,25,30)
prob <- c(40,100,15,5,2,1,1,1,1,1,1,1,1)
prob <- prob/sum(prob)
N <- 3000000
xpois <- rpois(N, mean(x))

x<- numeric(0)
for (k in seq_along(values)){
 x <- c(x, rep(values[k], as.integer(prob[k]*N)) )
}
Nr <- length(x)

n <-1000
l <- 1500
means <- numeric(l)
sds <- numeric(l)
# ns <- seq(from = 1000, to = 50000, by = 100)
for (k in 1:l){ #seq_along(ns)){
   # n<-ns[k]
   means[k] <- mean(sample(x, n))
   sds[k] <- sd(sample(x, n))
}

# http://stackoverflow.com/questions/7781798/seeing-if-data-is-normally-distributed-in-r
#hist(means)
k<- 1
plot(density(means), col = "red")
lines( xx <- seq(from = mean(means) - 3*sd(means), 
                  to = mean(means) + 3*sd(means), length.out = 2000),
      dnorm(xx, mean(means), sd(means)),
      col = "blue"
      )
abline(v=mean(x))
qqnorm(means)
qqline(means)

mean(means)
mean(x)

plot(0:9, summary(as.factor(x)), col = "red")
points(0:9, summary(as.factor(xpois[xpois<10])) ,col="blue")
