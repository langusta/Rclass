

####  Page rank:


pageRank <- function (M, beta, S,Sw, eps=0.0001, maxiter=1000){
   stopifnot(dim(M)[1]==dim(M)[2], beta>0, beta<=1)
   N <- dim(M)[1]

   t <- matrix(0, nrow=N, ncol=N)
   Sw<- Sw/sum(Sw)
   for(j in seq_along(S)) t[S[j], ] <- rep(Sw[j], N)
   A <- beta*M +(1-beta)*t

   r_n <- rep(1/N, N)
  for (j in 1:maxiter) {
     r_o <- r_n
     r_n<- A%*%r_o
     if (max(abs(r_n-r_o))<eps) break
  }

   r_n
}

mat <- matrix(c(0,0.5,0.5,0,1,0,0,0,0,0,0,1,0,0,1,0),nrow=4)
b <- 0.7
s <- c(1,2) #numery node'ow do ktorych moze sie odbyc teleport
s_w<- c(2,1) # wagi dla nodow do ktorych moze sie odbyc teleport

(pageRank(mat, b, s, s_w))
