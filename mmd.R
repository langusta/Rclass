

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


#### Hubs & Authorities

hits <- function(L, eps=0.0001, maxiter=1000){

   stopifnot(dim(L)[1]==dim(L)[2])
   stopifnot(sum(L*(1-L))==0)

   N <- dim(L)[1]

   h<- rep(1, N)
   a<- rep(1, N)
   for(j in 1:maxiter){
      h_old<-h
      a_old<-a
      a<-t(L)%*%h
      a<-a/max(a)
      h<-L%*%a
      h<-h/max(h)
      if( max(abs(h-h_old))<eps && max(abs(a-a_old))<eps ) break
   }

   list(auth=a, hub=h)
}

L<- matrix(c(0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0), nrow=4)
(hits(L))

#### Block-Stripe Algorithm - I/O size

bs_alg <- function (N, k, x) { # in GB

   #(8*N+92*N*x+(1-x)*k*N*(12+80/k) )/(2^30)
   (4*(k+1)*N+92*N*x+(1-x)*k*N*(12+80/k) )/(10^9)
}

(bs_alg(10^9, 2, 0.5))
(bs_alg(10^9, 3, 0.5))
(bs_alg(10^9, 2, 0.75))
(bs_alg(10^9, 3, 0.5))
