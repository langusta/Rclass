#include<Rcpp.h>
#include<algorithm>
using namespace Rcpp ;

//[[Rcpp::export]]
int mode (const IntegerVector x) {

  if (x.size() == 0) return NA_INTEGER;

  IntegerVector y = clone(x);
  std::sort(y.begin(), y.end());

  int mode = NA_INTEGER;
  int mode_licznik = 0;
  int ost = y[0];
  int licznik = 0;


  for (int k=0; k<y.size(); ++k){
    if(y[k] == ost){
      ++licznik;
    }else{
      if(licznik > mode_licznik) {
        mode = ost;
        mode_licznik = licznik;
      }
      ost = y[k];
      licznik = 1;
    }
  }

  if(licznik > mode_licznik) {
    mode = ost;
  }

  return mode;
}

//[[Rcpp::export]]
RObject simplify2array2(const List x) {
   // check if it is a list of numeric vectors of the same lengths
   int correct = 1;
   int length = 0;
   for(int k=0; k<x.size() ; ++k){
      if(!Rf_isNumeric(x[k])) {correct=0; break;}
      if (k==0) {NumericVector tmp(x[k]); length = tmp.size();}
      NumericVector tmp(x[k]);
      if(length != tmp.size()){correct=0; break;}
   }

   if(correct){
      NumericVector out(x.size()*length);
      for(int k=0; k<x.size(); ++k){
         NumericVector tmp(x[k]);
         for(int r=0; r<length; ++r){
            out[k*length+r] = tmp[r];
         }
      }
      if(length > 1) out.attr("dim") = IntegerVector::create(length, x.size());
      return out;
   } else {
      List out = Rcpp::clone(x);
      return out;
   }
}

//[[Rcpp::export]]
NumericMatrix perms(const int n) {
   if (n<0) stop("PERMS: n<0!");
   if (n==0) return NumericMatrix(0,0);

   int n_f = 1;
   for (int k=2; k<n+1;++k) n_f*=k;

   NumericMatrix out(n_f,n);
   if(n==1) {out[0]=1;return out;}

   NumericMatrix smaller = perms(n-1);

   for(int k=0;k<n;++k){ // for each position
      for(int r=0; r<smaller.nrow(); ++r){ // for each row
         // add "n" on k-th position
         for(int c=0; c < k ; ++c) out(k*smaller.nrow()+r,c) = smaller(r,c);
         out(k*smaller.nrow()+r,k) = n;
         for(int c=k+1; c < n ; ++c) out(k*smaller.nrow()+r,c) = smaller(r,c-1);
      }
   }
   return out;
}





