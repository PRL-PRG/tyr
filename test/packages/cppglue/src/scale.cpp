#include <Rcpp.h>
#include "scaler.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector scale_by(NumericVector x, double k) {
  cppglue::Scaler s(k);
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); i++) out[i] = s(x[i]);
  return out;
}

// [[Rcpp::export]]
bool all_named(List x) {
  return x.hasAttribute("names");
}
