// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gcdist1
NumericMatrix gcdist1(NumericVector lon, NumericVector lat, double eps);
RcppExport SEXP _gear_gcdist1(SEXP lonSEXP, SEXP latSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type lon(lonSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lat(latSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(gcdist1(lon, lat, eps));
    return rcpp_result_gen;
END_RCPP
}
// gcdist2
NumericMatrix gcdist2(NumericVector lon1, NumericVector lat1, NumericVector lon2, NumericVector lat2, double eps);
RcppExport SEXP _gear_gcdist2(SEXP lon1SEXP, SEXP lat1SEXP, SEXP lon2SEXP, SEXP lat2SEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type lon1(lon1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lon2(lon2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lat2(lat2SEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(gcdist2(lon1, lat1, lon2, lat2, eps));
    return rcpp_result_gen;
END_RCPP
}
// eucdist1
NumericMatrix eucdist1(NumericVector x, NumericVector y, double eps);
RcppExport SEXP _gear_eucdist1(SEXP xSEXP, SEXP ySEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(eucdist1(x, y, eps));
    return rcpp_result_gen;
END_RCPP
}
// eucdist2
NumericMatrix eucdist2(NumericVector x1, NumericVector y1, NumericVector x2, NumericVector y2, double eps);
RcppExport SEXP _gear_eucdist2(SEXP x1SEXP, SEXP y1SEXP, SEXP x2SEXP, SEXP y2SEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(eucdist2(x1, y1, x2, y2, eps));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_gear_gcdist1", (DL_FUNC) &_gear_gcdist1, 3},
    {"_gear_gcdist2", (DL_FUNC) &_gear_gcdist2, 5},
    {"_gear_eucdist1", (DL_FUNC) &_gear_eucdist1, 3},
    {"_gear_eucdist2", (DL_FUNC) &_gear_eucdist2, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_gear(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
