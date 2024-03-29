// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// addRowWiseC
NumericMatrix addRowWiseC(NumericMatrix WeightVectors, NumericVector DataPoint);
RcppExport SEXP _Umatrix_addRowWiseC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    rcpp_result_gen = Rcpp::wrap(addRowWiseC(WeightVectors, DataPoint));
    return rcpp_result_gen;
END_RCPP
}
// bestmatchC
int bestmatchC(NumericMatrix WeightVectors, NumericVector DataPoint);
RcppExport SEXP _Umatrix_bestmatchC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    rcpp_result_gen = Rcpp::wrap(bestmatchC(WeightVectors, DataPoint));
    return rcpp_result_gen;
END_RCPP
}
// bestmatchesC
NumericVector bestmatchesC(NumericMatrix WeightVectors, NumericMatrix DataPoints, int Columns);
RcppExport SEXP _Umatrix_bestmatchesC(SEXP WeightVectorsSEXP, SEXP DataPointsSEXP, SEXP ColumnsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type DataPoints(DataPointsSEXP);
    Rcpp::traits::input_parameter< int >::type Columns(ColumnsSEXP);
    rcpp_result_gen = Rcpp::wrap(bestmatchesC(WeightVectors, DataPoints, Columns));
    return rcpp_result_gen;
END_RCPP
}
// esomTrainedWeightVectorsConeC
void esomTrainedWeightVectorsConeC(NumericMatrix WeightVectors, NumericVector DataPoint, NumericVector indices, NumericVector DistancesToBm, double Radius, double LearningRate);
RcppExport SEXP _Umatrix_esomTrainedWeightVectorsConeC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP, SEXP indicesSEXP, SEXP DistancesToBmSEXP, SEXP RadiusSEXP, SEXP LearningRateSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DistancesToBm(DistancesToBmSEXP);
    Rcpp::traits::input_parameter< double >::type Radius(RadiusSEXP);
    Rcpp::traits::input_parameter< double >::type LearningRate(LearningRateSEXP);
    esomTrainedWeightVectorsConeC(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate);
    return R_NilValue;
END_RCPP
}
// esomTrainedWeightVectorsGaussC
void esomTrainedWeightVectorsGaussC(NumericMatrix WeightVectors, NumericVector DataPoint, NumericVector indices, NumericVector DistancesToBm, double Radius, double LearningRate);
RcppExport SEXP _Umatrix_esomTrainedWeightVectorsGaussC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP, SEXP indicesSEXP, SEXP DistancesToBmSEXP, SEXP RadiusSEXP, SEXP LearningRateSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DistancesToBm(DistancesToBmSEXP);
    Rcpp::traits::input_parameter< double >::type Radius(RadiusSEXP);
    Rcpp::traits::input_parameter< double >::type LearningRate(LearningRateSEXP);
    esomTrainedWeightVectorsGaussC(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate);
    return R_NilValue;
END_RCPP
}
// esomTrainedWeightVectorsMexicanHatC
void esomTrainedWeightVectorsMexicanHatC(NumericMatrix WeightVectors, NumericVector DataPoint, NumericVector indices, NumericVector DistancesToBm, double Radius, double LearningRate);
RcppExport SEXP _Umatrix_esomTrainedWeightVectorsMexicanHatC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP, SEXP indicesSEXP, SEXP DistancesToBmSEXP, SEXP RadiusSEXP, SEXP LearningRateSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DistancesToBm(DistancesToBmSEXP);
    Rcpp::traits::input_parameter< double >::type Radius(RadiusSEXP);
    Rcpp::traits::input_parameter< double >::type LearningRate(LearningRateSEXP);
    esomTrainedWeightVectorsMexicanHatC(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Umatrix_addRowWiseC", (DL_FUNC) &_Umatrix_addRowWiseC, 2},
    {"_Umatrix_bestmatchC", (DL_FUNC) &_Umatrix_bestmatchC, 2},
    {"_Umatrix_bestmatchesC", (DL_FUNC) &_Umatrix_bestmatchesC, 3},
    {"_Umatrix_esomTrainedWeightVectorsConeC", (DL_FUNC) &_Umatrix_esomTrainedWeightVectorsConeC, 6},
    {"_Umatrix_esomTrainedWeightVectorsGaussC", (DL_FUNC) &_Umatrix_esomTrainedWeightVectorsGaussC, 6},
    {"_Umatrix_esomTrainedWeightVectorsMexicanHatC", (DL_FUNC) &_Umatrix_esomTrainedWeightVectorsMexicanHatC, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_Umatrix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
