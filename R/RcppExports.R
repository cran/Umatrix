# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

addRowWiseC <- function(WeightVectors, DataPoint) {
    .Call(`_Umatrix_addRowWiseC`, WeightVectors, DataPoint)
}

bestmatchC <- function(WeightVectors, DataPoint) {
    .Call(`_Umatrix_bestmatchC`, WeightVectors, DataPoint)
}

bestmatchesC <- function(WeightVectors, DataPoints, Columns) {
    .Call(`_Umatrix_bestmatchesC`, WeightVectors, DataPoints, Columns)
}

esomTrainedWeightVectorsConeC <- function(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate) {
    invisible(.Call(`_Umatrix_esomTrainedWeightVectorsConeC`, WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate))
}

esomTrainedWeightVectorsGaussC <- function(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate) {
    invisible(.Call(`_Umatrix_esomTrainedWeightVectorsGaussC`, WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate))
}

esomTrainedWeightVectorsMexicanHatC <- function(WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate) {
    invisible(.Call(`_Umatrix_esomTrainedWeightVectorsMexicanHatC`, WeightVectors, DataPoint, indices, DistancesToBm, Radius, LearningRate))
}

