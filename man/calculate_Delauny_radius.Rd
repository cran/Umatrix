\name{calculate_Delauny_radius}
\alias{calculate_Delauny_radius}
\title{Calculate the Delauny graph based radius}

\description{Function to calculate the radius for data generation.}

\usage{
    calculate_Delauny_radius(Data, BestMatches, 
    Columns = 80,  Lines = 50,  Toroid = TRUE)
}

\arguments{
  \item{Data}{Matrix of data (as submitted to Umatrix generation)}
  \item{BestMatches}{Array with positions of Bestmatches}
  \item{Columns}{Number of columns of the Umatrix}
  \item{Lines}{Number of columns of the Umatrix}
  \item{Toroid}{Whether a toroid Umatrx was used}
}
\value{
Returns a list of results.
\item{neighbourDistances}{Distances on the Umatrix neigborhood matrix.}
\item{RadiusByEM}{Radius suggested by EM algorithm.}
}
\examples{\dontrun{
data("Hepta")
data("HeptaBMU")
DelaunyHepta <- calculate_Delauny_radius(Data = Hepta$Data, BestMatches = HeptaBMU,  Toroid = FALSE)
}}

\references{
Ultsch A, Lotsch J: Machine-learned cluster identification in high-dimensional data. J Biomed Inform. 2017 Feb;66:95-104. doi: 10.1016/j.jbi.2016.12.011. Epub 2016 Dec 28. 
}
