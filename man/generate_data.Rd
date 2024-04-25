\name{generate_data}
\alias{generate_data}
\title{Generative ESOM}

\description{Function to generate new data with the same structure as the input data.}

\usage{
    generate_data(Data, density_radius, Cls = NULL, gen_per_data = 10)
}

\arguments{
  \item{Data}{Matrix of data (as submitted to Umatrix generation)}
  \item{density_radius}{Numeric value of data generation radius}
  \item{Cls}{Classification of the data as a  vector}
  \item{gen_per_data}{New isntances per original iunstance to be generated}
}

\value{
Returns a list of results.
\item{original_data}{The input data.}
\item{original_classes}{The input classes.}
\item{generated_data}{The generated data.}
\item{generated_classes}{The generated classes.}
}
\examples{\dontrun{
data("Hepta")
data("HeptaBMU")
HeptaData <- Hepta$Data
HeptaCls <y- Hepta$Cls
HeptaGenerated <- generate_data(HeptaData, 1, HeptaCls )
}}

\references{
Ultsch A, Lotsch J: Machine-learned cluster identification in high-dimensional data. J Biomed Inform. 2017 Feb;66:95-104. doi: 10.1016/j.jbi.2016.12.011. Epub 2016 Dec 28. 
}
