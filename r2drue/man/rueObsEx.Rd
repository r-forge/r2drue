\name{rueObsEx}
\Rdversion{1.1}
\alias{aiObsMe}
\alias{aiObsEx}
\alias{rueObsMe}
\alias{rueObsEx}

\title{
2dRue indexes
}
\description{
Functions to calculate several files needed to calculate de 2dRue model.
}
\usage{
rueObsMe(rainFl, viFl, silent = FALSE)
aiObsMe(rainFl, petFl, FAO = FALSE, silent = FALSE)
rueObsEx(rainFl, viFl, preRainFl, nMonths = 6, silent = FALSE)
aiObsEx(rainFl, viFl, petFl, preRainFl, prePetFl, FAO=FALSE, nMonths = 6, silent = FALSE)
}
\arguments{
  \item{rainFl}{
Text list (vector) with the file names of the rainfall grids
}
  \item{viFl}{
Text list (vector) with the file names of the vegetation index grids
}
  \item{petFl}{
Text list (vector) with the file names of the evapotranspiration grids
}
  \item{preRainFl}{
Text list (vector) with the file names of the rainfall files outside the given period
}
  \item{prePetFl}{
Text list (vector) with the file names of the evapotranspiration files outside the given period
}
  \item{FAO}{
logical; If TRUE the index is according with the FAO definition
}
  \item{nMonths}{
A integer value representing the water acumulation period.
If a valid image filename is supplied, the value is read from the file.
}
  \item{silent}{
logical; if TRUE, comment and non-fatal CPL driver errors suppressed
}
}
\details{
rueObsMe calculate the mean observed RUE.
aiObsMe calculate the mean observed aridity index.
rueObsEx calculate the extrem observed RUE.
aiObsEx calculate the extrem aridity index.
}
\value{
The output of each function is a SpatialGridDataFrame class
}
\references{
Article submitted and under revision.
}
\author{
Gabriel del Barrio, Juan Puigdefabregas, Maria E. Sanjuan & Alberto Ruiz
}
\examples{
#not work...
#frain=rgf.read('rain.rgf')
#fpet=rgf.read('pet.rgf')
#fvi=rgf.read('ndvi.rgf')
#rome=rueObsMe(frain,fvi)
#summary(rome)
#image(rome)
#writeGDAL(rome,'rome.rst',drivername='RST',mvFlag=-99)
#aiome=rueObsMe(frain,fvi,fpet)
#writeGDAL(aiome,'aiome.rst',drivername='RST',mvFlag=-99)

#fprain=read.rgf('prerain.rgf')
#fppet=read.rgf('prepet.rgf')
#roex=rueObsEx(frain,fvi,fprain)
#aioex=rueObsEx(frain,fvi,fprain,fppet)
#writeGDAL(roex,'roex.rst',drivername='RST',mvFlag=-99)
#writeGDAL(aioex,'aioex.rst',drivername='RST',mvFlag=-99)

}