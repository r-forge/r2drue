\name{regStepRaster}
\Rdversion{1.1}
\alias{regStepRaster}
\title{
Step Regresion over raster images
}
\description{
This function realices a step regresion 
}
\usage{
regStepRaster(ndviFl, timeFl, aridFl, outFl, silent = FALSE, ...)
}

\arguments{
  \item{ndviFl}{
Text list (vector) with the file names of the vegetation index grids
}
  \item{timeFl}{
Text list (vector) with the file names of the time series
}
  \item{aridFl}{
Text list (vector) with the file names of the aridity index grids
}
  \item{outFl}{
A list of 7 file names to save the results
}
  \item{silent}{
logical; if TRUE, comment and non-fatal CPL driver errors suppressed
}
  \item{\dots}{
Any unmatched parameter will be passed to the writeGDAL routine.
}
}
\details{
ya vere
}
\value{
Returns no value, but save to disk seven image files with the values of
}
\author{
Gabriel del Barrio, Marieta SanJuan & Alberto Ruiz
}

