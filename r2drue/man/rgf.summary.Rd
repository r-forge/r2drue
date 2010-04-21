\name{rgf.summary}
\Rdversion{1.1}
\alias{rgf.summary}
\alias{rgf.when}
\title{
Function to summarize raster group files.
}
\description{
These functions may facilitate working with raster group files.
Provides functions to calculate summaries over a raster group, like Max, Min, Sum, Mean or Std, and to find when the selected summary is located in a raster group (ie. Order No. in the time series).
}
\usage{
rgf.summary(inFl, outFl, step= length(inFl), fun = "SUM", silent = FALSE, ...)
rgf.when(inFl, ref, order = "FIRST", silent = FALSE)
}

\arguments{
  \item{inFl}{
Text list (vector) with the names of the input raster files.
}
  \item{outFl}{
Text list (vector) with the names of the output raster files.
}
  \item{step}{
Number of images taken for each summary.
If not supply a value, the summary is made on the complete list.
}
  \item{fun}{
Summary function SUM, MAX, MIN, MEAN, COUNT, RANGE, SD, VAR,MEDIAN.
}
  \item{ref}{
The reference image.
}
  \item{order}{
"FIRST" or "LAST"; to find the fisrt or last ocurrence in the serie. 
}
  \item{silent}{
logical; if TRUE, comment and non-fatal CPL driver errors suppressed.
}
  \item{\dots}{
Any unmatched parameter will be passed to the writeGDAL function.
}
}
\details{
rgf.summary calculate statistic summaries over a set of images.

A summary can be calculated for the entire set of images, or for a subset. For example, let a sequence of 36 images representing the average monthly NDVI. An image can be made with the maximum NDVI in the entire series, or calculate the maximum NDVI for each year of the series (step = 12) or the maximum for each station (step = 3).Summaries may be done over the full list of images, or over defined subsets.

rgf.when compares a reference image with a list of images and return a new composite image where each cell contains the order number at which its reference value is found in the time input series.  

}
\value{
Rgf.summary not returns any value, but save on disk, the solicited summaries.

Rgf.when return a composite image as a spatialgriddataframe.
}
\author{
Alberto Ruiz, aruiz@eeza.csic.es
}
\examples{
#not work...
#rgf.create('ndvi','.rst',1970,1975,output='ndvi.rgf')
#ndvifiles=rgf.read('ndvi.rgf')
#rgf.summary(ndvifiles,'maxNdvi.rst',fun='MAX', drivername='RST')
#Anualnames=paste('maxNdvi',1970:1975,sep='_')
#maxNdvi=rgf.summary(ndvifiles,AnualNames, step=12,fun='MAX',drivername='RST')

}
