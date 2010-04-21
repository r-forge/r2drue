\name{rgf.create}
\Rdversion{1.1}
\alias{rgf.create}
\alias{rgf.read}

\title{
Functions to read and write raster group files. 
}
\description{
These functions may facilitate working with raster group files.
}
\usage{
rgf.read(inFl)
rgf.create(prefix,suffix='',ini,fin=ini,monthini=1,output)
}

\arguments{
  \item{inFl}{
Filename of the Rgf file to be read
}
  \item{prefix}{
Prefix of the output filenames
}
  \item{suffix}{
Sufix of the output filenames
}
  \item{ini}{
Initial year
}
  \item{fin}{
Final year
}
  \item{monthini}{
Initial month (numeric in the 1 to 12 range)
}
  \item{output}{
Filename to be written to disk
}
}
\details{
rgf.read reads a raster group file and returns a character vector with the read values.
rgf.create is useful to generate sequences of file names that contain an indicator of the month and year. Like in Ndvi199001.tif, Ndvi199002.tif ... Ndvi199012.tif, Ndvi199101.tif, Ndvi199102.tif ... Where a prefix (Ndvi) is concatenated to a sequence of years (1990,1991) and months(01,02,...,12) and ended with a suffix (.tif).
}
\value{
Rgf.read returns a character vector with the filenames in the rgf file.
Rgf.create returns a character vector. If you supply the output file name parameter, then the list is writen to disc.
}
\author{
Alberto Ruiz, aruiz@eeza.csic.es
}
\examples{
rgf.create('ndvi','.rst',1970,1975,output='ndvi.rgf')
ndvifiles=rgf.read('ndvi.rgf')

}
