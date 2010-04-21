\name{rasterStack}
\Rdversion{1.1}
\alias{rasterStack}
\title{
Stack bands from raster image files
}
\description{
rasterStack concatenates multiple raster files into a single file. Provides support to several interleaved schemas; BIP (Bands interleaved by pixel), BIL (Band interleaved by line)or BSQ (Bands sequential). The output concatenated file may be saved in Ascii or binary format.
}
\usage{
rasterStack(inFl, outFN, asc = FALSE, zip = FALSE, dec = 3, interleave = "BIP", silent = FALSE)
}
\arguments{
  \item{inFl}{
A List with the name of the files to be concatenated
}
  \item{outFN}{
The name of the output file
}
  \item{asc}{
Logical. If TRUE the output is a ASCII file
}
  \item{zip}{
Logical. If TRUE the output is zipped. Only used if asc is TRUE.
}
  \item{dec}{
Integer. If present, the output values are rounded to the specified number of decimal places. Only used if asc is TRUE.
}
  \item{interleave}{
The interleave format. BIP, BIL or BSQ.
}
  \item{silent}{
logical; if TRUE, comment and non-fatal CPL driver errors suppressed
}
}
\details{
In binary mode (default), the data is stored as 32bit Floating point number.
Only the first band of each image is used.
This feature is not intended to be of general utility. It is used in internal processes to arrange image data into a format optimum for the process.

}
\value{
An ASCII or binary file.
}

\author{
Alberto Ruiz. aruiz@eeza.csic.es
}
