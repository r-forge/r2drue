\name{petHgsm}
\Rdversion{1.1}
\alias{batchPetHgsm}
\alias{petHgsm}
\title{
Hargraves-Samani Potential Evapotranspiration function
}
\description{
Given a set of images of temperature and solar radiation, the potential evapotranspiration is calculated according to Hargraves-Samani equation. batchPet () provides a batch process to calculate the PET over several sets of images.
}
\usage{
petHgsm(Tmin,Tmax,Tmed,Rad,month)
batchPetHgsm(monthIni, Tmin, Tmax, Tmed, Rad, outFl, ...)
}
\arguments{
  \item{monthIni}{
A integer between 1 and 12 that is the month of the first image in the dataset.
}
  \item{month}{
The month to calc the PET (numeric 1 to 12)
}
  \item{Tmin}{
In petHgsm a SpatialGridDataFrame with minimum air temperature (Celsius degree)
In batchpetHgsm a character vector with the names of the files with minimum air temperature (Celsius degree)
}
  \item{Tmax}{
In petHgsm a SpatialGridDataFrame with maximum air temperature (Celsius degree)
In batchpetHgsm a character vector with the names of the files with maximum air temperature (Celsius degree)
}
  \item{Tmed}{
In petHgsm a SpatialGridDataFrame with mean air temperature (Celsius degree)
In batchpetHgsm a character vector with the names of the files with mean air temperature (Celsius degree)
}
  \item{Rad}{
In petHgsm a SpatialGridDataFrame with extra-terrestial solar radiation
In batchpetHgsm a character vector with the names of the files with extra-terrestial solar radiation
}
  \item{outFl}{
A list of file names to save the results
}
  \item{\dots}{
Any unmatched parameter will be passed to the writeGDAL routine.
}
}

\value{
petHgsm returns a spatialGridDataFrame representing the potential evapotranspiration. batchEtp write to disc, the calculated images for each set of images provided.
}
\references{
Estimating potential evapotranspiration.
Hargreaves, G.H., Samani, Z.A. 
Journal of the Irrigation & Drainage Division - ASCE 
Volume 108, Issue IR3, 1982, Pages 225-230  
}
\author{
Gabriel del Barrio, Marieta E. SanJuan & Alberto Ruiz
}
\examples{
#not run
## Read  the temperature and radiation data 
#tmin=readGDAL('Tmin_Ene1990')
#tmax=readGDAL('Tmax_Ene1990')
#tmea=readGDAL('Tmea_Ene1990')
#rad=readGDAL('SolarRad_Ene1990')
## Calc the PET for Ene 1990
#PetEne=petHgsm(tmin,tmax,tmea,rad, 1)
#writeGDAL(PetEne,'PetEne1990')
## Or to calculate and write to disk, in a batch process, a set of PETs
## create the Input file list
#Flmin=c('Tmin_Ene1990','Tmin_Feb1990', 'Tmin_Mar1990')
#Flmax=c('Tmax_Ene1990','Tmax_Feb1990', 'Tmax_Mar1990')
#Flmea=c('Tmea_Ene1990','Tmea_Feb1990', 'Tmea_Mar1990')
#Flrad=c('Rad_Ene','Rad_Feb', 'Rad_Mar')
#PETFileNames=c('PET_Ene1990','PET_Feb1990', 'PET_Mar1990')
##define the date of the first image
#iniDate=c(1,1990)
#batchPET(iniDate, Flmin, Flmax, Flmea, Flrad, PETFileNames)

}
