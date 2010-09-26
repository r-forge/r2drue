\name{r2dRue-package}
\Rdversion{1.1}
\alias{r2dRue-package}
\alias{r2dRue}
\docType{package}
\title{
2dRUE model functions
}
\description{
2dRUE is a methodology to make a diagnostic of land condition in a large territory during a given time period. 2dRUE was developed as a low cost and flexible methodology taking full advantage of readily available data from Global Environmental Monitoring satellites and geodatabases.

Assessment and monitoring are explicitly separated to yield estimates of status and trends of land condition respectively. In the assessment module, Rain Use Efficiency is implemented on two time-scales to yield estimates of biomass-maturity and productivity-resilience respectively. In the monitoring module, effects of time and aridity on vegetation growth are discriminated to detect intrinsic trends of aggradation or degradation in the vegetation cover.

2dRUE is based on archived time-series and requires two types of input data. The first one is a vegetation density index such as NDVI, for which several products are available (e.g. GIMMS, MEDOKADS, etc.). The second one are climate fields at a corresponding spatial resolution, involving mean maximum, mean and mean minimum temperatures and precipitation

The r2dRue software library contains all the relevant functions to compute the quantitative maps of assessment (mean and extreme observed RUE) and of monitoring (effects of time and aridity). It also has additional utilities to compute aridity indexes of both 2dRUE and FAO-UNEP formulae, and to make queries to the climate and vegetation archives.

The recommended sequence of functions for a concrete analysis is: rgf.create (optional), petHgsm (optional), edit2dRfile, read2dRfile, showInfo, assessment, monitoring and summarize. Alternatively, individual functions such as rgf.summary can be used to explore the data set and/or conduct a step-by-step analysis.

The r2dRue library has been developed within the EC project "A Surveillance System for Assessing and Monitoring of Desertification" (DeSurvey, Integrated Project No. 003950). See details at \url{www.desurvey.net}

}
\details{
\tabular{ll}{
Package: \tab r2dRue\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2009-12-17\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
}
\author{
Gabriel del Barrio, Juan Puigdefabregas, Maria E. Sanjuan & Alberto Ruiz\cr
Maintainer:
Alberto Ruiz, <aruiz@eeza.csic.es>
}
\references{
del Barrio, G., Puigdefabregas, J. Sanjuan, M.E., Stellmes, M. & Ruiz, A. (2010). Assessment and monitoring of land condition in the Iberian Peninsula, 1989-2000. Remote Sensing of Environment, 114: 1817-1832 \url{http://dx.doi.org/10.1016/j.rse.2010.03.009}
}
