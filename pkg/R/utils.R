# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

readIniFile <- function(filename, token='=', commenttoken=';') {
	
	strSplitOnFirstToken <- function(s, token="=") {
		pos <- which(strsplit(s, '')[[1]]==token)[1]
		if (is.na(pos)) {
			return(c(trim(s), NA)) 
		} else {
			first <- substr(s, 1, (pos-1))
			second <- substr(s, (pos+1), nchar(s))
			return(trim(c(first, second)))
		}
	}
	
	strsp <- function(s){strSplitOnFirstToken(s, token=token)}
	
	strSplitComment <- function(s,  token=";") { 
		# ";" is the start of a comment .
		strSplitOnFirstToken(s, token=";") 
	}
	strspcom <- function(s){strSplitComment(s, token=commenttoken)}
	
	
	if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	
	Lines <- readLines(filename,  warn = FALSE)
	Lines <- trim(Lines)
	
	ini <- lapply(Lines, strspcom) 
	
	Lines <- matrix(unlist(ini), ncol=2, byrow=TRUE)[,1]
	ini <- lapply(Lines, strsp) 
	
	ini <- matrix(unlist(ini), ncol=2, byrow=TRUE)
	ini <- subset(ini, ini[,1] != "")
	
	ns <- length(which(is.na(ini[,2])))
	if (ns > 0) {
		sections <- c(which(is.na(ini[,2])), length(ini[,2]))
		
# here I should check whether the section text is enclused in [ ]. If not, it is junk text that should be removed, rather than used as a section
		ini <- cbind("", ini)
		for (i in 1:(length(sections)-1)) {
			ini[sections[i]:(sections[i+1]), 1] <- ini[sections[i],2]
		}	
		ini[,1] <- gsub("\\[", "", ini[,1])
		ini[,1] <- gsub("\\]", "", ini[,1])
		sections <- sections[1:(length(sections)-1)]
		ini <- ini[-sections,]
	} else {
		ini <- cbind("", ini)	
	}
	
	colnames(ini) <- c("section", "name", "value")
	########### como dataframe
	out=as.data.frame(t(ini[,3]),stringsAsFactors=FALSE)
	names(out)=ini[,2]
	return(out)
}

.gdalWriteFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	i <- which(gd[,1] %in% c('VRT', 'MEM', 'MFF', 'MFF2'))
	gd[-i,]
}


isSupportedGDALFormat <- function(dname) {
	if (!require(rgdal)) { stop() }
	gd <- .gdalWriteFormats()
	res <- dname %in% gd[,1]	
	return(res)
}


# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3


###   cellValues   ###

if (!isGeneric("cellValues")) {
	setGeneric("cellValues", function(x, cells)
				standardGeneric("cellValues"))
}


setMethod("cellValues", signature(x='RasterLayer', cells='vector'), 
		function(x, cells) { 
			return(.readCells(x, cells))
		}
)

setMethod("cellValues", signature(x='RasterBrick', cells='vector'), 
		function(x, cells) { 
			return(.brickReadCells(x, cells))
		}
)


setMethod("cellValues", signature(x='RasterStack', cells='vector'), 
		function(x, cells) { 
			result <- matrix(ncol=nlayers(x), nrow=length(cells))
			for (i in seq(nlayers(x))) {
				result[,i] <- .readCells( x@layers[[i]], cells )
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- layerNames(x)
			}	
			result
		}
)

# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


canProcessInMemory <- function(raster, n=4) {
	if (.toDisk()) { return(FALSE) } 
	
	n <- n + (nlayers(raster) - 1)
	cells <- round(1.1 * ncell(raster))
	
	if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
		if ((cells * n) > 300000000) {
			return(FALSE) 
		}
	}
	
#	if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
#	# windows, function memory.size  available
#	memneed <- cells * 8 * n / (1024 * 1024)
#	memavail <- 0.5 * (memory.size(NA)-memory.size(FALSE))
#	if (memneed > memavail) {
#		return(FALSE)
#	} else {
#		return(TRUE)
#	}
#   } else {
	
	g <- gc()
	w <- getOption('warn')
	options('warn'=-1) 
	r <- try( matrix(0.1, ncol=n, nrow=cells), silent=TRUE )
	options('warn'= w) 
	
	if (class(r) == "try-error") {
		return( FALSE )
	} else {
		rm(r)
		g <- gc()
		return( TRUE ) 
	}
}


# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("distance")) {
	setGeneric("distance", function(x, ...)
				standardGeneric("distance"))
}	


setMethod('distance', signature(x='RasterLayer'), 
		
		function(x, filename='', ...) {
			
			r = edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
			
			pts <- try(  rasterToPoints(r, fun=function(z){z>0})[,1:2, drop=FALSE] )
			
			if (class(pts) == "try-error") {
				return( .distanceRows(x, filename=filename, ...) )
			}
			
			if (nrow(pts) == 0) {
				stop('RasterLayer has no NA cells (for which to compute a distance)')
			}
			
			out <- raster(x)
			
			if (.couldBeLonLat(out)) { disttype <- 'GreatCircle' } else { disttype <- 'Euclidean' }
			
			filename <- trim(filename)
			if (!canProcessInMemory(out, 2) && filename == '') {
				filename <- rasterTmpFile()
				
			}
			xy <- xFromCol(out, 1:ncol(out))
			xy <- cbind(xy, NA)
			
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
			} else {
				out <- writeStart(out, filename, ...)
			}
			
			pb <- pbCreate(nrow(out), type=.progress(...))
			for (r in 1:nrow(out)) {	
				vals <- getValues(x, r)
				i = which(is.na(vals))
				vals[] <- 0
				if (length(i) > 0) {
					xy[,2] <- yFromRow(out, r)
					for (c in i) {
						vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
					}
				}
				if (filename == "") {
					v[,r] <- vals
				} else {
					writeValues(out, vals, r)
				}
				pbStep(pb, r) 	
			}	
			pbClose(pb)
			
			if (filename == "") { 
				out <- setValues(out, as.vector(v)) 
			} else {
				writeStop(out)
			}
			return(out)
		}
)


