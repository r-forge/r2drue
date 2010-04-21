###############################################
# NAME: rgf.create
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################

rgf.create = function(prefix,suffix='',ini,fin=ini,monthini=1,output) {
	if (missing(ini)) stop('ini parameter is missing')
	if (missing(fin)) {rgf.create(prefix,suffix,ini,ini,monthini,output)} 
	else {
		if (monthini>1) fin=fin+1 
		rg=paste(prefix,rep(ini:fin,each=9),'0',1:9,suffix,sep='')
		rg=c(rg,paste(prefix,rep(ini:fin,each=3),10:12,suffix,sep=''))
		rg=sort(rg)
		if (monthini>1) rg=rg[monthini:(length(rg)-(12-monthini+1))] 		
		if (!missing(output)) write(c(length(rg),rg),file=output)
		rg
	}
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################

rgf.read = function (inFl){
	aux=as.vector(read.table(inFl)[,1])
	if (!is.na(as.integer(aux[1]))) {aux=aux[-1]} #comprobar si hay un numero en la primera linea y eliminarlo
	aux
}


###############################################
# NAME: rgf.when
# PURPOSE:
#     Compara una imagen de referencia con una lista de imagenes.
#     Para cada pixel se localiza la primera ocurrencia del valor 
#     de referencia en la lista de imagenes.
#
# INPUTS:
#       inFl: File names list of grid
#       ref: reference image 
#       order: 'FIRST' default, registra la primera ocurrencia
#              'LAST' registra la ultima ocurrencia
#       silent: logical Flag; if TRUE, comments outputs are supressed
# OUTPUTS:
#       A SpatialGridDataFrame/SpatialPixelDataframe class. 
###############################################

rgf.when = function (inFl,ref,order='FIRST',silent=FALSE) {
	if (!silent) print(paste('Searching for',order,'ocurrences'))
	ref=readGDAL(ref,silent=TRUE)
	n=length(inFl)

	when=0
	aux=0
	pb =txtProgressBar(min=0,max=n,char='*',width=20,style=3)
	for (i in 1:n) {
		aux=readGDAL(inFl[i],silent=TRUE)
		msk=(aux$band1==ref$band1)
		when=(when*!msk) + (i*msk)
		setTxtProgressBar(pb, i)
	}
	close(pb)
	aux$band1=when
	aux
}


###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################

rgf.summary = function(inFl,outFl,step=length(inFl),fun='SUM',silent=FALSE,...) {
	
	if (length(inFl)%%step!=0) stop(paste('elements in inFl must be a multiply of',step))
	if (length(outFl)!=floor(length(inFl)/step)) stop('elements in outFl is not correct')
	if (step==1) stop('step must be greater than 1')

	#Extraer el numero de elementos del array 
    	n=length(inFl)  #num de ficheros de entrada
    	nah=floor(n/step) #num de ficheros de salida

	print(paste('Procesing', n, 'files. Summary', fun, 'every',step))

	#branch para funciones acumulativas o no acumulativas (hay que leerlas por trozos)
	if (fun %in% c('SUM','MAX','MIN','MEAN')) {

		if (!silent) pb =txtProgressBar(min=0,max=n,char='*',width=20,style=3)

		aux=readGDAL(inFl[1],silent=TRUE)	

		for(i in 0:(nah-1)){
			dat=0
			for(j in 1:step) {
				switch (fun,
					SUM = {dat = dat+readGDAL(inFl[i*step+j],silent=TRUE)$band1},
					MIN = {dat = pmin(dat,readGDAL(inFl[i*step+j],silent=TRUE)$band1)},
					MAX = {dat = pmax(dat,readGDAL(inFl[i*step+j],silent=TRUE)$band1)},
					MEAN= {dat = dat+readGDAL(inFl[i*step+j],silent=TRUE)$band1/step},
				)
				if (!silent) setTxtProgressBar(pb, i*step+j)
			}
			aux$band1=dat
			writeGDAL(aux,outFl[i+1],...)
		}
	} else { #funciones no acumulativas 

	 	#num de filas de la imagen
	 	rows=GDALinfo(inFl[1])[1]
	 	#num de columnas de la imagen
	 	cols=GDALinfo(inFl[1])[2]

		#calculo size del buffer de lectura para que lea bloques de 100MB
		linesToRead=ceiling(100000000/(cols*n*8))
	 	#num de bloques de size LinesToRead en la imagen
	 	nblocks=ceiling(rows/linesToRead)

		if (!silent) pb =txtProgressBar(min=0,max=nah*nblocks,char='*',width=20,style=3)

		for(i in 0:(nah-1)){
			dat=0
			for (k in 0:(nblocks-1)){
			 	outdf=0
				for(j in 1:step) {
			  		aux=GDAL.open(inFl[i*step+j],TRUE)
					#offsets
					offsetIni=k*linesToRead+1
				 	offsetFin=offsetIni+linesToRead-1
				 	#no rebasar fin de archivo
				 	if (offsetFin>rows) offsetFin=rows
				 	#lee bloque de la imagen SIN cargarla completamente en memoria 
				 	Strip=aux[offsetIni:offsetFin,]
				 	#cerrar conexion
				 	GDAL.close(aux)
				 	#anexar como columna al dataframe de salida    
				 	outdf=cbind(outdf,Strip$band1)
				} 
				#actualizo progressbar		
				if (!silent) setTxtProgressBar(pb, k*(i+1))

				outdf=outdf[,-1]
				switch (fun,
					MEDIAN= {dat=c(dat,rowMedians(outdf))},
					RANGE = {dat=c(dat,rowRanges(outdf))},
					SD = {dat=c(dat,rowSds(outdf))},
					VAR= {dat=c(dat,rowVars(outdf))},
					COUNT ={dat=c(dat,rowCounts(outdf))}
				)					
			}

			aux=readGDAL(inFl[1],silent=TRUE)		
			dat=dat[-1]
			aux$band1=dat
			writeGDAL(aux,outFl[i+1],...)
		}
	}
	if (!silent) close(pb)
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
solarRad = function (img, day=15) {
	
	#comprobar que sea latlong
	#	
	DTOR=0.0174533 #cte de grados a radianes

	nRow=img@grid@cells.dim[2]
	nCol=img@grid@cells.dim[1]
	rowSize=img@grid@cellsize[2]
	lowerY=img@coords[3]
	lat=as.matrix(img)

	lat[1:nCol,]=rep((1:nRow*rowSize)+lowerY,each=nCol) #imagen donde cada pixel tiene el valor de su coord Y 
	
	lat=lat*DTOR
	
	dia=2*pi/365*day
      DST=1.00011+0.034221*cos(dia)+0.00128*sin(dia)+0.000719*cos(2*dia)+0.000777*sin(2*dia)
      DEC=0.006918-0.399912*cos(dia)+0.070257*sin(dia)-0.006758*cos(2*dia)+0.000907*sin(2*dia)-0.002697*cos(3*dia)+0.00148*sin(3*dia)
      AGH=acos(-tan(lat)*tan(DEC))
      RAD=898*DST*(sin(lat)*sin(DEC)*AGH+cos(lat)*cos(DEC)*sin(AGH))
 	
	img$band1=rev(as.vector(RAD))

	img	
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
solarRad12M = function (img, outFl, ...) {

	if (length(outFl)!=12) stop('Fl must be 12 file names')

	# valor juliano del dia 15 de cada mes
	DDA=c(15,45,75,106,136,167,197,228,259,289,320,350)

	for (i in 1:12) writeGDAL(solarRad(img,DDA[i]),outFl[i],...)
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################

petHgsm= function(Tmin, Tmax, Tmed, Rad, month){

	DiasMes=c(31,28,31,30,31,30,31,31,30,31,30,31)

	img=Tmed
	img$band1=0.0023*Rad$band1*0.01708*((Tmax$band1-Tmin$band1)^0.5)*(Tmed$band1+17.8)*DiasMes[month]
	img
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
batchPetHgsm= function(date,Tmin,Tmax,Tmed,Rad,outFl,...) {
	# comprobacion de los parametros
	if (length(date)!=2) stop('date parameter like: date=c(2001,1)')
	if (length(Tmax)==length(Tmin) && length(Tmax)==length(Tmed)){
		meses=length(Tmax)
	}
	else stop('Tmax,Tmin,Tmed must be of the same length')

	DiasMes=c(31,28,31,30,31,30,31,31,30,31,30,31)
	NumMes=c('00','01','02','03','04','05','06','07','08','09','10','11','12')


	for (i in 1:meses){
		#Asignamos Mes y year
      	if (date[2] >12) {
	      	date[2]=1
		      date[1]=date[1]+1
      	}

	      #Read de los ficheros
	  	tx=readGDAL(Tmax[i])
	  	tn=readGDAL(Tmin[i])
	  	tm=readGDAL(Tmed[i])

		j=ifelse(i%%12>0,i%%12,12) #adecuar indice i a indice de rad que va de 1 a 12
	  	rd=readGDAL(Rad[j])

		#Calculo Etp mes i
		img=petHgsm(Tmin=tn,Tmax=tx,Tmed=tm,Rad=rd,month=date[2])

	      writeGDAL(img, outFl[i],...)

       	#Pasamos al mes siguiente
	      date[2]=date[2]+1
    }
}


###############################################
#
# NAME: rueObsMe
# PURPOSE:
#     Reads n ndvi files and n rainfall grid files
#     Then Calculate the RueObsMed grid.
#
# INPUTS:
#       rainFl: File names list of rainfall grid 
#       viFl: File names list of vegetation index grid 
#       silent: logical Flag; if TRUE, comments outputs are supressed

# OUTPUTS:
#       Return RueObsMe as a SpatialGridDataFrame/SpatialPixelDataframe class. 

rueObsMe = function(rainFl, viFl, silent=FALSE) {

    if (length(rainFl)!=length(viFl)) stop('rainFl & viFl must have the same length')
    if (length(rainFl)%%12!=0) stop('rainFl must be multiply of 12')	
    if (length(rainFl)<12) stop('rainFl length must be greater or equal than 12')

    #Extraer el numero de elementos del array 
    n=length(rainFl) #num de meses en la serie
    nah=floor(n/12)    #num de years hidrologicos

    if (!silent) {
	print(paste('Procesing rueObdMe for', nah, 'years'))
	pb =txtProgressBar(min=0,max=n,char='*',width=20,style=3)
    }

    RueMed=0 

    # Para cada year hidrologico
    for (i in 0:(nah-1)) {	  
        SumRain=0
        SumNdvi=0
        #Para cada mes del year hidrologico
        for (j in 1:12){
            SumRain = SumRain+readGDAL(rainFl[i*12+j],silent=TRUE)$band1
            SumNdvi = SumNdvi+readGDAL(viFl[i*12+j],silent=TRUE)$band1
		if (!silent) setTxtProgressBar(pb, i*12+j)
	  }
        SumNdvi=SumNdvi/12
        RueMed=RueMed+(SumNdvi/SumRain)
    }
    RueMed=RueMed/nah
    aux=readGDAL(rainFl[1],silent=TRUE)
    aux$band1=RueMed
    if (!silent) close(pb)
    print(summary(aux))
    aux
}


##########################################################################
#
# NAME: GIaMed
#
# PURPOSE:
#     Reads n ndvi files and n rainfall files
#     Then Calculate the RUEMAX(ndvi_1..ndvi_n, rainfall_1..rainfall_n, m, errorfile) functiones with m#index
#
# INPUTS:
#       FilesArr: array [0..n#1] of String with name of IDRISI RST input files
#
#                 FilesArr[0]     # The no values mask filename
#                 FilesArr[1,12]  # The 12 precipitationes filename
#                 FilesArr[13,24] # The 12 evapotransipirationes files
#
# OUTPUTS:
#       Return Rue Maximun map,

aiObsMe=function (rainFl, petFl, FAO=FALSE, silent=FALSE){

    if (length(rainFl)!=length(petFl)) stop('rainFl & petFl must have the same length')
    if (length(rainFl)%%12!=0) stop('rainFl must be multiply of 12')	
    if (length(rainFl)<12) stop('rainFl length must be greater or equal than 12')

    #Extraer el numero de elementos del array 
    n=length(rainFl) #num de meses en la serie
    nah=floor(n/12) #num de years hidrologicos

    if (!silent){
	print(paste('Processing aiObsMe for', nah, 'years'))
	pb =txtProgressBar(min=0,max=n,char='*',width=20,style=3)
    }
    IaMed=0

    # Para cada year hidrologico
    for (i in 0:(nah-1)){
       SumRain=0
       SumPet=0
       #Para cada mes del year hidrologico
       for (j in 1:12){
           SumRain = SumRain+readGDAL(rainFl[i*12+j],silent=TRUE)$band1
           SumPet  = SumPet +readGDAL(petFl[i*12+j],silent=TRUE)$band1
	     if (!silent) setTxtProgressBar(pb, i*12+j)
       }

	 if (FAO) {
	      if (sum(SumPet  == 0, na.rm=TRUE) > 0) SumPet =SumPet +(SumPet  == 0) # cambio los pixels a 0mm de Pet acumulada por 1mm
            IaMed=IaMed+(SumRain/SumPet)
	 } else {
       	if (sum(SumRain == 0, na.rm=TRUE) > 0) SumRain=SumRain+(SumRain == 0) # cambio los pixels a 0mm de lluvia acumulada por 1mm
            IaMed=IaMed+(SumPet/SumRain)
	 }
    }

    IaMed=IaMed/nah
    aux=readGDAL(rainFl[1])
    aux$band1=IaMed
    if (!silent) close(pb)
    print(summary(aux))
    aux
}


############################333
# NAME: GRueMax
#
# PURPOSE:
#     Reads m ndvi files, m rainfall files, and a NMA map (numero de meses de acumulacion)
#     Then Calculate the RUEMAX
#
# INPUTS:
#       rainFl: File names list of rainfall grids 
#       viFl: File names list of vegetation index grids
#       preRainFl:  File names list of previous rainfall grids 
#       Nma: cte de acumulacion
#       silent: logical Flag; if TRUE, comments outputs are supressed
#
#
#
#
#
# OUTPUTS:
#       Return Rue Maximun map, and the Rue Max Month map

rueObsEx = function (rainFl, viFl, preRainFl, nMonths=6, silent=FALSE){

    if (length(rainFl)!=length(viFl)) stop('rainFl & viFl must have the same length')
    if (length(rainFl)%%12!=0) stop('rainFl must be multiply of 12')	
    if (length(rainFl)<12) stop('rainFl length must be greater or equal than 12')
    if (is.character(nMonths)) {
	faux=readGDAL(nMonths,silent=TRUE)
	m=faux$band1
	nMonths=max(m)
    } else {
	faux=readGDAL(rainFl[1],silent=TRUE)
	faux$band1=nMonths
	m=faux$band1
    }

    if (length(preRainFl)<nMonths) stop(paste('preRain length mus be at least ',nMonths,'elements'))

    #Extraer el numero de elementos del array 
    n=length(rainFl) #num de meses en la serie
    nah=floor(n/12) #num de years hidrologicos

    RainFifo=0 
    NdviMax=0
    NdviMaxMon=0
    Aux=0
    RueMax=0    

    # Calcula NdviMax y NdviMaxMonth
    if (!silent) {
       print('Processing vegetation index files')
       pb =txtProgressBar(min=1,max=n,char='*',width=20,style=3)
    } 
    for (i in 1:n) {
        Aux=readGDAL(viFl[i],silent=TRUE)$band1
        NdviMax=pmax(Aux,NdviMax)
        Msk=(Aux == NdviMax)
        NdviMaxMon=NdviMaxMon*(!Msk)+i*Msk #Mes indicado de 0 a n-1
        if (!silent) setTxtProgressBar(pb, i)
    }
    if (!silent) close(pb) 
    if (!silent) {
	    print('Processing rain files')
	    pb =txtProgressBar(min=1,max=nMonths,char='*',width=20,style=3)
    }
    #relleno fifo con los nMonths primeros meses    
    for (i in 1:nMonths) {
	 RainFifo=cbind(RainFifo,readGDAL(preRainFl[i],silent=TRUE)$band1)
       setTxtProgressBar(pb, i)
    }
    RainFifo=RainFifo[,-1] #eliminamos primera columna de 0s
    if (!silent) close(pb)

    if (!silent) {
      print(paste('Processing rueObsEx for', nah, 'years'))
      pb =txtProgressBar(min=1,max=n,char='*',width=20,style=3)
    }
#browser()
    #for each month in the serie
    for (i in 1:n) {
        #Calculate the mask of NdviMaxMon for month i
        #pixeles donde el ndvi maximo se obtuvo en el mes numero i
        MskMonth=(NdviMaxMon == i)       
        # Para cada valor de los nMonths posibles
        SumRain=0
        for (j in 1:nMonths) {
           #Calcular imagenes de lluvia acumulada en j meses
           SumRain=SumRain+RainFifo[,nMonths+1-j] #lo acumulamos
           #Calcula mascara de pixeles con m=j
           MsknMonths=(m == j)#
           if (sum(MsknMonths)>0) {
               #Calcular RueMax
               RueMax=RueMax*(!MskMonth)+(NdviMax/SumRain)*MsknMonths*MskMonth #overlay RueMax
           }
        }
	  #Desplazar Fifo e insertar mes
        RainFifo=cbind(RainFifo[,-1],readGDAL(rainFl[i],silent=TRUE)$band1)
        if (!silent) setTxtProgressBar(pb, i)
    }

    faux$band1=RueMax
    close(pb)
    print(summary(faux))
    faux
}

# NAME:
#       GIAMax
#
# PURPOSE:
#     Reads n ndvi files and n rainfall files
#     Then Calculate the RUEMAX(ndvi_1..ndvi_n, rainfall_1..rainfall_n, m, errorfile) functiones with m-index
#
# INPUTS:
#       FilesArr: array [0..n-1] of String with name of IDRISI RST input files
#
#                 FilesArr[0]     - The no values mask filename
#                 FilesArr[1,12]  - The 12 precipitationes filename
#                 FilesArr[13,24] - The 12 evapotransipirationes files
#
# OUTPUTS:
#       Return Rue Maximun map,

aiObsEx <-
function (rainFl, viFl, petFl, preRainFl, prePetFl, FAO=FALSE, nMonths=6, silent=FALSE) {
    if (length(rainFl)!=length(viFl)) stop('rainFl & viFl must have the same length')
    if (length(rainFl)%%12!=0) stop('rainFl must be multiply of 12')	
    if (length(rainFl)<12) stop('rainFl length must be greater or equal than 12')
    if (is.character(nMonths)) {
faux=readGDAL(nMonths,silent=TRUE)
m=faux$band1
nMonths=max(m)
    } else {
faux=readGDAL(rainFl[1],silent=TRUE)
faux$band1=nMonths
m=faux$band1
    }

    if (length(preRainFl)<nMonths) stop(paste('preRain length mus be at least ',nMonths,'elements'))

    #Extraer el numero de elementos del array 
    n=length(rainFl) #num de meses en la serie
    nah=floor(n/12) #num de years hidrologicos

    RainFifo=0 
    PetFifo=0  
    viMax=0
    viMaxMon=0
    Aux=0
    IaMax=0

    # Calcula NdviMax y NdviMaxMonth
    if (!silent) {
    print('Processing vegetation index files')
    pb =txtProgressBar(min=1,max=n,char='*',width=20,style=3)
    }
    for (i in 1:n) {
        Aux=readGDAL(viFl[i],silent=TRUE)$band1
        viMax=pmax(Aux,viMax)
        Msk=(Aux == viMax)
        viMaxMon=viMaxMon*(!Msk)+i*Msk #Mes indicado de 0 a n-1
        if (!silent) setTxtProgressBar(pb, i)
    }
    if (!silent) close(pb) 
 
    if (!silent) print('Processing data')
    pb =txtProgressBar(min=1,max=nMonths,char='*',width=20,style=3)
    #relleno fifo con los nMonths primeros meses    
    for (i in 1:nMonths) {
 RainFifo=cbind(RainFifo,readGDAL(preRainFl[i],silent=TRUE)$band1)
 PetFifo=cbind(PetFifo,readGDAL(prePetFl[i],silent=TRUE)$band1)
 if (!silent) setTxtProgressBar(pb, i)
    }
    RainFifo=RainFifo[,-1] #eliminamos primera columna de 0s
    PetFifo=PetFifo[,-1] #eliminamos primera columna de 0s
    if (!silent) close(pb)

    if (!silent) {
      print(paste('Processing aiObsEx for', nah, 'years'))
      pb =txtProgressBar(min=1,max=n,char='*',width=20,style=3)
    }
    #for each month in the serie
    for (i in 1:n) {
        #Calculate the mask of viMaxMon for month i
        #pixeles donde el vi maximo se obtuvo en el mes numero i
        MskMonth=(viMaxMon == i)       
        # Para cada valor de nMonths de los 12 posibles
        SumRain=0
	SumPet=0
        for (j in 1:nMonths) {
           #Calcular imagenes de lluvia acumulada en j meses
           SumRain=SumRain+RainFifo[,nMonths+1-j] #lo acumulamos
           SumPet=SumPet+PetFifo[,nMonths+1-j] #lo acumulamos
           #Calcula mascara de pixeles con nMonths=j
           MsknMonths=(m == j)#
           if (sum(MsknMonths)>0) {
               #Calcular IaMax
               if (FAO) {
                 if (sum(SumPet  == 0, na.rm=TRUE) > 0) SumPet =SumPet +(SumPet  == 0) # cambio los pixels a 0mm de Pet acumulada por 1mm
                 IaMax=IaMax*(!MskMonth)+(SumRain/SumPet)*MsknMonths*MskMonth #overlay RueMax
               } else {
                 if (sum(SumRain == 0, na.rm=TRUE) > 0) SumRain=SumRain+(SumRain == 0) # cambio los pixels a 0mm de lluvia acumulada por 1mm
                 IaMax=IaMax*(!MskMonth)+(SumPet/SumRain)*MsknMonths*MskMonth #overlay RueMax
               }
           }
        }
  #Desplazar Fifo e insertar mes
        RainFifo=cbind(RainFifo[,-1],readGDAL(rainFl[i],silent=TRUE)$band1)
        PetFifo=cbind(PetFifo[,-1],readGDAL(petFl[i],silent=TRUE)$band1)
        if (!silent) setTxtProgressBar(pb, i)
    }
    faux$band1=IaMax
    if (!silent) close(pb)
    print(summary(faux))
    faux
}


###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
regStepRaster=function(ndviFl,timeFl,aridFl,outFl,silent=FALSE,...){
	#comprueba condiciones de error
	if (length(outFl)!=7) stop('outFl may be a list of seven filenames')
	if (2*length(ndviFl)!=(length(timeFl)+length(aridFl))) stop('ndviFl, tempFl and aridFl, should have equal length')
	
	#Stack by pixel the filelists
	tmpFn1=tempfile()
	tmpFn2=tempfile()
	tmpFn3=tempfile()
	tmpFn4=tempfile()	#fichero temporal para almacenar resultados de la regresion paso a paso
	rasterStack(ndviFl,tmpFn1,interleave='BIP')
	rasterStack(timeFl,tmpFn2,interleave='BIP')
	rasterStack(aridFl,tmpFn3,interleave='BIP')

	#image info
	bands=length(ndviFl)	
	rows=GDALinfo(ndviFl[1])[1]
	cols=GDALinfo(ndviFl[1])[2]

	#calculo tamyear del buffer de lectura para que lea bloques de 5000 elementos aproximadamente
	#este es un size optimo para la funcion 'by'
	linesToRead=ceiling(5000/cols)
	#num de bloques de size LinesToRead en la imagen
	nblocks=ceiling(rows/linesToRead) 

	if (!silent) pb =txtProgressBar(min=0,max=nblocks,char='*',width=20,style=3)

	depf=file(tmpFn1,'rb')
	in1f=file(tmpFn2,'rb')	
	in2f=file(tmpFn3,'rb')
	outf=file(tmpFn4,'w')

	#por cada 
	for (i in 1:nblocks) {
		#no sobrepasar fin de fichero
		if (i*linesToRead>rows) {linesToRead=i*linesToRead-rows}
		#leer un linestoread de lineas del fichero de entrada
		 Y=readBin(depf,numeric(),cols*bands*linesToRead,size=4)
		X1=readBin(in1f,numeric(),cols*bands*linesToRead,size=4)
		X2=readBin(in2f,numeric(),cols*bands*linesToRead,size=4)
				
		df=cbind(Y,X1,X2,pixel=rep(1:(linesToRead*cols),each=bands))
		rm(Y,X1,X2)

		#calcular regresion multiple
		cn=regStepDF(df)

		#escribir salida
		write.table(round(cn,4),append=TRUE,sep='\t',file=outf,col.names=FALSE,row.names=FALSE)
		#actualizo progressbar
		setTxtProgressBar(pb,i)
	}
	close(depf)
	close(in1f)
	close(in2f)
	flush(outf)
	close(outf)
	close(pb)

	aux1=read.table(tmpFn4,header=FALSE,sep='\t')
	aux2=readGDAL(ndviFl[1],silent=TRUE)
	print('writing output files')
	for (i in 1:7) {
		aux2$band1=aux1[,i]
		writeGDAL(aux2,outFl[i],drivername='RST',mvFlag=0)
	}	
	file.remove(tmpFn1)	
	file.remove(tmpFn2)	
	file.remove(tmpFn3)	
	file.remove(tmpFn4)	
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
rasterStack=function(inFl,outFN,asc=FALSE,zip=FALSE,dec=3,interleave='BIP',silent=FALSE){

	#comprueba condiciones de error
	if (length(inFl)<2) stop('inFl must be at least of length 2')
	if (nchar(outFN)==0) stop('Empty output file name')
	if (interleave %in% c('BIL','BIP','BSQ') == FALSE) stop('Invalid interleave. Valid are BIL, BIP, BSQ')
    	if ((interleave %in% c('BIL','BSQ')) & (!missing(asc) || !missing(zip) || !missing(dec))) { 
		asc=FALSE
		zip=FALSE
		dec=3
		print('asc,zip,dec options are ignored in interleave mode BIL or BSQ')
	}

	#num de imagenes en la lista 
	nimg=length(inFl)

 	#num de filas y columnas de la imagen
 	rows=GDALinfo(inFl[1])[1]
 	cols=GDALinfo(inFl[1])[2]

	#calculo size del buffer de lectura para que lea bloques de 100MB
	linesToRead=ceiling(100000000/(cols*nimg*8))

	#num de bloques de size linesToRead en la imagen
	nblocks=ceiling(rows/linesToRead)
	
	#inicio progressbar
	if (!silent) pb=txtProgressBar(min=0,max=nblocks*nimg,char='*',width=20,style=3)
	
	#abrir fichero de salida en modo write + (texto, texto comprimido o binario)
	if (asc) { 
		if (zip) {
			ft=gzfile(outFN,'wt')
	 	} else ft=file(outFN,'wt')
	} else ft=file(outFN,'wb')

	if (interleave=='BSQ') {
		stop('sorry, not implemented yet...')
	}  else {

	#leemos lineToRead (ej. 200 lineas) de cada imagen 
	#anexamos cada bloque leido a la columna de un dataframe 
 	#escribimos en disco el datafame con la opcion de anexar 
 	#el proceso se repite para los nblocks necesarios
 	for (j in 0:(nblocks-1)){
		#iniciamos dataframe de salida
 	 	outdf=0
	 	for (i in 1:nimg){
	  		aux=GDAL.open(inFl[i],TRUE)  
		 	#offsets
			offsetIni=j*linesToRead+1
		 	offsetFin=(j+1)*linesToRead
			#no rebasar fin de archivo
			if (offsetFin>rows) offsetFin=rows
			#lee bloque de la imagen SIN cargarla completamente en memoria 
			Strip=aux[offsetIni:offsetFin,]
			#cerrar conexion
			GDAL.close(aux)
			#anexar como columna al dataframe de salida    
			outdf=cbind(outdf,Strip$band1)  
			#actualizo progressbar
			if (!silent) setTxtProgressBar(pb, i+nimg*j)
		} #for
		#convertir outdf a vector, y darle la forma adecuada para guardarlo en disco
		outdf=outdf[,-1]
		if (asc) {
			outdf=round(outdf,dec)
			write.table(outdf,ft,row.names=FALSE,col.names=FALSE,sep=',')
		} else {
			if (interleave=='BIL') writeBin(as.vector(as.matrix(outdf)),ft,size=4)
			if (interleave=='BIP' & !asc) writeBin(as.vector(t(as.matrix(outdf))),ft,size=4)
		}
	} #for
	close(ft)
	close(pb)
	}
}


###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
regStepDF=function (X){
	dimX=dim(X)[1]
	cols=max(X[,4])
	#quitar casos con algun NA
	#--------------MIRAR-------------esto puede originar que algun pixel pierda alguna banda, pero no todas!!!
	X=na.omit(X)
	ndimX=dim(X)[1]
	index=unique(X[,4])
	#si todo el dataframe es iniutilizable
	if (ndimX==0) return(matrix(NA,dimX,6))

	#num de bandas en la matriz
	N=sum(X[,4]==X[,4][1])
	ncases=ceiling(ndimX/N)

	#print(paste('dimX:',dimX,' dimXna:',ndimX,' ncases:',ncases))

	# Correlaciones simples entre variables segun Box 15.2
	aux=(unlist(by(X[,1:3],X[,4],cor)))
	dim(aux)=c(9,ncases)
	RX=t(aux[c(6,2,3),])
	#colnames(RX)=c('Rx1x2','Rx1Y','Rx2Y')

	# Coeficientes standard de regresion parcial
	# con ecuacion YP=BPY1*X1P+BPY2*X2P

	MBPY1=(RX[,2]-RX[,3]*RX[,1])/(1-RX[,1]^2)
	MBPY2=(RX[,3]-RX[,2]*RX[,1])/(1-RX[,1]^2)

	# Estadisticos de significacion de correlacion simple segun Box 15.4 con N<50
	DFS = N - 2
	MTS = RX * sqrt(DFS / (1 - RX^2))
	
	# Coeficiente de determinacion multiple
	MR2Y12 = RX[,2] * MBPY1 + RX[,3] * MBPY2
	
	# Estadistico de significacion de la regresion multiple con 2 variables independientes
	DFNUM = 2
	DFNUM2= 1
	DFDEN = N - 3
	MF = (MR2Y12 / 2) / ((1 - MR2Y12) / DFDEN)

	# Incremento en determinacion al meter la segunda variable
	ARX=abs(RX)
	MRXY=ifelse(ARX[,2]>ARX[,3],RX[,2],RX[,3])
	MV2 =ifelse(ARX[,2]>ARX[,3],2,1)
	MF2 = (MR2Y12 - MRXY^2) / ((1 - MR2Y12) / DFDEN)

	# Distribuciones de Probabilidad
	MPrR2Y12 = 1-pf(MF,DFNUM,DFDEN)
	MPrR2Y12i= 1-pf(MF2,DFNUM2,DFDEN)

	MPr = 2*(1-pt(abs(MTS),DFS))

	#MPrX1X2 = 2*(1-pt(abs(MTS[,1]),DFS))
	#MPrX1Y  = 2*(1-pt(abs(MTS[,2]),DFS))
	#MPrX2Y  = 2*(1-pt(abs(MTS[,3]),DFS))

	#matriz de salida del efecto de la aridez y el tiempo q[,1] tiempo, q[,2] aridez, q[,3] respuesta de la vegetacion (1,2,3 o 4)
	q=matrix(-1,ncases,7) #casos sin NA
	nq=matrix(NA,cols-ncases,7) #casos con NA
	colnames(q) =c('index','efect_time','efect_arid','vege_response','ta_simple','tv_simple','av_simple')
	colnames(nq)=c('index','efect_time','efect_arid','vege_response','ta_simple','tv_simple','av_simple')

	#indetifica el pixel al que peretenece el resultado
	q[,1]=index
	nq[,1]=(1:cols)[-index]
	# wi almacena los indices de los casos donde se cumple la condicion...
	# 1a condicion 
	wi=which((MPrR2Y12<=0.1) & (MPrR2Y12i<=0.1))
	q[wi,2:3]=cbind(MBPY1[wi],MBPY2[wi])
	# 2a condicion 
	wi=c(which((MPrR2Y12<=0.1) & (MPrR2Y12i>0.1) & (MV2==2)),which((MPrR2Y12>0.1) & (MPr[,2]<=0.1)))
	q[wi,2:3]=cbind(RX[wi,2],0)
	# 3a condicion 
	wi=c(which((MPrR2Y12<=0.1) & (MPrR2Y12i>0.1) & (MV2==1)),which((MPrR2Y12>0.1) & (MPr[,2]>0.1) & (MPr[,3]<=0.1)))
	q[wi,2:3]=cbind(0,RX[wi,3])
	# 4a condicion 
	wi=which(((MPrR2Y12>0.1) & (MPr[,2]>0.1) & (MPr[,3]>0.1)))
	q[wi,2:3]=cbind(0,0)
	# calculo de la respuesta de la vegetacion (rv) en funcion del efecto del tiempo (et) y la aridez (ea)
	#  et  ea  rv
	# no 0  0-> 1
	# no 0 no 0 -> 3
	#  0 no 0 -> 2
	#  0  0-> 4
	
	q[,4]=ifelse(q[,2]!=0,ifelse(q[,3]==0,1,3),ifelse(q[,3]==0,4,2))
	
	# calculo de variables mostrando el efecto simple
	q[,5]=ifelse(MPr[,1]<=0.1,RX[,1],0)
	q[,6]=ifelse(MPr[,2]<=0.1,RX[,2],0)
	q[,7]=ifelse(MPr[,3]<=0.1,RX[,3],0)

	q=rbind(q,nq)
	q=q[order(q[,1]),]
	# ----------vector de resultados
	return(q)

	# ----------devuelve vector completo para debug
	#round(c(R12, R1Y, R2Y, DFS, TS12, TS1Y, TS2Y, PrX1X2,PrX1Y,PrX2Y,BPY1, BPY2, BY1, BY2, A, R2Y12, DFNUM, DFDEN, F,PrR2Y12, V2, DFNUM2, F2,PrR2Y12i, DFM, TM1, TM2, TMP1, TMP2),6)
}


