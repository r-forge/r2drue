###############################################
# NAME: r2dRueWiz
# PURPOSE:
#     Read an 2dRue configuration file and perform actions acordely 
#     Caln calculate the monitoring, the assesment, both or none, acordely with the input.
# INPUTS:
#       conf: config file. If none is provided, it will be create through interactive questions to the user 
#       overwrite: Logical flag. When overwrite is TRUE, none of the existing files will be replaced by new outputs, forcing to stop the proccess if necessary. 
#       verbose: level of log detail
# OUTPUTS:
#       r2dRue wiz is a procedure that can produced all the output involved in r2dRue analisys.
#		It will be created a log file named as the config file but with its extension turned to .log
r2dRueWiz = function(conf='', overwrite=FALSE, verbose=0) {
	if (conf=='')  {
		TmpConfig=create2dRueConfigFile()
		r2dRueWiz(TmpConfig,overwrite,verbose)
	} 
	else {				
		o=createRunConfig(conf)
		showInfo(o)
		switch(o$acction,
				'm' = monitoring(o),
				'a' = assesment(o),
				'b' = {	assesment(o)
					monitoring(o)
				})
		Save(log,conf.log)
	}
}

###############################################
# NAME: assestment
# PURPOSE: Lee un fo
# INPUTS:
# OUTPUTS:
showInfo=function (o) {
	#print info
	aux='\n'
	aux=c(aux,'\n',sprintf('################### r2dRue RUN: %s',o$comment))
	aux=c(aux,'\n',sprintf('Original data: %d images, from %s to %s',o$sLength,format(o$sIniDate,'%b/%Y'),format(o$sEndDate,'%b/%Y')))
	aux=c(aux,'\n',sprintf('Analisys data: %d images, from %s to %s, %d Hidrological years starting at %s',o$rLength,format(o$rIniDate,'%b/%Y'),format(o$rEndDate,'%b/%Y'),o$rYears,month.name[o$mHidro]))
	aux=c(aux,'\n',sprintf('               %d acummulated months, %d preimages from  %s to %s',o$acum, length(o$ppet),format(o$rPreDates[1],'%b/%Y'),format(o$rPreDates[length(o$ppet)],'%b/%Y') ))
	aux=c(aux,'\n\n',sprintf('---------- Raster Group Info'))
	aux=c(aux,'\n',sprintf('viRgf     : %s',attr(o$vi,'path')))
	aux=c(aux,'\n',sprintf('            %s ...',paste(head(attr(o$vi,'files'),10),collapse=' ')))
	aux=c(aux,'\n',sprintf('rainRgf   : %s',attr(o$rain,'path')))
	aux=c(aux,'\n',sprintf('            %s ...',paste(head(attr(o$rain,'files'),10),collapse=' ')))
	aux=c(aux,'\n',sprintf('petRgf    : %s',attr(o$pet,'path')))
	aux=c(aux,'\n',sprintf('            %s ...',paste(head(attr(o$pet,'files'),10),collapse=' ')))
	aux=c(aux,'\n',sprintf('preRainRgf: %s',attr(o$prain,'path')))
	aux=c(aux,'\n',sprintf('            %s ...',paste(head(attr(o$prain,'files'),10),collapse=' ')))
	aux=c(aux,'\n',sprintf('prePetRgf : %s',attr(o$ppet,'path')))
	aux=c(aux,'\n',sprintf('            %s ...',paste(head(attr(o$ppet,'files'),10),collapse=' ')))
	aux=c(aux,'\n\n',sprintf('---------- Spatial Info'))
	aux=c(aux,'\n',sprintf('cols: %d  rows: %d  res: %f  proj: %s',o$gdal[1],o$gdal[2],o$gdal[6],attr(o$gdal,'projection')))	
	aux=c(aux,'\n',sprintf(''))
	cat(aux)	
}

###############################################
# NAME: assestment
# PURPOSE: Lee un fo
# INPUTS:
# OUTPUTS:
readConfigFile=function (conf) {	
	co=list(
			comment='',pOut='',mHidro='',acum='',viRgf='',rainRgf='',sYear='',sMonth='',
			yIni='',yEnd='',driver='',flag='',acction='',
			petRgf='',tmaxRgf='',tminRgf='',tmedRgf='',radRgf=''	
	)
	obligatorios=names(co)[1:13]
	obligatoriosPet=names(co)[15:17]
	f=readIniFile(conf)
	co[names(f)]=f
	if (any(co[obligatorios] == '')) stop(sprintf('Faltan parametros obligatorios en %s , check the conf file',conf))
	if (co$petRgf == '') {
		if (any(co[obligatoriosPet] == '')) stop(sprintf('Faltan parametros obligatorios para calculo de PET. Check the %s file',conf))
	}
	co
}

###############################################
# NAME: createRunConfig
# PURPOSE:
#     crea una lista con la informacion necesaria para una ejecucion de 2dRue
# INPUTS:
#     conf: lista de configuracion basica (sin campos calculados) 
# OUTPUTS:
createRunConfig=function (conf){
	#campos forzados a enteros
	enteros=c('mHidro','acum','sYear','sMonth','yIni','yEnd','flag')
	#definicion inicial
	o=list(
		#from config file
		comment='',pOut='',mHidro='',acum='',viRgf='',rainRgf='',sYear='',sMonth='',
		yIni='',yEnd='',driver='',flag='',acction='',petRgf='',tmaxRgf='',tminRgf='',tmedRgf='',radRgf='',
		#calculated		
		vi='',rain='',pet='',ppet='',prain='',rLength='',rIniDate='',rEndDate='',rDates='',rPreDates='',
		svi='',srain='',spet='',sIniDate='',sEndDate='',sDates='',sLength='',
		stmax='',stmin='',stmed='',srad='',
		#calculated spaciales
		gdal=''
	)
	
	#read fields from file
	co=readConfigFile(conf)
	o[names(co)]=co #copy fields from config file	
	o[enteros]=as.integer(co[enteros]) #pasar a enteros	
	
	#read rgf files
	o$svi=rgf.read(o$viRgf)
	o$srain=rgf.read(o$rainRgf)
	o$spet=rgf.read(o$petRgf)
	
	#calculate dates of the elements in the serie 
	o$sLength=length(o$svi)
	o$sIniDate=as.Date(paste(o$sYear,o$sMonth,1,sep='/'))
	o$sDates=seq(o$sIniDate,length.out=o$sLength,by='month')
	o$sEndDate=o$sDates[o$sLength]
		
	o$rYears=o$yEnd-o$yIni+1
	o$rLength=o$rYears*12	
	o$rIniDate=as.Date(paste(o$yIni,o$mHidro,1,sep='/'))
	o$rDates=seq(o$rIniDate,length.out=o$rLength,by='month')
	o$rEndDate=o$rDates[o$rLength]
		
	o$rPreDates=sort(seq(o$rIniDate,length.out=o$acum+1,by='-1 month')[-1])

	if (!all(o$rDates %in% o$sDates)) stop('check start and initial dates')
	if (!all(o$rPreDates %in% o$sDates)) stop('check start and initial dates')
	
	#calculate vi,rain,pet,ppet and prain series
	o$vi=o$svi[o$sDates %in% o$rDates]
	o$rain=o$srain[o$sDates %in% o$rDates]
	o$pet=o$spet[o$sDates %in% o$rDates]
	o$ppet=o$spet[o$sDates %in% o$rPreDates]
	o$prain=o$srain[o$sDates %in% o$rPreDates]
	
	#calculate atributes
	attr(o$vi,'path')=dirname(o$vi[1])
	attr(o$rain,'path')=dirname(o$rain[1])
	attr(o$pet,'path')=dirname(o$pet[1])
	attr(o$ppet,'path')=dirname(o$ppet[1])
	attr(o$prain,'path')=dirname(o$prain[1])
	attr(o$vi,'files')=basename(o$vi)
	attr(o$rain,'files')=basename(o$rain)
	attr(o$pet,'files')=basename(o$pet)
	attr(o$ppet,'files')=basename(o$ppet)
	attr(o$prain,'files')=basename(o$prain)	
	
	if (o$petRgf == '') {
		
	}
	
	o$gdal=GDALinfo(o$vi[1],silent=TRUE)	
	
	#show
	showInfo(o)
	#return	
	o	
}

###############################################
# NAME: create2dRueConfig
# PURPOSE:
# INPUTS:
# OUTPUTS:
# Return the name of a 2dRue .conf file 
create2dRueConfigFile = function(conf='') {
	text="##########################################\n############# r2dRue Wizard ##############\n#                                        #\nR2dRue Wizard te permite crear estrsya ñldkfj gñdlkfj gñsdkfj gñsdkfj gsñdkfj gñsldkfj 
       gñsdlkfj gñsdlkfjg ñsdlkfjg ñsdlkfj gñsdlkfj ñslkdj fñglksdj\n\n"
	
	quest=c(
			'comment','Description of this run?:',date(),
			'mHidro','Start month of hydrological year [1-12]?:','',
			'acum','Number of acummulation months for preceding rain:?','',
			'pOut','Output directory:?','',
			'viRgf','Vegetation Index raster group?:','',
			'rainRgf','Precipitation raster group?:','',
			'sYear','Start year of the series?:','',
			'sMonth','Start month of the series?:','',
			'petRgf','PET raster group or leave blank to create it?: ','',
			'tmaxRgf','Tmax raster group?:','',
			'tmedRgf','Tmed raster group?:','',
			'tminRgf','Tmin raster group?:','',
			'radRgf','Extraterrestial solar radiation raster group?:','',
			'yIni','Start year of this run?:','',
			'yEnd','End year of this run?:','',
			'driver','GIS format for output images','RST',
			'flag','Missing value','-999',
			'acction','Proceed with monitoring(m), assesment(a), both(b), none(n)?:','b',
			'fileName','Name of this parameter file?:', paste('rue',format(Sys.time(), "%Y%m%d%H%M"),'.conf',sep='')
	)
	quest=matrix(quest,ncol=3, byrow=TRUE, dimnames=list(NULL,c('var','question','defaultvalue')))
	response=as.data.frame(t(quest[,3]),stringsAsFactor=F)
	names(response)=quest[,1]		
	nq=nrow(quest)
	
	#err=try(rue=readIniFile(conf),TRUE)
	#if (class(err)=='try-error') {stop('generer nuevo')}
	cat(rep('\n',times=200))
	cat(text)
	#writeLines(strwrap(text,60))
	flush.console()
	for (i in 1:nq) {
		aux=readline(paste(quest[i,2],quest[i,3]))
		if (aux!='') response[i]=aux
	}
		
	conffile=as.vector(response$fileName)	
	write.table(t(response[-nq]),conffile,sep='=',quote=F,col.names=F)
	conffile	
}

###############################################
# NAME: Initial
# PURPOSE:
#     Read an 2dRue configuration file and prepare the enviroment to run it and check the inputs.
#     
# INPUTS:
#       conf: config file. 
# OUTPUTS:
#       boolean TRUE if all the options ar valid and the analisys can be done.
#		FALSE if any options is erroneus or incongruent
initial=function (filename){
	
	checkgroup=function(rgffile){		
		g=rgf.read(rgffile)
		cat('CHECKING ',rgffile,'\nHead of',rgffile,'\n',head(g),'...', fill=TRUE)
		return(g)
		pb =txtProgressBar(min=0,max=length(g),char='*',width=20,style=3)
		aux=GDALinfo(g[1],silent=TRUE)		
		for (i in g) { 
			aux2=GDALinfo(i,silent=TRUE)
			if (any(aux!=aux2)) stop(paste('incongruent series in file',i))
			setTxtProgressBar(pb, getTxtProgressBar(pb)+1)			
		}		
		close(pb)
		cat('\n')
		g
	}
	
	checkgroups=function(rgffile1,rgffile2){		
		g1=rgf.read(rgffile1)
		g2=rgf.read(rgffile2)
		cat('CHECKING spatial and time congruency',rgffile1,rgffile2,'\n')
		aux1=GDALinfo(g1[1],silent=TRUE)
		aux2=GDALinfo(g2[1],silent=TRUE)
		#TODO: checkear no solo col&row si no el resto de parametros 
		if ((any(aux1[1:2]!=aux2[1:2])) || (length(aux1)!=length(aux2))) {stop('incongruent series')}		
	}
	
	buildpet=function(o){				
		checkgroup(o$tmin)
		checkgroup(o$tmax)
		checkgroup(o$tmed)
		checkgroups(o$tmin,o$tmax)
		checkgroups(o$tmin,o$tmed)
		if (o$radGroup!='') {rad=rgf.read(o$radGroup)}
		else {
			rad=paste(o$pout,'/rad/rad',1:12,'.',o$driver)
			img=readGDAL(tmin[1])
			solarRad12M(img,aux,drivername=o$driver,mvFlag=o$flag)
		}
		batchPetHgsm(o$mesini,tmin,tmax,tmed,rad)
	}

	
	########################## Initial Start
	f=readConfigFile(filename)
	o=list(pOut='',yIni=0,yEnd=0,acum=0,vi='',rain='',pet='',ppet='',prain='',driver='',flag=0)	
	#check / in paths
	#	f=as.data.frame(t(sub('/$','',f[1,])),stringsAsFactors=FALSE) #quitar / de cualquier cadena que lo tuviera
	#check output directory		
	if (!file_test('-d',f$pOut)) stop('Output directory not accesible ',f$pOut)
	else o$pOut=f$pOut 
	#check meshidro
	if (!(f$mHidro %in% 1:12)) stop('mes hidro must be between 1:12')
	else f$mHidro=as.integer(f$mHidro)
	#check nacum
	if (!(f$acum %in% 1:12)) stop('nacum must be between 1:12')
	else o$acum=as.integer(f$acum)
	#check sMonth
	if (!(f$sMonth %in% 1:12)) stop('sMonth must be between 1:12')
	else o$sMonth=as.integer(f$sMonth)
	#check sMonth
	if (!(f$sYear %in% 1900:2100)) stop('sYear must be a valid year')
	else o$sYear=as.integer(f$sYear)
	#check driver
	if (isSupportedGDALFormat(f$driver)) o$driver=f$driver
	#check mvFlag is a number
	if (!is.finite(as.numeric(f$flag))) stop('mvFlag not a number')
	else o$flag=as.numeric(f$flag)
	#check ini and end years
	if (!(as.integer(f$yEnd)>=as.integer(f$yIni)+3)) stop('must have at least 3 years to do a regresion')
	else {
		o$yEnd=as.integer(f$yEnd)
		o$yIni=as.integer(f$yIni)
	}
	
	#check that Rain and Vi groups are temporaly and spatialy coherents	
	o$vi=checkgroup(f$viRgf)
	o$rain=checkgroup(f$rainRgf)
	checkgroups(f$viRgf,f$rainRgf)
	rgfLength=length(rgf.read(f$viRgf))
	
	#Create PET directory and build PET if needed
	if (f$petRgf=='') {		
		aux=getwd()
		petDir=paste(o$pOut,'/PET',sep='')
		#crear directorio Pet
		if (file.access(petDir)==-1) {
			if (!dir.create(petDir)) stop('no se puede crear petdir')
		}		
		sDate=as.Date(paste(o$sYear,o$sMonth,1,sep='/'))		
		f$petRgf='petGroup.rgf'
		write(paste(o$pOut,'/PET/pet',format(seq(sDate,length.out=rgfLength,by='months'),'%m%Y'),'.rst',sep=''),f$petRgf)
		#buildpet(o)
	}
	#check that Pet and Vi groups are temporaly and spatialy coherents
	o$pet=checkgroup(f$petRgf)
	#checkgroups(f$viRgf,f$petRgf)
	
	#calculo de los rgf pet,rain,vi 
	HM=f$mHidro
	SY=o$sYear
	SM=o$sMonth
	AYI=o$yIni
	AYE=o$yEnd
	if (HM>=SM) {
		SY=SY+1
		e1=12+HM-SM		
	} else {
		e1=HM-SM
	}
	e2=(AYI-SY)*12	
	i1=e1+e2+1
	i2=i1+(AYE-AYI)*12
	if (e2<0 | i2<0) {stop('error en las series')}	
	o$rain=o$rain[i1:i2]	
	o$pet=o$pet[i1:i2]
	o$vi=o$vi[i1:i2]
	
	#calculo de los rgf ppet,prain
	#calculo de meses previos al periodo de analisis necesarios
	#TODO: ajustar extension a driver
	rgf.summary(o$vi,'maxvi.rst',fun='MAX',drivername=o$driver,mvFlag=o$flag)
	#maxvi=readGDAL('maxvi.rst',silent=TRUE)
	whenvi=rgf.when(o$vi,'maxvi.rst')
	#calculo de meses previos necesarios 
	nmonthsneeded=o$acum-min(whenvi$band1)+1 # ej: 10 - 1 + 1 para 10 meses de acum con maximo VI en primer mes...
	if (nmonthsneeded<0) nmonthsneeded=0	
	i3=i1-nmonthsneeded
	o$prain=o$rain[i3:i1]
	o$ppet=o$pet[i3:i1]
		
	#return
	o
}


###############################################
# NAME: assestment
# PURPOSE:
# INPUTS:
# OUTPUTS:
assesment = function(o) {		
	#Make Indices 
	
	rueMe=rueObsMe(o$rain,o$vi)
	writeGDAL(rueMe,paste(o$pOut,'rueMed.rst',sep=''),drivername=o$driver,mvFlag=o$flag)	
	rueEx=rueObsEx(o$rain,o$vi,o$prain,nMonths=o$acum)
	writeGDAL(rueEx,'rueEx.rst',drivername=o$driver,mvFlag=o$flag)	
	iaMe=aiObsMe(o$rain,o$pet)
	writeGDAL(iaMe,'iaMed.rst',drivername=o$driver,mvFlag=o$flag)	
	iaEx=aiObsEx(o$rain,o$vi,o4prain,o$ppet,nMonths=o$acum)	
	writeGDAL(iaEx,'iaEx.rst',drivername=o$driver,mvFlag=o$flag)	
}



###############################################
# NAME: monitoring
# PURPOSE:
# INPUTS:
# OUTPUTS:
monitoring = function(o) {	
	#MAKE ndvi SUMMARIES BY HIDROLOGIC YEARS
	#TODO: poner bien la extenxion del driver
	
	annualVis=paste(o$pOut,'/vi',o$yIni:o$yEnd,'.',o$driver,sep='')
	rgf.summary(o$vi,annualVis,step=12,fun='MEAN',drivername=o$driver,mvFlag=o$flag)
	
	#make aiObsMed by hidrologic years
	annualIaMed=paste(o$pOut,'/iaMed',o$yIni:o$yEnd,'.',o$driver,sep='')
	n=length(annualIaMed)	
	for (i in 0:(n-1)) {
		etp12=o$pet[(1:12)+i*12]
		rain12=o$rain[(1:12)+i*12]
		aiMe=aiObsMe(rain12,etp12)
		writeGDAL(aiMe,annualIaMed[i+1],drivername=o$driver,mvFlag=o$flag)
	}
	
	#make annual time files
	annualTimes=paste(o$pOut,'/time',o$yIni:o$yEnd,'.',o$driver,sep='')
	aux=readGDAL(annualVis[1])	
	for (i in 0:(n-1)) {
		aux$band1=i+o$yIni
		writeGDAL(aux,annualTimes[i+1],drivername=o$driver,mvFlag=o$flag)
	}
	#make step by step regresion
	aux=c('f1','f2','f3','f4','f5','f6','f7')
	aux=paste(o$pOut,aux,'.',o$driver,sep='')
	regStepRaster(annualVis,annualTimes,annualIaMes,aux,drivername=o$driver,mvFlag=o$flag)
}


###############################################
# NAME: rueObsMe
# PURPOSE:
#     Reads n ndvi files and n rainfall grid files
#     Then Calculate the RueObsMed grid.
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
		print(paste('\nProcesing rueObdMe for', nah, 'years'))
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
	if (!silent){
		close(pb)
		cat('-------')
		print(summary(aux$band1))
	}
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
	plot()
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
# INPUTS:regStepRaster(vi,y,ia,fl,drivername='RST',mvFlag=-1)
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
	browser()
	#calculo size del buffer de lectura para que lea bloques de 5000 elementos aproximadamente
	filelength=bands*rows*cols
	#este es un size optimo para la funcion 'by'
	itemsToRead=trunc(500000/bands)	
	#num de bloques de size itemsToRead
	nblocks=ceiling(filelength/itemsToRead)
	#tamaño del ultimo bloque
	rest=filelength-(nblocks-1)*itemsToRead
	
	if (!silent) pb =txtProgressBar(min=0,max=nblocks,char='*',width=20,style=3)
	
	depf=file(tmpFn1,'rb')
	in1f=file(tmpFn2,'rb')	
	in2f=file(tmpFn3,'rb')
	outf=file(tmpFn4,'w')
	
	#por cada bloque 
	for (i in 1:nblocks) {
		#no sobrepasar fin de fichero
		if (i==nblocks) {itemsToRead=rest}
		#leer un linestoread de lineas del fichero de entrada
		Y=readBin(depf,numeric(),itemsToRead,size=4)
		X1=readBin(in1f,numeric(),itemsToRead,size=4)
		X2=readBin(in2f,numeric(),itemsToRead,size=4)
		
		df=cbind(Y,X1,X2,pixel=rep(1:(itemsToRead/bands),each=bands))
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
regStepDF=function (X){
	dimX=dim(X)[1]
	cols=max(X[,4])
	#quitar casos con algun NA
	aux=unique(X[is.na(X[,1]*X[,2]*X[,3]),4]) #lista de pixel con algun dato a NA
	X[X[,4] %in% aux,1]=NA	
	X=na.omit(X) #esto puede originar que algun pixel pierda alguna banda, pero no todas!!!

	ndimX=dim(X)[1]
	index=unique(X[,4])
	#si todo el dataframe es iniutilizable
	if (ndimX==0) return(matrix(NA,dimX,6))
	
	#num de bandas en la matriz
	N=sum(X[,4]==X[,4][1])
	ncases=length(index)
	
	
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



trace(initial,browser)
r2dRueWiz('rue.conf')
f=readIniFile('rue.conf')
g=readConfigFile('rue.conf')
do=function() {
	trace(createRunConfig,browser) 
	#r2dRueWiz('rue.conf')
	o=createRunConfig('rue.conf')
	print(readLines('rue.conf'))
	cat(sprintf('Original data: %d images, from %s to %s \n',o$sLength,format(o$sIniDate,'%b/%Y'),format(o$sEndDate,'%b/%Y')))
	cat(sprintf('Analisys data: %d images, from %s to %s, %d Hidrological years \n',o$rLength,format(o$rIniDate,'%b/%Y'),format(o$rEndDate,'%b/%Y'),o$rYears))
}


