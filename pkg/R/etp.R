DiasMes=c(31,28,31,30,31,30,31,31,30,31,30,31)

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
###############################################
petHgsm= function(Tmin, Tmax, Tmed, Rad, month){
	img=Tmed
	img$band1=0.0023*Rad$band1*0.01708*((Tmax$band1-Tmin$band1)^0.5)*(Tmed$band1+17.8)*DiasMes[month]
	img
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
# CHANGES: 27/01/2010 - bug en el calculo del indice de Rad (implica reescritura de codigo)
# CHANGES: 20/04/2010 - cambio de parametro date por monthIni
###############################################
batchPetHgsm= function(outFl,monthIni,Tmin,Tmed,Tmax,Rad,...) {
	# comprobacion de los parametros
	if (!(monthIni %in% 1:12)) stop('monthIni must be in 1:12')
	if (length(Tmax)==length(Tmin) && length(Tmax)==length(Tmed)){
		meses=length(Tmax)
	}
	else stop('Tmax,Tmin,Tmed must be of the same length')
	
	if (missing(Rad)) {
		aux=readGDAL(Tmax[1])
		Rad=paste('rad',1:12,'.RST',sep='')
		solarRad12M(aux,Rad,...)
	}
	
	#DiasMes=c(31,28,31,30,31,30,31,31,30,31,30,31)	
	
	#Asignamos mesRad
	mesRad=monthIni
	for (i in 1:meses){
		#Read de los ficheros
		tx=readGDAL(Tmax[i])
		tn=readGDAL(Tmin[i])
		tm=readGDAL(Tmed[i])
		rd=readGDAL(Rad[mesRad])
		
		#Calculo Etp mes i
		img=petHgsm(Tmin=tn,Tmax=tx,Tmed=tm,Rad=rd,month=mesRad)
		
		writeGDAL(img, outFl[i],...)
		
		#Pasamos al mes siguiente
		mesRad=mesRad+1
		if (mesRad>12) mesRad=1	      
	}
}

###############################################
# NAME: 
# PURPOSE:
# INPUTS:
# OUTPUTS:
# CHANGES: 20/04/2010 - bug en el computo de Lat
# CHANGES: 20/04/2010 - cambio en el computo de dia
# CHANGES: 20/04/2010 - quitar valor por defecto de day
# CHANGES: 23/05/2010 - stop si no es latlong
# TODO: si no es latlong proyectar a latlong calcular rad y reproyectar a original
# TODO: Justificar el 898
# TODO: ampliar para calculo simplificado de la FAO
###############################################
solarRad = function (img, day) {	
	
	if (is.projected(img)) stop('Cant calculate extraterrestial radiation over projected images')
	
	DTOR=0.0174533 #cte de grados a radianes
	
	nRow=img@grid@cells.dim[2]
	nCol=img@grid@cells.dim[1]
	rowSize=img@grid@cellsize[2]
	lowerY=img@coords[3] #Y del punto central
	lat=as.matrix(img)
	
	lat[1:nCol,]=rep((0:(nRow-1)*rowSize)+lowerY,each=nCol) #imagen donde cada pixel tiene el valor central de su coord Y 
	
	lat=lat*DTOR
	
	dia=2*pi/365*(day-1)
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
	#TODO: comprobarlo
	DDA=c(15,45,75,106,136,167,197,228,259,289,320,350)
	
	for (i in 1:12) writeGDAL(solarRad(img,DDA[i]),outFl[i],...)
	outFl
}