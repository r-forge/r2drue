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
# TODO: introducir el calculo de RAD en Etp ¿seguro? si, si RAD no esta calcularlo, si esta usarlo
###############################################
batchPetHgsm= function(monthIni,Tmin,Tmax,Tmed,Rad,outFl,...) {
	# comprobacion de los parametros
	if (!(monthIni %in% 1:12)) stop('monthIni must be in 1:12')
	if (length(Tmax)==length(Tmin) && length(Tmax)==length(Tmed)){
		meses=length(Tmax)
	}
	else stop('Tmax,Tmin,Tmed must be of the same length')
	
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
