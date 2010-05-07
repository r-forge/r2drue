library(r2dRue)

#paths
ptmin="J:/D/Trabajo/marieta/Clima Chile/proceso/interpolaciones/ventanaamplia/TMin/grid/"
ptmax="J:/D/Trabajo/marieta/Clima Chile/proceso/interpolaciones/ventanaamplia/TMax/grid/"
ptmed="J:/D/Trabajo/marieta/Clima Chile/proceso/interpolaciones/ventanaamplia/TMed/"
prain="J:/D/Trabajo/marieta/Clima Chile/proceso/interpolaciones/ventanaregion/RAIN/grid/"
pndvi="J:/D/Trabajo/marieta/Clima Chile/proceso/make_ndvi/"
prad="J:/D/Trabajo/marieta/Clima Chile/proceso/rue/etp/rad/"
petp="J:/D/Trabajo/marieta/Clima Chile/proceso/rue/etp/"

#rgf files
tmins=rgf.create(ptmin,'.rst',1982,1995,monthini=5)
tmaxs=rgf.create(ptmax,'.rst',1982,1995,monthini=5)
tmeds=rgf.create(ptmed,'.rst',1982,1995,monthini=5)
rains=rgf.create(prain,'.rst',1982,1995,monthini=5)
ndvi=rgf.create(pndvi,'.rst',1982,1995,monthini=5)
etps=rgf.create('etp','.rst',1982,1995,monthini=5)
prerains=rgf.create(prain,'.rst',1981,1982,monthini=11)[1:6]
prendvis=rgf.create(pndvi,'.rst',1981,1982,monthini=11)[1:6]

rads=paste(prad,'radm',1:12,'.rst',sep='')


#MAKE TMED
n=length(tmins)
for (i in 1:n) {
	a=readGDAL(tmins[i])
	b=readGDAL(tmaxs[i])
	c=a
	c$band1=(a$band1+b$band1)/2	
	writeGDAL(c,tmeds[i],drivername='RST',mvFlag=-99)
}

#make rad
setwd(prad)
dem=readGDAL('dem.rst')
solarRad12M(dem,rads,drivername='RST')

#make ETP
setwd(petp)
batchPetHgsm(c(1982,5),tmins,tmeds,tmaxs,rads,etps,drivername='RST',mvFlag=-1)

#######
## Reproyect rain, and ETP to GIMMS size
#######


setwd("J:/D/Trabajo/marieta/Clima Chile/proceso/rue/")
pndvi="J:/D/Trabajo/marieta/Clima Chile/proceso/rue/ndvi/"
prain="J:/D/Trabajo/marieta/Clima Chile/proceso/rue/rain/"
petp="J:/D/Trabajo/marieta/Clima Chile/proceso/rue/etp/"

rains=rgf.create(prain,'.rst',1982,1995,monthini=5)
ndvis=rgf.create(pndvi,'.rst',1982,1995,monthini=5)
etps=rgf.create(petp,'.rst',1982,1995,monthini=5)
prerains=rgf.create(prain,'.rst',1981,1982,monthini=11)[1:6]
preetps=rgf.create(petp,'.rst',1981,1982,monthini=11)[1:6]

prerains=rgf.create(prain,'.rst',1981,1982,monthini=7)[1:10]
preetps=rgf.create(petp,'.rst',1981,1982,monthini=7)[1:10]




#Make Indices 
rueMe=rueObsMe(rains,ndvis)
rueEx=rueObsEx(rains,ndvis,prerains,nMonths=10)
iaMe=aiObsMe(rains,etps)
iaEx=aiObsEx(rains,ndvis,etps,prerains,preetps,nMonths=10)

writeGDAL(iaMe,'IaMed.rst',drivername='RST',mvFlag=-1)
writeGDAL(iaEx,'IaEx.rst',drivername='RST',mvFlag=-1)



#Comprobaciones previas al proceso de cracion del RUE

#Filter negative values in rain
for (i in rains) {
	a=readGDAL(i,silent=T)
	if (any(a$band1<0,na.rm=T)) print(i)
#	a$band1[a$band1<0]=0
#	writeGDAL(a,i,drivername='RST',mvFlag=-99)
}

#Check negative values in tmax-tmin
tmaxmin=rgf.create('maxmin','.rst',1981,1996)
n=length(tmins)
c=readGDAL(tmins[1])
c$band1=10000
for (i in 1:n) {
	a=readGDAL(tmins[i],silent=T)
	b=readGDAL(tmaxs[i],silent=T)
	c$band1=c$band1+((b$band1-a$band1)<0)*i #pixel=nº veces que tmax < tmin
	#c$band1=min(c$band1,b$band1-a$band1,na.rm=T)
	if (any((b$band1-a$band1)<0,na.rm=T) ) {
		print(tmins[i])
		#stop()
	}
}
writeGDAL(c,'mindifmaxmin.rst',drivername='RST',mvFlag=-99)



mskall=readGDAL('mskall.rst')
for (i in 1:n) {
	aux=readGDAL(annualndvis[i])
	aux$band1=aux$band1^mskall$band1
	writeGDAL(aux,annualndvis[i],drivername='RST',mvFlag=-1)
}





#make step 
regStepRaster(annualndvis,annualTime,annualiaMe,paste('f',1:7,'.rst',sep=''),drivername='RST',mvFlag=-1)




