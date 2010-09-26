################################################
#
# ejemplo de sesion tipica para el uso de r2dRue
#



#library(r2dRue)
# Cargar las fuentes de r2dRue ya que no esta la libreria como tal... 
# est no es necesario para un usuario final que solo hara >library(r2dRue)
###################################
	library(rgdal)
	library(raster)
	pSource='c:/R_pilot\\'
	sources=c('r2drue.r','rgf.r','etp.r','utils.r')
	sources=paste(pSource,sources,sep='')
	for (i in 1:4) {source(sources[i])}
###################################

#cambiar al directorio de trabajo
	wd='c:/R_pilot'
	setwd(wd) 

###################################
#Los rgf, son responsabilidad del usuario
#puede crearlos con un editor de textos, 
#con un programa de gis, o con R, o a lapiz...
#Son archivos de texto que enumeren nombres completos 
#de archivos, directorio y extension incluidos
#Asi que los RGF tipicos de idrisi no son suficientes como tal, 
#ya que no tienen path ni extension, es cosa del usuario completarlo
#
#
#Si lo quieres hacer con R, es relativamente facil si los nombres de archivo 
#son coherentes con nuestro sistema de nomenclatura
#
#	rgf.create('c:\pilot\tmin','.rst',1997,2003,12,'tmin.rgf') #si son años completos
#	y editar tmin.rgf si te sobra algun archivo...
#
#	otra opcion
#	tmin=dir(patt='tmn')
#	tmin=tmin[grep('rst',tmin)]
#	tmin=paste(wd,'/',tmin,sep='')
#	write(tmin,'tmin.rst')
# a mi me da igual al final quiero los rgf bien formados....

#cargar rgf
tmin=rgf.read('tmin.rgf')
tmed=rgf.read('tmed.rgf')
tmax=rgf.read('tmax.rgf')

###################################
#Crear las ETP a partir de las Temp
###################################
#crear rgf con nombres de salida

pet=gsub('tmn-m','pet',tmin)
write(pet,'pet.rgf')

batchPetHgsm(pet,12,tmin,tmed,tmax, drivername='RST',mvFlag=-999) # la series comienzan en DIC de ahi el 12

#puedes ver las pet creadas en idrisi o con rgdal...
image(readGDAL(pet[1]))

###################################
#Empieza el 2dRUE
###################################

#definir fichero de entrada
#lo puedes hacer a mano en el bloc de notas o con el wizard... 
#pero mete estos datos....

########### PILOT1.conf ##########
#comment=prueba1 ferlo
#viRgf=ndvi.rgf
#rainRgf=prec.rgf
#petRgf=pet.rgf
#mHidro=6
#acum=6
#pOut=c:/R_pilot
#sYear=1997
#sMonth=12
#yIni=1998
#yEnd=2002
#driver=RST
#flag=-999
##################################

editr2dRfile('Pilot1.conf') #creamos pilot1.conf con el wizard

och=readr2dRfile('Pilot1.conf') #leemos el contenido sobre och

showInfo(och) #mostramos info estado actual

assesment(och) # realizamos el assement sobre och
showInfo(och) #mostramos info estado actual

monitoring(och) # realizamos el monitoring sobre och
showInfo(och) #mostramos info estado actual

resume(och) #resumimos datos para poder hacer graficas
r2dRplot(och,'assesment1')
r2dRplot(och,'assesment2')
r2dRplot(och,'box',var='vi')
r2dRplot(och,'box',var='rain')
r2dRplot(och,'rain')
r2dRplot(och,'density',var='vi')
r2dRplot(och,'pixel',pixel=316)
r2dRplot(och,'monitoring')

i=100
r2dRplot(och,'pixel',pixel=i);i=i+1;



####################
#### test libraries
####################



