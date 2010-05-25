################################################
#
# ejemplo de sesion tipica para el uso de r2dRue
#
# caso de tener las PET listas

#library(r2dRue)

library(rgdal)
library(raster)

source('r2drue.r')
source('rgf.r')
source('etp.r')
source('utils.r')

setwd(' poner aqui tu directorio de trabajo')

editr2dRFile('chile.conf')

och=r2dRRead('chile.conf')

showinfo(och)

assessment(och)
showinfo(och)
r2dRplot(och,'assesment')

monitoring(och)
showinfo(och)
r2dRplot(och,'monitoring')

resume(och)
r2dRplot(och,'box',var='vi')
r2dRplot(och,'box',var='rain')
r2dRplot(och,'rain')
r2dRplot(och,'density',var='vi')
r2dRplot(och,'pixel',pixel=316)

i=100
r2dRplot(och,'pixel',pixel=316);i=i+1;





