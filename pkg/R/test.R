################################################
#
# ejemplo de sesion tipica para el uso de r2dRue
#
# caso de tener las PET listas

library(r2dRue)

basePath='c:/SpainAnalysis'

viPath='c:/SpainAnalysis/vi'
rainPath='c:/SpainAnalysis/rain'
petPath='c:/SpainAnalysis/pet'
outPath='c:/SpainAnalysis/out'

setwd(basePath)

vi=rgf.read('c:/SpainAnalysis/vi/vi.rgf')
rain=rgf.read('c:/SpainAnalysis/vi/vi.rgf')

r2dRueWiz()

#esto crea un fich de configuracion, y todos las salidas pertinentes en un dir indicado
# salidas: out
#			rain.rgf, vi.rgf, pet.rgf, prain.rgf, ppet.rgf
#			name.conf, name.LOG, name.RDTA
#
#		   assesment
#			rueMed, rueEx, aiMed, aiEx
#
#		   monitoring	
#			f1,f2,f3,f4,f5,f6,f7
#			viMean1970, viMean1971...
#			aiMean1970, aiMean1971...
#			
r2dRueWiz() #para crear otra ejecucion...

#o bien modificar con el bloc de notas el txto de name.conf

r2dRueWiz('name.conf')

rop=createRunOpt('name.conf')

rop$yIni=1970

monitoring(rop)

assesment(rop)





