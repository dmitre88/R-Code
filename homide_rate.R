#=================================================================================#
# Homicide rate                                                                   #
# January 2018                                                                   #
# Author: David Mitre Becerril                                                    #
# Objective: estimate the homicide rate per 100,000 inhabitants per municipality  #
#            in Mexico.                                                           #
# Note: the sum of the homicides per municipality could differ from the national  #
#       sum due to those with unspecified municipality. The "homicide.rate"       #
#       function can be applied from 2010 onwards.                                #
# Data sources that can be used with this function                                #
# http://www.beta.inegi.org.mx/proyectos/registros/vitales/mortalidad/            #
# http://catalogo.datos.gob.mx/dataset/proyecciones-de-la-poblacion-de-mexico     #
#=================================================================================#
# Indicator formula                                                               #
# Rate=(A/B)*100,000, where                                                       #
# A=Total homicides per municipality of ocurrency                                 #
# B=Total population per municipality                                             #
# An homicide is a death classify with the code X85 to Y09 under the International#
# Classification of Diseases 10th revision (ICD-10).                              #
#=================================================================================#
#homicide.rate(INEGI, CONAPO, year)
#INEGI: dataframe of the mortality data (DEFUN.dbf) retrieved from INEGI.
#CONAPO: dataframe of the municipality population data (baseprymunMX.csv) retrieved from CONAPO.
#integer value={2010, 2011, 2012, ...}
#output: a data frame with the following variables:
#cve: code of the municipality
#mun: name of the municipality
#state: code of the state
#state2: name of the state 
#year: year of the data
#homicides: homicides per municipality
#pob: population per municipality
#rate: homicide rate per municipality


#Function to estimate homicide rate
homicide.rate<-function(INEGI, CONAPO, year){

  #Packages needed to run
  packages<-c("foreign", "plyr", "stringr", "dplyr")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
  }
  lapply(packages, require, character.only=TRUE)
  
  #Population by municipality
  names(CONAPO)<-c("row", "date", "ent", "id_ent", "mun", "id_mun", "cve", "sex", "age", "pob")
  CONAPO$cve<-as.numeric(as.character(CONAPO$id_ent*1000)) + as.numeric(as.character(CONAPO$id_mun))
  POB<-aggregate(pob~cve, CONAPO[CONAPO$date%in%year,], sum)
  POB2<-data.frame(unique(CONAPO[,c("cve", "mun")]))
  
  #Dummy variable of deaths by homicide
  code<-c("X85", "X86", "X87", "X88", "X89", "X90", "X91", "X92", "X93", "X94", "X95", "X96", 
         "X97", "X98","X99", "Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08","Y09")
  INEGI$hom<-ifelse(substr(INEGI$CAUSA_DEF,1,3)%in%code, 1, 0)
  
  #Create variable of municipalities by code
  INEGI$cve2<-as.numeric(as.character(INEGI$ENT_OCURR))*1000 + as.numeric(as.character(INEGI$MUN_OCURR))
  
  #Aggregate homicides by municipality
  HOM<-aggregate(hom~cve2, INEGI, sum) #every row of the dataset is an homicide
  names(HOM)<-c("cve", "homicides")
  
  #Join datasets
  HOM<-join(POB, HOM, by="cve")
  HOM<-join(HOM, POB2, by="cve")
  
  #Estimate homicides rate
  HOM$homicides<-ifelse(is.na(HOM$homicides), 0, HOM$homicides)
  HOM$rate<-(HOM$homicides/HOM$pob)*100000
  
  #Format and create state and year variables
  HOM$state<-ifelse(HOM$cve>10000, substr(HOM$cve,1,2), substr(HOM$cve,1,1))
  HOM$year<-year
  HOM$state2<-HOM$state
  HOM$state2[HOM$state2==	0	]<-	'National'
  HOM$state2[HOM$state2==	1	]<-	'Aguascalientes'
  HOM$state2[HOM$state2==	2	]<-	'Baja California'
  HOM$state2[HOM$state2==	3	]<-	'Baja California Sur'
  HOM$state2[HOM$state2==	4	]<-	'Campeche'
  HOM$state2[HOM$state2==	5	]<-	'Coahuila'
  HOM$state2[HOM$state2==	6	]<-	'Colima'
  HOM$state2[HOM$state2==	7	]<-	'Chiapas'
  HOM$state2[HOM$state2==	8	]<-	'Chihuahua'
  HOM$state2[HOM$state2==	9	]<-	'Mexico City'
  HOM$state2[HOM$state2==	10	]<-	'Durango'
  HOM$state2[HOM$state2==	11	]<-	'Guanajuato'
  HOM$state2[HOM$state2==	12	]<-	'Guerrero'
  HOM$state2[HOM$state2==	13	]<-	'Hidalgo'
  HOM$state2[HOM$state2==	14	]<-	'Jalisco'
  HOM$state2[HOM$state2==	15	]<-	'Mexico'
  HOM$state2[HOM$state2==	16	]<-	'Michoacan'
  HOM$state2[HOM$state2==	17	]<-	'Morelos'
  HOM$state2[HOM$state2==	18	]<-	'Nayarit'
  HOM$state2[HOM$state2==	19	]<-	'Nuevo Leon'
  HOM$state2[HOM$state2==	20	]<-	'Oaxaca'
  HOM$state2[HOM$state2==	21	]<-	'Puebla'
  HOM$state2[HOM$state2==	22	]<-	'Queretaro'
  HOM$state2[HOM$state2==	23	]<-	'Quintana Roo'
  HOM$state2[HOM$state2==	24	]<-	'San Luis Potosi'
  HOM$state2[HOM$state2==	25	]<-	'Sinaloa'
  HOM$state2[HOM$state2==	26	]<-	'Sonora'
  HOM$state2[HOM$state2==	27	]<-	'Tabasco'
  HOM$state2[HOM$state2==	28	]<-	'Tamaulipas'
  HOM$state2[HOM$state2==	29	]<-	'Tlaxcala'
  HOM$state2[HOM$state2==	30	]<-	'Veracruz'
  HOM$state2[HOM$state2==	31	]<-	'Yucatan'
  HOM$state2[HOM$state2==	32	]<-	'Zacatecas'
  
  #Output of the function
  HOM<-HOM[,c("cve", "mun", "state", "state2", "year", "homicides", "pob", "rate")]
  return(HOM)
}
