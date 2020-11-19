setwd("C:/Users/ortiz/Google Drive/CCC/Tareas/RM_CCC/Database")

#Install 
#install.packages("tidyverse")
library("tidyverse")

#install.packages("writexl")
library(writexl)

library(readxl)

#33 columns
RM <- read_excel("RM_OCT2020_V1(18NOV2020).xlsx", 
                 col_types = c("text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "numeric", "date", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "numeric", "numeric", 
                               "numeric", "text", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))
                          

#CON 35 columnas con nueva variable por tamaño 975
#RM <- read_excel("RM_AGO21_2020V1.xlsx", 
                  col_types = c("text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "text"))

#CON 32 columnas con nueva variable por tamaño 975
#M_2010V1 <- read_excel("Database/RM_2010V1.xlsx", 
                        col_types = c("text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "date", "date", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

MARE<-data.frame(RM)
#To sort XYEAR by registration and renewed date from janueary to december
sort(MARE$MAT_REN, decreasing=FALSE)

#Check if there are any registration or renew duplicated (TRUE means that there are duplicated)
table(duplicated(MARE$MATRICULA))

MARE2=MARE[!duplicated(MARE$MATRICULA), ]
table(duplicated(MARE2$MATRICULA))

#MARE2 is the data base cleaned by Matricula duplicated

#To drop the NIT duplicated rows

  #1. Get the NIT=0 values apart 
MARE3=MARE2[!(MARE2$NIT!=0), ]
table(duplicated(MARE2$NIT))
sort(MARE2$MAT_REN, decreasing=FALSE)

  #2. Drop all the NIT duplicated 
MARE4=MARE2[!duplicated(MARE2$NIT), ]
MARE4=MARE4[!(MARE4$NIT==0), ]
table(duplicated(MARE4$NIT))

  #3. Merge the Data base cleaned by NIT with the data base with NIT=0
MARE5=rbind(MARE4, MARE3)


#To  clean the Comuna colum
MARE5$COMUNA = as.character(gsub("Comuna 01", "Comuna 1", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 02", "Comuna 2", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 03", "Comuna 3", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 04", "Comuna 4", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 05", "Comuna 5", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 06", "Comuna 6", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 07", "Comuna 7", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 08", "Comuna 8", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna 09", "Comuna 9", MARE5$COMUNA))

MARE5$COMUNA = as.character(gsub("Comuna Dagua", "Dagua", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna Jamundi", "Jamundi", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna La Cumbre", "La Cumbre", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna Vijes", "Vijes", MARE5$COMUNA))
MARE5$COMUNA = as.character(gsub("Comuna Yumbo", "Yumbo", MARE5$COMUNA))

MARE5$CIUDAD = as.character(gsub("Bogota", "Cali", MARE5$CIUDAD))
MARE5$CIUDAD = as.character(gsub("Palmira", "Cali", MARE5$CIUDAD))
MARE5$CIUDAD = as.character(gsub("Piendamo", "Cali", MARE5$CIUDAD))
MARE5$CIUDAD = as.character(gsub("Calima Darien", "Cali", MARE5$CIUDAD))
MARE5$CIUDAD = as.character(gsub("Sevilla", "Cali", MARE5$CIUDAD))

#To know how many missing values are in the city column and replace them for Cali
table(is.na(MARE5$CIUDAD))
MARE5$CIUDAD[is.na(MARE5$CIUDAD)] = "Cali" 


#Adding the year in a new colum
MARE5$Sin.valor.de.medida=NULL
MARE5$AÑO="2020"

NOM_COD_BARRIOS <- read_excel("NOM_COD_BARRIOS.xlsx", 
                          col_types = c("text", "text", "text"))


NOM_COD_BARRIOS$BARRIO_REG=toupper(NOM_COD_BARRIOS$BARRIO_REG)

MARE5$BARRIO=toupper(MARE5$BARRIO)

MARE5=merge(MARE5, NOM_COD_BARRIOS, by.x = "BARRIO", by.y = "BARRIO_REG", all.x = TRUE)

MARE5$BARRIO=NULL

CIIU_COR <- read_excel("CIIU_COR.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", "text"))

MARE6=merge(MARE5, CIIU_COR, by.x = "CODIGO_CIIU", by.y = "CODIGO_CIIU", all.x = TRUE)

#This code is to change the name of the columns and make them like in the formulary
colnames(MARE6)[which(colnames(MARE6) %in% c("Año.de.FECHA_RENOVACION", "Mes.de.FECHA_MATRICULA", "FECHA_MAT_REN", "TOT_ACTIVOS", "UTILILIDAD_PERDIDA", "VENTAS", "TOT_PASIVO", "UTILILIDAD_BRUTA") )]
                                        <- c("ULTIMO_ANO_RENOVADO","FECHA_MATRICULA", "FECHA_MATRICULA_RENOVACION", "ACTIVO_TOTAL", "UTILILIDAD_OPERACIONAL", "INGRESOS_ACTIVIDAD_ORDINARIA", "PASIVO_TOTAL", "RESULTADO_PERIODO")

#To save the database as an Excel file
write_xlsx(MARE6, "RM_OCT2020_V2(18NOV2020).xlsx")





