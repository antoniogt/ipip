# This script is to collect and adapt the data

## Read data
data01 <- read.csv(
  paste(ruta,"01.Pacientes_Estratificacion.csv",sep=""),
  header = T, stringsAsFactors = T, sep = '|',
  na.strings = c('', ' ', 'NA'), fileEncoding = 'latin1'
)

data03 <- read.csv(
  paste(ruta,"03. Ingresos_Estancias_Servicio.csv",sep=""),
  header = T, stringsAsFactors = F, sep = '|',
  na.strings = c('', ' ', 'NA'), fileEncoding = 'latin1'
)


# Get UCI IDs
u <- data03[which(grepl("Medicina Intensiva",data03$SERVICIO_RESPONSABLE)),]
uci_id = unique(u$ID_PACIENTE)


# Add ICU column
tablafeno = data01
tablafeno$UCI = 0
tablafeno[tablafeno$ID_PACIENTE %in% uci_id, "UCI"] <- 1
tablafeno = tablafeno[,c(3,4,5,8:11,13:33)]

# Remove patients with no age records
tablafeno <- tablafeno[-which(tablafeno$EDAD<0),]
tablafeno = na.omit(tablafeno)

# Convert to boolean values
tablafeno <- update_columns(
  tablafeno,
  which(names(tablafeno) == "DM") : which(names(tablafeno) == "DC"),
  as.logical
)

# Remove patients with no sex records and rename several features
tablafeno$SEXO <- as.character(tablafeno$SEXO)
tablafeno$SEXO [tablafeno$SEXO  == "D"] <- NA
tablafeno$SEXO <- as.factor(tablafeno$SEXO)
tablafeno <- tablafeno %>% mutate(SEXO = droplevels(recode(SEXO, H = "Male", M = "Female" )))
colnames(tablafeno)[2] = 'Age'
colnames(tablafeno)[3] = 'Gender'
colnames(tablafeno)[6] = "Status"
colnames(tablafeno)[5] = "DiagnosticTest"


# Remove other columns and recode the status and diatnostic test
comorbilidades <- names(tablafeno)[which(names(tablafeno)=="GMA") : which(names(tablafeno)=="DC")]
variables <- c(
  "Age","Gender","DiagnosticTest","Status","ESTRATO",comorbilidades
)
tablafeno <- tablafeno[ which(tablafeno$Status=='CURADO'| tablafeno$Status=='FALLECIDO'), ]
tablafeno <- tablafeno %>% mutate(Status = droplevels(recode(Status, CURADO = "Discharge", FALLECIDO = "Expired" )))
tablafeno <- tablafeno %>% mutate(DiagnosticTest = droplevels(recode(DiagnosticTest, ANTIGENO = "Antigen", PCR = "PCR", SEROLOGICO = "Serologic" )))



# Add Obesidad, Asthma and recode hospitalized and UCI column
tablafeno$Obesity = 0
tablafeno[str_which(tablafeno$ETIQUETA, "Obesidad" ), "Obesity"] <- 1
tablafeno$Asthma = 0
tablafeno[str_which(tablafeno$ETIQUETA, "Asma" ), "Asthma"] <- 1
colnames(tablafeno)[7] = "Hospitalized"
tablafeno <- tablafeno %>% mutate(Hospitalized = droplevels(recode(Hospitalized, NO = "Outpatient", SI = "Hospitalized" )))
tablafeno$UCI = as.factor(tablafeno$UCI) 
tablafeno <- tablafeno %>% mutate(UCI = droplevels(recode(UCI, "1" = "ICU", "0" = "No" )))

# Write the final table
write.table(tablafeno, "Fenotipos&Patologias_Tabla.tsv", sep = "\t")
