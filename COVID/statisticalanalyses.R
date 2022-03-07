# Statistical analyses

## Expired vs Discharge
library(multcomp)
tablafeno = read.table(file = "Fenotipos&Patologias_Tabla.tsv", sep = "\t")
tablafeno$Gender = as.factor(tablafeno$Gender)
tablafeno$Status = as.factor(tablafeno$Status)
Discharge = filter(tablafeno, tablafeno$Status == "Discharge")
Expired = filter(tablafeno, tablafeno$Status == "Expired")

# Age
difage = t.test( Expired$Age, Discharge$Age)
difage


# NPC
npcdif = t.test(Expired$NPC,Discharge$NPC)
npcdif



# NSIST
nsistdif = t.test( Expired$NSIST, Discharge$NSIST)

nsistdif




# NSIST
obesitydif = t.test(Discharge$Obesity, Expired$Obesity)

obesitydif


# Gender
chisq.test(tablafeno$Status, tablafeno$Gender, correct=FALSE)

# Obesity
chisq.test(tablafeno$Status, tablafeno$Obesity, correct=FALSE)

# EPOC
chisq.test(tablafeno$Status, tablafeno$EPOC, correct=FALSE)

# DM 
chisq.test(tablafeno$Status, tablafeno$DM, correct=FALSE)

# IC 
chisq.test(tablafeno$Status, tablafeno$IC, correct=FALSE)


# DEP
chisq.test(tablafeno$Status, tablafeno$DEP, correct=FALSE) 


# CI
chisq.test(tablafeno$Status, tablafeno$CI, correct=FALSE) 

# HTA
chisq.test(tablafeno$Status, tablafeno$HTA, correct=FALSE) 

# ACV
chisq.test(tablafeno$Status, tablafeno$ACV, correct=FALSE) 

# IRC
chisq.test(tablafeno$Status, tablafeno$IRC, correct=FALSE) 

# CIR
chisq.test(tablafeno$Status, tablafeno$CIR, correct=FALSE) 

# OST
chisq.test(tablafeno$Status, tablafeno$OST, correct=FALSE) 

# artrosis 
chisq.test(tablafeno$Status, tablafeno$ARTROSIS, correct=FALSE) 

# Artritis
chisq.test(tablafeno$Status, tablafeno$ARTRITIS, correct=FALSE) 


# DEM 
chisq.test(tablafeno$Status, tablafeno$DEM, correct=FALSE)


# Asthma No hay diferencias
chisq.test(tablafeno$Status, tablafeno$Asthma, correct=FALSE)

# DC NO HAY
chisq.test(tablafeno$Status, tablafeno$DC, correct=FALSE)  

# VIH NO HAY
chisq.test(tablafeno$Status, tablafeno$VIH, correct=FALSE) 


# Outpatient vs Hospitalized


tablafeno = read.table(file = "Fenotipos&Patologias_Tabla.tsv", sep = "\t")
tablafeno$Gender = as.factor(tablafeno$Gender)
tablafeno$Status = as.factor(tablafeno$Status)
ICU = filter(tablafeno, tablafeno$UCI == "ICU")
nonicu = filter(tablafeno, tablafeno$Hospitalized == "Hospitalized" & tablafeno$UCI == "No")
newdficu = rbind(ICU,nonicu)


# Age
difage = t.test( ICU$Age, nonicu$Age)

difage




# NPC
npcdif = t.test(ICU$NPC,nonicu$NPC)

npcdif



# NSIST
nsistdif = t.test( ICU$NSIST, nonicu$NSIST)

nsistdif


t.test(ICU$Gender, nonicu$Gender)




# Gender
chisq.test(newdficu$UCI, newdficu$Gender, correct=FALSE)

# Obesity
chisq.test(newdficu$UCI, newdficu$Obesity, correct=FALSE)

# EPOC
chisq.test(tablafeno$Status, tablafeno$EPOC, correct=FALSE)

# DM 
chisq.test(tablafeno$Status, tablafeno$DM, correct=FALSE)

# IC 
chisq.test(tablafeno$Status, tablafeno$IC, correct=FALSE)


# DEP
chisq.test(tablafeno$Status, tablafeno$DEP, correct=FALSE) 


# CI
chisq.test(tablafeno$Status, tablafeno$CI, correct=FALSE) 

# HTA
chisq.test(tablafeno$Status, tablafeno$HTA, correct=FALSE) 

# ACV
chisq.test(tablafeno$Status, tablafeno$ACV, correct=FALSE) 

# IRC
chisq.test(tablafeno$Status, tablafeno$IRC, correct=FALSE) 

# CIR
chisq.test(tablafeno$Status, tablafeno$CIR, correct=FALSE) 

# OST
chisq.test(tablafeno$Status, tablafeno$OST, correct=FALSE) 

# artrosis 
chisq.test(tablafeno$Status, tablafeno$ARTROSIS, correct=FALSE) 

# Artritis
chisq.test(tablafeno$Status, tablafeno$ARTRITIS, correct=FALSE) 


# DEM 
chisq.test(tablafeno$Status, tablafeno$DEM, correct=FALSE)


# Asthma 
chisq.test(tablafeno$Status, tablafeno$Asthma, correct=FALSE)

# DC 
chisq.test(tablafeno$Status, tablafeno$DC, correct=FALSE)  

# VIH 
chisq.test(tablafeno$Status, tablafeno$VIH, correct=FALSE)


# Figures
library(reshape2)

df <- tablafeno %>%
  mutate(Age = round(Age,2)) %>%
  select(Status, Age, NPC, NSIST) %>%
  melt(id.vars = "Status", variable.name = "CARACTERISTICA", value.name = "VALOR")

df$CARACTERISTICA <- as.character(df$CARACTERISTICA)
df$CARACTERISTICA [df$CARACTERISTICA  == "NPC"] <- "Number of comorbidities"
df$CARACTERISTICA [df$CARACTERISTICA  == "NSIST"] <- "Number of affected systems"


ggplot(df, aes(x = VALOR, y = ..density.., fill = Status)) +
  geom_histogram(alpha = 0.6, binwidth = 1, position = position_identity()) +
  facet_wrap(~ CARACTERISTICA, scales = "free_x")  + ylab("Density")+  xlab("Value") + 
  guides(fill=guide_legend(title="Outcome")) + theme(axis.title.x = element_blank()) + theme_classic()


ggsave(filename = "Figuras/FigureExvsDisv2.tiff", path = ".", width = 3000, height = 1500, device='tiff', units = "px", dpi = 300)

ggsave(filename = "Figuras/FigureExvsDisv2.png", path = ".", width = 3000, height = 1500, device='png', units = "px", dpi = 300)



tablafeno$Hospitalized = ifelse(tablafeno$UCI == "ICU","ICU",tablafeno$Hospitalized)
tablafeno$Hospitalized = as.factor(tablafeno$Hospitalized) 
df <- tablafeno %>%
  mutate(Age = round(Age,2)) %>%
  select(Hospitalized, Age, NPC, NSIST) %>%
  melt(id.vars = "Hospitalized", variable.name = "CARACTERISTICA", value.name = "VALOR")

df$CARACTERISTICA <- as.character(df$CARACTERISTICA)
df$CARACTERISTICA [df$CARACTERISTICA  == "NPC"] <- "Number of comorbidities"
df$CARACTERISTICA [df$CARACTERISTICA  == "NSIST"] <- "Number of affected systems"


ggplot(df, aes(x = VALOR, y = ..density.., fill = Hospitalized)) +
  geom_histogram(alpha = 0.6, binwidth = 1, position = position_identity()) +
  facet_wrap(~ CARACTERISTICA, scales = "free_x") +  xlab("Value")  + ylab("Density") +  guides(fill=guide_legend(title="Outcome")) + theme(axis.title.x = element_blank()) + theme_classic() + scale_fill_manual(values=c("#F8766D", "#00BFC4", "#E69F00"))


ggsave(filename = "Figuras/Figurehospv2.png", path = ".", width = 3000, height = 1500, device='png', units = "px", dpi = 300)

ggsave(filename = "Figuras/Figurehospv2.tiff", path = ".", width = 3000, height = 1500, device='tiff', units = "px", dpi = 300)


# OR figure


library(epiDisplay)

# For different features Asthma + Obesity + IC + CI + ACV + DEP + HTA + IRC + EPOC + CIR + OST + ARTROSIS + ARTRITIS + DM
glm1 <- glm(Status~DEM  + Age + Gender, 
            family=binomial, data=tablafeno)
logistic.display(glm1)


library(data.table)

ortabla = fread("TablasFinales/ORparacalculo.csv")


p <- ggplot(ortabla, aes(x = ortabla$OR, y = reorder(ortabla$Description, -ortabla$OR))) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ortabla$upper, xmin = ortabla$lower), size = .5, height = 
                   .3, color = "black") +
  geom_point(size = 2, color = "#E69F00")  +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),text = element_text(size = 13)) +
  ylab("") +
  xlab("Odds ratio") + 
  ggtitle("Adjusted Odds Ratio COVID-19 Death") +  xlim(0, 3)

ggsave(filename = "Figuras/oddsratio.png", path = ".", width = 2200, height = 2400, device='png', units = "px", dpi = 300)
ggsave(filename = "Figuras/oddsratio.tiff", path = ".", width = 2200, height = 2400, device='tiff', units = "px", dpi = 300)