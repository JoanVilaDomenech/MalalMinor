rm(list=ls())
library(readxl)
library(writexl)
library(Hmisc)
library(gdata)

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor/")
# setwd("C:/Users/user/Dropbox/NCodern/MalalMinor/")

catpre <- read_excel("./dat/Cat20220121B.xlsx", sheet = 1, col_names = TRUE)
caspre <- read_excel("./dat/Cas20220121B.xlsx", sheet = 1, col_names = TRUE)

cat <- data.frame(catpre)
cas <- data.frame(caspre)

xrename <- data.frame(read_excel("./dat/TransfoNames.xlsx", sheet = 1, col_names = TRUE))

xrename$cat[xrename$cat%nin%names(cat)]
names(cat)[names(cat)%nin%xrename$cat]
cat <- rename.vars(cat, c("Hora.d.inici", "Hora.de.finalització", "Correu", "Nom"), 
                   c("Start.time", "Completion.time", "Email", "Name" ))
for (i in 1:ncol(cat)){
  cat <- rename.vars(cat, xrename$cat[i], xrename$ok[i])
  }

xrename$cas[xrename$cas%nin%names(cas)]
names(cas)[names(cas)%nin%xrename$cas]
cas <- rename.vars(cas, c("Hora.d.inici", "Hora.de.finalització", "Correu", "Nom"), 
                   c("Start.time", "Completion.time", "Email", "Name" ))

for (i in 1:ncol(cas)){
  cas <- rename.vars(cas, xrename$cas[i], xrename$ok[i])
}

cat$lang <- "CAT"
cas$lang <- "CAS"
dat <- rbind(cat,cas)

for (i in 1:(ncol(dat)-1)){
  Hmisc::label(dat[, i]) <- names(catpre)[i]
}
Hmisc::label(dat$lang) <- "Idioma enquesta"

# Dia Enquesta
dat$DiaEnq <- substr(dat$HoraFi, 1, 10)
Hmisc::label(dat$DiaEnq) <- "Dia que omple la enquesta"

# Removing some characters
for(i in 1:ncol(dat)){
  dat[,i] <- sub("\\n", "", dat[,i])
  dat[,i] <- sub("\\r", "", dat[,i])
}                    


save(dat, file="./dat/dat.rda")

source("normalitza.R")
datok <- dat
save(datok, file="./dat/datok.rda")

################################################################################
################################################################################
################################################################################
rm(list=ls())
library(Hmisc)
library(gdata)

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor/")
load("./dat/datok.rda")

catnames <- data.frame(read_excel("./dat/Cat20220121.xlsx", sheet = 1, col_names = TRUE))
catlabel <- as.character(read_excel("./dat/Cat20220121.xlsx", sheet = 1, col_names = FALSE, n_max = 1))

cat <- data.frame("cat"=names(catnames), "xlabel"=catlabel)

xrename <- data.frame(read_excel("./dat/TransfoNames.xlsx", sheet = 1, col_names = TRUE))
cat <- merge(cat, xrename[, c("ok", "cat")], by= "cat", all.x=T)
cat$ok <- with(cat, ifelse(cat=="Hora.d.inici", "HoraIni", ok))
cat$ok <- with(cat, ifelse(cat=="Hora.de.finalització", "HoraFi", ok))
cat$ok <- with(cat, ifelse(cat=="Correu", "Correu", ok))
cat$ok <- with(cat, ifelse(cat=="Nom", "Nom", ok))

cat$ok[cat$ok%nin%names(datok)]
names(datok)[names(datok)%nin%cat$ok]

for(i in 1:nrow(cat)){
  Hmisc::label(datok[, cat$ok[i]]) <- cat$xlabel[i]  
}
datok$afectat <- with(datok, ifelse(relacio=="Soc la persona afectada", "Y", "N"))
Hmisc::label(datok$afectat) <- "És la persona afectada"

xday <- as.Date(paste(as.character(as.numeric(datok$anyneix)), "06", "30", sep="-"))
datok$age <- floor((as.Date(datok$DiaEnq)-xday)/365.25)
Hmisc::label(datok$age) <- "Edat (calculada)"

datok$edat <- as.numeric(datok$edat)
Hmisc::label(datok$edat) <- "Quina és la vostra edat? Contesteu amb un número"
save(datok, file="./dat/datok.rda")

################################################################################
################################################################################
################################################################################
rm(list=ls())

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor")
load("./dat/datok.rda")
datok$zip <- with(datok, ifelse(zip=="O8001", "08001", zip))

# save(datok, file="./dat/datok.rda")
x <- attr(datok$relacio, "label")
datok$relacio <- with(datok, ifelse(lang=="CAT" & ID==76, "Parella", relacio))
datok$relacio <- with(datok, ifelse(relacio=="Soy pareja y madre de dos hijas afectadas", "Parella", relacio))
Hmisc::label(datok$relacio) <- x

x <- attr(datok$anyneix, "label")
datok$anyneix <- with(datok, ifelse(anyneix=="1950 jo, 1983 el fill", NA, anyneix))
datok$anyneix <- with(datok, ifelse(anyneix=="Artrogriposis multiple congenita", NA, anyneix))
Hmisc::label(datok$anyneix) <- x

x <- attr(datok$estudisafec, "label")
datok$estudisafec <- with(datok, ifelse(estudisafec=="tots dos", NA, estudisafec))
datok$estudisafec <- with(datok, ifelse(estudisafec%in%c("Discapacitat intelectual", "Funcionaria"), "Altres", estudisafec))
Hmisc::label(datok$estudisafec) <- x

datok$infant <- with(datok, ifelse(age<=12, "<=12anys", "12+"))
Hmisc::label(datok$infant) <- "Infant (<=12a)"

x <- attr(datok$afectat, "label")
datok$afectat <- with(datok, ifelse(lang =="CAT" & ID==76, "N", afectat))
Hmisc::label(datok$afectat) <- x

x <- attr(datok$sexe, "label")
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==9, "Femení", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==39, "Femení", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==43, "Femení", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==62, "Masculí", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==127, "Femení", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==130, "Femení", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAT" & ID==154, "Masculí", sexe))
datok$sexe <- with(datok, ifelse(lang =="CAS" & ID==30, "Femení", sexe))
Hmisc::label(datok$sexe) <- x

datok$before2010 <- as.numeric(with(datok, ifelse(is.na(anydiag) | nchar(anydiag)>4, NA, anydiag)))
datok$before2010 <- with(datok, ifelse(before2010 < 2010, "Abans 2010", "2010 +"))
datok$before2010 <- factor(datok$before2010, levels = c("Abans 2010", "2010 +"))
Hmisc::label(datok$before2010) <- "Any diagnòstic"


x <- attr(datok$tempsproces, "label")
datok$tempsproces <- with(datok, ifelse(tempsproces =="3, 3, 20, 10", NA, tempsproces))
datok$tempsproces <- with(datok, ifelse(tempsproces =="5 y 24", NA, tempsproces))
datok$tempsproces <- with(datok, ifelse(tempsproces%in%c("En procés", "Estudi genètic", "Ho desconec"), NA, tempsproces))
Hmisc::label(datok$tempsproces) <- x

datok$TempsDiag <- with(datok, ifelse(is.na(tempsproces), NA, 
                              ifelse(tempsproces%in%c("Diagnòstic neonatal", "Diagnòstic prenatal"), NA, tempsproces)))
datok$x <- as.numeric(unlist(lapply(with(datok, strsplit(TempsDiag," ")), function(x) x[1])))
datok$y <- unlist(lapply(with(datok, strsplit(TempsDiag," ")), function(x) x[2]))
datok$x <- with(datok, ifelse(y == "anys", x*12, x))
datok$TempsDiag <- with(datok, ifelse(is.na(tempsproces), NA, 
                              ifelse(tempsproces%in%c("Diagnòstic neonatal", "Diagnòstic prenatal"), tempsproces, NA)))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x <= 3, "de 1 a 3 mesos", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 3 & x <=6, ">3 i <=6 mesos", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 6 & x <=12, ">6 i <=12 mesos", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12 & x <=12*2, ">1 i <=2 anys", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12*2 & x <=12*5, ">2 i <=5 anys", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12*5 & x <=12*10, ">5 i <=10 anys", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12*10 & x <=12*20, ">10 i <=20 anys", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12*20 & x <=12*30, ">20 i <=30 anys", TempsDiag))
datok$TempsDiag <- with(datok, ifelse(!is.na(x) & x > 12*30, ">30 anys", TempsDiag))
datok$TempsDiag <- factor(datok$TempsDiag, levels= c("Diagnòstic prenatal", "Diagnòstic neonatal", 
                                  "de 1 a 3 mesos", ">3 i <=6 mesos", ">6 i <=12 mesos", 
                                  ">1 i <=2 anys", ">2 i <=5 anys", ">5 i <=10 anys", 
                                  ">10 i <=20 anys", ">20 i <=30 anys", ">30 anys"))
Hmisc::label(datok$TempsDiag) <- "Temps proces diagnòstic"
datok <- remove.vars(datok, c("x", "y"))

x <- attr(datok$edatdiag, "label")
datok$edatdiag <- with(datok, ifelse(edatdiag%in%c("11 años y 38 años", "16 / 19", "37, 37, 37, 26"), NA, edatdiag))
Hmisc::label(datok$edatdiag) <- x

datok$x <- as.numeric(with(datok, ifelse(is.na(edatdiag), NA, ifelse(nchar(edatdiag)>2, NA, edatdiag))))
datok$AgeDiag <- with(datok, ifelse(is.na(edatdiag), NA, 
                              ifelse(edatdiag=="menys de 3 mesos", "<3mesos",
                              ifelse(edatdiag=="Entre 3 i menys de 6 mesos", ">3 i <=6 mesos", 
                              ifelse(edatdiag=="entre 6 i menys de 12 mesos", ">6 i <=12 mesos", NA)))))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x >=1 & x <= 2, ">1 i <=2 anys", AgeDiag))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x > 2 & x <= 5, ">2 i <=5 anys", AgeDiag))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x > 5 & x <=10, ">5 i <=10 anys", AgeDiag))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x > 10 & x <=20, ">10 i <=20 anys", AgeDiag))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x > 20 & x <=30, ">20 i <=30 anys", AgeDiag))
datok$AgeDiag <- with(datok, ifelse(!is.na(x) & x > 30, ">30 anys", AgeDiag))
datok$AgeDiag <- factor(datok$AgeDiag, levels=c("<3mesos", ">3 i <=6 mesos",  ">6 i <=12 mesos",
                              ">1 i <=2 anys", ">2 i <=5 anys", ">5 i <=10 anys", ">10 i <=20 anys",
                              ">20 i <=30 anys", ">30 anys"))
Hmisc::label(datok$AgeDiag) <- "Edat moment diagnòstic"


xxx <- c("Pediatra", "Pediatre dels meus fills", "Pediatria")
x <- attr(datok$vaprimersinto, "label")
datok$vaprimersinto <- with(datok, ifelse(vaprimersinto%in%xxx, NA, vaprimersinto))
datok$vaprimersinto <- with(datok, ifelse(vaprimersinto=="Nounat", "Diagnòstic Prenatal", vaprimersinto))
datok$vaprimersinto <- with(datok, ifelse(vaprimersinto=="Diagnòstic prenatal", "Diagnòstic Prenatal", vaprimersinto))
Hmisc::label(datok$vaprimersinto) <- x

xxx <- c("No aplicable", "Primera simptomes detectats al neixer al mateix hospital", "Tots", "varis, ja que tenia sintomes molt diversos")
x <- attr(datok$professprimer, "label")
datok$professprimer <- with(datok, ifelse(professprimer%in%xxx, "Altres", professprimer))
Hmisc::label(datok$professprimer) <- x

x <- attr(datok$prefessinicia, "label")
datok$prefessinicia <- with(datok, ifelse(prefessinicia=="No aplicable", "Altres", prefessinicia))
Hmisc::label(datok$prefessinicia) <- x

x <- attr(datok$quantsprofess, "label")
datok$quantsprofess <- factor(datok$quantsprofess, levels = c("No ho sé", "Cap",
                              "Menys de 3", "Entre 3 i menys de 6", "Entre 6 i menys de 9",
                              "Entre 9 i menys de 12", "Entre 12 i menys de 15", 
                              "Entre 15 i menys de 18", "Entre 18 i menys de 21",
                              "Entre 21 i menys de 30", "Entre 30 i menys de 40",
                              "Entre 40 i menys de 60", "60 i més"))
Hmisc::label(datok$quantsprofess) <- x

x <- attr(datok$suporprofess, "label")
datok$suporprofess <- with(datok, ifelse(suporprofess=="No aplicable", NA, suporprofess))
Hmisc::label(datok$suporprofess) <- x

x <- attr(datok$inforebuda, "label")
datok$inforebuda <- with(datok, ifelse(inforebuda=="No aplicable", NA, inforebuda))
Hmisc::label(datok$inforebuda) <- x

x <- attr(datok$assessgen, "label")
datok$assessgen <- with(datok, ifelse(is.na(assessgen) | assessgen =="No aplicable", "Missing/No aplicable", assessgen))
datok$assessgen <- with(datok, ifelse(assessgen =="1-3 veces semana", "1-3 cops setmana", assessgen))
datok$assessgen <- factor(datok$assessgen, levels = c("Missing/No aplicable", "Diàriament", 
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$assessgen) <- x

x <- attr(datok$CDIAP, "label")
datok$CDIAP <- with(datok, ifelse(is.na(CDIAP) | CDIAP =="No aplicable", "Missing/No aplicable", CDIAP))
datok$CDIAP <- factor(datok$CDIAP, levels = c("Missing/No aplicable",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l’any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$CDIAP) <- x

x <- attr(datok$Fisioterapia, "label")
datok$Fisioterapia <- with(datok, ifelse(is.na(Fisioterapia) | Fisioterapia =="No aplicable", "Missing/No aplicable", Fisioterapia))
datok$Fisioterapia <- factor(datok$Fisioterapia, levels = c("Missing/No aplicable", "Diàriament",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$Fisioterapia) <- x

x <- attr(datok$grupsSuport, "label")
datok$grupsSuport <- with(datok, ifelse(is.na(grupsSuport) | grupsSuport =="No aplicable", "Missing/No aplicable", grupsSuport))
datok$grupsSuport <- factor(datok$grupsSuport, levels = c("Missing/No aplicable", "Diàriament",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$grupsSuport) <- x

x <- attr(datok$Infermeria, "label")
datok$Infermeria <- with(datok, ifelse(is.na(Infermeria) | Infermeria =="No aplicable", "Missing/No aplicable", Infermeria))
datok$Infermeria <- factor(datok$Infermeria, levels = c("Missing/No aplicable", "Diàriament",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$Infermeria) <- x

x <- attr(datok$Logopdia, "label")
datok$Logopdia <- with(datok, ifelse(is.na(Logopdia) | Logopdia =="No aplicable", "Missing/No aplicable", Logopdia))
datok$Logopdia <- factor(datok$Logopdia, levels = c("Missing/No aplicable", "Diàriament",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
Hmisc::label(datok$Logopdia) <- x

putorden <- function(xvari){
  temp <- datok[, xvari]
  x <- attr(temp, "label")
  temp <- ifelse(is.na(temp) | temp =="No aplicable", "Missing/No aplicable", temp)
  temp <- factor(temp, levels = c("Missing/No aplicable", "Diàriament",
                          "1-3 cops setmana", "1-3 cops al mes", "1-2 cops cada sis mesos", 
                          "1 cop a l'any o de manera molt ocasional", "No hi tinc accés, i en voldria tenir"))
  Hmisc::label(temp) <- x
  return(temp)
  }

datok$MedFami <-  putorden("MedFami")

datok$Nutricio <-  putorden("Nutricio")

datok$Pediatria <-  putorden("Pediatria")

datok$Psicologia <-  putorden("Psicologia")

datok$Rehabilitacio <-  putorden("Rehabilitacio")

datok$ServEspe <-  putorden("ServEspe")

datok$TreballSoci <-  putorden("TreballSoci")

datok$AltresServeis <-  putorden("AltresServeis")

putorden2 <- function(xvari){
  temp <- datok[, xvari]
  x <- attr(temp, "label")
  temp <- ifelse(is.na(temp) | temp =="No aplicable", "Missing/No aplicable", temp)
  temp <- factor(temp, levels = c("Missing/No aplicable", "Molt insatisfet/a",
                          "Poc satisfet/a", "Neutre", "Satisfet/a", "Molt satisfet/a"))
  Hmisc::label(temp) <- x
  return(temp)
  }
datok$SatisEspeciali <- putorden2("SatisEspeciali")
                      
datok$SatisCoordi <- putorden2("SatisCoordi")

datok$PresaDecisi <- putorden2("PresaDecisi")

datok$SuporPorfess <- putorden2("SuporPorfess")

datok$SuportTreball <- putorden2("SuportTreball")

putorden3 <- function(xvari){
  temp <- datok[, xvari]
  x <- attr(temp, "label")
  temp <- ifelse(is.na(temp) | temp =="No aplicable", "Missing/No aplicable", temp)
  temp <- factor(temp, levels = c("Missing/No aplicable", "No m'ho puc pagar",
                          "M'ho puc pagar"))
  Hmisc::label(temp) <- x
  return(temp)
}

datok$PagarTx <- putorden3("PagarTx")
datok$PagarProdu <- putorden3("PagarProdu")
datok$PatgarAdap <- putorden3("PatgarAdap")
datok$PagarTecno <- putorden3("PagarTecno")
datok$PagarBolquer <- putorden3("PagarBolquer")
datok$Pagarpersona <- putorden3("Pagarpersona")
datok$PagarRehab <- putorden3("PagarRehab")
datok$PagarPsico <- putorden3("PagarPsico")
datok$AltresPaga <- putorden3("AltresPaga")

datok$Prestacions <- putorden2("Prestacions")
datok$SuporMestre <- putorden2("SuporMestre")
datok$CoordinaMestre <- putorden2("CoordinaMestre")
datok$DecisioMestre <- putorden2("DecisioMestre")

putorden4 <- function(xvari){
  temp <- datok[, xvari]
  x <- attr(temp, "label")
  temp <- ifelse(is.na(temp) | temp =="No aplicable", "Missing/No aplicable", temp)
  temp <- factor(temp, levels = c("Missing/No aplicable", "Gens de necessitat",
                          "Poca necessitat", "Neutre", "Bastanta necessitat", "Molta necessitat"))
  Hmisc::label(temp) <- x
  return(temp)
}

datok$EscolaInfermeria <- putorden4("EscolaInfermeria")
datok$Vetllador <- putorden4("Vetllador")
datok$AdaptacioEntorn <- putorden4("AdaptacioEntorn")
datok$AdaptacioTecno <- putorden4("AdaptacioTecno")
datok$AdapacioPriscomot <- putorden4("AdapacioPriscomot")
datok$NecessitaLogope <- putorden4("NecessitaLogope")
datok$MestreSuport <- putorden4("MestreSuport")
datok$PsicoPedagog <- putorden4("PsicoPedagog")
datok$AltresNecessita <- putorden4("AltresNecessita")

putorden5 <- function(xvari){
  temp <- datok[, xvari]
  x <- attr(temp, "label")
  temp <- ifelse(is.na(temp) | temp =="No aplicable", "Missing/No aplicable", temp)
  temp <- factor(temp, levels = c("Missing/No aplicable", "Gens suport",
                          "Poc suport", "Neutre", "Bastant suport", "Molt suport"))
  Hmisc::label(temp) <- x
  return(temp)
}

datok$ExplicaInfant <- putorden5("ExplicaInfant")
datok$ExplicaAmics <- putorden5("ExplicaAmics")
datok$ExplicaFamilia <- putorden5("ExplicaFamilia")

datok$SuportSalutLabor <- putorden2("SuportSalutLabor")
datok$SuportCompanys <- putorden2("SuportCompanys")


datok$AtenInfer <- putorden4("AtenInfer")
datok$FeinaWC <- putorden4("FeinaWC")
datok$FeinaTecno <- putorden4("FeinaTecno")
datok$FeinaTelema <- putorden4("FeinaTelema")
datok$AltresFeina <- putorden4("AltresFeina")

datok$FeinaExplica <- putorden5("FeinaExplica")
datok$SalutLabora <- putorden5("SalutLabora")
datok$AmicsFeina <- putorden5("AmicsFeina")


save(datok, file="./dat/datok.rda")

dat <- datok
dat$xxx <- with(dat, ifelse(is.na(relacio) & is.na(nomalaltia), 1, 0))
dat$xxx <- with(dat, ifelse(lang=="CAT" & ID==125, 1, xxx))
dat <- subset(dat, xxx==0)




################################################################################
################################################################################
################################################################################
rm(list=ls())
library(readxl)
library(writexl)
library(Hmisc)
library(gdata)

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor/")

load("./dat/datok.rda")

dat <- datok
dat$xxx <- with(dat, ifelse(is.na(relacio) & is.na(nomalaltia), 1, 0))
dat$xxx <- with(dat, ifelse(lang=="CAT" & ID==125, 1, xxx))
dat <- subset(dat, xxx==0)
dat <- subset(dat, !is.na(nomalaltia))

# Levels: Abans 2010 2010 +

dat$xxx <- as.numeric(dat$anyneix)
dat$before2010 <- as.character(dat$before2010)
dat$before2010 <- with(dat, ifelse(ID==9 & lang =="CAT" & nomalaltia=="Immunodeficiència combinada", "2010 +", before2010))
dat$before2010 <- with(dat, ifelse(ID==63 & lang =="CAT" & nomalaltia=="Síndrome d’Ehlers-Danlos(SED)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(is.na(before2010) & xxx >=2010, "2010 +", before2010))
dat$before2010 <- with(dat, ifelse(ID==65 & lang =="CAT" & nomalaltia=="Síndrome d’Ehlers-Danlos(SED)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==72 & lang =="CAT" & nomalaltia=="Síndrome d’Ehlers-Danlos(SED)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==74 & lang =="CAT" & nomalaltia=="Malformació de Chiari tipus 1", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(is.na(before2010) & as.character(TempsDiag)%in%c(">10 i <=20 anys", ">20 i <=30 anys", ">30 anys"), "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==106 & lang =="CAT" & nomalaltia=="Atàxia hereditària SCA 42", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==122 & lang =="CAT" & nomalaltia=="Es comporta com OSTEOGENESI IMPERFECTA. Però no ho poden diagnosticat .", "2010 +", before2010))
dat$before2010 <- with(dat, ifelse(ID==9 & lang =="CAS" & nomalaltia=="Atàxia de Friedrich (FRDA)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==39 & lang =="CAS" & nomalaltia=="Síndrome d’Ehlers-Danlos(SED)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==51 & lang =="CAS" & nomalaltia=="Síndrome d’Ehlers-Danlos(SED)", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==63 & lang =="CAS" & nomalaltia=="Paraparèsia espàstica hereditària", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==64 & lang =="CAS" & nomalaltia=="Sindrome EDS", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==134 & lang =="CAS" & nomalaltia=="Esclerodèrmia+altres", "Abans 2010", before2010))
dat$before2010 <- with(dat, ifelse(ID==145 & lang =="CAS" & nomalaltia=="Retinosi pigmentària", "Abans 2010", before2010))
dat$yyy <- dat$before2010

datok$idunic <- with(datok, paste(ID, lang, sep="-"))
dat$idunic <- with(dat, paste(ID, lang, sep="-"))

(repes <- with(datok,table(idunic)))[repes>1]

datok <- merge(datok, dat[, c("idunic", "yyy")], by="idunic", all.x=TRUE)
table(dat$yyy, useNA = "ifany")

xxx <- Hmisc::label(datok$before2010)
datok$before2010 <- factor(with(datok, ifelse(is.na(before2010), yyy, as.character(before2010))), levels = c("Abans 2010", "2010 +"))
Hmisc::label(datok$before2010) <- xxx
save(datok, file="./dat/datok.rda")

################################################################################
################ afegir CIPs  ##################################################
################################################################################
rm(list=ls())
library(readxl)
library(Hmisc)
library(gdata)
library(readxl)

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor/")

load("./dat/datok.rda")


cip <- data.frame(read_excel("./dat/ArreglaCIP.xlsx", sheet = 1, col_names = TRUE))

(repes <- with(cip ,table(zip)))[repes>1]

datok <- merge(datok, cip[, c("zip", "resideix")], by = "zip", all.x =TRUE)

datok$resideix <- with(datok, ifelse(resideix=="error", NA, resideix))
datok$resideix <- with(datok, ifelse(resideix=="Girona", "Girona Capital", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Lleida", "Lleida Capital", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Tarragona", "Tarragona Capital", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Provincia Girona", "Girona Provincia", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Provincia Lleida", "Lleida Provincia", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Provincia Tarragona", "Tarragona Provincia", resideix))
datok$resideix <- with(datok, ifelse(resideix=="Provincia Barcelona", "Barcelona Provincia", resideix))

Hmisc::label(datok$resideix) <- "Lloc on resideix"

save(datok, file="./dat/datok.rda")


