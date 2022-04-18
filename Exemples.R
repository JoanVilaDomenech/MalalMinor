rm(list=ls())

library(Hmisc)
library(gdata)
library(Epi)
library(compareGroups)
library(xtable)

library(ggplot2)

setwd("/Users/jvila/Dropbox/NCodern/MalalMinor/")

load("./dat/datok.rda")
dat <- datok

dat$xxx <- with(dat, ifelse(is.na(relacio) & is.na(nomalaltia), 1, 0))
dat$xxx <- with(dat, ifelse(lang=="CAT" & ID==125, 1, xxx))
dat <- subset(dat, xxx==0)

# Per veure el que hi ha
Hmisc::label(dat$SuportSalutLabor)
x <- with(dat, table(SuportSalutLabor, useNA = "ifany"))
x <- cbind(x, round(prop.table(x)*100, 2))
attr(x, "dimnames")[[2]] <- c("n", "%")
x

# eliminant Missing/No aplicable (apart de ser una persona afectada)
subdat <- subset(dat, !is.na(afectat) & afectat =="Y" & SuportSalutLabor!="Missing/No aplicable")
subdat$SuportSalutLabor <- factor(subdat$SuportSalutLabor, levels(subdat$SuportSalutLabor)[levels(subdat$SuportSalutLabor)%nin%"Missing/No aplicable"])
print(createTable(compareGroups(before2010~SuportSalutLabor, data=subdat), 
            show.all = TRUE, all.last = TRUE), header.labels = c("all" = "TOTS"))



# sense distingir moment
yyy <- prop.table(with(subdat, table(SuportSalutLabor)))*100
tobeplot <- data.frame(xxx=names(yyy), yyy)
# tobeplot <- tobeplot[order(tobeplot$Freq, decreasing = TRUE), ] # per posar ordre

ggplot(tobeplot, aes(x=xxx, y=Freq, fill=xxx))+
geom_bar(stat="identity", show.legend = FALSE)+
scale_fill_brewer(palette="Dark2") +
scale_x_discrete(limits=tobeplot$xxx)  + 
labs(x = NULL, y = "Percentatge", title =Hmisc::label(dat$SuportSalutLabor)) +
theme_minimal()

# per grups abans/despres 2010
yyy <- prop.table(with(subdat, table(SuportSalutLabor, before2010)), margin = 2)*100

abans <- data.frame(cbind("Abans 2010", yyy[, attr(yyy, "dimnames")$before2010 == "Abans 2010"]))
abans$xxx <- rownames(abans)
names(abans) <- c("moment", "yyy", "xxx")

despres <- data.frame(cbind("2010 +", yyy[, attr(yyy, "dimnames")$before2010 == "2010 +"]))
despres$xxx <- rownames(despres)
names(despres) <- c("moment", "yyy", "xxx")

tobeplot <- rbind(abans, despres)
tobeplot$yyy <- as.numeric(tobeplot$yyy)

tobeplot$moment <- factor(tobeplot$moment, levels = c("Abans 2010", "2010 +"))
tobeplot$xxx <- factor(tobeplot$xxx, levels = abans$xxx)

ggplot(tobeplot, aes(x=moment, y=yyy, fill=xxx))+
geom_bar(stat="identity", position="dodge",show.legend = TRUE)+
scale_fill_brewer(palette="Dark2") +
# scale_x_discrete(limits=tobeplot$team)  +
labs(x = NULL, y = "Percentatge", title =Hmisc::label(dat$SuportSalutLabor), fill = NULL ) +
theme_minimal()

