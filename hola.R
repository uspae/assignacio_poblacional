library(readxl)
library(tidyverse)
CMBD_SMA <- read_excel("T:/DESA/SISTEMES DE PAGAMENT/Morbiditat/data/raw_data/CMBD_SMA.xlsx")
head(CMBD_SMA)
setwd("T:/DESA/SISTEMES DE PAGAMENT/Morbiditat")

CMBD_SMA<-CMBD_SMA%>%
  rename("nia"="NIA",
         "data_ingres"="DATA_INICI_PERIODE")
CMBD_SMA<-CMBD_SMA%>%
  mutate(V1=nia)
CMBD_SMA<-CMBD_SMA%>%
  mutate(VISITES=ifelse(is.na(VISITES),1,VISITES))

CMBD_SMA<-uncount(CMBD_SMA,VISITES)

CMBD_SMA[is.na(CMBD_SMA)] <- " "

CMBD_SMA<-CMBD_SMA%>%
  mutate(data_ingres=as.Date(data_ingres, format = "%d/%m/%y")) %>% 
  mutate(data_ingres = as.character(format(data_ingres, "%d/%m/%Y")))
  

MORBID <- CMBD_SMA %>% 
  mutate(nia = paste0(nia,mapply(paste0, mapply(rep, "0", 15 - nchar(nia)), 
                             collapse = "")),
         data_ingres = paste0(mapply(paste0, mapply(rep, "0", 
                                                    10 - nchar(data_ingres)), 
                                     collapse = ""), 
                              data_ingres), 
         proveidor = "1",
         DP = paste0(DP,mapply(paste0,mapply(rep, " ", 8 - nchar(DP)), 
                            collapse = "")), 
         DS1 = paste0(DS1,mapply(paste0, mapply(rep, " ", 8 - nchar(DS1)), 
                             collapse = "")), 
         DS2 = paste0(DS2,mapply(paste0, mapply(rep, " ", 8 - nchar(DS2)), 
                             collapse = "")), 
         DS3 = paste0(DS3,mapply(paste0, mapply(rep, " ", 8 - nchar(DS3)), 
                             collapse = "")), 
         DS4 = paste0(DS4,mapply(paste0, mapply(rep, " ", 8 - nchar(DS4)), 
                             collapse = "")), 
         DS5 = paste0(DS5,mapply(paste0, mapply(rep, " ", 8 - nchar(DS5)), 
                             collapse = "")), 
         DS6 = paste0(DS6,mapply(paste0, mapply(rep, " ", 8 - nchar(DS6)), 
                             collapse = "")), 
         DS7 = paste0(DS7,mapply(paste0, mapply(rep, " ", 8 - nchar(DS7)), 
                             collapse = "")), 
        
         PP = paste0(PP,mapply(paste0, mapply(rep, " ", 7 - nchar(PP)), 
                            collapse = "")), 
         PS1 = paste0(PS1,mapply(paste0, mapply(rep, " ", 7 - nchar(PS1)), 
                             collapse = "")), 
         codificacio="9",lloc_servei="6"
        ) %>% 
  select(nia,data_ingres,lloc_servei, proveidor, 
         codificacio, DP, DS1, DS2, DS3, DS4, DS5, DS6, PP, PS1,V1)



library(data.table)
RCA2022<-fread("D:/SM/RCA22_SM_1.txt",sep=";", encoding = "Latin-1",
               select = c("V1","V2","V5"))

RCA2022<-RCA2022%>%
  rename("sexe"="V5",
         "data_naix"="V2")

r<-RCA2022%>%
  mutate(data_naix=as.Date(data_naix, format = "%Y/%m/%D")) %>% 
  mutate(data_naix = as.character(format(data_naix, "%d/%m/%Y")))

MORBID<-MORBID%>%
  mutate(V1=as.numeric(nia))

MORBID$V1<-as.numeric(MORBID$V1)
MORBID<-left_join(MORBID,r)

MORBID<-MORBID%>%
  select(-V1)

MORBID<-MORBID%>%
  mutate(sexe=ifelse(sexe=="Home","1","6"))

MORBID<-MORBID%>%
select(nia, sexe, data_naix, data_ingres,lloc_servei, proveidor, 
       codificacio, DP, DS1, DS2, DS3, DS4, DS5, DS6,PP, PS1)
MORBID<-MORBID%>%
  mutate(data_discharge=data_ingres)

MORBID<-MORBID%>%
  select(nia, sexe, data_naix, data_ingres,data_discharge,lloc_servei, proveidor, 
         codificacio, DP, DS1, DS2, DS3, DS4, DS5, DS6,PP, PS1)

view(head(MORBID))
apply(MORBID,2,nchar)
MORBID<-MORBID%>%
  mutate(data_naix=format(as.Date(data_naix,"%Y-%m-%d"),"%d/%m/%Y"))

view(head(MORBID))
MORBID<-MORBID%>%
  drop_na()
MORBID2 <- MORBID %>% 
  unite(x, colnames(MORBID), sep = "", remove = TRUE)

fwrite(MORBID2,"D:/SM/MORBID_SMA_2_El_retorno2.txt", col.names = FALSE)
