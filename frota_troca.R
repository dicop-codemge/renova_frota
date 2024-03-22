#LIBRARIES
library(openxlsx)
library(dplyr)
library(tidyr)
library(lpSolve)
library(lpSolve)
library(stringr)

#CARREGANDO ARQUIVOS
frota<-read.xlsx(file.choose())
frota_transformada <- pivot_longer(frota, cols = -Ano.de.Fabricação, names_to = "Ano", values_to = "Qtde")
frota<-filter(frota_transformada,frota_transformada$Qtde!=0)
colnames(frota)<-c("TP","ANO","QTDE")
frota$TP<-gsub(' 1','',frota$TP)
frota$TP<-gsub(' 2','',frota$TP)
frota$TP<-gsub(' 3','',frota$TP)
frota$TP<-gsub(' 4','',frota$TP)
frota$TP<-gsub(' 5','',frota$TP)
rm(frota_transformada)

setwd(choose.dir())

#PARAMETROS DE MÉDIA
max<-0
med<-0
i<-as.numeric(format(Sys.Date(),"%Y"))+1

#CALCULO IDADE
frota$ANO<-as.numeric(frota$ANO)
frota$IDADE<-i-frota$ANO
frota2<-frota


for(max in 1:30){
  for(med in 1:20){
    
    frota2<-frota
    compra_ano<-frota2 %>% dplyr::select(TP,QTDE)
    compra_ano[1:nrow(compra_ano),]<-NA
    compra_ano$ANO_COMPRA<-NA
    for(i in 2024:2054){
    frota2$IDADE<-i-as.numeric(frota2$ANO)
    if(max(frota2$IDADE)>=max){
      compra<-filter(frota2,IDADE>=max)
      compra$ANO<-i
      compra$IDADE<-0
      compra_anocorrente<-compra %>% dplyr::select(TP,QTDE) %>% dplyr::group_by(TP) %>% dplyr::summarise(QTDE=sum(QTDE))
      compra_anocorrente$ANO_COMPRA<-i
      compra_ano<-rbind(compra_ano,compra_anocorrente)
      frota2<-filter(frota2,IDADE<max)
      frota2<-rbind(frota2,compra)
      frota2<-frota2 %>% dplyr::select(TP,ANO,QTDE,IDADE) %>% dplyr::group_by(TP,ANO) %>% dplyr::summarise(QTDE=sum(QTDE))
      frota2$IDADE<-i-as.numeric(frota2$ANO)
    }else if((sum(frota2$IDADE*frota2$QTDE)/sum(frota2$QTDE))>med){
      while(sum(frota2$IDADE*frota2$QTDE)/sum(frota2$QTDE)>med){
        compra<-filter(frota2,IDADE == max(frota2$IDADE))
        compra$ANO<-i
        compra$IDADE<-0
        compra_anocorrente<-compra %>% dplyr::select(TP,QTDE) %>% dplyr::group_by(TP) %>% dplyr::summarise(QTDE=sum(QTDE))
        compra_anocorrente$ANO_COMPRA<-i
        compra_ano<-rbind(compra_ano,compra_anocorrente)
        frota2<-filter(frota2,IDADE != max(frota2$IDADE))
        frota2<-rbind(frota2,compra)
        frota2<-frota2 %>% dplyr::select(TP,ANO,QTDE,IDADE) %>% dplyr::group_by(TP,ANO) %>% dplyr::summarise(QTDE=sum(QTDE))
        frota2$IDADE<-i-as.numeric(frota2$ANO)
        }
      }
    }
    compra_ano$MED<-med
    compra_ano$MAX<-max
    compra_ano<-filter(compra_ano,!is.na(compra_ano$TP))
    write.csv2(compra_ano,str_c('compra_ano_',med,'_',max,'.csv'),row.names=F)
  }
}





