#LIBRARIES
library(openxlsx)
library(dplyr)
library(tidyr)
library(lpSolve)
library(lpSolve)
library(stringr)

#CARREGANDO ARQUIVOS
frota<-read.xlsx('frota_atual.xlsx')
frota_transformada <- pivot_longer(frota, cols = -Ano.de.Fabricação, names_to = "Ano", values_to = "Qtde")
frota<-filter(frota_transformada,frota_transformada$Qtde!=0)
colnames(frota)<-c("TP","ANO","QTDE")
frota$TP<-gsub(' 1','',frota$TP)
frota$TP<-gsub(' 2','',frota$TP)
frota$TP<-gsub(' 3','',frota$TP)
frota$TP<-gsub(' 4','',frota$TP)
frota$TP<-gsub(' 5','',frota$TP)
rm(frota_transformada)
custo<-read.xlsx("custo_por_veiculo.xlsx")
custo$`CUSTO.(jul/23)`<-NULL

#CALCULO IDADE
i<-as.numeric(format(Sys.Date(),"%Y"))
frota$ANO<-as.numeric(frota$ANO)
frota$IDADE<-i-frota$ANO
frota2<-frota

#PARAMETROS DE MÉDIA E MÁXIMA
max<-1
med<-1

for(max in 1:30){
  for(med in 1:20){
    if(med<=max){
    frota2<-frota
    compra_ano<-frota2 %>% dplyr::select(TP,QTDE)
    compra_ano[1:nrow(compra_ano),]<-NA
    compra_ano$ANO_COMPRA<-NA
    for(i in 2024:2054){
    frota2$IDADE<-i-as.numeric(frota2$ANO)
    if(max(frota2$IDADE)>=max){
      compra<-filter(frota2,IDADE>=max)
      compra$ANO<-i
      compra<-compra %>% dplyr::select(TP,ANO,QTDE) %>% dplyr::group_by(TP,ANO) %>% dplyr::summarise(QTDE=sum(QTDE),IDADE=0)
      compra_anocorrente<-compra %>% dplyr::select(TP,QTDE) %>% dplyr::group_by(TP) %>% dplyr::summarise(QTDE=sum(QTDE))
      compra_anocorrente$ANO_COMPRA<-i
      compra_ano<-rbind(compra_ano,compra_anocorrente)
      frota2<-filter(frota2,IDADE<max)
      frota2<-rbind(frota2,compra)
      frota2<-frota2 %>% dplyr::select(TP,ANO,QTDE,IDADE) %>% dplyr::group_by(TP,ANO) %>% dplyr::summarise(QTDE=sum(QTDE))
      frota2$IDADE<-i-as.numeric(frota2$ANO)
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
    }else if (sum(frota2$IDADE*frota2$QTDE)/sum(frota2$QTDE)>med){
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
    print(str_c("Cenário ",max," ",med," terminado."))
  }
  }
}

##PARAMETROS DE CUSTOS
budget<-seq(10000000,2000000000,by=10000000)
frota2<-frota
colnames(custo)<-c("TP","CUSTO_UN")
frota2<-merge(frota2,custo,by.x="TP",by.y="TP")
frota2 <- frota2 %>% mutate(index = row_number())
dados_duplicados <- frota2 %>% slice(rep(1:n(), QTDE)) %>% select(-QTDE) 
rownames(dados_duplicados) <- NULL
frota2<-arrange(dados_duplicados,TP,ANO)
rm(dados_duplicados)
frota2$index<-NULL
frota2$ID<-c(1:nrow(frota2))

j<-2024
i<-0
custo<-arrange(custo,desc(CUSTO_UN))
custo$CUSTO_UN<-0.025*custo$CUSTO_UN

setwd('VEICULOS_VENDIDOS')

for(i in 1:length(budget)){
  frota3<-rbind(filter(frota2,TP=="PADRON"),filter(frota2,TP=="BASICO"))
  for(j in 2024:2029){
    custo_aquisicao<-0
    while(custo_aquisicao<=budget[i]){
      venda<-frota3[1,]
      frota3[1,]<-NA
      frota3<-filter(frota3,!is.na(TP))
      venda$ANO<-j
      frota3<-rbind(frota3,venda)
      custo_aquisicao<-custo_aquisicao+venda$CUSTO_UN[1]
    }
  }
  veiculos_vendidos<-filter(frota3,ANO %in% c(2024:2054))
  veiculos_vendidos$budget<-budget[i]*5
  write.csv(veiculos_vendidos,str_c('veiculos_comprados_budget_',budget[i],'.csv'),row.names = F)
}
