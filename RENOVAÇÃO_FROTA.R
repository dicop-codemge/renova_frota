

#SOBRE
#Contexto: renova??o de frota (?nibus operando al?m da idade)
#Reequil?brio contratual em favor das concession?rias: receita mais baixa, apesar do ajuste tarif?rio



{
  # Passos iniciais
  
  # Definição do Diretório de Trabalho 
  setwd("C:/Users/guilhermecardoso/OneDrive - CODEMGE/Codemge/ACORDO OPERACIONAL FROTA RMBH/Renova_frota")
  
  # Verificar o diretório de trabalho
  getwd()
  
  # Remover todos objetos do Environment
  rm(list = ls())
  
} # Passos iniciais


#Instalando os pacotes
install.packages("openxlsx")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lpSolve")
install.packages("stringr")
install.packages("ggplot2")


#LIBRARIES
library(openxlsx)
library(dplyr)
library(tidyr)
library(lpSolve)
library(stringr)
library(ggplot2)

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
custo<-read.xlsx("Precos_Mercado.xlsx")
custo$`CARROCERIA_mb`<-NULL
custo$`CHASSI_caio`<-NULL
custo$`VALOR2`<-NULL

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
      print(str_c("Cen?rio ",max," ",med," terminado."))
    }
  }
}

n1 = length(30)
n2 = length(20)

sx = length(max)
sy = length(med)

data = expand.grid(X1 = sx, Y1 = sy)


##PARAMETROS DE OR?AMENTOS
budget<-seq(25000000,1000000000,by=25000000)
colnames(custo)<-c("TP","CUSTO_UN")
frota<-merge(frota,custo,by.x="TP",by.y="TP")
frota <- frota %>% mutate(index = row_number())
dados_duplicados <- frota %>% slice(rep(1:n(), QTDE)) %>% select(-QTDE) 
rownames(dados_duplicados) <- NULL
frota<-arrange(dados_duplicados,TP,ANO)
rm(dados_duplicados)
frota$index<-NULL
frota$ID<-c(1:nrow(frota))
frota2<-frota


j<-2026
i<-1
#custo<-arrange(custo,desc(CUSTO_UN))
#custo$CUSTO_UN<-0.025*custo$CUSTO_UN
frota2$ANO_CORRENTE<-2024
frota$ANO_CORRENTE<-2024
frota4<-frota
budget<-as.integer(budget)

setwd('SUBS')
file.remove(dir(getwd(),pattern = "*.csv$",full.names = TRUE))


## OBSERVAR MUDANÇA NAS CATEGORIAS DE VEÍCULO MEDIO->PADRON ",filter(frota2,TP=="MEDIO")"

#Novos condicionantes (REVISÃO Diego):

## Carros novos alugados em 2024 (idade=0) permanecem até 2028 (idade = 1,2,...) (OK)
## Não há carro novo (idade=0) a partir de 2025 (OK)
## Número total de carros inalterado a partir de 2024 (OK)

## Compra Sintran: novos carros no ano j (2024-2028) de acordo com o budget i (OK)
## Carros velhos substituídos por novos, a cada compra Sintran (OK)
## Veículos alugados em 2024 ainda devem ser considerados na frota (OK)

## NOVOS CONDICIONANTES 10/06
## SUBSTITUIÇÃO DA FROTA (PADRON -> MÉDIO; ARTC -> PADRON15M)
## O FATOR COMPENSAÇÃO DEVE SER APLICADO
## ** PARA FAZER A SUBSTIUIÇÃO, FOI ATRIBUÍDO AO TIPO SUBSTITUÍDO O PREÇO DO TIPO ALOCADO (EX. PADRON 800 MIL - PREÇO DE MÉDIO)



# Function to calculate current average age of the fleet
average_age <- function(frota) {
  mean(frota$IDADE)
}



# Assuming 'custo' dataframe contains the cost information by TP type

# Load necessary libraries if not already loaded
library(dplyr)
library(readr)

# Assuming 'custo' dataframe contains the cost information by TP type

for (i in 1:length(budget)) {
  frota <- frota4
  frota2 <- frota
  
  # Initialize new columns for tracking the origin of the vehicles
  frota2$ID_aluguel <- NA
  frota2$ID_venda <- NA
  
  for (j in 2024:2028) {
    # Filter and arrange frota2 for each TP and current year j
    frota3_padron <- filter(frota2, TP == "PADRON")
    frota3_articula <- filter(frota2, TP == "ARTICULA")
    frota3 <- rbind(frota3_articula, frota3_padron)
    frota3 <- arrange(frota3, desc(IDADE)) # Prioritize older vehicles
    
    custo_aluguel <- 0
    custo_aquisicao <- 0
    
    # While loop for aluguel within budget and vehicles available (only in 2024)
    if (j == 2024) {
      while (custo_aluguel <= budget[i] && nrow(frota3) > 0) {
        aluguel <- frota3[1, ]
        frota3 <- frota3[-1, , drop = FALSE]
        
        # Set ANO and IDADE for vehicles in 2024
        aluguel$ANO <- j
        aluguel$IDADE <- 0
        
        # Substitute PADRON with MEDIO and ARTICULA with PADRON_15M
        if (aluguel$TP == "PADRON") {
          aluguel$TP <- "MEDIO"
        } else if (aluguel$TP == "ARTICULA") {
          aluguel$TP <- "PADRON_15M"
        }
        
        # Update the cost according to the type from the custo table
        aluguel$CUSTO_UN <- custo$CUSTO_UN[custo$TP == aluguel$TP][1]
        
        # Calculate aluguel cost
        custo_aluguel <- custo_aluguel + (aluguel$CUSTO_UN) * (1.2) * (0.025) * (12)
        
        # Add the ID to the ID_aluguel column and reset the ID column
        aluguel$ID_aluguel <- aluguel$ID
        aluguel$ID <- NA
        
        # Update frota2
        frota2 <- rbind(frota2[!frota2$ID %in% aluguel$ID_aluguel, ], aluguel)
        
        # Re-sort frota3 to maintain priority on older vehicles
        frota3 <- arrange(frota3, desc(IDADE))
      }
    }
    
    # While loop for venda within budget and vehicles available
    while (custo_aquisicao <= budget[i] && nrow(frota3) > 0) {
      venda <- frota3[1, ]
      frota3 <- frota3[-1, , drop = FALSE]
      
      # Set ANO and IDADE for vehicles in the current year
      venda$ANO <- j
      venda$IDADE <- 0
      
      # Substitute PADRON with MEDIO and ARTICULA with PADRON_15M
      if (venda$TP == "PADRON") {
        venda$TP <- "MEDIO"
      } else if (venda$TP == "ARTICULA") {
        venda$TP <- "PADRON_15M"
      }
      
      # Update the cost according to the type from the custo table
      venda$CUSTO_UN <- custo$CUSTO_UN[custo$TP == venda$TP][1]
      
      # Calculate venda cost
      custo_aquisicao <- custo_aquisicao + (venda$CUSTO_UN)
      
      # Add the ID to the ID_venda column and reset the ID column
      venda$ID_venda <- venda$ID
      venda$ID <- NA
      
      # Update frota2
      frota2 <- rbind(frota2[!frota2$ID %in% venda$ID_venda, ], venda)
      
      # Re-sort frota3 to maintain priority on older vehicles
      frota3 <- arrange(frota3, desc(IDADE))
    }
    
    # Update frota2 with current year
    frota2$ANO_CORRENTE <- j
    
    # Update frota with frota2 for current year
    if (j == 2024) {
      frota <- frota2
    } else {
      frota <- rbind(frota, frota2)
    }
  }
  
  # Calculate IDADE
  frota$IDADE <- frota$ANO_CORRENTE - frota$ANO
  
  # Convert ID, ID_aluguel, and ID_venda to 0/1 columns
  frota$ID_Count <- as.integer(!is.na(frota$ID))
  frota$ID_aluguel_Count <- as.integer(!is.na(frota$ID_aluguel))
  frota$ID_venda_Count <- as.integer(!is.na(frota$ID_venda))
  
  # Write to CSV
  frota$budget <- budget[i]
  write.csv2(frota, paste0('Subs_SIMULT_', budget[i], '.csv'), row.names = FALSE)
}














                                                  


    



