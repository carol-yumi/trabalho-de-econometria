# Carregando bibliotecas

install.packages("readxl")
library(readxl)
library(dplyr)
library(stringr) 


# Preparação dos dados 

# carregando a base de dados em Excel
dados <- read_excel(file.choose())

# filtrando-a para trabalharmos apenas com os dados de 2022
base <- dados %>% 
  filter(str_detect(Data_Apreensao, "2022"))


# Definindo as variáveis de interesse 

# variáveis principais
apreen <- base$Quantidade_Item # variável dependente -> quantidade apreendida
lat <- base$Latitude # variável independente -> latitude (proxy para clima)

# controles
# dummy de zona urbana ou rural
urbano <- ifelse(base$Zona_Ocorrencia == "URBANA", 1, 0) 
# dummies das regiões
N <- ifelse(base$UF == c("AC","RO","AM","RR","PA","AP","TO"), 1, 0) 
NE <- ifelse(base$ UF == c("MA","PI","BA","SE","AL","PE","PB","RN","CE"), 1, 0)
CO <- ifelse(base$UF == c("MT","MS","GO","DF"), 1, 0)
SE <- ifelse(base$UF == c("SP","RJ","ES","MG"), 1, 0) 
S <- ifelse(base$UF == c("PR","SC","RS"), 1, 0)
# dummies dos tipos de drogas (excluindo barbitúricos e merla)
anf <- ifelse(base$Tipo_Apreensao == "Anfetaminas", 1, 0) 
coc <- ifelse(base$Tipo_Apreensao == "Cocaína/Cloridrato de Cocaína", 1, 0) 
crack <- ifelse(base$Tipo_Apreensao == "Crack", 1, 0) 
ecst <- ifelse(base$Tipo_Apreensao == "Ecstasy", 1, 0) 
hax <- ifelse(base$Tipo_Apreensao == "Haxixe", 1, 0) 
lp <- ifelse(base$Tipo_Apreensao == "Lança-perfume", 1, 0) 
lsd <- ifelse(base$Tipo_Apreensao == "LSD", 1, 0) 
mac <- ifelse(base$Tipo_Apreensao == "Maconha", 1, 0) 
med <- ifelse(base$Tipo_Apreensao == "Medicamentos", 1, 0) 
pmac <- ifelse(base$Tipo_Apreensao == "Pés de Maconha", 1, 0) 
# dummies do local de apreensão
viap <- ifelse(base$Tipo_Local_Ocorrencia == "VIAPUBLICA", 1, 0)
res <- ifelse(base$Tipo_Local_Ocorrencia == "RESIDENCIA", 1, 0)
com <- ifelse(base$Tipo_Local_Ocorrencia == "COMERCIAL", 1, 0)
BR <- ifelse(base$Tipo_Local_Ocorrencia == "BR", 1,0)

# Rodando as regressões 

# apenas a variável independente
reg_simples <- lm(apreen ~ lat, data = base)
summary(reg_1)

# adicionando os controles
reg_contr <- lm(apreen ~ lat + N + NE + CO + SE + S + anf + coc + crack + ecst + hax + lp + lsd + mac + med + pmac + viap + res + com + BR , data = base)
summary(reg_contr)


