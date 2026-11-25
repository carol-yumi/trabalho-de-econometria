# Limpeza
rm(list = ls())
gc()

# Instalação e carregamento dos pacotes necessários
pacotes <- c("dplyr", "data.table", "AER", "modelsummary", "car", "lmtest", "stringr")
install.packages(pacotes)
for (i in pacotes) {
  library(i, character.only = TRUE)}
library(readr)

# Preparação dos dados 
## carregando a base de dados em csv corrigindo a coluna de latitude para a formatação correta das casas decimais (algumas observações estão sem a separação decimal) 

## evitar notação científica na exibição do console
options(scipen = 999)

### carregar a base garantindo que pontos não sejam apagados como separador de milhar
dados <- read_delim(file.choose(), 
                    delim = ";", 
                    locale = locale(decimal_mark = ",", grouping_mark = ""))

### corrige latitude dividindo por 10^10 apenas onde não há separador decimal
dados <- dados %>%
  mutate(Latitude  = ifelse(abs(Latitude) > 90, Latitude / 1e10, Latitude))

dados <- as.data.table(dados)


## filtrando-a para trabalharmos apenas com os dados de 2022
base <- dados %>% 
  filter(str_detect(Data_Apreensao, "2022"))

## verificação de dados ausentes
relatorio_na <- data.table(
  Coluna = names(base),
  Qtd_NA = colSums(is.na(base)),
  Porcentagem = round(colSums(is.na(base)) / nrow(base) * 100, 2)
)

relatorio_na[Qtd_NA > 0][order(-Porcentagem)]

## verificação de outliers
hist(base$Quantidade_Item)
hist(base$Latitude)

## estatísticas descritivas
summary(base$Quantidade_Item)
summary(base$Latitude)

## remoção de outliers
quantile(base$Quantidade_Item, 0.99)
base <- base %>%
  filter(Quantidade_Item <= 1645)
hist(base$Quantidade_Item)
quantile(base$Quantidade_Item, 0.99)

base <- base %>%
  filter(Quantidade_Item <= 814)
hist(base$Quantidade_Item)
quantile(base$Quantidade_Item, 0.99)

base <- base %>%
  filter(Quantidade_Item <= 541)
hist(base$Quantidade_Item)
quantile(base$Quantidade_Item, 0.99)

base <- base %>%
  filter(Quantidade_Item <= 383)
hist(base$Quantidade_Item)
quantile(base$Quantidade_Item, 0.99)

summary(base$Quantidade_Item)
summary(base$Latitude)
var(base$Quantidade_Item)

# Definindo as variáveis de interesse 
## variáveis principais
apreen <- base$Quantidade_Item # variável dependente -> quantidade apreendida
lat <- base$Latitude # variável independente -> latitude (proxy para clima)

base <- base %>%
  mutate(Latitude_Categórica = case_when(
    Latitude >= -32.600 & Latitude <= -25 ~ 1, # Subtropical
    Latitude > -25 & Latitude <= -10 ~ 2, # Tropical
    Latitude > -10 & Latitude <= 4.443 ~ 3 # Equatorial
  ))

lat_categ <- base$Latitude_Categórica

## controles
### dummy de zona urbana ou rural
urbano <- ifelse(base$Zona_Ocorrencia == "URBANA", 1, 0)
mean(urbano)

### dummies das regiões
N <- ifelse(base$UF %in% c("AC","RO","AM","RR","PA","AP","TO"), 1, 0) 
NE <- ifelse(base$ UF %in% c("MA","PI","BA","SE","AL","PE","PB","RN","CE"), 1, 0)
CO <- ifelse(base$UF %in% c("MT","MS","GO","DF"), 1, 0)
SE <- ifelse(base$UF %in% c("SP","RJ","ES","MG"), 1, 0) 
S <- ifelse(base$UF %in% c("PR","SC","RS"), 1, 0)

mean(N)
mean(NE)
mean(CO)
mean(SE)
mean(S)

### dummies dos tipos de drogas (excluindo barbitúricos e merla por falta de dados)
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

mean(anf)
mean(coc)
mean(crack)
mean(ecst)
mean(hax)
mean(lp)
mean(lsd)
mean(mac)
mean(med)
mean(pmac)

### dummies do local de apreensão
viap <- ifelse(base$Tipo_Local_Ocorrencia == "VIAPUBLICA", 1, 0)
res <- ifelse(base$Tipo_Local_Ocorrencia == "RESIDENCIA", 1, 0)
com <- ifelse(base$Tipo_Local_Ocorrencia == "COMERCIAL", 1, 0)
BR <- ifelse(base$Tipo_Local_Ocorrencia == "BR", 1,0)

mean(viap)
mean(res)
mean(com)
mean(BR)

# Gráfico de dispersão
plot(apreen ~ lat, 
     data = base,
     main = "gráfico quantidade apreendida x latitude", 
     xlab = "latitude",
     ylab = "quantidade apreendida")

# Rodando as regressões

## Latitude como variável contínua
### apenas a variável independente
reg_simples <- lm(log(apreen + 1) ~ lat)

bptest(reg_simples)
bptest(reg_simples, ~ fitted(reg_simples) + I(fitted(reg_simples)^2))

summary(reg_simples)

### adicionando os controles
reg_contr <- lm(log(apreen + 1) ~ lat + N + NE + CO + SE + anf + coc 
                + crack + ecst + lp + lsd + mac + med + pmac + viap + com + BR)

bptest(reg_contr)
bptest(reg_contr, ~ fitted(reg_contr) + I(fitted(reg_contr)^2))

coeftest(reg_contr, vcov. = vcovHC, type = "HC1")

### montagem da tabela
teste <- list(simples = reg_simples, quadrático = reg_quad, múltiplo = reg_contr)
modelsummary(teste, output = "latex", fmt = fmt_sci(digits = 3))

## Latitude como variável categórica
### apenas variável categórica
reg_categ <- lm(log(apreen + 1) ~ lat_categ)

bptest(reg_categ)
bptest(reg_categ, ~ fitted(reg_categ) + I(fitted(reg_categ)^2))

summary(reg_categ)

### adicionando os controles
reg_contr_categ <- lm(log(apreen + 1) ~ lat_categ + N + NE + CO
                      + SE + anf + coc + crack + ecst + lp + lsd + mac + med 
                      + pmac + viap + com + BR)

bptest(reg_contr_categ)
bptest(reg_contr_categ, ~ fitted(reg_contr_categ) + I(fitted(reg_contr_categ)^2))

coeftest(reg_contr_categ, vcov. = vcovHC, type = "HC1")

### montagem da tabela
modelos <- list(simples = reg_categ, quadratico = reg_quad_categ, múltiplo = reg_contr_categ)
modelsummary(modelos, output = "latex")