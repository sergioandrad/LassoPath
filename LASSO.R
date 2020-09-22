## Visualizando as soluções do LASSO
# Carregando pacotes e troca o diretorio de trabalho
library(glmnet)
library(tidyverse)
library(gganimate)
library(patchwork)
library(latex2exp)
setwd('C://Users//sergi//Desktop//Trabalho - Ronaldo')

# Preparando os dados
houses <- read_csv('houses.csv')
    X  <- houses %>% 
            dplyr::select(-c(X1,id,date,lat,long,zipcode, yr_renovated, sqft_basement)) %>%
            model.matrix(price~., .)
    y  <- houses %>%
            dplyr::select(price) %>%
            as.matrix()

# MQO
modelMQO   <- glmnet(x = X, y = y, alpha = 1, standardize = TRUE, lambda = 0)
coefsMQO   <- coef(modelMQO)[,1]    
    
# LASSO
gridLength <- 1000
grid       <- 10^seq(5.3,-3, length = gridLength)
model      <- glmnet(x = X, y = y, alpha = 1, standardize = TRUE, lambda = grid)

# Montando os data frames com os valores de coeficiente, % do MQO e lambda:
numctodf <- function(x){data.frame(as.list(x))}

beta2    <-  numctodf(coef(model)[,1])
for(k in c(2:gridLength)){
beta2    <- bind_rows(beta2, numctodf(coef(model)[,k]))
}

beta2pad   <-  numctodf(coef(model)[,1]/coefsMQO)
for(k in c(2:gridLength)){
  beta2pad <- bind_rows(beta2pad, numctodf(coef(model)[,k]/coefsMQO))
}

# Função que transforma o data frame dos coeficientes num df long indexado por variavel e valor de lambda
cleanCoefs <- function(x){data.frame(x, lambda = data.frame(model$lambda)) %>% 
                dplyr::select(-1,-2) %>% 
                group_by(model.lambda) %>% 
                gather(key, value, -model.lambda) %>% rename(Variavel = key, lambda = model.lambda)}

# Cria o df de coeficientes e o df em % do valor de MQO e joina ambos
beta2      <- cleanCoefs(beta2)
beta2pad   <- cleanCoefs(beta2pad)
betasFinal <- left_join(x = beta2, y = beta2pad, by = c('lambda'='lambda', 'Variavel'='Variavel'))

# Caminho dos coeficientes, comparação LASSO vs MQO:
g1 <- betasFinal %>% ggplot(aes(x = lambda,y = value.x)) + 
        geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle(TeX('Coeficientes vs. $\\lambda$')) + 
        xlab(TeX('$\\lambda$')) + ylab('Coeficiente') 

 
g2 <- betasFinal %>% ggplot(aes(x = value.y, y = value.x))+ geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle('LASSO/MQO (%)') + 
        xlab(TeX('$||\\beta_{R}||.||\\beta_{MQO}||^{-1}$')) + ylab('Coeficiente')  + guides(color = F) 
  
g3 <- betasFinal %>% ggplot(aes(x = lambda,y = value.y)) + 
        geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle(TeX('$||\\beta_{L}||.||\\beta_{MQO}||^{-1}$ vs. $\\lambda$')) + 
        xlab(TeX('$\\lambda$')) + ylab('%') + guides(color = F)  

# Organiza os graficos utilizando o patchwork:
g1/(g2+g3)
