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
    X  <- model.matrix(price~.,houses %>% 
            dplyr::select(-c(X1,id,date,lat,long,zipcode, yr_renovated, sqft_basement, waterfront,sqft_lot15, sqft_living15, sqft_above)))[,-1]
    y  <- houses %>%
            dplyr::select(price) %>%
            as.matrix()
    
# LASSO
gridLength <- 1000 
grid       <- 10^seq(6, 1,length = gridLength)
model      <- glmnet(x = X, y = y, alpha = 1, standardize = TRUE, lambda = grid)
# model %>% plot(xvar = 'lambda', label = T)

# Montando os data frames com os valores de coeficiente, % do MQO e lambda:
numctodf <- function(x){data.frame(as.list(x))}

beta2    <-  numctodf(coef(model)[,1])
for(k in c(2:(gridLength))){
beta2    <- bind_rows(beta2, numctodf(coef(model)[,k]))
}

beta2pad   <-  numctodf(abs(coef(model)[,1])/abs(coef(model)[,gridLength]))
for(k in c(2:(gridLength))){
  beta2pad <- bind_rows(beta2pad, numctodf(abs(coef(model)[,k])/abs(coef(model)[,gridLength])))
}

lambda = data.frame(log(model$lambda,base = 10)) %>% rename(lambda = 1)
# Função que transforma o data frame dos coeficientes num df long indexado por variavel e valor de lambda
cleanCoefs <- function(x){data.frame(x, lambda) %>% 
                dplyr::select(-1,-2) %>% 
                group_by(lambda) %>% 
                gather(key, value, -lambda) %>% rename(Variavel = key)}

# Cria o df de coeficientes e o df em % do valor de MQO e joina ambos
beta2      <- cleanCoefs(beta2)
beta2pad   <- cleanCoefs(beta2pad)
betasFinal <- left_join(x = beta2, y = beta2pad, by = c('lambda'='lambda', 'Variavel'='Variavel'))

# Caminho dos coeficientes, comparação LASSO vs MQO:
g1 <- betasFinal %>% ggplot(aes(x = lambda, y = value.x)) + 
        geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle(TeX('Coeficientes vs. $\\lambda$')) + 
        xlab(TeX('log$\\lambda$')) + ylab('Coeficiente') 

 
g2 <- betasFinal %>% ggplot(aes(x = value.y, y = value.x))+ geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle('LASSO/MQO (%)') + 
        xlab(TeX('$||\\beta_{R}||.||\\beta_{MQO}||^{-1}$')) + ylab('Coeficiente')  + guides(color = F) 
  
g3 <- betasFinal %>% ggplot(aes(x = lambda,y = value.y)) + 
        geom_line(aes(color = Variavel), lwd=1.2) +
        ggtitle(TeX('$||\\beta_{L}||.||\\beta_{MQO}||^{-1}$ vs. $\\lambda$')) + 
        xlab(TeX('log$\\lambda$')) + ylab('%') + guides(color = F)  

# Organiza os graficos utilizando o patchwork:
g1/(g2+g3)

