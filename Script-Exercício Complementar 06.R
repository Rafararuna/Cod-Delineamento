##Questão 1 - Lista 5

#Reproduzir o exemplo 8.8 de Oehlert, página 187.

#Carregando pacotes:

library(tidyverse)
library(lawstat)
library(lmtest)
library(car)
library(EnvStats)
library(olsrr)
library(nortest)
library(PMCMRplus)

#Primeiro vamos abrir o banco de dados e ver a sua estrutura:
  
dados = read.table(file.choose(),header = TRUE)

head(dados)
str(dados)

#Agora vamos fazer um reformatação nas variáveis:

dados$alg = as.factor(dados$alg)
dados$seq = as.factor(dados$seq)
dados$psize = as.factor(dados$psize)
dados$ram = as.factor(dados$ram)
dados$y <- log(dados$y)

#Agora, montamos o modelo e fazemos a anova:
  
mod1 = aov(y ~ seq + psize + ram + alg + seq*psize + seq*ram + seq*alg + psize*ram + psize*alg + ram*alg + seq*psize*ram + seq*ram*alg + seq*psize*alg + psize*ram*alg,data=dados)

summary(mod1)
anova(mod1)

#Agora vamos realizar o o diagnóstico:

plot(mod1$fitted.values,rstudent(mod1),pch=16)
abline(h=2,col=2)
abline(h=-2,col=2)

#Verificando Normalidade

library(lawstat)
library(lmtest)
library(car)
library(EnvStats)
library(olsrr)
library(nortest)
library(PMCMRplus)

qqnorm(mod1$residuals, pch=16)
qqline(mod1$residuals,col=2)

shapiro.test(mod1$residuals)

ad.test(mod1$residuals)

ks.test(mod1$residuals,'pnorm',mean=mean(mod1$residuals),sd=sd(mod1$residuals))

#Verificando a homocedasticidade da variância:
  
bptest(mod1)

#Verificando a independência dos erros:

dwtest(mod1)

a <- names(effects(mod1))
tabela <- tibble(a = if_else(a == "","residuals",a), e = as.vector(effects(mod1))) %>% 
  mutate(te = gsub('[0-9]+', '',a)) %>% 
  mutate(te = factor(te,levels = unique(te))) %>% 
  mutate(te=)

ggplot(tabela,aes(te,e)) +
  geom_point()+
  xlab("Tipo de Efeito") +
  ylab("Valor do efeito") +
  theme_bw()+
  theme(axis.text = element_text(angle = 45,vjust = 0.5, hjust=1)) 