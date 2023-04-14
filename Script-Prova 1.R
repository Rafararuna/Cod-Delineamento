###############################PROVA 1######################################################

## Questão 1

## Questão 2

#a) 

#Fazendo o cálculo para M=0:
  
v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_1 <-  c(v_i,v_n,v_n,v_n,v_n, v_i, v_n, v_n,v_n, v_n, v_i, v_n,v_n, v_n, v_n, v_i)

X_1 <- matrix(valores_1, ncol=4) # matriz de delineamento X
print(X_1)

#Fazendo o cálculo para t_1=0:

v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_2 <-  c(v_i,v_i,v_i,v_i,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i)

X_2 <-  matrix(valores_2, ncol=4) # matriz de delineamento X 
print(X_2)

#Fazendo o cálculo para t_4=0:

v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_3 <-  c(v_i,v_i,v_i,v_i,v_i,v_n,v_n,v_n,v_n,v_i,v_n,v_n,v_n,v_n,v_i,v_n)

X_3 <-  matrix(valores_3, ncol=4) # matriz de delineamento X
print(X_3)

#b)

#Sabemos que:
Y <-  c(7.66,6.98,7.80,5.26,5.44,5.80,7.41,7.33,7.04,3.51,2.91,3.66)

#Fazendo o cálculo para M=0:

#Da questão anterior, temos que:
v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_1 <-  c(v_i,v_n,v_n,v_n,v_n, v_i, v_n, v_n,v_n, v_n, v_i, v_n,v_n, v_n, v_n, v_i)

X_1 <-  matrix(valores_1, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y # que são os betas estimados
print(b_estimado)

#Fazendo o cálculo para t_1=0:

#Da questão anterior, temos que:
v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_2 <-  c(v_i,v_i,v_i,v_i,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i)

X_2 <-  matrix(valores_2, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_2)%*%X_2)%*%t(X_2)%*%Y # são os betas estimados
print(b_estimado)

#Fazendo o cálculo para t_4=0:

#Da questão anterior, temos que:
v_i <-  c(1,1,1) # que é o vetor identidade
v_n <-  c(0,0,0) # que é o vetor nulo

valores_3 <-  c(v_i,v_i,v_i,v_i,v_i,v_n,v_n,v_n,v_n,v_i,v_n,v_n,v_n,v_n,v_i,v_n)

X_3 = matrix(valores_3, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_3)%*%X_3)%*%t(X_3)%*%Y # são os betas estimados
print(b_estimado)

#c)

#Sabemos que:
Y = c(7.66,6.98,7.80,5.26,5.44,5.80,7.41,7.33,7.04,3.51,2.91,3.66)

tratamentos = rep(1:4,each=3)

banco = data.frame(y=Y,trat=factor(tratamentos))

ANOVA = aov(y~trat, banco)
summary(ANOVA)
aov(y~trat-1,banco)$coefficients

# Comparando os resultados com as estimativas obtidas com a PROC GLM do SAS, nota-se que 
# as médias dos tratamentos obtidas no código acima é igual a matriz dos betas estimados 
# na restrição M=0, a qual é formada pelas médias dos tratamentos .

## Questão 3

#Abrindo o banco:
tabela_3=tribble(~y, ~dieta,
                 3.52, 1,
                 3.36, 1,
                 3.57, 1,
                 4.19, 1,
                 3.88, 1,
                 3.76, 1,
                 3.94, 1,
                 3.47, 2,
                 3.73, 2,
                 3.38, 2,
                 3.87, 2,
                 3.69, 2,
                 3.51, 2,
                 3.35, 2,
                 3.64, 2,
                 3.54, 3,
                 3.52, 3,
                 3.61, 3,
                 3.76, 3,
                 3.65, 3,
                 3.51, 3,
                 3.74, 4,
                 3.83, 4,
                 3.87, 4,
                 4.08, 4,
                 4.31, 4,
                 3.98, 4,
                 3.86, 4,
                 3.71, 4)

tabela_3$dieta <- as.factor(tabela_3$dieta)

#a) Para a restrição sum(t_i) = 0, temos que: 

m_geral <- mean(tabela_3$y)
print(m_geral)

#calculando a média de cada tratamento:
m_de_tratamento <-  tabela_3 %>% 
  group_by(dieta) %>% 
  summarise(m_de_tratamento=mean(y)) %>%
  ungroup() %>% 
  pull(m_de_tratamento) # que são as médias de cada tratamento

efeitos_de_tratamento <-   m_de_tratamento - m_geral # que são os valores dos tau_i's, que são os estimadores que a questão pede.
print(efeitos_de_tratamento)

#b) Para a restrição sum(t_i) = 0, temos que: 

ggplot() +
  geom_boxplot(aes(tabela_3$dieta,tabela_3$y-m_geral)) +
  geom_point(aes(c(1,2,3,4),efeitos_de_tratamento), col= 5) +
  xlab("Tratamentos") +
  ylab("Valores") +
  theme_bw()

#c)

attach(tabela_3)
ANOVA <-  aov(formula = y~dieta, tabela_3)
summary(ANOVA)

# Analisando a saída da tabela da análise de variância, temos que o p-valor foi de 0,0102. 
# Considerando um teste F e um alfa de de 10%, ou seja, 0,01, concluimos que não temos evidências 
# para rejeitar a hipótese nula (M_1=M_2=M_3=M_4), já que p-valor é maior que o alfa.

#d) Não, pois não rejeitamos a hipótese nula (M_1=M_2=M_3=M_4), ou seja, as medias são iguais, 
#   dessa forma, não faz sentido realizar comparações múltiplas.
#   Resumindo, só seria possível de fazer alguma comparação múltipla, se tivéssemos reiejeitado 
#   a hipotése nula, o que significaria que pelo menos uma média de tratamento se difere dos 
#   demais.

