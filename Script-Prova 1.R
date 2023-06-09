###############################PROVA 1######################################################

## Quest�o 1

## Quest�o 2

#a) 

#Fazendo o c�lculo para M=0:
  
v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_1 <-  c(v_i,v_n,v_n,v_n,v_n, v_i, v_n, v_n,v_n, v_n, v_i, v_n,v_n, v_n, v_n, v_i)

X_1 <- matrix(valores_1, ncol=4) # matriz de delineamento X
print(X_1)

#Fazendo o c�lculo para t_1=0:

v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_2 <-  c(v_i,v_i,v_i,v_i,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i)

X_2 <-  matrix(valores_2, ncol=4) # matriz de delineamento X 
print(X_2)

#Fazendo o c�lculo para t_4=0:

v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_3 <-  c(v_i,v_i,v_i,v_i,v_i,v_n,v_n,v_n,v_n,v_i,v_n,v_n,v_n,v_n,v_i,v_n)

X_3 <-  matrix(valores_3, ncol=4) # matriz de delineamento X
print(X_3)

#b)

#Sabemos que:
Y <-  c(7.66,6.98,7.80,5.26,5.44,5.80,7.41,7.33,7.04,3.51,2.91,3.66)

#Fazendo o c�lculo para M=0:

#Da quest�o anterior, temos que:
v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_1 <-  c(v_i,v_n,v_n,v_n,v_n, v_i, v_n, v_n,v_n, v_n, v_i, v_n,v_n, v_n, v_n, v_i)

X_1 <-  matrix(valores_1, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y # que s�o os betas estimados
print(b_estimado)

#Fazendo o c�lculo para t_1=0:

#Da quest�o anterior, temos que:
v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_2 <-  c(v_i,v_i,v_i,v_i,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i,v_n ,v_n ,v_n ,v_n ,v_i)

X_2 <-  matrix(valores_2, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_2)%*%X_2)%*%t(X_2)%*%Y # s�o os betas estimados
print(b_estimado)

#Fazendo o c�lculo para t_4=0:

#Da quest�o anterior, temos que:
v_i <-  c(1,1,1) # que � o vetor identidade
v_n <-  c(0,0,0) # que � o vetor nulo

valores_3 <-  c(v_i,v_i,v_i,v_i,v_i,v_n,v_n,v_n,v_n,v_i,v_n,v_n,v_n,v_n,v_i,v_n)

X_3 = matrix(valores_3, ncol=4) # matriz de delineamento X

#Com o resultado acima, conseguimos calcular:
b_estimado <-  solve(t(X_3)%*%X_3)%*%t(X_3)%*%Y # s�o os betas estimados
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
# as m�dias dos tratamentos obtidas no c�digo acima � igual a matriz dos betas estimados 
# na restri��o M=0, a qual � formada pelas m�dias dos tratamentos .

## Quest�o 3

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

#a) Para a restri��o sum(t_i) = 0, temos que: 

m_geral <- mean(tabela_3$y)
print(m_geral)

#calculando a m�dia de cada tratamento:
m_de_tratamento <-  tabela_3 %>% 
  group_by(dieta) %>% 
  summarise(m_de_tratamento=mean(y)) %>%
  ungroup() %>% 
  pull(m_de_tratamento) # que s�o as m�dias de cada tratamento

efeitos_de_tratamento <-   m_de_tratamento - m_geral # que s�o os valores dos tau_i's, que s�o os estimadores que a quest�o pede.
print(efeitos_de_tratamento)

#b) Para a restri��o sum(t_i) = 0, temos que: 

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

# Analisando a sa�da da tabela da an�lise de vari�ncia, temos que o p-valor foi de 0,0102. 
# Considerando um teste F e um alfa de de 10%, ou seja, 0,01, concluimos que n�o temos evid�ncias 
# para rejeitar a hip�tese nula (M_1=M_2=M_3=M_4), j� que p-valor � maior que o alfa.

#d) N�o, pois n�o rejeitamos a hip�tese nula (M_1=M_2=M_3=M_4), ou seja, as medias s�o iguais, 
#   dessa forma, n�o faz sentido realizar compara��es m�ltiplas.
#   Resumindo, s� seria poss�vel de fazer alguma compara��o m�ltipla, se tiv�ssemos reiejeitado 
#   a hipot�se nula, o que significaria que pelo menos uma m�dia de tratamento se difere dos 
#   demais.

