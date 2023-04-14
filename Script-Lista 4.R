######################LISTA 4######################################

## Questão 1 - ?


## Questão 2 

controle <- c(45,39,40,45,42)
glucose <- c(25,28,30,29,33)
frutose <- c(28,31,24,28,27)
sucrose <- c(31,37,35,33,34)

data.frame()

mod2 <- lm(controle~glucose+frutose+sucrose)

#normalidade: sao normais

shapiro.test(mod2$residuals)

#homocedasticidade da variancia: são homogeneas

install.packages("lmtest")
require(lmtest)
library(lmtest)

bptest(mod2)

ols_test_breusch_pagan(mod2)

#independencia dos erros: não são independentes

require(lmtest)
dwtest(mod2)

## Questão 3

dados <- c(14.3, 16.0, 17.3, 17.5, 17.8, 18.7, 18.8, 18.9,
           20.0, 20.8, 21.4, 22.7, 23.2, 25.6, 27.8)


# (a) 

qqnorm(dados, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra",font = 2)
qqline(dados,col=2,type=2)

# (b) Só observando o gráfico, é razoavel sim dizer que os dados semguem uma normalidade, ja que os dados estao proximos da reta

shapiro.test(dados) #pra dizer com certeza,faz-se o shapiro, o qual nao rejeita Ho, ou seja, segue uma normalidade.

# (c)

qqnorm(rnorm(n = 80,mean = 5, sd = 2), 
       main="QQPlot da Normal Padrão",
       xlab = "Quantis Teóicos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rnorm(n = 80,mean = 5, sd = 2),col=2)
qqnorm(rt(80 ,5), 
       main="QQPlot da t com 5 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rt(80 ,5),col=2)
qqnorm(rt(80 ,20), 
       main="QQPlot da t com 20 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rt(80 ,20),col=2)
qqnorm(rt(80 ,40), 
       main="QQPlot da t com 40 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rt(80 ,40),col=2)
qqnorm(rcauchy(80), 
       main="QQPlot da Cauchy",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rcauchy(80),col=2)
qqnorm(rchisq(80 ,5), 
       main="QQPlot da Qui-Quadrado com 5 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rchisq(80 ,5),col=2)
qqnorm(rchisq(80 ,20), 
       main="QQPlot da Qui-Quadrado com 20 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rchisq(80 ,20),col=2)
qqnorm(rchisq(80 ,40), 
       main="QQPlot da Qui-Quadrado com 40 gl ",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       font.label=2, font=2)
qqline(rchisq(80 ,40),col=2)

## 4 - teste de brown-forysthe, teste de levene ou teste de bresch-pagan

## 5

tsp25 <- c(11.4,11,11.3,9.5)
tsp5 <- c(27.8,29.2,26.8,26)
tsp75 <- c(47.6,47,47.3,45.5)
tsp1 <- c(61.6,62.4,63,63.9)

### (a) Unidade exprimental é o fermento e a unidade observacional é a massa.

### (b) ?

### (c)

y <- c(11.4,11,11.3,9.5,
       27.8,29.2,26.8,26,
       47.6,47,47.3,45.5,
       61.6,62.4,63,63.9)
x <- c(rep(0.25,4),rep(0.5,4),rep(0.75,4),rep(1,4))

data <- data.frame(x,y)
data$x <- factor(data$x,ordered = T)

attach(data)

mod5 <- aov(y~x,data=data)
summary.lm(mod5)

summary(mod5,
        split=list(x=list
                   (linear=1, quadratic=2,
                     cubic=3))) 
pred <- predict(mod5)

### (d) 

y <- c(11.4,11,11.3,9.5,
       27.8,29.2,26.8,26,
       47.6,47,47.3,45.5,
       61.6,62.4,63,63.9)
x <- c(rep(0.25,4),rep(0.5,4),rep(0.75,4),rep(1,4))

mod5 <- lm(y~x)

#normalidade: sao normais

shapiro.test(mod5$residuals)

#homocedasticidade da variancia: são homogeneas

install.packages("lmtest")
require(lmtest)
library(lmtest)

bptest(mod5)

ols_test_breusch_pagan(mod5)

#independencia dos erros: são independentes

require(lmtest)
dwtest(mod5)

## Questão 6. Oehlert - Exercício 6.1.

db_8 = tribble(~Melatonin, ~Mean, ~SD,
             '0nmole', 3296, 90,
             '1nmole', 2574, 153,
             '10nmole', 1466, 207,
             '100nmole', 692, 332)

n_i = 8
t = 4
N = n_i*t

#Resposta: O desvio-padrão e, portanto, a variância não são estáveis, estando, aparentemente negativamente relacionados com a média.

## Questão 7. Oehlert - Exercício 6.2.

db_7 = tribble(~tratamento, ~obs1, ~obs2, ~obs3, ~obs4, ~obs5,
             1, 26*10^2, 29*10^2, 20*10^2, 22*10^2, 32*10^2,
             2, 35*10^3, 23*10^3, 20*10^3, 30*10^3, 27*10^3,
             3, 29*10^5, 23*10^5, 17*10^5, 29*10^5, 20*10^5)

db_7 = db_7 %>% 
  pivot_longer(-tratamento,names_to = 'obs',values_to = 'valor') %>% 
  mutate(tratamento=factor(tratamento))

attach(db_7)
tapply(valor, tratamento, sd)

tdb_7 = db_7 %>% 
  mutate(tvalor=valor/case_when(tratamento==1~10^2,
                                tratamento==2~10^3,
                                tratamento==3~10^5))


attach(tdb_7)

mod = lm(tvalor~tratamento,data=tdb_7)

ehat = mod$residuals
ybar = predict(mod)

ks.test(ehat,"pnorm",mean=mean(ehat),sd=sqrt(var(ehat)))
shapiro.test(ehat)

plot(ybar,ehat,main="Residuos vs Medias de Trat")
abline(h=0)

car::leveneTest(tvalor~tratamento,data=tdb_7)





## Questão 8. Oehlert - Exercício 6.4.

db_8=tribble(~tratamento,~obs1,~obs2,~obs3,~obs4, ~obs5,
             'A', 17, 20, 15, 21, 28,
             'B', 7, 11, 15, 10, 10,
             'C', 11, 9, 5, 12, 6,
             'D', 5, 4, 3, 7, 6)

db_8=db_8 %>% 
  pivot_longer(-tratamento,names_to = 'obs',values_to = 'valor')

attach(db_8)

tapply(valor,tratamento,sd)

boxcox=MASS::boxcox(valor~tratamento, data=db_8,
                    lambda = seq(-2,2,length=100))

boxcox$x[boxcox$y==max(boxcox$y)] #lambda estimado

boxcox_aux=boxcox$x[boxcox$y>=max(boxcox$y)-0.5*qchisq(p=0.95,df=1)]
boxcox_aux[c(1,length(boxcox_aux))] # intervalo de lambda com 95% de confiança



## Questão 9. Oehlert - Exercício 6.5.

## Questão 10. Oehlert - Problem 6.1