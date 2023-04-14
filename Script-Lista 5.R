######################LISTA 5######################################

##1)

dados <- emp08.8

#estrutura dos dados:
head(dados)
str(dados)

#reformatando os dados:
dados$alg = as.factor(dados$alg)
dados$seq = as.factor(dados$init)
dados$size = as.factor(dados$size)
dados$alloc = as.factor(dados$ram)
dados$lfaults <- log(dados$faults)

#montando o modelo:
modelo <- aov(lfaults ~ seq + size + alloc + alg
              + seq*size + seq*alloc + seq*alg
              + size*alloc + size*alg + alloc*alg
              + seq*size*alloc 
              + seq*alloc*alg
              + seq*size*alg
              + size*alloc*alg, data = dados)

anova(modelo)
summary(modelo)

library(car)
Anova(lm(lfaults ~ seq + size + alloc + alg
         + seq*size + seq*alloc + seq*alg
         + size*alloc + size*alg + alloc+alg
         + seq*size*alloc 
         + seq*alloc*alg
         + seq*size*alg
         + size*alloc*alg, data = dados,
         contrasts = list(seq=contr.sum,
                          size-contr.sum,
                          alloc=contr.sum,
                          alg=contr.sum)),type=3)


##2)

#a)

y <- c(91.3,89.9,90.7,91.4,89.3,88.1,90.4,91.4,89.5,87.6,88.3,90.3,
    87.3,89.4,91.5,88.3,92.3,91.5,90.6,94.7,93.1,90.7,91.5,89.8)
base <-  rep(1:2,each=12)
solvente <-  c( rep(1:3,each=4))
solvente <-  c(solvente,solvente)

dados <-  data.frame(y=y,base=factor(base),solvente=factor(solvente))
head(dados,n=10)

mod2 <- aov(y~base*solvente,data=dados)
anova2 <- anova(mod2)

#b)

model.tables(mod2,type='means')
# médias marginais
# a última tabela tem as médias por tratamento arredondadas

n <- 4
a <- 2
b <- 3
MSE <- anova2$`Mean Sq`[4]

erro_padrao <- sqrt(MSE/n) 

#ou:

library(phia)
interactionMeans(mod2)
# médias e erros padrão para tratamentos obtidos automaticamente

#c)

Ho: interação = 0
H1: interação != 0

MSAB <- anova2$`Mean Sq`[3]
MSE <- anova2$`Mean Sq`[4]

est_F <- MSAB/MSE # = anova2$`F value`[3]

p_valor <- 1-pf(est_F,2,18) # = anova2$`Pr(>F)`[3]
cat("O p-valor é",p_valor)

#Resposta: Rejeitamos Ho para um alpha de 0,05. O próximo passo é buscar saber 
#          quais tratamentos são diferentes de quais tratamentos, e qual o melhor.

#d)

mod1 <- lm(y ~ -1 + solvente:base)
mod2 <- aov(y ~ solvente*base)

TukeyHSD(mod2, "solvente:base")

ct <- rbind('Solvente1' = c(1,0,0,-1,0,0)/2,
            'Solve2' = c(0,1,0,0,-1,0)/2,
            'Solve3' = c(0,0,1,0,0,-1)/2)

estimable(mod1,ct)
k <- 3 
alpha <- 0.05/ k ; alpha 

x <- rbind(c(1,-1,0,-1,1,0)/2, c(1,0,-1,-1,0,1)/2)

cB <- rbind('1-2' = x,
            '1-3' = c(1,0,-1,0,0,0)/2,
            '2-3' = c(0,1,0,0,0,-1)/2)

estimable(mod1,cB)

interaction.plot(x.factor = data$base,
                 trace.factor = data$solvente,
                 response = data$y,
                 fun = mean,
                 type = "b",
                 col = c("blue", "red", "green"),
                 pch = c(1,2,3),
                 fixed = T,
                 leg.bty = "o")



# install.packages("phia") 
library(phia)
intmeans <- interactionMeans(mod2)
intmeans

#e)

#pacotes
library(lawstat)
library(lmtest)
library(car)
library(EnvStats)
library(olsrr)
library(nortest)
#install.packages("PMCMRplus")
library(PMCMRplus)

#independencia dos erros: p-valor > alpha, ou seja, nao rejeita Ho, os erros sao independentes

dwtest(mod2)

runs.test(mod2$residuals)

#Normalidade: pelo grafico, podemos dizer que os dados aparentam ter normalidade, ja que estao perto da reta
#             pelos testes, vimos que , em ambos os tres, o p-valor é maior que o alpha de 0,05, ou seja, os dados seguem normalidade.

qqnorm(mod2$residuals, pch=16)
qqline(mod2$residuals,col=2)

shapiro.test(mod2$residuals)

ad.test(mod2$residuals)

ks.test(mod2$residuals,'pnorm',mean=mean(mod2$residuals),sd=sd(mod2$residuals))

#homogeneidade da variancia: em ambos os testes, p-valor > alpha de 0,05, ou seja, as variancias sao homogneneas

leveneTest(mod2,center='median')

leveneTest(mod2,center='mean')

bptest(mod2,studentize = F)

hartleyTest(mod2$residuals,g=base)

##3) 

y <- c(90.7,91.4,89.3,88.1,
       90.4,89.5,87.6,88.3,
       90.3,87.3,88.3,91.5,
       94.7,93.1,90.7,91.5)
base <- c(rep(1,9),rep(2,7))
solvente <- c(1,1,2,2,2,3,3,3,3,1,1,1,2,3,3,3)

dados <- data.frame(y=y,
                    base=factor(base),
                    solvente=factor(solvente))
head(dados,n=10)

mod3 <- aov(y~base*solvente,data=dados)
summary(mod3)

#a)

mod3 <- aov(y~base*solvente,data=dados)
summary(mod3)
anova3 <- anova(mod3)

Ho: interação = 0
H1: interação != 0

MSAB <- anova3$`Mean Sq`[3]
MSE <- anova3$`Mean Sq`[4]

est_F <- MSAB/MSE # = anova3$`F value`[3]

p_valor <- 1-pf(est_F,2,10) # = anova3$`Pr(>F)`[3]
cat("O p-valor é",p_valor)

#Resposta: p-valor menor que o alpha de 0,05, ou seja, rejeita a hipotese nula

#b)

#Média das células:

tapply(dados$y,list(dados$base,dados$solvente),mean)

model.tables(mod3,type="means")

#erro padrao:

N <- 16
a <- 2
b <- 3

sigma_estimado <- MSE

sy11. <- sqrt(sigma_estimado/2)
sy21. <- sqrt(sigma_estimado/3)
sy12. <- sqrt(sigma_estimado/3)
sy22. <- sqrt(sigma_estimado/1)
sy13. <- sqrt(sigma_estimado/4)
sy23. <- sqrt(sigma_estimado/3)

#ou,

#install.packages("phia")
library(phia)
interactionMeans(mod3) #EsTimativa de MQ

#c) ?

ct <- rbind('Solve1' = c(1,0,0,-1,0,0)/2,
            'Solve2' = c(0,1,0,0,-1,0)/2,
            'Solve3' = c(0,0,1,0,0,-1)/2)
ct

estimable(lm,ct)

k <- 3 #?
alpha <- 0.05/ k ; alpha

#d) As estimativas de mínimos quadrados para Mij = y_barra_ij, ou seja, as estimativas de mínimos quadrados são as médias observadas, não há diferença.

##4)

y <- c(90.7,89.3,89.5,87.3,85.7,86.1)
base <- factor(c(10,10,10,20,20,20))
solvente <- factor(c(3,5,7,3,5,7))

dados <- data.frame(y=y,base=base,solvente=solvente)
head(dados,n=10)

#a)

#Teste de Não Aditividade de Tukey:

Ho: interação = 0
H1: interação != 0

#install.packages("daewr")
library(daewr)
Tukey1df(dados) #primeira coluna tem de ser a resposta

#Resposta: p-valor = 0,54, ou seja , maior que o alpha de 0,05, ou seja, nao rejeita Ho.

#b)

#Polinomios Ortogonais:

mod4 <- lm(y ~ base*solvente, data=dados)
anova(mod4)

bc <- as.ordered(dados$base)

sc <- as.ordered(dados$solvente)

bL <- contr.poly(bc)[bc,".L"]

sL <- contr.poly(sc)[sc,".L"]

sQ <- contr.poly(sc)[sc,".Q"]

modcLL <- lm(dados$y ~ bc + sc + bL:sL)
anova(modcLL)

pred <- predict(modcLL,nowdata = data.frame(bc,sc,bL,sL))
pred.means <- aggregate(pred,by=list(bc=bc,sc=sc),"mean")

#modcLQ <- lm(dados$y ~ bc + sc + bL:sQ)
#anova(modcLQ)

#base <- pred.means$bc
#intoraction.plot(pred.means$sc,base,pred.means$x,type = "means",
                 pch = c(18,24,22),leg.bty="o",lwd=2,col = c(1,2,3),
                 xlab = "Solvente",
                 ylab = "% de Reação",
                 ylim = c(85,95))

#c) Acho que sim, no teste de Tukey para não aditividade, não rejeitamos Ho (p-valor = 0,54), e pelo
#   método de polinômios ortogonais, também não rejeitamos Ho (p-valor = 1)

##5)

y <- c(1.8,2.1,2.0,2.1,4.6,5.0,7.5,7.9,2.2,2.4,4.2,4.0,5.4,5.6,9.8,9.2,
    2.8,3.2,4.4,4.8,8.7,8.4,13.2,13.0,3.2,3.6,3.3,3.5,5.7,5.8,10.9,11.1)
tela <-  factor(rep(1:4,each=8))
temperatura <-  factor(rep(c(210,210,215,215,220,220,225,225),4))

dados5 <- data.frame(y=y,tela=tela,temperatura=temperatura)
head(dados5,n=10)
str(dados5)

#a)

mod5 <- aov(y~tela*temperatura,data=dados5)
anova(mod5)
summary(mod5)
#b)

anova5 <- anova(mod5)

#ou,

Ho: interação = 0
H1: interação != 0

MSAB <- anova5$`Mean Sq`[3]
MSE <- anova5$`Mean Sq`[4]

est_F <- MSAB/MSE # = anova5$`F value`[3]

p_valor <- 1-pf(est_F,9,16) # = anova5$`Pr(>F)`[3]
cat("O p-valor é",p_valor)

#Resposta: p-valor < alpha, então, rejeita-se Ho.

# c) e d) ?

Ac <- as.ordered(dados$tela)
Bc <- as.ordered(dados$temp)

AL <- contr.poly(Ac)[Ac,".L"]
BL <- contr.poly(Bc)[Bc,".L"]

AQ <- contr.poly(Ac)[Ac, ".Q"]
BQ <- contr.poly(Bc)[Bc, ".Q"]

AC <- contr.poly(Ac)[Ac, ".C"]
BC <-  contr.poly(Bc)[Bc, ".C"]

modLQC <- lm(dados$y ~ Ac + Bc + AL:BL+ AL:BQ + AL:BC+
               AQ:BL + AQ:BQ + AQ:BC + AC:BL + AC:BQ +
               AC:BC)
anova(modLQC)

modpol1 <- lm(dados$y ~  BL + BQ)
anova(modpol1)

ftl <- 262.144/0.05 ; ftl
ftq <- 21.78/0.05 ; ftq

pf(ftl, 1, 16, lower.tail = F)
pf(ftq, 1, 16, lower.tail = F)

mod1cL <- lm(dados$y ~ Ac + Bc + Ac:BL + Ac:BQ +Ac:BC)
anova(mod1cL)

pred <- predict(mod1cL, newdata = data.frame(Ac,Bc,AL,BL))
pred.means <- aggregate(pred, by=list(Ac=Ac, Bc=Bc),"mean")
Alcool <- pred.means$Ac
interaction.plot(pred.means$Bc, Alcool, pred.means$x, type="b",
                 pch=c(18,24,22),leg.bty="o", lwd=2, col=c(1,2,4),
                 xlab="Proporção",
                 ylab="Emissão de CO")

#e)

model.tables(mod5,type='means', se=TRUE)
#O output acima tem a média geral, a média marginal para cada nível do fator tela, a média marginal 
#para cada nível do fator temperatura, e a média de cada tratamento.

#erro padrao:

N <- 16
a <- 4
b <- 4

sigma_estimado <- MSE

erro_padrao <- sqrt(MSE/N)

#ou,

#install.packages("phia")
library(phia)
interactionMeans(mod5) #EsTimativa de MQ

#f)

int <- interactionMeans(mod5)
plot(int)
#No painel inferior esquerdo, o gráfico sugere interação entre tela e temperaturas 210 e 215.
#No painel superior direito, há uma interação clara entre as linhas azul e vermelha, ou seja, 
#entre os níveis de temperatura e os níveis de tela.


##6)

agron <- data.frame(matrix(c(1,1,122,0,0,
                             1,2,72.5,0,0.5,
                             1,3,52,0,1,
                             1,4,36.25,0,1.5,
                             1,5,29.25,0,2,
                             2,1,82.75,20,0,
                             2,2,84.75,20,0.5,
                             2,3,71.5,20,1,
                             2,4,80.5,20,1.5,
                             2,5,72,20,2,
                             3,1,65.75,40,0,
                             3,2,60.75,40,0.5,
                             3,3,79.5,40,1,
                             3,4,65.75,40,1.5,
                             3,5,82.5,40,2,
                             4,1,68,60,0,
                             4,2,70,60,0.5,
                             4,3,68.75,60,1,
                             4,4,77.25,60,1.5,
                             4,5,68.25,60,2,
                             5,1,57.5,80,0,
                             5,2,60.75,80,0.5,
                             5,3,63,80,1,
                             5,4,69.25,80,1.5,
                             5,5,73.25,80,2),25,5,byrow = T))
colnames(agron) <- c("insel", "herbl","weight",
                     "inset","herbi")
head(agron,n=25)

# a) e b)

agron$inset <- factor(agron$inset)
agron$herbi <- factor(agron$herbi)

mod6 <- aov(weight ~ inset*herbi,data=agron)
summary(mod6)
anova(mod6)

# c) e d)

ic <- as.ordered(agron$inset)

hc <- as.ordered(agron$herbi)

iL <- contr.poly(ic)[ic,".L"]

hL <- contr.poly(hc)[hc,".L"]

iQ <- contr.poly(ic)[ic,".Q"]

hQ <- contr.poly(hc)[hc,".Q"]

iC <- contr.poly(ic)[ic,".C"]

hC <- contr.poly(hc)[ic,".C"]

modpo1 <- lm(agron$weight ~ iL + iQ + iC + hL + hQ + hC +
               iL:hL + iL:hQ + iL:hC +
               iQ:hL + iQ:hQ + iQ:hC +
               iC:hL + iC:hQ + iC:hC)

anova(modpo1)

#7) Olhar no arquivo do victor

#8)

y <- c(3.5,3.0,3.6,2.9,2.1,4.5,4.0,3.1,7.2,6.8,6.7,4.8,7.5,6.9,6.8,9.3,
    4.1,5.6,5.8,4.8,4.5,4.6,5.3,7.3,4.1,5.3,5.3,4.8,3.2,5.0,7.2,
    6.7,6.7,5.2,4.2,4.5,2.7,5.1,3.7,5.0,4.5,4.9,4.7,4.5)
gum <- c(rep(1,8),rep(2,8),rep(3,10),rep(4,9),rep(5,9))
protein <- c(1,1,2,2,3,3,4,5,1,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5,
          1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5)

data <- data.frame(y=y,gum=factor(gum),protein=factor(protein))
head(data)
str(data)

mod8 <- aov(y~ gum*protein,data=data)
anova(mod8)

#Pelos resultados do experimento, vemos que o fator "Gum" (goma) tem um efeito bastante significativo 
#na avaliação, com p -valor na ordem de 10^-5. 

#Já o fator Proteína não tem um efeito significativo, com p-valor igual a 0,189.

#E a interação Goma:Proteína também não tem um efeito significativo, com p-valor igual a 0,626.


#Enfim, para determinar quais combinações de goma/proteína são diferentes em relação às avaliações, fazemos um teste de Tukey:

TukeyHSD(mod8, 'gum:protein')

#2:5 significativamente diferente de 1:1, 2:5 significativamente diferente de 5:1, 2:4 diferente de 1:3 se considerarmos 
#alpha = 0.1, 2:5 diferente de 1:3 com p-valor igual a 0.02, 2:5 diferente 1:5 com p-valor igual 0.08, se considerarmos alpha = 0.1.

##9

dados = read.csv("")
head(dados,n=10)
str(dados)

dados$doença = factor(dados$doença)
dados$droga = factor(dados$droga)

#a)

modelo9 = aov(y ~ 0+droga*doença, data=dados)
anova(mod9)

#Repare que o modelo indica que a interação droga:doença não é significante, com p-valor = 0.8469

#coeficientes:

mod9$coefficients

#b) Repare que teremos uma cobinação 12,2 = c12,2 = 66 comparações. Vamos realizar o teste de Tukey:

TukeyHSD(modelo5,"droga:doença",conf.level=0.95)

#c) ?

#d) ?

#e) ?

##10

dados = read.csv("")
head(dados,n=10)
str(dados)

dados$doença = factor(dados$doença)
dados$droga = factor(dados$droga)

#a)

mod10 = aov(y ~ droga*doença, data=dados)
anova(mod10)

#Vemos que os dois fatores Droga e Doença são significante, e rejeitamos a hipótese de interação dos 
#fatores devido ao p-valor = 0.847

#coeficientes:

mod10$coefficients