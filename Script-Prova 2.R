##################################PARTE1##################################

##5)

y <- c(1.8,2.1,2.0,2.1,4.6,5.0,7.5,7.9,2.2,2.4,4.2,4.0,5.4,5.6,9.8,9.2,
       2.8,3.2,4.4,4.8,8.7,8.4,13.2,13.0,3.2,3.6,3.3,3.5,5.7,5.8,10.9,11.1)
tela <-  factor(rep(1:4,each=8))
temperatura <-  factor(rep(c(210,210,215,215,220,220,225,225),4))

dados5 <- data.frame(y=y,tela=tela,temperatura=temperatura)
head(dados5,n=10)
str(dados5)

mod5 <- aov(y~tela*temperatura,data=dados5)
anova(mod5)
summary(mod5)


shapiro.test(mod5$residuals)

ks.test(mod5$residuals,'pnorm',mean=mean(mod5$residuals),sd=sd(mod5$residuals))

leveneTest(mod5,center='median')

leveneTest(mod5,center='mean')

bptest(mod5)


ting.lm <??? lm ( y ~ ???1 + tela:temperatura, data=dados5)

#######################################################################

##################################PARTE2##################################

#QUESTAO 8

y <- c(3.5,3.0,3.6,2.9,2.1,4.5,4.0,3.1,7.2,6.8,6.7,4.8,7.5,6.9,6.8,9.3,
       4.1,5.6,5.8,4.8,4.5,4.6,5.3,7.3,4.1,5.3,5.3,4.8,3.2,5.0,7.2,
       6.7,6.7,5.2,4.2,4.5,2.7,5.1,3.7,5.0,4.5,4.9,4.7,4.5)
gum <- c(rep(1,8),rep(2,8),rep(3,10),rep(4,9),rep(5,9))
protein <- c(1,1,2,2,3,3,4,5,1,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5,
             1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5)

data <- data.frame(y=y,gum=factor(gum),protein=factor(protein))
head(data)
str(data)

mod1 <- aov(y~ gum*protein,data=data)
anova(mod1)

car::Anova(lm(y~ gum*protein,data=data),type = 2)

car::Anova(lm(y~ gum*protein,data=data),type = 3)

mod2 <- aov(y~ protein*gum,data=data)
anova(mod2)

car::Anova(lm(y~ protein*gum,data=data),type = 2)

car::Anova(lm(y~ protein*gum,data=data),type = 3)


car::Anova(lm(y~ gum+protein+gum*protein,data=data),type = 2)

car::Anova(lm(y~ gum*protein,data=data),type = 2)


#poder do teste
ni=2 
a=5
b=5
alpha=0.05
t=a*b
sumtau2=14.053
mse=19.655/19
nu1=(a-1)*(b-1)
nu2=a*b*(ni-1)
lambda=ni*sumtau2/mse
phi=sqrt(lambda/(a*b))
df=nu2
fcrit=qf(1-alpha,nu1,nu2)
poder=1-pf(fcrit,nu1,nu2,lambda) # = 0.7486

ni=3 
a=5
b=5
alpha=0.05
t=a*b
sumtau2=14.053
mse=19.655/19
nu1=(a-1)*(b-1)
nu2=a*b*(ni-1)
lambda=ni*sumtau2/mse
phi=sqrt(lambda/(a*b))
df=nu2
fcrit=qf(1-alpha,nu1,nu2)
poder=1-pf(fcrit,nu1,nu2,lambda) # = 0.9727



