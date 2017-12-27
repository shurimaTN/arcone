# Telecharger R depuis
# https://cran.r-project.org/bin/windows/base/
# Ensuite, telecharger RStudio depuis
# https://www.rstudio.com/products/rstudio/download/

#####################################################

help("cos") # aide pour la fonction "cos"
?cos #idem
help.search("cos") # cherche dans l'aide là où il y a "cos"

a=5
b=a+8
b
x=c(-2,1,5,3,-5,0)
x[1]
x[3]
x[2:4]
x<3
x[c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE)]
x[x<3]
6:10
seq(from=1, to=2, by=0.25)
seq(from=3, to=4, length=7)

x=c(-2,1,5,3,-5,0,0.1,4)
sum(x)
mean(x)
var(x) # estimateur sans biais de la variance
mean((x-mean(x))^2)
6/5*mean((x-mean(x))^2)
sd(x) # ecart-type
median(x)
summary(x)

for (i in 1:10)
{ 
  if (i>2) {print(i)} 
}

A=matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
A[3,2]
A[2,]
A[1:2,]

mafonction = function(x,y){
  z=x+y
  return(z)
}
mafonction(2,7)

x=seq(from=-10, to=10, by=0.1)
plot(x,x^2) # graphe
plot(x,x^2, xlab="les x", ylab="les x^2",col="red",main="parabole",type="l")
# graphe 3D
persp(1:10,1:20,matrix(1:200,nrow=10,ncol=20)^2,col=topo.colors(15))

runif(20) # 20 simulations de la loi uniforme sur [0,1]
dev.off() # effacer tous les graphiques
U=runif(1000)
length(U[U<1/3])/length(U) # presque la proba que U<1/3
hist(U, freq=FALSE) #histogramme; 'freq=false' pour normaliser (surface=1)
absc_u=seq(from =0,to = 1,by = 0.01);
lines(absc_u,dunif(absc_u),col="red") # on superpose la densité de la loi uniforme

rexp(10,rate=2) # loi exponentielle Exp(rate)
mean(rexp(100,rate=2))
mean(rexp(10000,rate=2))


rnorm(20) # simulation de la loi normale N(0,1)
pnorm(0) # fonction de repartition de la loi normale N(0,1)
pnorm(5.5, mean=5, sd=2) # fonction de repartition de la loi normale N(mean,sd^2)
qnorm(0.5, mean=5, sd=2) # quantile (inverse de la fonction de repartition)
dnorm(0, mean=0, sd=1) # densit de la loi normale N(mean,sd^2)
dev.off() #clear plot
Z=rnorm(100000)
hist(Z, freq=FALSE) #'freq=false' pour un histo normalisé
absc_z=seq(from =-3,to = 3,by = 0.01);
lines(absc_z,dnorm(absc_z),col="red")

# test de Student : tester l'hypothese (nulle) que la vraie moyenne = mu
# l'hypothese alternative est par défaut "vraie moyenne differente de mu"
donnees=rnorm(100000,mean=5,sd=1)
t.test(donnees,mu=7) 
t.test(donnees,mu=5)
t.test(rnorm(10,mean=5,sd=1),mu=5)

t.test(donnees,mu=5, conf.level=0.99) # intervalle de confiance à 99% pour la vraie moyenne
t.test(donnees,mu=5, conf.level=0.9999)
t.test(donnees,mu=5, conf.level=0.2)

t.test(donnees,mu=5,alternative = "less")

# test (de Student) d'egalite entre deux moyennes (à variances égales)
t.test(rnorm(100,mean=5), rnorm(100,mean=1))
t.test(rnorm(100,mean=5), rnorm(100,mean=5))

# test (de Fisher) d'egalite entre les variances
donnees=rnorm(1000,mean=5,sd=1)
autres_donnees=rnorm(1000,mean=8,sd=1)
var.test(donnees,autres_donnees)
autres_donnees=rnorm(1000,mean=5,sd=2)
var.test(donnees,autres_donnees)

# test de Shapiro-Walk de normalité (hypothese nulle=normalite des donnees)
donnees=rnorm(1000)
shapiro.test(donnees)
donnees=runif(1000)
shapiro.test(donnees)

#test de Kolmogorov-Smirnov d'adequation entre deux distributions (lois)
#(hypothese nulle = adequation entre les deux distributions)
donnees=rnorm(10000, mean=5, sd=1)
ks.test(donnees, "pnorm", mean=5, sd=1)
donnees=runif(10000)
ks.test(donnees, "pnorm", mean=5, sd=1)
donnees_1=runif(1000)
donnees_2=runif(1000)
ks.test(donnees_1, donnees_2)
donnees_3=rnorm(1000)
ks.test(donnees_1, donnees_3)


# test khi-deux d'independance
# hypothese nulle = independance
hommes = c(100, 120, 60) # 100 hommes aiment gout chocolat, 120 vanille et 60 fraise
femmes = c(350, 200, 90) # 350 femmes aiment gout chocolat, 200 vanille et 90 fraise
tous= as.data.frame(rbind(hommes, femmes))
names(tous) = c('chocolat', 'vanille', 'fraise')
tous
chisq.test(tous)


x=read.table("shoesize.txt",sep="\t",header=TRUE,dec=",")

dim(x)
nrow(x)
ncol(x)


summary(x);
s=x[,"Size"]
h=x[,"Height"]


sf=s[1:187] # size for F
sm=s[188:tail(s, n = 1)] # size for M
hf=h[1:187] # height for F
hm=h[188:tail(s, n = 1)] # height for M

dev.off() # efface les graphiques
par(mfrow=c(3,3)) # crée a lignes et b colonnes de 
                  # sous-fenetres plot (comme subplot de matlab)

hist(sf,freq=FALSE) #'freq=false' pour un histogramme normalisé (de surface=1)
lines(sort(sf),dnorm(sort(sf), mean = mean(sf), sd = sd(sf)),col="red")
hist(sm,freq=FALSE) #'freq=false' pour un histo normalisé
lines(sort(sm),dnorm(sort(sm), mean = mean(sm), sd = sd(sm)),col="red")
hist(s,freq=FALSE) #'freq=false' pour un histo normalisé
lines(sort(s),dnorm(sort(s), mean = mean(s), sd = sd(s)),col="red")

hist(hf,freq=FALSE)
lines(sort(hf),dnorm(sort(hf), mean = mean(hf), sd = sd(hf)),col="red")
hist(hm,freq=FALSE)
lines(sort(hm),dnorm(sort(hm), mean = mean(hm), sd = sd(hm)),col="red")
hist(h,freq=FALSE)
lines(sort(h),dnorm(sort(h), mean = mean(h), sd = sd(h)),col="red")



mean(h)
var(h)
cov(s,h)
cov(s,h)/(sd(s)*sd(h))
cor(s,h)
cor(sf,hf)
cor(sm,hm)

shapiro.test(h) # test de normalité

