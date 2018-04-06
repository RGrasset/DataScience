#Cours 2/Tests univariés quantitatifs
Cherokee=read.table("cherokeeVOT.txt", header=T)
library(EnvStats)
VOT=Cherokee$VOT
Cherokee$year=as.factor(Cherokee$year) #On rappelle que R ne considère pas year comme une variable qualitative
year=Cherokee$year


#Tests sur la distribution
#Comparaison avec une loi exponentielle
#Graphiquement
hist(VOT, freq=F, main="Répartition de la variable VOT", xlab="VOT")
x=seq(0,200,0.1)
y=dexp(x,rate=1/mean(VOT))
lines(cbind(x,y), cex=0.5,col='red')

#Graphiquement avec QQ plots
qqplot(qexp(seq(0,1,0.01), rate=1/mean(VOT)),VOT,main= "Q-Q plot contre loi exponentielle", xlab="Quantiles théoriques", ylab="Quantiles observés")
qqline(VOT,distribution=function(p) qexp(p,rate=1/mean(VOT)))

#Proprement
ks.test(VOT,function(p)pexp(p,rate=1/mean(VOT)))
ks.test(jitter(VOT),function(p)pexp(p,rate=1/mean(VOT)))

#Comparaison avec la loi normale
shapiro.test(VOT)

# I. une variable seule
#a) Modèle gaussien
#on veut les cherokee de l'année 1971 seulement
VOT71=VOT[year==1971]
#on vérifie que VOT71 est bien généré par une loi gaussienne (vérification du modèle)
#graphiquement: (histogramme)
#numériquement: shapiro.test
hist(VOT71)
shapiro.test(VOT71)
# si on s'intéresse maintenant à la dispersion (= variance)
# graphiquement
boxplot(VOT71) #sur le boxplot la dispersion correspond à la verticale (hauteur) du boxplot
# numériquement
# variance observée
var(VOT71)
# test statistique "on veut que prouver que la variance > ou égale à 1000"
# H0: la variance est inférieure ou égale à 1000
# H1: la variance est stricteement supérieure à 1000
varTest(VOT71, sigma.squared = 1000, alternative = "greater")
# la p.value est supérieure à 0.05 donc on accepte H0 --> on a 18% de chance de se tromper en rejetant l'hypothèse nulle, donc on ne le fait pas
# on peut aussi regarder l'intervalle de confiance qui est d'environ 795 (il descend bas pour la variance observée, donc pas plus grande que 1000 --> c'est dû au fait qu'il n'y ait que 24 cas à étudier)
# la barre du milieu du boxplot correspond à la médiane (dans cas gaussien: médiane = +/- moyenne)
# moyenne observée
mean(VOT71)
# on veut prouver que la moyenne est > ou égale à 90
# H0: la moyenne est inférieur ou égale à 90
# H1: la moyenne est strictement supérieure à 90
t.test(VOT71, mu = 90, alternative = "greater")
# p.value = 0.006 donc on rejette H0 --> on a 0.006 chance de se tromper donc on rejette H0 
# si on regarde l'intervalle de confiance on remarque que mu71 est compris dans cet intervalle à 95%

# II. pourquoi le modèle gaussien ?
# théorème central limite
# peu importe la loi d'échantillonage, si on regarde toutes les moyennes pour un échantillon assez grand alors on peut considérer le modèle comme gaussien
# 1ère partie: moyennes jusqu'à X10 qui suivent une loi exponentielle
# on prend la moyenne observée et on simule 1000
hist(rexp(10,rate = 1), freq = FALSE, main="Répartition d'un échantillon de loi exponentielle de taille 10")
x=rep(0,1000)
for (i in 1:1000)
{
  x[i]=mean(rexp(10,rate=1))
}
x=sort(x)
hist(x,freq=F, main="Répartition des moyennes d'échantillons de taille 10")
y=dnorm(x,mean = mean(x), sd=sd(x))
lines(cbind(x,y),col='red')
shapiro.test(x)
# graphiquement ça suit quasi une loi normale
# résultat du test --> p.value inférieure à 0.05 donc ce n'est pas valide, on rejette H0
# 2ème partie: moyennes jusqu'à X100 qui suivent une loi exponentielle
hist(rexp(100,rate = 1), freq = FALSE, main="Répartition d'un échantillon de loi exponentielle de taille 100")
x=rep(0,1000)
for (i in 1:1000)
{
  x[i]=mean(rexp(100,rate=1))
}
x=sort(x)
hist(x,freq=F, main="Répartition des moyennes d'échantillons de taille 100")
y=dnorm(x,mean = mean(x), sd=sd(x))
lines(cbind(x,y),col='red')
shapiro.test(x)
# p.value inférieure à 0.05 donc on rejette l'hypothèse nulle

# en pratique: plus de 50 individus pour estimer une quantité (moyenne ou variance) --> cas gaussien (en gros plus on a de monde mieux c'est, en dessous de 50 on a pas le droit)

# b) Modèle non-gaussien
# on sort les VOT de 2001
VOT01=VOT[year==2001]
shapiro.test(VOT01)
# p.value inférieure à 0.05 donc modèle pas gaussien
# graphiquement
boxplot(VOT01)
# On fait un test sur la médiane observée: wilcox
median(VOT01)
# on veut prouver que la médiane est supérieure ou égale à 60
# H0: la médiane est inférieure ou égale à 60
# H1: la médiane est strictement supérieure à 60
wilcox.test(jitter(VOT01), mu = 60, alternative = "greater", conf.int = TRUE, conf.level = 0.98)
# TEST NON PARAMETRIQUES = EN DEHORS DU MODELE GAUSSIEN
# à chaque fois qu'on a un test non paramétrique, R veut que toutes les observations soient différentes du coup on veut perturber les valeurs pour avoir un truc "propre" donc on utilise la fonction "jitter"
# la p.value est inférieure à 0.05 donc on rejette H0
# le VOT médian des cherokee de l'année 2001 est plus grand que 60 avec +/- 4.08-05 chances de se tromper donc on rejette H0

# TEST NON PARAMETRIQUES = TEST SUR LA MEDIANE
# MODELE GAUSSIEN = TEST SUR LA MOYENNE

# comparaison wilcoxon/t.test
# si on prend VOT71 on est dans le cas gaussien
# ici H0: mu = 90
# H1: mu est différent de 90
# --> on veut juste savoir soit l'un soit l'autre donc on emploie la fonction "two sided"
t.test(VOT71, mu = 90, alternative = "two.sided", conf.int = TRUE, conf.level = 0.98)
wilcox.test(VOT71, mu = 90, alternative = "two.sided", conf.int = TRUE, conf.level = 0.98)
# intervalle de confiance est plus précis quand on emploie la méthode gaussienne, donc le t.test

# II. variable à expliquer qualitative, variable explicative qualitative à 2 modalités
# VOT à expliquer 
# comme variable explicative on prend le facteur "year"
# est-ce que l'année influence les VOT des cherokee
# 1) exploration graphique 
boxplot(VOT~year, main="Répartition des VOT Cherokee conditionnellement à l'année")
#graphiques en treillis - différents types de graphs
library(lattice)
# premier graph
bwplot(~VOT|year, data=Cherokee,main="Répartition des VOT suivant l'année")
# 2ème graph
histogram(~VOT|year, data = Cherokee,type="density", main="Répartition des VOT suivant l'année") 
# 3ème graph
densityplot(~VOT|year, panel = function(x){          #Ici on a superposé l'histogramme avec la gaussienne qui semble coller le mieux
  panel.histogram(x,type='density',col='blue',equal.widths = TRUE,breaks=NULL)
  panel.mathdensity(dmath = dnorm, args = list(mean=mean(x),sd=sd(x)),col='red')})
#on voit quen 2001 il y a des observations un peu éloignées

# modèle: gaussien ? non car pas plus de 50 individus par cas, on fait donc un test de shapiro
# vérification distributions conditionnelles gaussiennes
#on applique le test de shapiro sur la variable d'intérêt (VOT) conditionnellement à la variable explicative (year)
tapply(VOT, year, shapiro.test)
# le premier résultat est pour VOT71 = 0.75 donc c'est ok, c'est gaussien
# le deuxième est pour 2001 = < 0.05 donc c'est pas ok, c'est pas gaussien (probablement dû aux observations un peu éloignées)
# on peut toujours se débarasser des valeurs aberrantes pour respecter la loi gaussienne
# VOT aberrants = > 170
aber=which(year==2001&VOT>=170)
VOT2=VOT[-aber]
year2=year[-aber]
tapply(VOT2,year2,shapiro.test)
# là on remarque bien que ça fonctionne dans les deux cas

# a) modèle gaussien
# VOT(ij) = mu(modèle globale) + alpha(j) + E(i,j)
# avec i = cherokee(i), year = j, alpha = effet fixe (de l'année à laquelle la mesure a été prise), E = erreur de mesure


# --------------------------------------------------------------------------------
# cours 3
Cherokee=read.table("cherokeeVOT.txt", header=T)
library(EnvStats)
VOT=Cherokee$VOT
Cherokee$year=as.factor(Cherokee$year) #On rappelle que R ne considère pas year comme une variable qualitative
year=Cherokee$year
#Retrait valeurs aberrantes
aber=which(year==2001&VOT>=170)
VOT=VOT[-aber]
year=year[-aber]
# on avait enlevé les valeurs abérrantes
# Modèle Gaussien, égalité des variances conditionnelles
# graphiquement
boxplot(VOT~year)
# ou
hist(VOT[year==2001]-mean(VOT[year==2001]),freq=FALSE,col='red',xlim=c(-100,100),xlab='VOT conditionnels recentrés', main = "Répartitions des VOT conditionnels autour de leur moyenne")
hist(VOT[year==1971]-mean(VOT[year==1971]),freq=FALSE,add=TRUE)
legend('topleft', expression("VOT 2001", "VOT 1971"), pch=c(0,15),col=c("black","red"),bty = "n")
# on voit pas trop sur le boxplot (qq valeurs abérrantes encore) mais on se rend compte sur l'histogramme que c'est plus étalé en 
# la dispersion est donc plus grande en 2001
# numériquement
# Modèle 1 = VOT(ij) = mu(modèle globale) + alpha(j) + E(i,j)
# H0 = la variance de 2001 est égale à celle de 1971
# H1 = la variance de 2001 est strictement supérieure à celle de 71
# validité d'un sous modèle à l'intérieur d'un modèle
var.test(VOT[year==2001],VOT[year==1971], alternative = "greater")
# si H0 = la même mais H1 = les deux variances sont différentes
var.test(VOT[year==2001],VOT[year==1971], alternative = "two.sided")
# p value = 0.005 donc on est sûr à 99.5% que les deux variances sont différentes (IC)

# modèle gaussien, variances différentes, moyennes conditionnelles
# graphiquement
# on regarde pas la dispersion mais le trait
boxplot(VOT~year)
# le VOT moyen est plus petit en 2001
# numériquement on va faire un t.test
# H0: la moyenne de 2001 est supérieure ou égale à la moyenne de 71
# H1: la moyenne de 2001 est strictement inférieure à la moyenne de 71
t.test(VOT[year==2001],VOT[year==1971], alternative = "less", conf.level=0.98, var.equal = FALSE)
# la p.value est inférieure à 0.05 donc on rejette H0 et donc la VOT moyenne de 2001 est bien inférieure à celle de 71
# IC est sur la différence des VOT = il y a 95% de chances que le VOT moyen de 2001 soit plus petit de 17 blabla que en 71

# modèle gaussien, variances égales, moyennes conditionnelles
t.test(VOT[year==2001],VOT[year==1971], alternative = "less", conf.level=0.98, var.equal = TRUE)
# RQ: les tests sont plus précis et les IC plus fins dans les petits modèles

# dans le cas non gaussien
# on récupère toutes les VOT, y compris celles qui ont des valeurs abérrantes
#Cadre non Gaussien (on remet les valeurs aberrantes)
Cherokee=read.table("cherokeeVOT.txt", header=T)
VOT=Cherokee$VOT
Cherokee$year=as.factor(Cherokee$year) #On rappelle que R ne considère pas year comme une variable qualitative
year=Cherokee$year
# égalité des dispersions conditionnelles
# graphiquement
boxplot(VOT~year)
# numériquement
# H0: la dispersion en 2001 est égale à celle de 1971
# H1: les dispersions ne sont pas égales
# VOT est la variable quanti à expliquer et year est le facteur explicatif
fligner.test(VOT,year)
# la p.value est supérieure à 0.05 donc on accepte H0


# validation du modèle utilisé (si pas déjà fait)
# vérification graphique
histogram(~VOT|year)
# numériquement on va recentrer les distributions conditionnelles et diviser par la dispersion
stand=function(x){(x - median(x))/sd(x)}
ks.test(stand(VOT[year==2001]),stand(VOT[year==1971]))
# p.value = 0.38

# Modèle non paramétirque, égalité des dispersions, tests sur les médianes conditionnelles
# graphiquement
boxplot(VOT~year)
# numériquement
wilcox.test(VOT[year==2001], VOT[year==1971], mu=0, conf.int = TRUE, alternative = "less")
# H0: médiane 01 esr supérieure ou égale à médiane 71
# H1: médiane 01 strictement < médiane 71
wilcox.test(VOT01, VOT71, mu = 0, alternative = "less")
# p.value est < à 0.05 donc on rejette
# H0 et le VOT médian de 01 est bien inférieure à celui de 71
# on a 0.3% de chance de se tromper
# on peut faire le test de wilcox seulement si les dispersions sont égales, sinon ça ne fonctionne pas
# test et IC de wilcox moins précis que les t.tests

# on charge "ratings"
library(languageR)
data("ratings")
weight=ratings$meanWeightRating
size=ratings$meanSizeRating

# Y à expliqur quantitatif, X explicatif à deux modalités, mesures répétées sur le même individu
# F = 0 ou 1 suivant que l'importance est déterminée par le corpus ou les gens
# I = l'importance du lexème
# cas spécial, il va y avoir un problème d'indépendance 
# modèle mixte, on va avoir dans le modèle un effet dû à l'individu (effet aléatoire dû à l'individu)