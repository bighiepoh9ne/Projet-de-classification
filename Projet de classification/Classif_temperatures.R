# ------ Documentation
# Documentation pour FactomineR (short) : http://www.duclert.org/r-analyse-donnees/ACP-analyse-composantes-principales-R.php
# Documentation pour FactomineR (complete) : https://cran.r-project.org/web/packages/FactoMineR/FactoMineR.pdf
# Adresse du Mooc (Husson) : http://factominer.free.fr/course/MOOC_fr.html
# Adresse de ressources Husson : http://factominer.free.fr/course/FactoTuto_fr.html
# Adresse de ghub Husson :  https://husson.github.io/

# ----- Installation au prealable de FactoMineR
# install.packages("FactoMineR")

# ------ Exemple du cours 
# Exemple Temperatures - Classification


# 1- Charge FactoMineR et importe les donnees en precisant que le nom des individus est dans la premiere colonne 
library(FactoMineR)
library(vtable)
setwd("C:/Users/OHCE7285/OneDrive - orange.com/Bureau/Local/Univ/Cours/2024 Cours M2")
temperature<-read.csv("temperat.csv",header=TRUE,sep=";",row.names=1)
# Donne un resume de la base de donnees (stats descriptives)
summary(temperature)

#Statistiques descriptives avec le package vtable
st(data = temperature)


#  3 - Classification de type K-means
# Preparation des donnees
temperature1 <- scale(temperature[,1:12]) # les variables sont standardisees
# Determination du nombre de classes par le calcul de l'inertie intra classes 
wss <- (nrow(temperature1)-1)*sum(apply(temperature1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(temperature1,
   centers=i)$withinss)
par(mar = c(1, 1, 1, 1))
plot(1:15, wss, type="b", xlab="Nombre de classes",
  ylab="Somme des inerties intra-classe")
# Mise en place de la classification de type K means 
fit <- kmeans(temperature1, 3) # 3 cluster solution
# Moyenne des variables par classe 
aggregate(temperature1,by=list(fit$cluster),FUN=mean)
# Affectation des individus a leur classe
temperatureKM <- data.frame(temperature, fit$cluster)


# 4 - CAH basee sur le critere de Ward 
d <- dist(temperature[,1:12], method = "euclidean") # la matrice des distances
fit <- hclust(d, method="ward.D")
plot(fit) # Affiche le dendrogramme
groups <- cutree(fit, k=3) # coupe l'arbre en 3 classes
# dessine le dendrogramme en delimitant chaque classe par un rectangle rouge
rect.hclust(fit, k=3, border="red")


# 5 -La fonction utilisee dans Husson
res.pca <- PCA(temperature,ind.sup=24:35,quanti.sup=13:16,quali.sup=17)
res.hcpc <- HCPC(res.pca)
res.hcpc <- HCPC(res.pca,t.levels="all")
res.hcpc$desc.var
res.hcpc$desc.axe
res.hcpc$desc.ind