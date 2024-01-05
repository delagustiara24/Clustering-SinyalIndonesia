kp=read.csv(file.choose(),header=TRUE,sep=",")
kp
summary(kp)
#melakukan scalling data
kp1 <- scale(kp[,2:3])#Standarisasi atau scalling data
kp1
#package
library(fpc)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
summary(is.na(kp1)) #melihat missing value
summary(kp1) #melihat deskriptif data
boxplot(kp1) #melihat data outlier

#menampilkan nilai kmo
library(psych)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(kp1)


#cek multiko
multikolinearitas<-cor(kp1)
multikolinearitas
# pamk (penentuan jumlah cluster)
library(fpc)
pamk.result <- pamk(kp1)
pamk.result
pamk.result$nc
#menampilkan grafik sillhouette
fviz_nbclust(kp1, pam, method = "silhouette")
pam.hasil <- pam(kp1, 2)
#menunjukkan penomoran atau label cluster dari masing-masing data
summary(pam.hasil)
#menunjukkan nilai rata-rata metode sihlouette dari masing-masing cluster,
#Metric menunjukkan metode jarak yang dipakai yaitu Eucledian Distance
pam.hasil$medoidss
pam.hasil$diss
#melihat label cluster jumlah penduduk
data.frame(kp$Provinsi,pam.hasil$clustering)
data.frame(kp,pam.hasil$cluster)
#visualisasi kmedoidss dari dari masing-masing cluster
fviz_cluster(pam.hasil)
#menghitung nilai rata2
kp%>%
  mutate(Cluster = pam.hasil$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
#pengujian multiko
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(kp1, histogram=TRUE, pch=19)

#melihat label berdasarkan pengelompokan 
label1<-data.frame(kp$Provinsi,pam.hasil$clustering)
label1
kelompok1 <- subset(label1, pam.hasil.clustering==1)
kelompok1
kelompok2 <- subset(label1, pam.hasil.clustering==2)
kelompok2
