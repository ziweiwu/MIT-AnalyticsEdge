dailykos = read.csv("dailykos.csv")
str(dailykos)
summary(dailykos)

#convert data to matrix then to vector
dailymatrix=as.matrix(dailykos)
dailyvector = as.vector(dailykos)

#set number of cluster to 8 and run k mean
k=7
set.seed(3000)
daily_KMC = kmeans(dailyvector, centers = k, iter.max = 1000)
str(daily_KMC)
#subset cluster data in 7 sets
KmeansCluster1 = subset(dailykos, daily_KMC$cluster == 1)

KmeansCluster2 = subset(dailykos, daily_KMC$cluster == 2)

KmeansCluster3 = subset(dailykos, daily_KMC$cluster == 3)

KmeansCluster4 = subset(dailykos, daily_KMC$cluster == 4)

KmeansCluster5 = subset(dailykos, daily_KMC$cluster == 5)

KmeansCluster6 = subset(dailykos, daily_KMC$cluster == 6)

KmeansCluster7 = subset(dailykos, daily_KMC$cluster == 7)

tail(sort(colMeans(KmeansCluster1)))

tail(sort(colMeans(KmeansCluster2)))

tail(sort(colMeans(KmeansCluster3)))

tail(sort(colMeans(KmeansCluster4)))

tail(sort(colMeans(KmeansCluster5)))

tail(sort(colMeans(KmeansCluster6)))

tail(sort(colMeans(KmeansCluster7)))

