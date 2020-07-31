# clusterCrit Example

x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2),
           + matrix(rnorm(100, mean = 1, sd = 0.5), ncol = 2),
           + matrix(rnorm(100, mean = 2, sd = 0.5), ncol = 2))


# clusterSim example 

# Example 1
library(clusterSim)
data(data_ratio)
# d <- dist.GDM(data_ratio)
d <- dist(data_ratio, method="euclidean")
d_eucl<-data.matrix(d_eucl)
c <- pam(d, 5, diss = TRUE)
# cl <- c$clustering
cluster_labels<-read.csv("C:\\Users\\z003vrzk\\.spyder-py3\\Scripts\\ML\\c-index\\c-index\\RData_labels.csv",
                         row.names=1)
cl<-cluster_labels
icq <- index.C(d,c$clustering)
print(icq)

# write.table(data_ratio, file="C:\\Users\\z003vrzk\\.spyder-py3\\Scripts\\ML\\c-index\\c-index\\RData.csv", sep=",")
# write.table(cl, file="C:\\Users\\z003vrzk\\.spyder-py3\\Scripts\\ML\\c-index\\c-index\\RData_labels.csv", sep=",")


index.C<-function(d,cl)
{
  ddist<-d
  d<-data.matrix(d)
  DU<-0
  r<-0
  for (i in 1:max(cl))
  {
    t<-d[cl==i,cl==i]		
    n<-sum(cl==i)
    print(n)
    if (n>1)
    {
      DU=DU+sum(t)/2
    }
    r<-r+n*(n-1)/2
  }
  Dmin=sum(sort(ddist)[1:r])
  Dmax=sum(sort(ddist,decreasing = T)[1:r])
  if(Dmin==Dmax)
    result<-NA
  else
    result<-(DU-Dmin)/(Dmax-Dmin)
  result		
}