## Python native implementation of C Index described by Hubert & Levin

### Description
Calculates Hubert & Levin C index - internal cluster quality index

The C Index was described by Hubert & Levin (1976) in “A General Statistical Framework for Assessing Categorical Clustering in Free Recall.” Psychological Bulletin, 83(6), 1072–1080.  Its purpose is to compare the dispersion of clusters of data relative to the total dispersion in a dataset.  Ideally, the value of the number of clusters that minimizes the C Index will also be the optimal number of clusters to partition a dataset.  

### More Description
The C Index is calculated as 
`cindex = (Sw - Smin) / (Smax - Smin)`

Sw is the sum of within-cluster distance measurements (only point-wise combinations of data are summed within each cluster - not between clusters)
Smin is the sum of the Nw smallest point-wise distances between points within the entire dataset
Smax is the sum of the nw largest point-wise distances between points within the entire dataset
Nw is the total number of pairs of observations belonging to the same cluster. It is the same as total combinations of points within clusters taken two at a time

Installation
```bash
pip install c_index
```

Example usage
```python
from c_index import (calc_c_index, calc_cindex_clusterSim_implementation,
calc_cindex_nbclust_implementation,
pdist_array)

xs = np.array([[1,2,1.5,1.75,1.33,0.88],
                [5,5.5,4.88,6.33,5.01,4.95]]) # Cluster 1, 2 x values
ys = np.array([[8.9,8.5,7.89,8.25,8.85,8.29],
                [1.25,1.14,1.85,0.85,0.79,0.96]]) # Cluster 1,2 y values
X = np.stack((xs.ravel(),ys.ravel()), axis=1)
cluster_labels = np.array([0,0,0,0,0,0,1,1,1,1,1,1])

cindex = calc_c_index(X, cluster_labels)

# nbclust implementation takes an array of pointwise differences
distances_array = pdist_array(X) 

# NbClust C Index python equivalent
cindex = calc_cindex_nbclust_implementation(distances_array, cluster_labels)

# clusterSim C Index python equivalent
cindex = calc_cindex_clusterSim_implementation(distances_array, cluster_labels)
```

The C Index is already implemented in a number of R Packages, including [clusterSim](https://cran.r-project.org/web/packages/clusterSim/clusterSim.pdf), [clusterCrit](https://cran.r-project.org/web/packages/clusterCrit/clusterCrit.pdf), and [NbClust](https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust)


This packages implementation differs from these packages in a couple ways.

### ClusterSim differences
Note these two lines (R Code) :
```r
	Dmin=sum(sort(ddist)[1:r])
	Dmax=sum(sort(ddist,decreasing = T)[1:r])
```
THey include the whole distance array, which includes all permutations of distances between points (instead of combinations). This means the high end and low end are double counted. I don't think that is the correct way to
calculate C Index, but maybe they have a specific reason for doing so.

## NbClust differences
Note these lines (R Code) :
```r
    Dmin = min(v_min)
    Dmax = max(v_max)
    result <- (DU - r * Dmin)/(Dmax * r - Dmin * r)
```
Instead of sorting and calculating the Nw minimum and maximum distance measurements between points, they simply multiply 'r' by the minimum and maximum values of distances.  I'm not sure what their motivation was for doing that, but sometimes it does give a better estimate of the number of clusters than the described method above.