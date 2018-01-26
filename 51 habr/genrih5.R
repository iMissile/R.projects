
library(foreach)
library(iterators)

cls <- parallel::makeCluster(parallel::detectCores()-1)
doParallel::registerDoParallel(cls)

d <- scale(iris[,-5])
centroids <- 2:33 # число кластеров

total.witness <- foreach(i=iter(centroids), .combine = c, .packages = "stats") %dopar% {
  fit <- kmeans(x = d,centers = i,iter.max = 30,nstart = 3)
  fit$tot.withinss
}

length(total.witness) # почему-то 4..........

doParallel::stopImplicitCluster();
parallel::stopCluster(cls);
rm(cls);
gc()

