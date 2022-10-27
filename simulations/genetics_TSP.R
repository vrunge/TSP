
n <- 10
data <- matrix(runif(2*n), n, 2)

croisement <- function(data)
{
  n <- dim(data)[2]
  p <- sample(n, 1) #position echange
  temp <-  data[2,p] 
  data[2,p] <- data[1,p] 
  data[1,p] <- temp

  while(length(unique(data[1,])) < n)
  {
    p <- setdiff(which(data[1,] == temp), p)
    temp <-  data[2,p] 
    data[2,p] <- data[1,p] 
    data[1,p] <- temp
  }
  return(data)
}

#####

cites <- function(data, nb_Chrom = 10, select =  floor(dim(data)[1]/2), iterMax = 100)
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  
  #initialisation
  chrom <- t(apply(matrix(0, nb_Chrom, n), 1, function(x) sample(n)))
 
  for(i in 1:iterMax)
  {
    print(chrom)
    ### 1) DISTANCES TOURS
    dist_tours <- rep(0, nb_Chrom)
    for(k in 1:nb_Chrom)
    {
      for(j in 1:(n-1)){dist_tours[k] <- dist_tours[k] + distances[chrom[k,j],chrom[k,j+1]]} 
      dist_tours[k] <- dist_tours[k] + distances[chrom[k,n],chrom[k,1]]
    }
    ### 2) selection
    chrom[1:select, ] <- chrom[order(dist_tours)[1:select],]
    
    ### 3) croisement
    for(k in seq(select + 1, n-1, by = 2))
    {
      chrom[c(k,k+1),] <- croisement(chrom[sample(1:select, size = 2, prob = select:1),])
    }
    if(k < n-1){chrom[n,] <- croisement(chrom[sample(1:select, size = 2, prob = select:1),])[1,]}
    
    ### 3) mutation
    
    
  }
  
  return(chrom)
}


cites(data, iterMax = 5)


##### PLOT


n <- 100
res <- graph(n,20)
res
plot(res$x, res$y, xlim = c(0,1), ylim = c(0,1))
for(i in 1:dim(res$edge)[1]){segments(x0 = res$x[res$edge[i,1]],
                                      y0 = res$y[res$edge[i,1]], 
                                      x1 = res$x[res$edge[i,2]], 
                                      y1 = res$y[res$edge[i,2]], lwd = 0.1)}
points(res$x[1], res$y[1], col = 2, cex = 2)
points(res$x[n], res$y[n], col = 2, cex = 2)


