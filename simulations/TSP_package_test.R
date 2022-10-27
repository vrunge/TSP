

###
### greedy algorithms TEST
###


n <- 40
villes <- matrix(runif(2*n), n, 2)

res <- NN_TSP(villes)
plot(tour = res, data = villes)
t1 <- tour_length(res, villes)

res <- greedy_TSP_min(villes)
plot(tour = res, data = villes)
t2 <- tour_length(res, villes)

res <- greedy_TSP_best(villes)
plot(tour = res, data = villes)
t3 <- tour_length(res, villes)

res <- greedy_TSP_max(villes)
plot(tour = res, data = villes)
t4 <- tour_length(res, villes)

c(t4,t3,t2,t1)




################################
###### data length vector ######
################################

#mytest <- seq(from = 100, to = 1500, by = 10)
my_n_vector_LOG <- seq(from = log(10), to = log(100), by = log(10)/10)
my_n_vector <- round(exp(my_n_vector_LOG))
my_n_vector
diff(log(my_n_vector))



##################################
###### Initialize dataframe ######
##################################
p <- 10 ### répétition
df <- data.frame(matrix( nrow = 4 * length(my_n_vector), ncol = 2 + p))
colnames(df) <- c("type", "n", 1:p)
dim(df)

library(parallel)
nbCores <- 8
j <- 1

for(n in my_n_vector)
{
  print(n)
  villes <- matrix(runif(2*n), n, 2)
  liste1 <- mclapply(1:p, FUN = one.simu_time_TSP,
                      data = villes,
                     algo = "NN",
                     mc.cores = nbCores)

  liste2 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = villes,
                     algo = "best",
                     mc.cores = nbCores)

  liste3 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = villes,
                     algo = "min",
                     mc.cores = nbCores)

  liste4 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = villes,
                     algo = "max",
                     mc.cores = nbCores)


  df[j ,] <- c("NN", n, do.call(cbind, liste1))
  df[j+1 ,] <- c("best", n, do.call(cbind, liste2))
  df[j+2 ,] <- c("min", n, do.call(cbind, liste3))
  df[j+3 ,] <- c("max", n, do.call(cbind, liste4))
  j <- j + 4
}

library(reshape2)
df <- melt(df, id.vars = c("type","n"))


data_summary <- function(data, varname, groupnames)
{
  require(plyr)
  summary_func <- function(x, col)
  {
    c(mean = mean(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], 0.025), q3 = quantile(x[[col]], 0.975))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- df
df2[,2] <- as.double(df[,2])
df2[,3] <- as.double(df[,3])
df2[,4] <- as.double(df[,4])
summary(df2)

df_new <- data_summary(df2, varname="value",
                           groupnames=c("type","n"))

theMin <- min(df_new[,3:5],df_new[,3:5])
theMax <- max(df_new[,3:5],df_new[,3:5])

################################
###### PLOT with ggplot2 #######
################################
colnames(df_new)
# Everything on the same plot
ggplot(df_new, aes(x = n, y = value, col=type)) +  scale_x_log10()+ scale_y_log10(limits = c(theMin, theMax))  +
  labs(y = "time in seconds") +  labs(x = "length of the time series") +
  geom_point(size = 2, aes(shape = type)) +
  geom_errorbar(aes(ymin=`q1.2.5%`, ymax=`q3.97.5%`), width=.01) +
  scale_colour_manual(values = c("best" = "#0080FF",
                                 "max" = " dark blue", "min" = "blue", "NN" = "red")) +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.text=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.position = c(0.7, 0.1),
        legend.title = element_blank())



R1 <- df_new[df_new$type == "NN",c(2,3)]
l1 <- lm(log(value) ~ log(n), data = R1, )
summary(l1)
l1$coefficients



R2 <- df_new[df_new$type == "best",c(2,3)]
l2 <- lm(log(value) ~ log(n), data = R2, )
summary(l2)
l2$coefficients



R3 <- df_new[df_new$type == "min",c(2,3)]
l3 <- lm(log(value) ~ log(n), data = R3, )
summary(l3)
l3$coefficients



R4 <- df_new[df_new$type == "max",c(2,3)]
l4 <- lm(log(value) ~ log(n), data = R4, )
summary(l4)
l4$coefficients

