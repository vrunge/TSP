##  GPL-3 License
## Copyright (c) 2022 Vincent Runge

#' Nearest Neighbour TSP
#'
#' @description Nearest Neighbour algorithm for TSP
#' @param data matrix of positions for cities in (x,y)
#' @param type "one" or "all" to start from one city only or to repeat the algorithm for all possible starting cities
#' @return a vector of indices ("best" order of the cities to visit)
NN_TSP <- function(data, type = "one")
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL

  if(type == "all"){start_test <- 1:n}else{start_test <- sample(n,1)}

  for(l in start_test) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)

    for(i in 1:(n-1))  #tour avec i villes (boucle d'insertion des villes)
    {
      best_insertion_LGR <- Inf
      for(j in 1:(n-i)) #pour chaque ville restante
      {
        LGR <-  distances[tour[[l]][i], to_visit[j]]
        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- c(tour[[l]], to_visit[j])
          to_visit_temp <- to_visit[-j]
        }
      }
      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }

  #####
  best_LGR <- Inf
  for(m in start_test) #choisit le meilleur des n tours trouvés
  {
    LGR <-  sum(distances[tour[[m]] + n*(c(tour[[m]][-1], tour[[m]][1]) - 1)])
    if(LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour[[m]]
    }
  }

  attr(best_tour, "class") <- "TSP"

  return(best_tour)
}


#' Greedy TSP with closest city insertion
#'
#' @description Greedy TSP with  closest city insertion
#' @param data matrix of positions for cities in (x,y)
#' @param type "one" or "all" to start from one city only or to repeat the algorithm for all possible starting cities
#' @return a vector of indices ("best" order of the cities to visit)
greedy_TSP_min <- function(data, type = "one")
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL

  if(type == "all"){start_test <- 1:n}else{start_test <- sample(n,1)}

  for(l in start_test) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)

    for(i in 1:(n-1))  #tour avec i villes (boucle d'insertion des villes)
    {
      distance_min <- Inf
      for(j in 1:(n-i)) #pour chaque ville restante
      {
        for(k in 1:i) #on teste la distance possible de cette ville aux i positions
        {
          LGR <-  distances[tour[[l]][k], to_visit[j]]
          if(LGR < distance_min)
          {
            distance_min <- LGR
            my_choice <- j
          }
        }

      }

      # on choisit la meilleure insertion
      best_insertion_LGR <- Inf
      j <- my_choice

      for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
      {
        if(k < i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][k+1]] - distances[tour[[l]][k], tour[[l]][k+1]]}
        if(k == i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][1]] - distances[tour[[l]][k], tour[[l]][1]]}

        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- append(tour[[l]], to_visit[j], after = k)
          to_visit_temp <- to_visit[-j]
        }
      }

      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }

  #####
  best_LGR <- Inf
  for(m in start_test) #choisit le meilleur des n tours trouvés
  {
    LGR <-  sum(distances[tour[[m]] + n*(c(tour[[m]][-1], tour[[m]][1]) - 1)])
    if(LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour[[m]]
    }
  }

  attr(best_tour, "class") <- "TSP"

  return(best_tour)
}


#' Greedy TSP with best distance insertion
#'
#' @description Greedy TSP with minimal distance insertion
#' @param data matrix of positions for cities in (x,y)
#' @param type "one" or "all" to start from one city only or to repeat the algorithm for all possible starting cities
#' @return a vector of indices ("best" order of the cities to visit)
greedy_TSP_best <- function(data, type = "one")
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL

  if(type == "all"){start_test <- 1:n}else{start_test <- sample(n,1)}

  for(l in start_test) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)

    for(i in 1:(n-1))  #tour avec i villes (boucle d'insertion des villes)
    {
      best_insertion_LGR <- Inf
      for(j in 1:(n-i)) #pour chaque ville restante
      {
        for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
        {
          if(k < i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][k+1]] - distances[tour[[l]][k], tour[[l]][k+1]]}
          if(k == i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][1]] - distances[tour[[l]][k], tour[[l]][1]]}

          if(LGR < best_insertion_LGR)
          {
            best_insertion_LGR <- LGR
            tour_temp <- append(tour[[l]], to_visit[j], after = k)
            to_visit_temp <- to_visit[-j]
          }
        }
      }
      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }

  #####
  best_LGR <- Inf
  for(m in start_test) #choisit le meilleur des n tours trouvés
  {
    LGR <-  sum(distances[tour[[m]] + n*(c(tour[[m]][-1], tour[[m]][1]) - 1)])
    if(LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour[[m]]
    }
  }

  attr(best_tour, "class") <- "TSP"

  return(best_tour)
}


#' Greedy TSP with farthest city insertion
#'
#' @description Greedy TSP with farthest city insertion
#' @param data matrix of positions for cities in (x,y)
#' @param type "one" or "all" to start from one city only or to repeat the algorithm for all possible starting cities
#' @return a vector of indices ("best" order of the cities to visit)
greedy_TSP_max <- function(data, type = "one")
{
  if(type == "all"){return(greedy_TSP_max_all(data))}
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  max_dist <- which.max(distances)
  tour <- c((max_dist  - 1) %% n + 1, (max_dist - 1)%/% n + 1)
  to_visit <- setdiff(1:n, tour)

  for(i in 2:(n-1))  #tour déjà construit avec  i villes (boucle d'insertion des villes)
  {
    distance_max <- -Inf

    for(j in 1:(n-i)) #pour chaque ville restante (de n-2 à 1)
    {
      distance_min <- Inf
      for(k in 1:i) #on cherche la distance min de cette ville aux i positions
      {
        LGR <-  distances[tour[k], to_visit[j]]
        if(LGR < distance_min)
        {
          distance_min <- LGR
          to_visit_temp <- j
        }
      }
      #### si ce min est plus grand qu'un autre min
      if(distance_max < distance_min)
      {
        distance_max <- distance_min
        to_visit_temp_max <- to_visit_temp
      }

    }
    ### best insertion of to_visit_temp_max
    j <- to_visit_temp_max
    best_insertion_LGR <- Inf
    for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
    {
        if(k < i){LGR <-  distances[tour[k], to_visit[j]] + distances[to_visit[j], tour[k+1]] - distances[tour[k], tour[k+1]]}
        if(k == i){LGR <-  distances[tour[k], to_visit[j]] + distances[to_visit[j], tour[1]] - distances[tour[k], tour[1]]}

        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- append(tour, to_visit[j], after = k)
          to_visit_temp <- to_visit[-j]
        }
    }
    tour <- tour_temp
    to_visit <- to_visit_temp

  }


  attr(tour, "class") <- "TSP"

  return(tour)
}




#' greedy_TSP_max_all
#'
#' @description greedy_TSP_max_all
#' @param data matrix of positions for cities in (x,y)
#' @return a vector of indices ("best" order of the cities to visit)
greedy_TSP_max_all <- function(data)
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL

  for(l in 1:n) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)

    for(i in 1:(n-1))  #tour déjà construit avec  i villes (boucle d'insertion des villes)
    {
      distance_max <- -Inf

      for(j in 1:(n-i)) #pour chaque ville restante (de n-2 à 1)
      {
        distance_min <- Inf
        for(k in 1:i) #on cherche la distance min de cette ville aux i positions
        {
          LGR <-  distances[tour[[l]][k], to_visit[j]]
          if(LGR < distance_min)
          {
            distance_min <- LGR
            to_visit_temp <- j
          }
        }
        #### si ce min est plus grand qu'un autre min
        if(distance_max < distance_min)
        {
          distance_max <- distance_min
          to_visit_temp_max <- to_visit_temp
        }

      }
      ### best insertion of to_visit_temp_max
      j <- to_visit_temp_max
      best_insertion_LGR <- Inf
      for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
      {
        if(k < i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][k+1]] - distances[tour[[l]][k], tour[[l]][k+1]]}
        if(k == i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][1]] - distances[tour[[l]][k], tour[[l]][1]]}

        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- append(tour[[l]], to_visit[j], after = k)
          to_visit_temp <- to_visit[-j]
        }
      }
      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }

  #####
  best_LGR <- Inf
  for(m in 1:n) #choisit le meilleur des n tours trouvés
  {
    LGR <-  sum(distances[tour[[m]] + n*(c(tour[[m]][-1], tour[[m]][1]) - 1)])
    if(LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour[[m]]
    }
  }

  attr(best_tour, "class") <- "TSP"

  return(best_tour)
}
