
############ Modeling ############################################################################

### Function 1 - clustering ###

clusterizar <- function(dorig, m){
  
  set.seed(100)
  
  dadosm <- dorig[dorig$month == m, ]
  
  mydata = dadosm[3]
  max_aux1 = max(dadosm[3])
  min_aux1 = min(dadosm[3])
  
  if (max_aux1>0) {mydata[,1] = (mydata[,1] - min_aux1) / (max_aux1 - min_aux1)}  
  
  wss = wss_acum = NULL
  min_size = NULL
  clusterizacoes = list()
  
  for(i in 1:6){
    c = kmeans(mydata,centers=i,iter.max = 15)
    wss[i] = sum(c$withinss)
    if(i == 1){wss_acum = wss[i]}else{wss_acum[i] = wss_acum[i-1]+wss[i]}
    min_size[i] = min(c$size)
    clusterizacoes[[i]] = c
  }
  
  wss_aux = wss_acum/sum(wss)
  idx = which(min_size != 1)
  wss_aux2 = wss_aux[idx]
  wss_aux3 = rbind(wss_aux2, idx)
  if (last(wss_aux3[1,]) < 0.95) { 
    column = which.max(abs(wss_aux3[1,]))
    aux_num_cluster = wss_aux3[2,column]
  } else {
    column = which(abs(wss_aux2) >= 0.95)[1]
    aux_num_cluster = wss_aux3[2,column]
  }
  
  clusterizacao = clusterizacoes[[aux_num_cluster]]
  
  centroides <- clusterizacao$centers
  agrupamento<- clusterizacao$cluster

  #Ascending order of the centroids
  
  ordem <- order(centroides)
  novo_indice <- seq_along(ordem)
  nomes_novos <- setNames(novo_indice, ordem)
  clusters_reordenados <- nomes_novos[as.character(agrupamento)]
  centroides_reordenados <- centroides[ordem]
  size_reordenado <- as.vector(table(clusters_reordenados))

  centroides_reordenados = (centroides_reordenados * (max_aux1 - min_aux1)) + min_aux1
  
  return(list(centroides_reordenados, clusters_reordenados, size_reordenado))
  
}


### Function 2 - transition matrix ###

transition_matrix<-function(mun,m){
  
  ## Matrix non accumulated
  
  if(m<12){ 
    Pind_matriz = matrix(nrow = length(centroides[[(12*(mun-1))+m]]), ncol = length(centroides[[(12*(mun-1))+m+1]]), data = 0)
  }else{ #December to January case
    Pind_matriz = matrix(nrow = length(centroides[[(12*(mun-1))+m]]), ncol = length(centroides[[(12*(mun-1))+1]]), data = 0)
  }
  
  case <- as.data.frame(municip[[mun]])
  
  month_or <- case[case$month == m, ]
  
  if (m<12){
  
    month_dt <- case[case$month == m+1, ]
    
    for (i in 1:nrow(Pind_matriz)) { 
      total_or <- sum(month_or$cluster == i)
      
      if (total_or > 0) { 
        
        for (j in 1:ncol(Pind_matriz)) {
          transitions <- sum(month_or$cluster == i & 
                             month_dt$cluster == j)
          
          Pind_matriz[i, j] <- transitions / total_or
        }
      }
    }
  } else{ #December to January case
  
    month_dt <- case[case$month == 1, ]
    
    month_or <- month_or %>% mutate(next_year = year + 1)
    month_dt <- month_dt %>% rename(next_year = year)
    
    trans_dec_jan <- month_or %>%
      inner_join(month_dt, by = "next_year", suffix = c("_dec", "_jan"))
    
    for (i in 1:nrow(Pind_matriz)) { 
      total_dec <- sum(trans_dec_jan$cluster_dec == i)
      
      if (total_dec > 0) { 
        
        for (j in 1:ncol(Pind_matriz)) { 
          transitions <- sum(trans_dec_jan$cluster_dec == i & 
                             trans_dec_jan$cluster_jan == j) 
          
          Pind_matriz[i, j] <- transitions / total_dec  
        }
        
      }
    }
  }
  
  ## Matrix accumulated
  
  Pacum_matriz <- Pind_matriz
  for(z in 2:ncol(Pind_matriz)){
    Pacum_matriz[,z]<-Pacum_matriz[,z-1]+Pacum_matriz[,z]
  }
  
  return(list(Pind_matriz, Pacum_matriz))
  
}

############ Future Scenarios Simulation #########################################################

# Function 3 - cluster simulation

simulat_state <- function(state1, matrix_tr, rand){
  column=1
  while (column <= ncol(matrix_tr)) {
    if(column==1 && rand<=matrix_tr[state1,column]) {
      state2=column
      column=ncol(matrix_tr)+1
    }else if(matrix_tr[state1,column]<rand && rand<=matrix_tr[state1,column+1]) {
      state2=column+1
      column=ncol(matrix_tr)+1
    }else {
      column=column+1
    }
  }
  return(state2)
}

# Function 4 - cluster element sampling

simulat_value <- function(possible_values,rand){
  all_values <- c(possible_values[[3]], possible_values[[5]][1])
  indice <- ceiling(rand * length(all_values))
  drawn_value <- all_values[indice]
  return(drawn_value)
}

