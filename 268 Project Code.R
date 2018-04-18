#### DATA ####

  Mar2.morn <- "A very solid mid-period D swell is peaking through the day today offering up chest-head high surf to good exposures while standouts see overhead-well overhead sets during the morning Winds are light from the D now attempting to smooth out the chop and texture leftover from yesterdays weather Tide is rising to a N HIGH at T slowing things down at many breaks and hiding the swell"
  Report2 <- 'Moderate mid period swell from the west rising rapidly later in the next few days. Breezy east-northeast winds, whitecapping conditions with moderate choppy seas, winds dropping rapidly later. Small mid period swell from the west rising later in the later part of the week. Light and variable east-northeast winds, Increasing later and switching to the west-northwest.'
  Report3 <- 'The surf looks generally more rideable this afternoon on the low tide, although surf is still pretty small. Average spots in town are struggling to get up to waist high, while the better breaks are around waist high on the sets. Wind is light and surrfce conditions are mostly clean'
  Report4 <- 'Poor quality surf prevails across the region this afternoon with minor swell mix and bumpy conditions due to S wind. Most breaks are knee high and below, while best spots are occasionally up to waist high. '
  Report5 <- 'The surf may have improved a touch from this morning as the tide has dropped, but quality is still poor due to small size. Surface conditions are still generally pretty clean, but most breaks are under waist high. Standouts are seeing the occasional waist high set. '
  
  Report.list <- strsplit(Mar2.morn, ' ')
  Report.list = lapply(Report.list, tolower)
  
  n = 3
  N = length(Report.list[[1]]) - n + 1


#### FUNCTIONS ####

## split a text into histories ##

  history.mat = matrix(0, nrow = N, ncol = n)
  
  for (i in 1:(length(Report.list[[1]]) - n + 1)){
    history.mat[i, ] = Report.list[[1]][c(i:(i+(n-1)))]
    i = i + 1
  } 

## beta function ##

  beta <- function(position, word){
    ind = which(history.mat[, position] == word)
    return(history.mat[ind, ])
  }

## set of all betas ##

  beta.set <- function(position){
    all.beta = list()
    index = 1
    for (w in unique(history.mat[, position])){
      all.beta[[index]] = beta(position, w)
      index = index + 1
    }
    return(all.beta)
  }

## generate L and R set ##

  l.and.r <- function(x, h.p){
    j = 1
    k = 1
    L = matrix(0, nrow = N, ncol = n)
    R = matrix(0, nrow = N, ncol = n)
    for (v in unique(h.p[, x])){
      if (runif(1) < .5){
        r = length(which(h.p[, x] == v))
        L[c(j:(j+r-1)), ] = h.p[which(h.p[, x] == v), ]
        j = j + r
      }else{
        r = length(which(h.p[, x] == v))
        R[c(k:(k+r-1)), ] = h.p[which(h.p[, x] == v), ]
        k = k + r
      }
    }
    L = L[-c(j:N), ]
    R = R[-c(k:N), ]
    return(list(L, R))
  }

## likelihood given an L and R ##
  
  lik.fun <- function(L, R){
    lik = 0
    for (w in unique(rbind(L, R)[, n])){
      lik.star1 = length(which(L[, n] == w)) * log(length(which(L[ , n] == w))/length(L[, n])) 
      lik.star2 = length(which(R[, n] == w)) * log(length(which(R[, n] == w))/length(R[, n]))
      if(!(is.nan(lik.star1) | is.nan(lik.star2))){
        lik = lik + lik.star1 +lik.star2
      }
    }
    return(lik)
  }
  
## move between L and R, return result ##
  
  move.fun.l <- function(x, v, L, R){
    L.to.remove = which(L[, x] == v)
    L.star = L[-L.to.remove, ]
    R.star = rbind(R, L[L.to.remove, ])
    
    if (lik.fun(L, R) < lik.fun(L.star, R.star)){
      L = L.star
      R = R.star
    }
    return(list(L, R))
  }
  
  move.fun.r <- function(x, v, L, R){
    R.to.remove = which(R[, x] == v)
    R.star = R[-R.to.remove, ]
    L.star = rbind(L, R[R.to.remove, ])
    
    if (lik.fun(L, R) < lik.fun(L.star, R.star)){
      L = L.star
      R = R.star
      move = TRUE
    }
    return(list(L, R))
  }
  

## Iterate through leave splitting ##
  
  loop = TRUE
  tree = as.list(rep(0, 100))
  tree[[1]] = history.mat
  i = 1
  
  
  while(loop)    
    for (node in tree[[i]]){
      
      x = sample(c(1, 2), 1)
      split = l.and.r(x, node56)
      split.original = split
      
      for (v in unique(split[[1]][, x])){
        split = move.fun.l(x, v, split[[1]], split[[2]])
      }
      
      for (v in unique(split[[2]][, 2])){
        split = move.fun.r(x, v, split[[1]], split[[2]])
      }
      
      
      
      
      
      
      
      
      if (length(as.vector(split[[1]])) != length(as.vector(split.original[[1]]))){
        if(length(tree[[i+1]]) == 1){
          tree[[i+1]] = split
        }else{
          tree[[i+1]] = append(tree[[i+1]], split)
        }
      }
      
      if(length(unlist(split[[1]])) == length(unlist(split.original[[1]]))){
        if(sum(1*(as.vector(split[[1]])) == as.vector(split.original[[1]])) == length(as.vector(split[[1]]))){
          if(length(tree[[i+1]]) == 1){
            tree[[i+1]] = split
          }else{
            tree[[i+1]] = append(tree[[i+1]], split)
          }
        }
      }
    }
    
    
    
    

  