# function to generate a block correlation structure
block_builder <- 
  function(p, rho, size){
    n.blocks <- p/size 
    out <- matrix(rep(0, p^2), ncol = p) 
    for(i in c(0:(n.blocks-1))){
      out[c(i*size+1):c(i*size+size), c(i*size+1):c(i*size+size)] <- rho
    }
    diag(out) <- 1
    return(out)
  }

