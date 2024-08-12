my_cor_sif_ano <- function(df,out='cor'){

  sif <- df$sif
  ano <- df$ano

  corre <- cor.test(sif,ano)
  correl_est <- as.numeric(corre$estimate)

  if(out=='cor'){
    return(correl_est)
  }
  if(out=='pvalue'){
    if(is.nan(correl_est)){
      correl_est <- 0
      p <- 1
    }else{
      p <- corre$p.value
      if(is.nan(p)) p <- 1
    }
    return(p)
  }else{
    return('erro')
  }
}

my_cor_sif_prec <- function(df,out='cor'){
  sif <- df$sif
  prec <- df$prec

  corre <- cor.test(sif,prec)
  correl_est <- as.numeric(corre$estimate)

  if(out=='cor'){
    return(correl_est)
  }
  if(out=='pvalue'){
    if(is.nan(correl_est)){
      correl_est <- 0
      p <- 1
    }else{
      p <- corre$p.value
      if(is.nan(p)) p <- 1
    }
    return(p)
  }else{
    return('erro')
  }
}

my_cor_ano_prec <- function(df,out='cor'){
  prec <- df$prec
  ano <- df$ano

  corre <- cor.test(ano,prec)

  correl_est <- as.numeric(corre$estimate)

  if(out=='cor'){
    return(correl_est)
  }
  if(out=='pvalue'){
    if(is.nan(correl_est)){
      correl_est <- 0
      p <- 1
    }else{
      p <- corre$p.value
      if(is.nan(p)) p <- 1
    }
    return(p)
  }else{
    return('erro')
  }
}
