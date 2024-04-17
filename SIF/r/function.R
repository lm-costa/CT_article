
meu_cv <- function(x){
  100*sd(x)/mean(x)
}

meu_erro_padrao <- function(x){
  sd(x)/sqrt(length(x))
}


est_desc <- function(x){
  n <- length(x)
  n_na <- sum(is.na(x)) # <<<<<<<------------
  x<- na.omit(x)
  m <- mean(x)
  dp <- sd(x)
  md <- median(x) # quantile(x, 0.50)
  cv <- meu_cv(x)
  mini <- min(x)
  maxi <- max(x)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  s2 <- var(x)
  g1 <- agricolae::skewness(x)
  g2 <- agricolae::kurtosis(x)
  epm <- meu_erro_padrao(x)
  normtest <- shapiro.test(x)


  return(c(N = n,
           N_perdidos = n_na, # <<<<<<<<<--------
           Media = m,Mediana = md,
           Min = mini,Max = maxi,
           Var = s2,DP = dp,
           Q1 = q1,Q3 = q3,
           CV = cv,EPM = epm,
           G1 = g1,G2 = g2,
           Shapiro=normtest$p.value))
}



## function to create a linear model for each pixel
linear_reg <- function(df,output="beta1"){
  # model for each grid cell
  modelo <- lm(sif ~ date, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # beta
  if(output=="beta1"){
    return(beta_1)
  }

  # p value
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }
  #
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(sif = mean(sif), na.mr=TRUE) |>
      dplyr::pull(sif)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }

  if(output == 'betaerror'){
    betaerror <- as.numeric(sqrt(diag(vcov(modelo)))[2])
    return(betaerror)
  }

  if(output == 'modelerror'){
    modelerror <- summary(modelo)$sigma
    return(modelerror)
  }
}

def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}


tema_mapa <- function(){
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(color="black",fill = "white"),
      panel.grid.major = ggplot2::element_line(color="black",linetype = 3)),
    ggspatial::annotation_scale(
      location="bl",
      height = ggplot2::unit(0.2,"cm")),
    ggspatial::annotation_north_arrow(
      location="tr",
      style = ggspatial::north_arrow_nautical,
      height = ggplot2::unit(1.5,"cm"),
      width =  ggplot2::unit(1.5,"cm"))
  )
}


detrend_fun <- function(df){
  # model for each grid cell
  modelo <- lm(sif ~ id_x, data=df)
  sif_est = modelo$coefficients[1] + modelo$coefficients[2]*df$id_x
  delta=sif_est - df$sif
  sifr = (modelo$coefficients[1]-delta)-(mean(df$sif)-modelo$coefficients[1])

  return(sifr)
}



#' Função utilizada para extração de colunas
#' específicas de arquivo ncdf4 para xco2
my_ncdf4_extractor <- function(ncdf4_file){
  df <- ncdf4::nc_open(ncdf4_file)
  if(df$ndims!=0){
    dft <- data.frame(
      'lon' = ncdf4::ncvar_get(df,varid='Longitude'),
      'lat' = ncdf4::ncvar_get(df,varid='Latitude'),
      'time' = ncdf4::ncvar_get(df,varid='Delta_Time'),
      'sza'= ncdf4::ncvar_get(df, varid = 'SZA'),
      'sif_740' = ncdf4::ncvar_get(df,varid='Daily_SIF_740nm'),
      'sif_757' = ncdf4::ncvar_get(df,varid='Daily_SIF_757nm'),
      'sif_771' = ncdf4::ncvar_get(df,varid='Daily_SIF_771nm'),
      'quality_flag' = ncdf4::ncvar_get(df,varid='Quality_Flag')
    ) |>
      dplyr::filter(lon < -35 & lon >-75 & lat < 5 & lat >-35)|>
      dplyr::filter(quality_flag==0) |>
      tibble::as_tibble()
  }
  ncdf4::nc_close(df)
  return(dft)
}

#' Função utilizada para downloads
my_ncdf4_download <- function(url_unique,
                              user="input your user",
                              password="input your password"){
  if(is.character(user)==TRUE & is.character(password)==TRUE){
    n_split <- length(
      stringr::str_split(url_unique,
                         "/",
                         simplify=TRUE))
    filenames_nc <- stringr::str_split(url_unique,
                                       "/",
                                       simplify = TRUE)[,n_split]
    repeat{
      dw <- try(download.file(url_unique,
                              paste0("SIF/data-raw/",filenames_nc),
                              method="wget",
                              extra= c(paste0("--user=", user,
                                              " --password ",
                                              password))
      ))
      if(!(inherits(dw,"try-error")))
        break
    }
  }else{
    print("input a string")
  }
}
