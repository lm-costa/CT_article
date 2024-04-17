source('SIF/r/function.r')

url_filename <- list.files("SIF/url/",
                            pattern = ".txt",
                            full.names = TRUE)

urls <- read.table(url_filename) |>
  dplyr::filter(!stringr::str_detect(V1,".pdf"))
n_urls <- nrow(urls)

my_ncdf4_download(urls[1,1])

###
tictoc::tic()
furrr::future_pmap(list(urls[,1],"input your user","input your passoword"),
                   my_ncdf4_download)
tictoc::toc()
