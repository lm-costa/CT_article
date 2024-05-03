df <- read.csv("data/trend_arcmap_classified.csv")


df <- df |>
  dplyr::filter(RASTERVALU !=-9999) |>
  dplyr::mutate(
    biome=
      dplyr::case_when(
      RASTERVALU == 1 ~'AMZ',
      RASTERVALU == 2 ~ 'CAA',
      RASTERVALU == 3 ~'CER',
      RASTERVALU ==4~'AF',
      RASTERVALU ==5~'PMP',
      RASTERVALU == 6 ~'PNT'
      )
  )

library(ExpDes.pt)

dic(df$biome,df$beta_line)
