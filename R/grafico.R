tema_mapa <- function(){
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(color="black",fill = "white"),
      panel.grid.major = ggplot2::element_line(color="gray",linetype = 3)),
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
