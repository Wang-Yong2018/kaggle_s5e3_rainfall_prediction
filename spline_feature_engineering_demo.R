library(rlang)

library(patchwork)
data = train

plot_smoother <- function( deg_free=2,x_name=col_name) {
  df <- train
  plt <- ggplot(df, aes(x = !!sym(x_name), y =cloud )) +
    geom_point(alpha = .2) +
    geom_smooth(
      method = lm,
      formula = y ~ splines::ns(x, df = deg_free),
      color = "blue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "cloud rate")
  return(plt)
}
list('day','dewpoint','temparature')|>
        map(\(x)

            ( plot_smoother(2,x) + plot_smoother(5,x)) / ( plot_smoother(20,x) + plot_smoother(100,x) )
            )
