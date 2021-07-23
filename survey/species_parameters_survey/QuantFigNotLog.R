QuantFigNotLog <- function(data1, data2, data3){
  lbls <- setNames(paste(unique(data2$Species), "; Total CL:", data2$CL, "; \nN:", data3$n,
                         "; Mean:", round(data2$WtAve), "\U00B1", round(data2$S),
                         "SE"), unique(data2$Species))
  ggplot() +
    geom_point(data = data1, aes(x = mResponse, y = CL, size = N, fill = N),
               alpha = 0.5, shape = 21, color = "black", stroke = 1.5) +
    geom_vline(data = data2, aes(xintercept = WtAve), lty = 2, col = "blue", lwd = 1) +
    geom_rect(data = data2, aes(xmin = dnSD, xmax = upSD, ymin = 0, ymax = 100), color = "lightblue", alpha = 0.25) +
    facet_wrap(~Species,
               labeller = labeller(Species = setNames(unlist(lbls), unique(data1$Species)))) +
    theme_bw() +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), 
          title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
    ylim(0, 100) +
    scale_size_continuous(range = c(9, 12), breaks = pretty_breaks(4)) +
    scale_fill_viridis(option = "D", breaks = pretty_breaks(4)) +
    guides(fill = guide_legend(), size = guide_legend()) +
    ylab("Confidence Level") +
    xlab("Answer Response") 
}