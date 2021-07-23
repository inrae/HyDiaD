QuantFigLogScale <- function(data1, data2, data3){
  lbls <- setNames(paste(unique(data2$Species), ";Total CL:", data2$CL, ";N:", data3$n,
                         #"\nMean:", round(data2$WtAve), "+", round(data2$upSD), ",-", round(data2$dnSD),
                         #"SE"), unique(data2$Species))
                         "\nMean:", scientific(data2$WtAve, digits = 2), "+", scientific(data2$upSD, digits = 1),
                         ",-", scientific(data2$dnSD, digits = 1),"SE"), unique(data2$Species))
  ggplot() + 
    geom_point(data = data1, aes(x = mResponse, y = CL, size = N, fill = N), 
               alpha = 0.5, shape = 21, color = "black", stroke = 1.5) +
    geom_vline(data = data2, aes(xintercept = WtAve), lty = 2, col = "blue", lwd = 1) +
    geom_rect(data = data2, aes(xmin = dnSD, xmax = upSD, ymin = 0, ymax = 100), color = "lightblue", alpha = 0.25) +
    facet_wrap(~Species, #nrow = 1, ncol = 4, 
               labeller = labeller(Species = setNames(unlist(lbls), unique(data1$Species)))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 18), 
          title = element_text(size = 18), 
          strip.text.x = element_text(size = 14)) +
    ylim(0, 100) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +#, 
    scale_size_continuous(range = c(6, 12), breaks = pretty_breaks(4)) +
    scale_fill_viridis(option = "D", breaks = pretty_breaks(4)) +
    guides(fill = guide_legend(), size = guide_legend()) +
    ylab("Confidence Level") +
    xlab("Answer Response (Log Scale)") 
}
