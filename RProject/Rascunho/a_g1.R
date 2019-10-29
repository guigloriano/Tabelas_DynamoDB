require(ggplot2)

g1_1 <- function(dia, d){
  p <- ggplot(data = d, aes(x = hora, group = 1)) + geom_line(aes(y=E_CC, color = "CC")) + geom_line(aes(y=E_CA, color ="CA")) + geom_line(aes(y=rad_sol*10.45, color = "Irradiação x kWp (ins)"), linetype = "dashed") + geom_line(aes(y=rad_sol_avg*10.45, color = "Irradiação x kWp (avg)"), linetype = "dashed") + geom_hline(yintercept = 8200)
  p <- p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), axis.text.y = element_text(size = 14), legend.position="bottom", legend.text = element_text(size = 16)) + scale_y_continuous(breaks = seq(0,15000,500), limits = c(0, 11000)) + scale_color_manual("", values=c("red", "blue", "black", "purple"))
  png(filename = paste("ufv-ufms2/imagens/potencia_radiacao_avg_ins/",gsub("[.]","_", dia),'.png', sep = ""), width = 1000, height = 537, units = 'px')
  print(p)
  dev.off()
}

g1_0 <- function(d){
  dias <- unique(d$data)
  for(dia in dias){
    g1_1(dia, d[d$data == dia,])
  }
}
