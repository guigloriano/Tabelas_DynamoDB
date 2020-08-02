
gf_mar <- ggplot(data = conjmar2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*67:88) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Março/20") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_mar)


p = ggplot() + theme_bw() + 
  geom_line(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 0.5) +
  geom_line(aes(x = sm, y = Dio_list, group=2), color = "red", size = 0.5) +
  geom_point(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 1) +
  geom_point(aes(x = sm, y = Dio_list, group=2), color = "red", size = 1) +
  xlab('Dias') +
  ylab('Potência') + 
  scale_x_continuous(breaks=seq(0, 100, 5))  +
  scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))

print(p)
