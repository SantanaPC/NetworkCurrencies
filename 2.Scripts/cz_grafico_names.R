
pol<-read.table("czvalues.txt", header=T)###aqui precisa inserir os dados de c e z
###mandei um arquivo modelo para organizar os dados junto
summary(pol)

library(ggplot2)###precisa desse pacote
library(ggrepel)
###a unica coisa que precisa mudar ? o corte se nao for usar do olesen, ai muda 0.62 e 2.5 para os cortes do modelo nulo
###da pra mudar tb a organizacao das cores dos pontos...

###grafico para aves e plantas

ggplot(data = pol, aes(x=c, y=z, fill=grupo))+ 
  geom_point(shape=21, size = 4, alpha = 0.7) + 
  labs(color="Birds groups") +
  #coord_cartesian(xlim = c(-0.0,1.0),ylim = c(-1.0,9.0))+
  
  scale_y_continuous(name = expression(paste("Papel dentro do modulo ", italic("(z)"))),
                     #breaks = seq(-1.0, 9.0, 2),
                     limits=c(-1.0,9.0)) +
  #scale_fill_manual(values=c("NA", "deeppink1", "red1", "gray70", "maroon2"))+
  
  scale_x_continuous(name = expression(paste("Conectividade entre modulos ", italic("(c)"))),
                     #breaks = seq(-1.0, 9.0, 2),
                     limits=c(0.0,1.0)) +
  geom_hline(yintercept=2.5, linetype="dotted", color = "black", size=0.5)+
  geom_vline(xintercept =0.62, linetype="dotted", color = "black", size=0.5)+
  theme_bw()+
    
  theme(plot.title = element_text(size = 15, family = "sans", face = "bold"),
        text = element_text(size = 13, family = "sans"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  geom_text_repel(aes(label=ifelse(c>=0.55 | z>=2, gsub("_", " ", especies), "")), nudge_x = 0.05, nudge_y = 0.12)


