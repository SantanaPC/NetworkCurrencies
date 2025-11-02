
pol<-read.table("czvalues.txt", header=T)###aqui precisa inserir os dados de c e z
###mandei um arquivo modelo para organizar os dados junto
summary(pol)

library(ggplot2)###precisa desse pacote
library(ggrepel)
###a unica coisa que precisa mudar ? o corte se nao for usar do olesen, ai muda 0.62 e 2.5 para os cortes do modelo nulo
###da pra mudar tb a organizacao das cores dos pontos...

CZ = read.csv2("1.RawData/valores_cez_chamae.txt", header = TRUE, sep="\t")
CZ$bee<-as.factor(CZ$bee)
str(CZ)
###grafico para aves e plantas

ggplot(data = CZ, aes(x=c_freq, y=z_freq))+ 
  geom_point(shape=21, size = 4, alpha = 0.7) 
  coord_cartesian(xlim = c(-0.0,1.0),ylim = c(-1.0,9.0))
  #labs(color="Birds groups") +
  
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

  
  ggplot(aes(x = prop_DC, y = prop_DR, fill = region_type))+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_arrow_segment(
    aes(x = new_DC, xend = prop_DC,
        y = new_DR, yend = prop_DR),
    color = "grey",
    arrow_head = NULL,
    arrow_mid  = arrow_head_wings(offset = 30,
                                  inset = 60),
    resect_head = 2,
    resect_fins = 2
  )+
  geom_point(
    shape = 21,
    size = 2.5
  )+
  geom_point(
    aes(x = new_DC ,
        y = new_DR),
    alpha = 0.5,
    shape = 21,
    size = 2.5
  )+
  facet_wrap(.~period,axes = "all",
             axis.labels = "margins"
              )+
  scale_fill_manual(
    values = c(
      "Flower Buzz" = "#c85d00",
      "Anther Buzz" = "#ac00e8",
      "Theft" = "#e72881",
      "Robber" = "#1f8b7f",
      )
  )+
  scale_x_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0.05, 0))
  )+
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0.05, 0))
  )+
  theme_classic()+
  theme(
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.background = element_blank(),
    panel.spacing = unit(5, "pt"),
    panel.spacing.x = unit(15, "pt"),
    plot.margin = margin(5,15,5,5,"pt"),
    axis.line = element_line(lineend = "round"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )+
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1),
                  clip = "off")
      )
    )+
    scale_x_continuous(
      labels = scales::label_percent(),
      expand = expansion(mult = c(0.05, 0))
    )+
    scale_y_continuous(
      labels = scales::label_percent(),
      expand = expansion(mult = c(0.05, 0))
    )+
    theme_classic()+
    theme(
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(face = "bold"),
      legend.position = "none",
      plot.background = element_blank(),
      panel.spacing = unit(5, "pt"),
      panel.spacing.x = unit(15, "pt"),
      plot.margin = margin(5,15,5,5,"pt"),
      axis.line = element_line(lineend = "round"),
      axis.text = element_text(color = "black"),
      axis.ticks = element_line(color = "black")
    )+
    coord_cartesian(xlim = c(0,1),
                    ylim = c(0,1),
                    clip = "off")
