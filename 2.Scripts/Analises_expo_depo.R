library(ggplot2)
library(lme4)
library(DHARMa)
library(AICcmodavg)

setwd("C:/Users/loren/OneDrive/Desktop/Doutorado/projeto/planilhas/analises/planilhas_certas")
dir()

efic = read.csv2("dados_obs.csv", header = TRUE) #dados performance
str(efic)

efic$ch_sp<-as.factor(efic$ch_sp)
efic$bee_sp<-as.factor(efic$bee_sp)
efic$id_flow<-as.factor(efic$id_flow)
temp<-as.data.frame(summary(efic$bee_sp))
dim(temp)
temp[temp$`summary(efic$bee_sp)`>=3,]
summary(efic$ch_sp)

efic_sub<-efic[efic$bee_sp!="Eufriesea_nigrohirta"|
    efic$bee_sp!="Exomalopsis_tomentosa"|
    efic$bee_sp!="Oxaea_sp.01"|
    efic$bee_sp!="Ptiloglossa_sp.05",]

##Analisando os modelos##
efic_lm1 = lmer(polen_depo~log(polen_expor)*bee_sp+(1|ch_sp), data = efic)
efic_lm2= lmer(polen_depo~log(polen_expor)+(1|ch_sp), data = efic)
efic_lm3 = lmer(polen_depo~bee_sp+(1|ch_sp), data = efic)
efic_lm4 = lmer(polen_depo~1+(1|ch_sp), data = efic)

efic_lm1_poison<-glmer.nb(polen_depo~log(polen_expor)*bee_sp+(1|ch_sp), data = efic)
summary(efic_lm1_poison)

##Comparando modelos##
anova(efic_lm1, efic_lm2) #manter efic_lm1
anova(efic_lm1, efic_lm3) #manter efic_lm1
anova(efic_lm1, efic_lm4) #manter efic_lm1

summary(efic_lm1)
anova(efic_lm1, efic_lm1_poison)

eficresiduals=plot(simulateResiduals(efic_lm1))
plot(eficresiduals)

##Gráficos##

###Grafico boxplot abelha e deposição

levels(efic$bee_sp)
efic$bee_sp <- reorder(efic$bee_sp, efic$polen_depo, FUN = mean)#reordenei para o gráfico mostrar da menor deposição para a maior

ggplot(efic, aes(x=bee_sp, y=polen_depo, fill=bee_sp)) +
  geom_boxplot(width=0.4,lwd=0.75, colour="black")+
  theme_test() +
  xlab("Bee specie") + ylab("Pollen deposition") +
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size=18), axis.text.y= element_text(size=16), axis.title.y = element_text(size=18),
        legend.text=element_text(size=9), legend.position = "top")+ 
  labs(fill= NULL) 

###Grafico boxplot abelha e exportação (em log)

efic$bee_sp <- reorder(efic$bee_sp, efic$polen_expor, FUN = mean)#reordenei para o gráfico mostrar da menor deposição para a maior

ggplot(efic, aes(x=bee_sp, y=log(polen_expor), fill=bee_sp)) +
  geom_boxplot(width=0.4,lwd=0.75, colour="black")+
  theme_test() +
  xlab("Bee specie") + ylab("Pollen export")+ 
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size=18), axis.text.y= element_text(size=16), axis.title.y = element_text(size=18),
        legend.text=element_text(size=9), legend.position = "top") + 
  labs(fill= NULL)

#media exportação de polen das abelhas#
exp= aggregate(efic$polen_expor, list(efic$bee_sp), FUN=mean)
sd_exp = aggregate(efic$polen_expor, list(efic$bee_sp), FUN=sd)
exp$sd_exp<-sd_exp$x
names(exp)<-c("bee_sp", "mean_exp")

mean_depo = aggregate(efic$polen_depo, list(efic$bee_sp), FUN=mean)
sd_depo = aggregate(efic$polen_depo, list(efic$bee_sp), FUN=sd)
exp$mean_depo<-mean_depo$x
exp$sd_depo<-sd_depo$x

mean_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp), FUN=mean)
sd_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp), FUN=sd)

exp$mean_exp_log<-mean_exp_log$x
exp$sd_exp_log<-sd_exp_log$x

###Grafico da relação entre exportação e deposição

ggplot(exp)+
  geom_errorbar(aes(x=mean_depo, ymin=mean_exp-sd_exp, ymax=mean_exp+sd_exp, color=bee_sp), width=.4, size=0.5, alpha = 0.5)+
  geom_errorbarh(aes(y=mean_exp, xmin=mean_depo-sd_depo, xmax=mean_depo+sd_depo, color=bee_sp), width=.4, size=0.5, alpha = 0.5) + 
  geom_point(aes(x=mean_depo, y=mean_exp, color=bee_sp), size = 2, shape = 16, )+
  theme_bw() +labs(x="Mean pollen deposition", y = "Mean pollen export")+theme(legend.position = "none")+theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1)) +theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))

###Grafico da relação entre exportação (em log) e deposição

ggplot(exp)+
  geom_errorbar(aes(x=mean_depo, ymin=mean_exp_log-sd_exp_log, ymax=mean_exp_log+sd_exp_log, color=bee_sp), width=.4, size=0.5, alpha = 0.5)+geom_errorbarh(aes(y=mean_exp_log, xmin=mean_depo-sd_depo, xmax=mean_depo+sd_depo, color=bee_sp), width=.4, size=0.5, alpha = 0.5) + 
  geom_point(aes(x=mean_depo, y=mean_exp_log, color=bee_sp), size = 2, shape = 16, )+ theme_bw() +labs(x="Mean pollen deposition", y = "Mean pollen export(Log)")+theme(legend.position = "none")+theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1)) +theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))
