setwd("/Users/pamela_santana/My Drive/(7) Colab/Lorena")

library(ggplot2)
library(lme4)
library(DHARMa)
library(AICcmodavg)
library(mvabund)
library(postHoc)
library(emmeans)
library(RVAideMemoire)
library(cowplot)

efic = read.csv2("dados_obs_grupos.csv", header = TRUE)
str(efic)

efic$id_exp<-as.factor(efic$id_exp)
efic$id_flow<-as.factor(efic$id_flow)
efic$bee_sp<-as.factor(efic$bee_sp)
efic$ch_sp<-as.factor(efic$ch_sp)
efic$func_group<-as.factor(efic$func_group)

summary(efic)

hist(efic$polen_depo)#cara de zero-inflated

#################################################################
#Testanto se existe diferença na deposição e remoção de polen dos
#diferentes grupos funcionais####################################

##################################################################
#DEPOSIÇÃO########################################################
depo_mod1 = lmer(polen_depo~func_group+(1|ch_sp), data=efic)
depo_mod2 = manyglm(polen_depo~func_group, data=efic)
depo_mod3 = glmer(polen_depo~func_group+(1|ch_sp), family = poisson, data=efic)
depo_mod4 = glmer.nb(polen_depo~func_group+(1|ch_sp), data=efic)
depo_mod5 = glm.nb(polen_depo~func_group*ch_sp, data=efic)
depo_mod6 = glm.nb(polen_depo~func_group, data=efic)
depo_mod7 = lmer(polen_depo~1+(1|ch_sp), data=efic)

bbmle::AICtab(depo_mod1,depo_mod2,depo_mod3, depo_mod4, depo_mod5, depo_mod6,depo_mod7)

plot(simulateResiduals(depo_mod5))
MuMIn::AICc(depo_mod4,depo_mod5)
summary(depo_mod5)
anova(depo_mod5)

#Investigando quais grupos diferem entre si
emS <- emmeans::emmeans(depo_mod4, pairwise ~ func_group,
               regrid="log",
               type="response")
emS<-emS$emmeans
emS$contrasts
emS<-as.data.frame(emS)

plot(emS)+theme_bw() +labs(y="", x = "Response")

ggplot()+ geom_point(data=emS, mapping=aes(x=emS[,1], y=emS[,2]), size=2.5, shape=19, fill=c("black"), color="black")+
  geom_errorbar(data=emS, mapping=aes(x=emS[,1], ymin=asymp.LCL, ymax=asymp.UCL), width=.2, size=0.5, color="black")
 
  scale_y_continuous(limits=c(-0.03,1.10), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))+theme_cowplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+labs(x="", y = "Pollen on stigma probability")+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+theme(text = element_text(size = 12)) + theme(axis.text.x=element_text(hjust = 1)) +theme(axis.title = element_text(size = 16))+theme(axis.text.x = element_text(size = 12)) + scale_x_discrete(labels=c("First male Control", "First male Flower", "Second male Flower"))+ geom_text(data=emS, label = c("a", "b", "a"), aes(y = c(1.10,1.10,1.10), x = emS[,1]), size = 6)

####################################################################
#EXPORTAÇÃO#########################################################
expor_anova=aov(polen_expor~func_group, data=efic)

expor_mod1 = glm.nb(log(polen_expor)~func_group, data=efic)
expor_mod2 = manyglm(polen_expor~func_group, data=efic)

summary(expor_anova) #p<2e-16 ***
summary(expor_mod1)
summary(expor_mod2)

anova(expor_mod1) #p<2.2e-16 ***
anova(expor_mod2) #p = 0.001 ***

pairwise.t.test(efic$polen_depo, efic$func_group, p.adj='bonferroni') 
#todos são diferentes

############################################
#Plotando os gráficos Expo~Functional_groups
pdf('Pollen removal.pdf', width=10.33071, height=6.33071)
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
levels(efic$func_group)
efic$func_group <- reorder(efic$func_group, efic$polen_expor, FUN = mean)#reordenei para o gráfico mostrar da menor deposição para a maior
ggplot(efic, aes(x=func_group, y=polen_expor, fill=func_group)) +
  ylim(0,30000) +
  geom_boxplot(width=0.4,lwd=0.75,colour="black")+
  facet_wrap(~ch_sp, nrow = 2, ncol = 5)+
  theme_bw() +
  scale_fill_manual(values=c("#c85d00", "#ac00e8", "#e72881", "#1f8b7f"))+
  xlab("Bee functional groups") + ylab("Pollen removal") +
  theme(axis.title.x = element_text(size=18),axis.text.x = element_text(angle = 30), axis.text.y= element_text(size=12), axis.title.y = element_text(size=18),legend.position = "none")+
  labs(fill= NULL)
dev.off()

############################################
#Plotando os gráficos Depo~Functional_groups
pdf('Pollen deposition.pdf', width=10.33071, height=6.33071)
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
levels(efic$func_group)
efic$func_group <- reorder(efic$func_group, efic$polen_depo, FUN = mean)#reordenei para o gráfico mostrar da menor deposição para a maior
ggplot(efic, aes(x=func_group, y=polen_depo, fill=func_group)) +
  ylim(0,100) +
  geom_boxplot(width=0.4,lwd=0.75, colour="black")+
  theme_test() +
  scale_fill_manual(values=c("#1f8b7f", "#c85d00", "#ac00e8", "#e72881"))+
  xlab("Bee functional groups") + ylab("Pollen deposition") +
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size=18), axis.text.y= element_text(size=12), axis.title.y = element_text(size=18),
        legend.position = "none")
dev.off()


#########################################################################
#Performance das abelhas na exportação e deposição de polen na comunidade
#de chamaecristas########################################################


efic_lm1 = lmer(polen_depo~log(polen_expor)*bee_sp+func_group+(1|ch_sp), data = efic)
efic_lm2 = lmer(polen_depo~log(polen_expor)*bee_sp+(1|ch_sp), data = efic)
efic_lm3 = lmer(polen_depo~log(polen_expor)*func_group+(1|ch_sp), data = efic)
efic_lm4 = lmer(polen_depo~log(polen_expor)+(1|ch_sp), data = efic)
efic_lm_nulo = lmer(polen_depo~1+(1|ch_sp), data = efic)
efic_lm8_nb = glmer.nb(polen_depo~log(polen_expor)*bee_sp+func_group+(1|ch_sp), data = efic)

##Comparando modelos##
anova(efic_lm1, efic_lm2) #manter efic_lm1
anova(efic_lm1, efic_lm3) #manter efic_lm1
anova(efic_lm1, efic_lm4) #manter efic_lm1
anova (efic_lm1, efic_lm_nulo) #manter efic_lm1 
anova (efic_lm1, efic_lm8_nb) #trocar para binomial negativo  

summary(efic_lm8_nb)
anova(efic_lm8_nb)
efic_lm8_nb

eficresiduals=plot(simulateResiduals(efic_lm8_nb))
plot(efic_lm8_nb)

#####################################################
#Criando novos dados para plotar gráfico de eficácia#
exp= aggregate(efic$polen_expor, list(efic$bee_sp, efic$func_group), FUN=mean)
sd_exp = aggregate(efic$polen_expor, list(efic$bee_sp, efic$func_group), FUN=sd)
exp$sd_exp<-sd_exp$x
names(exp)<-c("bee_sp", "func_group", "mean_exp")
mean_depo = aggregate(efic$polen_depo, list(efic$bee_sp, efic$func_group), FUN=mean)
sd_depo = aggregate(efic$polen_depo, list(efic$bee_sp, efic$func_group), FUN=sd)
exp$mean_depo<-mean_depo$x
exp$sd_depo<-sd_depo$x
mean_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp, efic$func_group), FUN=mean)
sd_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp, efic$func_group), FUN=sd)
exp$mean_exp_log<-mean_exp_log$x
exp$sd_exp_log<-sd_exp_log$x


pdf('DepositionvesusExportation_bee.pdf', width=10.33071, height=6.33071)
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
ggplot(exp)+
  geom_errorbar(aes(x=mean_depo, ymin=mean_exp_log-sd_exp_log, ymax=mean_exp_log+sd_exp_log, color=func_group), width=.4, size=0.5, alpha = 0.5)+
  geom_errorbarh(aes(y=mean_exp_log, xmin=mean_depo-sd_depo, xmax=mean_depo+sd_depo, color=func_group), width=.4, size=0.5, alpha = 0.5) + 
  geom_point(aes(x=mean_depo, y=mean_exp_log, color=func_group), size = 2, shape = 16, )+ 
  theme_bw() +labs(x="Mean pollen deposition", y = "Mean pollen exportarion (log)")+
  theme(legend.position = "none")+theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1)) +
  theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))
dev.off()

pdf('DepositionvesusExportation_groups.pdf', width=10.33071, height=6.33071)
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
ggplot(exp)+
  geom_errorbar(aes(x=mean_depo, ymin=mean_exp_log-sd_exp_log, ymax=mean_exp_log+sd_exp_log, color=func_group), width=.4, size=0.5, alpha = 0.5)+
  geom_errorbarh(aes(y=mean_exp_log, xmin=mean_depo-sd_depo, xmax=mean_depo+sd_depo, color=func_group), size=0.5, alpha = 0.5) + 
  geom_point(aes(x=mean_depo, y=mean_exp_log, color=func_group), size = 2, shape = 16, )+ 
  xlim(-1,5)+ ylim(0,10.5)+
  theme_bw() +
  labs(x="Mean pollen deposition", y = "Mean pollen exportarion (log)")+
  theme(legend.position= "none")+theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1)) +
  theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))
dev.off()

pdf('DepositionvesusExportation_groups_reta.pdf', width=10.33071, height=6.33071)
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
ggplot(exp)+
  geom_errorbar(aes(x=mean_depo, ymin=mean_exp_log-sd_exp_log, ymax=mean_exp_log+sd_exp_log, color=func_group), width=.4, size=0.5, alpha = 0.5)+
  geom_errorbarh(aes(y=mean_exp_log, xmin=mean_depo-sd_depo, xmax=mean_depo+sd_depo, color=func_group), size=0.5, alpha = 0.5) + 
  geom_point(aes(x=mean_depo, y=mean_exp_log, color=func_group), size = 2, shape = 16, )+ 
  xlim(-1,5)+ ylim(0,10.5)+
  geom_smooth(aes(x=mean_depo, y=mean_exp_log))+ #tentativa de plotar a reta
  theme_bw() +
  labs(x="Mean pollen deposition", y = "Mean pollen exportarion (log)")+
  theme(legend.position= "none")+theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1)) +
  theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))
dev.off()

#########################################################################
##Analises de rede#######################################################

library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)
library(igraph)


rede_freq = read.csv2("rede_frequencia.csv", header = TRUE, row.names = 1) #medida de frequencia = intera??o total da especie de abelha na especie de planta, aqui s? tem as especies de plantas/abelhas que foram estimadas quanto a deposi??o de polen 

rede_deposicao = read.csv2("rede_deposicao3.csv", header = TRUE, row.names = 1) #medida de deposi??o = media da quantidade de polen depositado no estigma
rede_performace_f = read.csv2("rede_eficacia_f.csv", header = TRUE, row.names = 1) #medida de eficacia = quantidade de polen depositado x frequencia da intera??o

rede_remocao = read.table("rede_remocao7.txt", header = TRUE) #medida de remo??ao = media da quantidade de polen removido das anteras
rede_performace_m = read.csv2("performace_masculina.csv", header = TRUE, row.names = 1)#medida de eficacia = quantidade de polen removido x frequencia da intera??o

# Explorando a rede
rownames(diamantina)
colnames(rede_freq)
rownames(rede_freq)
colnames(rede_deposicao)
rownames(rede_deposicao)
colnames(rede_eficacia_f)
rownames(rede_eficacia_f)

#Visualizando
#REDE FREQ PARCIAL#
plotweb(rede_freq)
plotweb(rede_freq, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede

#REDE DEPOSI??O#
plotweb(rede_deposicao)
plotweb(rede_deposicao, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede

#REDE REMO??O#
plotweb(rede_remocao)
plotweb(rede_remocao, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede
plotweb(rede_remocao2, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede

#REDE EFICACIA - DEPOSI??O#
plotweb(rede_eficacia_f)
plotweb(rede_eficacia_f, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede

#REDE EFICACIA - REMO??OO#
plotweb(rede_eficacia_m)
plotweb(rede_eficacia_m, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede


###########################

#Conectancia
#Calculando diferentes metricas

#FREQ#
networklevel(rede_freq, index="connectance")

#DEPOSICAO#
networklevel(rede_deposicao, index="connectance")

#REMO??O#
networklevel(rede_remocao, index="connectance")

#EFICACIA F#
networklevel(rede_performace_f, index="connectance")

#EFICACIA M#
networklevel(rede_performace_m, index="connectance")

#Especiliza??o
#FREQ#
networklevel(rede_freq, index="H2")

#DEP#
networklevel(rede_deposicao, index="H2")

#REM#
networklevel(rede_remocao, index="H2")

#EFI#
networklevel(rede_performace_f, index="H2")

networklevel(rede_performace_m, index="H2")

#Aninhamento
#FREQ#
nested(rede_freq, method = "NODF2")

#DEP#
nested(rede_deposicao, method = "NODF2")

#REM#
nested(rede_remocao, method = "NODF2")

#EFI#
nested(rede_performace_f, method = "NODF2")

nested(rede_performace_m, method = "NODF2")


#--------------------------------

# Modularidade

#FREQ#
mod.freq<-computeModules(rede_freq, method="DormannnStrauss",steps = 10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede      
mod.freq<-metaComputeModules(rede_freq, N=5, method="Beckett",steps = 10E7)
mod.freq@likelihood #valor do indice de modularidade
plotModuleWeb(mod.freq)

#DEP#
mod.dep<-computeModules(rede_deposicao, method="Beckett", steps=10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.dep<-metaComputeModules(rede_deposicao, N=5, method="Beckett",steps = 10E7)
mod.dep@likelihood #valor do indice de modularidade
plotModuleWeb(mod.dep)

#REM#
mod.rem<-computeModules(rede_remocao, method="Beckett", steps=10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.rem<-metaComputeModules(rede_remocao, N=5, method="Beckett",steps = 10E7)
mod.rem@likelihood
plotModuleWeb(mod.rem)

#EFI#
mod.efi<-computeModules(rede_performace_f, method="Beckett", steps=10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.efi<-metaComputeModules(rede_performace_f, N=5, method="Beckett",steps = 10E7)
mod.efi@likelihood #valor do indice de modularidade
plotModuleWeb(mod.efi)

mod.efiM<-computeModules(rede_performace_m, method="Beckett", steps=10E8) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.efiM<-metaComputeModules(rede_performace_m, N=5, method="Beckett",steps = 10E7)
mod.efiM@likelihood #valor do indice de modularidade
plotModuleWeb(mod.efiM)

#generalidade de cada um dos níveis das rede#
networklevel(rede_freq, index="generality")
networklevel(rede_deposicao, index="generality")
networklevel(rede_performace_f, index="generality")
networklevel(rede_performace_m, index="generality")
networklevel(rede_remocao, index="generality")

#força e especialização de cada espécie de cada rede#
specieslevel(rede_freq, index=c("d","species strength"))
specieslevel(rede_deposicao, index=c("d","species strength"))
specieslevel(rede_performace_f, index=c("d","species strength"))
specieslevel(rede_remocao, index=c("d","species strength"))
specieslevel(rede_performace_m, index=c("d", "species strength"))

#dados com vaores de força e d das espécies#
z_plant=read.csv2("z_score_plant.csv", header = TRUE, row.names = 1)
z_bee=read.csv2("z_score_bee.csv", header = TRUE, row.names = 1)

############################################################
#testando se existe diferença entre força e especilização das esp. em cada rede#
#TESTE T#
##BEE##

t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_f)
t.test(z_bee$z_d_freq, z_bee$z_d_perf_f)

t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_m)
t.test(z_bee$z_d_freq, z_bee$z_d_perf_m)

t.test(z_bee$z_strength_freq, z_bee$z_strength_remo)
t.test(z_bee$z_d_freq, z_bee$z_d_remo)

t.test(z_bee$z_strength_freq, z_bee$z_strength_depo)
t.test(z_bee$z_d_freq, z_bee$z_d_depo)

t.test(z_bee$z_strength_depo, z_bee$z_strength_perf_f)
t.test(z_bee$z_d_depo, z_bee$z_d_perf_f)

t.test(z_bee$z_strength_depo, z_bee$z_strength_remo)
t.test(z_bee$z_d_depo, z_bee$z_d_remo)

t.test(z_bee$z_strength_depo, z_bee$z_strength_freq)
t.test(z_bee$z_d_depo, z_bee$z_d_freq)

t.test(z_bee$z_strength_depo, z_bee$z_strength_perf_m)
t.test(z_bee$z_d_depo, z_bee$z_d_perf_m)

t.test(z_bee$z_strength_perf_f, z_bee$z_strength_freq)
t.test(z_bee$z_d_perf_f, z_bee$z_d_freq)

t.test(z_bee$z_strength_perf_f, z_bee$z_strength_depo)
t.test(z_bee$z_d_perf_f, z_bee$z_d_depo)

t.test(z_bee$z_strength_perf_f, z_bee$z_strength_remo)
t.test(z_bee$z_d_perf_f, z_bee$z_d_remo)

t.test(z_bee$z_strength_perf_f, z_bee$z_strength_perf_m)
t.test(z_bee$z_d_perf_f, z_bee$z_d_perf_m)

t.test(z_bee$z_strength_remo, z_bee$z_strength_freq)
t.test(z_bee$z_d_remo, z_bee$z_d_freq)

t.test(z_bee$z_strength_remo, z_bee$z_strength_depo)
t.test(z_bee$z_d_remo, z_bee$z_d_depo)

t.test(z_bee$z_strength_remo, z_bee$z_strength_perf_f)
t.test(z_bee$z_d_remo, z_bee$z_d_perf_f)

t.test(z_bee$z_strength_remo, z_bee$z_strength_perf_m)
t.test(z_bee$z_d_remo, z_bee$z_d_perf_m)

t.test(z_bee$z_strength_perf_m, z_bee$z_strength_freq)
t.test(z_bee$z_d_perf_m, z_bee$z_d_freq)

t.test(z_bee$z_strength_perf_m, z_bee$z_strength_depo)
t.test(z_bee$z_d_perf_m, z_bee$z_d_depo)

t.test(z_bee$z_strength_perf_m, z_bee$z_strength_perf_f)
t.test(z_bee$z_d_perf_m, z_bee$z_d_perf_f)

t.test(z_bee$z_strength_perf_m, z_bee$z_strength_remo)
t.test(z_bee$z_d_perf_m, z_bee$z_d_remo)

##PLANT##
t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_f)
t.test(z_plant$z_d_freq, z_plant$z_d_perf_f)

t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_m)
t.test(z_plant$z_d_freq, z_plant$z_d_perf_m)

t.test(z_plant$z_strength_freq, z_plant$z_strength_remo)
t.test(z_plant$z_d_freq, z_plant$z_d_remo)

t.test(z_plant$z_strength_freq, z_plant$z_strength_depo)
t.test(z_plant$z_d_freq, z_plant$z_d_depo)

t.test(z_plant$z_strength_depo, z_plant$z_strength_perf_f)
t.test(z_plant$z_d_depo, z_plant$z_d_perf_f)

t.test(z_plant$z_strength_depo, z_plant$z_strength_remo)
t.test(z_plant$z_d_depo, z_plant$z_d_remo)

t.test(z_plant$z_strength_depo, z_plant$z_strength_freq)
t.test(z_plant$z_d_depo, z_plant$z_d_freq)

t.test(z_plant$z_strength_depo, z_plant$z_strength_perf_m)
t.test(z_plant$z_d_depo, z_plant$z_d_perf_m)

t.test(z_plant$z_strength_perf_f, z_plant$z_strength_freq)
t.test(z_plant$z_d_perf_f, z_plant$z_d_freq)

t.test(z_plant$z_strength_perf_f, z_plant$z_strength_depo)
t.test(z_plant$z_d_perf_f, z_plant$z_d_depo)

t.test(z_plant$z_strength_perf_f, z_plant$z_strength_remo)
t.test(z_plant$z_d_perf_f, z_plant$z_d_remo)

t.test(z_plant$z_strength_perf_f, z_plant$z_strength_perf_m)
t.test(z_plant$z_d_perf_f, z_plant$z_d_perf_m)

t.test(z_plant$z_strength_remo, z_plant$z_strength_freq)
t.test(z_plant$z_d_remo, z_plant$z_d_freq)

t.test(z_plant$z_strength_remo, z_plant$z_strength_depo)
t.test(z_plant$z_d_remo, z_plant$z_d_depo)

t.test(z_plant$z_strength_remo, z_plant$z_strength_perf_f)
t.test(z_plant$z_d_remo, z_plant$z_d_perf_f)

t.test(z_plant$z_strength_remo, z_plant$z_strength_perf_m)
t.test(z_plant$z_d_remo, z_plant$z_d_perf_m)

t.test(z_plant$z_strength_perf_m, z_plant$z_strength_freq)
t.test(z_plant$z_d_perf_m, z_plant$z_d_freq)

t.test(z_plant$z_strength_perf_m, z_plant$z_strength_depo)
t.test(z_plant$z_d_perf_m, z_plant$z_d_depo)

t.test(z_plant$z_strength_perf_m, z_plant$z_strength_perf_f)
t.test(z_plant$z_d_perf_m, z_plant$z_d_perf_f)

t.test(z_plant$z_strength_perf_m, z_plant$z_strength_remo)
t.test(z_plant$z_d_perf_m, z_plant$z_d_remo)

##Calculando valores de c $ z dentro descrever o papel de cada espécie na modularidade da rede##

czvalues(mod.freq, level="higher")
czvalues(mod.freq, level="lower")

##ERRO##???????????
#Error in dim(slot(moduleWebObject, "originalWeb")) == 0 || dim(slot(moduleWebObject,  : 
'length = 2' in coercion to 'logical(1)'#

#salvando as imagens de redes#
pdf('rede_freq.pdf', width=12.33071, height=10.33071)
plotweb(rede_freq, method="normal", text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#000000", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#CCCCCC") #muda as cores da rede
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
dev.off()

pdf('rede_dep.pdf',width=12.33071, height=10.33071)
plotweb(rede_deposicao, method="normal", text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#000000", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#CCCCCC") #muda as cores da rede
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
dev.off()

pdf('rede_efi.pdf', width=12.33071, height=10.33071)
plotweb(rede_eficacia_f, method="normal", text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#000000", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#CCCCCC") #muda as cores da rede
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
dev.off()

pdf('rede_remocao.pdf', width=12.33071, height=10.33071)
plotweb(rede_remocao, method="normal", text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#000000", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#CCCCCC") #muda as cores da rede
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
dev.off()

pdf('rede_efiM.pdf', width=12.33071, height=10.33071)
plotweb(rede_performace_m, method="normal", text.rot=90, col.low="#fcce74", col.high="#000000", bor.col.high= "#000000", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#CCCCCC") #muda as cores da rede
par(mar=c(5,4,2,1), mgp=c(2.5,1,0))
dev.off()
