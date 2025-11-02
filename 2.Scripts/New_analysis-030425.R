setwd("D:/Doutorado/projeto/planilhas/analises/planilhas_certas")
dir()


library(ggplot2)
library(lme4)
library(DHARMa)
library(AICcmodavg)
library(mvabund)
library(postHoc)


efic = read.csv2("dados_obs.csv", header = TRUE)
str(efic)

efic$ch_sp<-as.factor(efic$ch_sp)
efic$bee_sp<-as.factor(efic$bee_sp)
efic$id_flow<-as.factor(efic$id_flow)
efic$func_group=as.factor(efic$func_group)


#################################################################
#Testanto se existe diferença na deposição e remoção de polen dos
#diferentes grupos funcionais####################################

##################################################################
#DEPOSIÇÃO########################################################
depo_anova=aov(polen_depo~func_group, data=efic)

depo_mod1 = glm.nb(polen_depo~func_group, data=efic)
depo_mod2 = manyglm(polen_depo~func_group, data=efic)


summary(depo_anova) #p<2e-16 ***
summary(depo_mod1)
summary(depo_mod2)

anova(depo_mod2) # p = 0.001 ***
anova(depo_mod1) # p < 2.2e-16 ***

#Investigando quais grupos diferem entre si
pairwise.t.test(efic$polen_depo, efic$func_group, p.adj='bonferroni')
#theft and robber são iguais

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
  geom_boxplot(width=0.4,lwd=0.75, colour="black")+
  theme_test() +
  scale_fill_manual(values=c("#c85d00", "#ac00e8", "#e72881", "#1f8b7f"))+
  xlab("Bee functional groups") + ylab("Pollen removal") +
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size=18), axis.text.y= element_text(size=12), axis.title.y = element_text(size=18),
        legend.position = "none")+
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

install.packages("bipartite")
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
colnames(rede_performace_f)
rownames(rede_performace_f)
colnames(rede_performace_m)
rownames(rede_performace_m)

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

#Especilizacao
#FREQ#
H2_freq = networklevel(rede_freq, index="H2")

#EFI#
H2_f = networklevel(rede_performace_f, index="H2")

H2_m = networklevel(rede_performace_m, index="H2")

#modelos nulos# gerar 1000 redes aleatórias utilizando vaznull

freq.random<- nullmodel(rede_freq, N=1000,method="vaznull")
f.random<- nullmodel(rede_performace_f, N=1000,method="vaznull")
m.random<- nullmodel(rede_performace_m, N=1000,method="vaznull")

H2.freq.random<- unlist(sapply(freq.random, networklevel, index="H2"))
H2.f.random <- unlist(sapply(f.random, networklevel, index="H2"))
H2.m.random <- unlist(sapply(m.random, networklevel, index="H2"))

#No fim, gere o valor de p
p.valuefr <- sum(H2_freq<H2.freq.random)/1000
p.valuefr
p.valuef <- sum(H2_f<H2.f.random)/1000
p.valuef
p.valuem <- sum(H2_m<H2.m.random)/1000
p.valuem

# ou intervalo de confiança
quantile(unlist(H2.freq.random), c(0.025, 0.975))

quantile(unlist(H2.f.random), c(0.025, 0.975))

quantile(unlist(H2.m.random), c(0.025, 0.975))

#Aninhamento
#FREQ#
wnodf_freq = nested(rede_freq, method = "weighted NODF")

#EFI#
wnodf_f = nested(rede_performace_f, method = "weighted NODF")

wnodf_m = nested(rede_performace_m, method = "weighted NODF")

#modelos nulos# 
wnodf.freq.random<- unlist(sapply(freq.random, networklevel, index="weighted NODF"))
wnodf.f.random <- unlist(sapply(f.random, networklevel, index="weighted NODF"))
wnodf.m.random <- unlist(sapply(m.random, networklevel, index="weighted NODF"))

#intervalos de confiança#
quantile(unlist(wnodf.freq.random), c(0.025, 0.975))
quantile(unlist(wnodf.f.random), c(0.025, 0.975))
quantile(unlist(wnodf.m.random), c(0.025, 0.975))

#No fim, gere o valor de p
p.valuefr <- sum(wnodf_freq<wnodf.freq.random)/1000
p.valuefr
p.valuef <- sum(wnodf_f<wnodf.f.random)/1000
p.valuef
p.valuem <- sum(wnodf_f<wnodf.m.random)/1000
p.valuem


#--------------------------------

# Modularidade

#FREQ#
mod.freq<-computeModules(rede_freq, method="DormannnStrauss",steps = 10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede      
mod.freq<-metaComputeModules(rede_freq, N=5, method="Beckett",steps = 10E7)
mod.freq=mod.freq@likelihood #valor do indice de modularidade
plotModuleWeb(mod.freq)

#EFI#
mod.efi<-computeModules(rede_performace_f, method="Beckett", steps=10E7) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.efi<-metaComputeModules(rede_performace_f, N=5, method="Beckett",steps = 10E7)
mod.efi=mod.efi@likelihood #valor do indice de modularidade
plotModuleWeb(mod.efi)

mod.efiM<-computeModules(rede_performace_m, method="Beckett", steps=10E8) #cria modulo aleatorios dentro dos seus dados e depois te da um resultado da modularidade final e real da sua rede
mod.efiM<-metaComputeModules(rede_performace_m, N=5, method="Beckett",steps = 10E7)
mod.efiM=mod.efiM@likelihood #valor do indice de modularidade
plotModuleWeb(mod.efiM)

#modelos nulos# 
nulls_freq <- nullmodel(rede_freq, N=1000, method="vaznull")
nulls_f <- nullmodel(rede_performace_f, N=1000, method="vaznull")
nulls_m <- nullmodel(rede_performace_m, N=1000, method="vaznull")

modules.nulls_freq <- sapply(nulls_freq, computeModules, step=10E7)
like.nulls_freq <- sapply(modules.nulls_freq, function(x) x@likelihood)

modules.nulls_f <- sapply(nulls_f, computeModules, step=10E7)
like.nulls_f <- sapply(modules.nulls_f, function(x) x@likelihood)

modules.nulls_m <- sapply(nulls_m, computeModules, step=10E7)
like.nulls_m <- sapply(modules.nulls_m, function(x) x@likelihood)

quantile(unlist(like.nulls_freq), c(0.025, 0.975))
quantile(unlist(like.nulls_f), c(0.025, 0.975))
quantile(unlist(like.nulls_m), c(0.025, 0.975))

modelo_H2 <- aov(H2 ~ network, data = metrics)
anova(modelo_H2)

tukey_H2 <- TukeyHSD(modelo_H2)
print(tukey_H2)


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
?specieslevel

#dados com vaores de força e d das espécies#
z_plant=read.csv2("z_score_plant.csv", header = TRUE, row.names = 1)
z_bee=read.csv2("z_score_bee.csv", header = TRUE, row.names = 1)

############################################################
#testando se existe diferença entre força e especilização das esp. em cada rede#
#test t#
##BEE##

t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_m, paired = TRUE)
t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_f, paired = TRUE)
t.test(z_bee$z_strength_perf_f, z_bee$z_strength_perf_m, paired = TRUE)
##TODOS P SIGNIFICATIVO##

t.test(z_bee$z_d_freq, z_bee$z_d_perf_f, paired = TRUE)
t.test(z_bee$z_d_freq, z_bee$z_d_perf_m, paired = TRUE)
t.test(z_bee$z_d_perf_f, z_bee$z_d_perf_m, paired = TRUE)

#testando se existe correlação entre força e especilização das esp. em cada rede#
##BEE##
indices=read.table("indices_especies.txt", header=TRUE)

cor.test(indices$z_strength_freq, indices$z_strength_perf_f)
cor.test(indices$z_strength_freq, indices$z_strength_perf_m)
cor.test(indices$z_strength_perf_f, indices$z_strength_perf_m)

cor.test(indices$z_d_freq, indices$z_d_perf_f)
cor.test(indices$z_d_freq, indices$z_d_perf_m)
cor.test(indices$z_d_perf_f, indices$z_d_perf_m)

cor.test(indices$c_freq, indices$c_perf_fem)
cor.test(indices$c_freq, indices$c_perf_masc)
cor.test(indices$c_perf_fem, indices$c_perf_masc)

cor.test(indices$z_freq, indices$z_perf_fem)
cor.test(indices$z_freq, indices$z_perf_masc)
cor.test(indices$z_perf_fem, indices$z_perf_masc)

##PLANT##
t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_m)
t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_f) #P SIG#
t.test(z_plant$z_strength_perf_f, z_plant$z_strength_perf_m) #P SIG#

t.test(z_plant$z_d_freq, z_plant$z_d_perf_m)
t.test(z_plant$z_d_freq, z_plant$z_d_perf_f)
t.test(z_plant$z_d_perf_f, z_plant$z_d_perf_m)


##Calculando valores de c $ z dentro descrever o papel de cada espécie na modularidade da rede##

czvalues(mod.freq, level="higher")
czvalues(mod.freq, level="lower")


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