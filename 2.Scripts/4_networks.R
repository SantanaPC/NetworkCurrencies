#########################################################################
##Analises de rede#######################################################

library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)
library(igraph)
library("RColorBrewer")


rede_freq = read.csv2("1.RawData/rede_frequencia.csv", sep = "," , header = TRUE, row.names = 1) #medida de frequencia = intera??o total da especie de abelha na especie de planta, aqui s? tem as especies de plantas/abelhas que foram estimadas quanto a deposi??o de polen 

rede_deposicao = read.csv2("1.RawData/rede_deposicao3.csv", header = TRUE, row.names = 1) #medida de deposi??o = media da quantidade de polen depositado no estigma
rede_performace_f = read.csv2("1.RawData/rede_eficacia_f.csv", header = TRUE, row.names = 1) #medida de eficacia = quantidade de polen depositado x frequencia da intera??o

rede_remocao = read.table("1.RawData/rede_remocao7.txt", header = TRUE) #medida de remo??ao = media da quantidade de polen removido das anteras
rede_performace_m = read.csv2("1.RawData/performace_masculina.csv", header = TRUE, row.names = 1)#medida de eficacia = quantidade de polen removido x frequencia da intera??o

# Explorando a rede
#rownames(diamantina)
colnames(rede_freq)
rownames(rede_freq)
colnames(rede_deposicao)
rownames(rede_deposicao)
colnames(rede_eficacia_f)
rownames(rede_eficacia_f)

#Visualizando
library(pheatmap)
library(RColorBrewer)

col <- colorRampPalette(c("white", brewer.pal(11, "RdYlBu")[1:9]))(256)
#inverte o gradiente (vermelho = alto)

pheatmap(
  as.matrix(rede_freq),
  scale = "none",
  col = col,
  cluster_rows = TRUE,           # agrupa por similaridade (ou FALSE se quiser fixo)
  cluster_cols = TRUE,
  border_color = NA,             # remove bordas entre células
  cellwidth = 12,                # ajusta largura das células
  cellheight = 12,               # ajusta altura das células
  fontsize = 10,                 # texto mais limpo
  fontsize_row = 9,              
  fontsize_col = 9,
  angle_col = 45,                # rotaciona nomes das colunas
  main = "Interaction frequency heatmap",
  legend = TRUE
)
#REDE FREQ PARCIAL#
plotweb(rede_freq)
plotweb(rede_freq, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede

#REDE DEPOSI??O#
plotweb(rede_deposicao)
plotweb(rede_deposicao, text.rot=90, col.low="#999999", col.high="#000000", bor.col.high= "#999999", bor.col.low="#000000", bor.col.interaction="#333333", col.interaction="#666666") #muda as cores da rede


#Laranja "#FF6A00"
#Roxo  "#63077D"
#Verde  "#1C796F"

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
