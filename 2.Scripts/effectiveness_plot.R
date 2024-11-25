
efic = read.csv2("1.RawData/dados_obs_grupos.csv", header = TRUE) #dados 
q1<-efic$polen_depo
q2<-log(efic$polen_expor)    
label<-efic$func_group
myxlab="Pollen deposition"
myylab="Pollen exportation (log)"

### General elements ###
    
    d <- data.frame(x = q1, y = q2, label = label)  
    
    effplot <- 
        ggplot(data = d, aes(x, y)) + 
        theme_bw() +
        labs(x = myxlab, y = myylab) +
        theme(legend.title = element_blank())
    
    
    
    ##### Plotting contour lines ####
    q1.error <- sd(q1)
    q2.error <- sd(q2)
        
    ## Define lower and upper bounds ##
        x.lower <- ifelse(is.null(q1.error), 0, min(0, q1 - q1.error, na.rm = TRUE))
        x.upper <- ifelse(is.null(q1.error), max(q1), max(q1, q1 + q1.error, na.rm = TRUE))
        y.lower <- ifelse(is.null(q2.error), 0, min(0, q2 - q2.error, na.rm = TRUE))
        y.upper <- ifelse(is.null(q2.error), max(q2), max(q2, q2 + q2.error, na.rm = TRUE))
        
        ## Calculate values ##
        df <- expand.grid(x = seq(x.lower - 0.05*x.lower, x.upper + 0.05*x.upper, length.out = 500),
                          y = seq(y.lower - 0.05*y.lower, y.upper + 0.05*y.upper, length.out = 500))
        df$z <- df$x * df$y
        
        ## Define line breaks ##
        ## other alternatives, see ?classIntervals
lines.breaks = "quantile"
nlines = 5

            lbreaks <- classInt::classIntervals(df$z, n = nlines + 1, 
                                                style = lines.breaks)$brks
 
        #### Preparing curve labels ####
            
        brk <- lbreaks[-c(1, length(lbreaks))]
        xlabel <- rep(max(df$x) + 0.05*max(df$x), times = length(brk))
        ylabel <- brk/xlabel
        
        # trying pretty labels:
        lines.labels <- ifelse(brk > 10, as.character(round(brk)), 
                                   as.character(signif(brk, digits = 2)))
        xy.labels <- data.frame(x = xlabel, y = ylabel, label = lines.labels)
        xy.labels <- subset(xy.labels, y > y.lower)
        
        lines.color = "grey50"      
        
        ### Add lines to plot ###
        effplot <- effplot +
            geom_contour(aes(x, y, z = z), data = df, colour = lines.color, 
                         breaks = lbreaks, linewidth  = 0.3) + 
            geom_text(aes(x, y, label = label), data = xy.labels)
        
        

        ### Error bars ###
    
         d <- data.frame(d, x.error = q1.error)
        #d$x.error[is.na(d$x.error)] <- 0
        effplot <- effplot + 
            geom_errorbarh(aes(xmin = x - x.error, xmax = x + x.error), data = d)
 
        d <- data.frame(d, y.error = q2.error)
        #d$y.error[is.na(d$y.error)] <- 0
        effplot <- effplot + 
            geom_errorbar(aes(ymin = y - y.error, ymax = y + y.error), data = d)
    
    ### Draw points ###
    pts.size = 2
    pts.shape = NULL
    pts.color = NULL
   
    exp = aggregate(efic$polen_depo, list(efic$bee_sp), FUN=mean)
    sd_depo = aggregate(efic$polen_depo, list(efic$bee_sp), FUN=sd)
    exp$sd_depo<-sd_depo$x
    colnames(exp)<-c("bee_sp", "mean_depo", "sd_depo")
    
    mean_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp), FUN=mean)
    sd_exp_log<-aggregate(log(efic$polen_expor), list(efic$bee_sp), FUN=sd)
    
    exp$mean_exp_log<-mean_exp_log$x
    exp$sd_exp_log<-sd_exp_log$x
    #unique(efic$bee_sp)
    
    library(dplyr)
    efic_unique <- efic |>
        distinct(bee_sp, .keep_all = TRUE)
    
    comb <- left_join(exp, efic_unique, by = "bee_sp")
    exp$func_group<-comb$func_group
    exp$func_group<-as.factor(exp$func_group)
    levels(exp$func_group)<-c("flow_buzz", "ant_buzz", "robber", "theft")
    
    if (is.null(pts.color) & is.null(pts.shape)) {
    length(exp$mean_exp_log)
      
        effplot + 
        geom_errorbar(aes(x=mean_depo, y=mean_exp_log, ymin=mean_exp_log-sd_exp_log, ymax=mean_exp_log+sd_exp_log), color="darkgrey", width=.3, linewidth=0.5, data=exp)+
        geom_errorbarh(aes( y=mean_exp_log, x=mean_depo, xmin = mean_depo - sd_depo, xmax = mean_depo + sd_depo), height = 0.1, color="darkgrey",data=exp)+
        scale_colour_manual(values=c("#c85d00", "#ac00e8", "#e72881", "#1f8b7f")) +
        geom_point(aes(x=mean_depo, y=mean_exp_log, fill=exp$func_group, colour=exp$func_group),size = 3, shape = 16, data=exp)+
        labs(x="Mean pollen deposition", y = "Mean pollen export(Log)")+
        theme(text = element_text(size = 16)) + theme(axis.text.x=element_text(hjust = 1))+ theme(axis.title = element_text(size = 15))+theme(axis.text.x = element_text(size = 12))+theme_cowplot()
        #+ geom_text_repel(aes(x=mean_depo, y=mean_exp_log), size = label.size, label = exp$func_group, data = exp, nudge_y = 0.5, segment.size = 0.2, segment.alpha = 0.75)

