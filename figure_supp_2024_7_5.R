rm(list = ls())
setwd("D:/Deng yuehua")

### libaray packages ###
pacman::p_load(char = c('tidyverse','vegan','readxl','openxlsx','ggpmisc','sjPlot','pracma','plotbiomes','piecewiseSEM',
                        'MuMIn','ggrepel','broom','ggpubr','nlme','lsmeans','xtable','broom','patchwork','ggthemes'))

## diy_theme
mytheme<-  theme(axis.title.x = element_text(size = 14, vjust = -0.5, hjust = 0.5, family="sans"))+
  theme(axis.title.y = element_text(size = 14, vjust = 1.5, hjust = 0.5, family="sans"))+ 
  theme(axis.text.x = element_text(size =14, vjust = 0.8, hjust = 0.5,family="sans"))+  
  theme(axis.text.y = element_text(size =14, vjust = 0.5, hjust = 0.5, family="sans"))+
  theme(axis.line.x=element_line(color="black"))+
  theme(axis.line.y=element_line(color="black"))+
  theme(axis.ticks.x=element_line(color="black"))+
  theme(axis.ticks.y=element_line(color="black"))+
  theme(plot.margin = unit(c(0.5,1,0.4,0.3),"cm"))+
  theme(plot.title = element_text(size=9, colour = "black", family="sans",hjust=0.5))+
  theme(panel.grid = element_blank())+ 
  theme(axis.ticks.length.x = unit(0.15, 'cm'),axis.ticks.length.y = unit(0.15, 'cm'))

############################################
# standardize function (z-score)
my_scale <- function(x){
  x1 <- (x-mean(x))/sd(x)
  return(x1)
}

##read data##
df = read.csv('1phenological paper/data/data for figures/data for figures.csv')

# 使用 rm() 删除这些变量  
rm(list =  ls()[!ls() %in% c('df','mytheme','my_scale')])
################################################################
#####FigS1:The fitting of EVI raw data to filtered data #########
################################################################


#########################################################################################
#######FigS4:Effects of climatic factors on diversity and stability #########
#########################################################################################
#########################################################################################
dd <- df %>% select(PI,ydiv,STA_max_EVI,STA_avg_EVI,phe_var_based_EVI,MAT_scale,MAP_scale)
### Effects of MAT on diversity and stability
MAT_result <- c()
for (i in 2:5){
  response <- dd[ , i]
  MAT <- dd[ , 6] # MAT
  formula <- lm(response ~ MAT)
  result <- summary(formula)$coefficients[2, 1:4]
  r2 <- summary(formula)$adj.r.squared
  t <- c(colnames(dd)[i], result, r2)
  MAT_result <- rbind(MAT_result, t)
}
colnames(MAT_result) <- c("response", "estimate", "se", "t value", "p", "r2")
MAT_result = as.data.frame(MAT_result)
MAT_result$predictor = rep(c('MAT'),each=4)
MAT_result[,c(2:6)] = lapply(MAT_result[,c(2:6)], as.numeric)
MAT_result$CI_low = MAT_result$estimate-MAT_result$se
MAT_result$CI_high = MAT_result$estimate+MAT_result$se
### Effects of MAP on diversity and stability
MAP_result <- c()
for (i in 2:5){
  response <- dd[ , i]
  MAP <- dd[ , 7] # MAP
  formula <- lm(response ~ MAP)
  result <- summary(formula)$coefficients[2, 1:4]
  r2 <- summary(formula)$adj.r.squared
  t <- c(colnames(dd)[i], result, r2)
  MAP_result <- rbind(MAP_result, t)
}
colnames(MAP_result) <- c("response", "estimate", "se", "t value", "p", "r2")
MAP_result = as.data.frame(MAP_result)
MAP_result$predictor = rep(c('MAP'),each=4)
MAP_result[,c(2:6)] = lapply(MAP_result[,c(2:6)], as.numeric)
MAP_result$CI_low = MAP_result$estimate-MAP_result$se
MAP_result$CI_high = MAP_result$estimate+MAP_result$se
climate_result = rbind(MAT_result,MAP_result) %>% select(predictor,everything())

response1 <- rep(c("Plant richness","Stability of annual maximum EVI",
                   "Stability of annual mean EVI","Phenological variability"),each=1,times=2)

climate_result$response1 = response1
climate_result$response1 = factor(climate_result$response1, 
                                  levels =c("Stability of annual maximum EVI","Stability of annual mean EVI",
                                            "Phenological variability","Plant richness"))
mycolor = c('#009E73',"#E69F00","#0072B2","#512d69")
climate_result$col = c(mycolor[1],mycolor[4],mycolor[3],mycolor[2],mycolor[1],mycolor[4],mycolor[3],mycolor[2])
climate_result$fill=climate_result$col
climate_result$fill[climate_result$CI_low*climate_result$CI_high < 0]='white'

t1 = subset(climate_result,climate_result$predictor=='MAT')
(MAT_sta <- ggplot(data=t1,aes(x=response1, y=estimate, ymin=CI_low, ymax=CI_high)) +
    geom_hline(yintercept=0, lty=2,size=1.2,color="grey") +
    geom_pointrange(shape=21,fill=t1$fill,color=factor(t1$col),size=0.9) +
    scale_color_manual(values=mycolor)+
    coord_flip() +
    xlab(NULL) + 
    ylab("Effect size (95% CI)")+  
    theme_bw() +
    mytheme+
    ggtitle("Mean annual temperature") )
t2 = subset(climate_result,climate_result$predictor=='MAP')
(MAP_sta <- ggplot(data=t2,aes(x=response1, y=estimate, ymin=CI_low, ymax=CI_high)) +
    geom_hline(yintercept=0, lty=2,size=1.2,color="grey") +
    geom_pointrange(shape=21,fill=t1$fill,color=factor(t1$col),size=0.9) +
    scale_color_manual(values=mycolor)+
    coord_flip() +
    xlab(NULL) + 
    ylab("Effect size (95% CI)")+  
    theme_bw() +
    mytheme+
    ggtitle('Mean annual precipitation') )
figS4 <- plot_grid(MAT_sta, MAP_sta, labels = c("A", "B"), ncol = 2)
# figS4 <- MAT_sta+MAP_sta+plot_annotation(tag_levels = 'A')
figS4
# ggsave(figS4,file="FigS4.pdf",width =16,height =8, dpi = 800)

#################################################################################################################
#######FigS5: The relationships between diversity, phenological variability and stability based onmax EVI #######
#################################################################################################################
df1 = df %>% 
  select(PI,grid,ydiv,STA_avg_EVI,STA_max_EVI,phe_var_based_EVI) %>% 
  group_by(PI) %>% 
  summarise_at(vars(ydiv:phe_var_based_EVI),
               list(mean = mean,sd = parameters::standard_error)) %>% 
  mutate(PI = factor(PI))

names(df1)
# [1] "PI"                     "ydiv_mean"              "STA_avg_EVI_mean"       "STA_max_EVI_mean"      
# [5] "phe_var_based_EVI_mean" "ydiv_sd"                "STA_avg_EVI_sd"         "STA_max_EVI_sd"        
# [9] "phe_var_based_EVI_sd" 

summary(lm(STA_max_EVI~Adjusted_latitude,df))##p-value: 0.417
summary(lm(STA_max_EVI~Adjusted_latitude+I(Adjusted_latitude^2),df))##p:0.06679,r2=0.02319 ,F2,146=2.757

(sta_plot <- ggplot(df, aes(y = STA_max_EVI, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color="#512d69") +
    # geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.5, colour ="black") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2, colour ="#512d69", se=T) +
    labs(x="Adjusted latitude (°)",y='Stability of annual maximum EVI (ln)') +
    scale_y_continuous(limits = c(1,4),breaks = c(seq(1,4, by = 1)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)

summary(lm(STA_max_EVI~ydiv,df))##p-value: 0.003395,Adjusted R-squared:   0.05048  
(sta_div <- ggplot(df1, aes(x=ydiv_mean, y=STA_max_EVI_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df1,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=STA_max_EVI_mean-STA_max_EVI_sd, ymax=STA_max_EVI_mean+STA_max_EVI_sd))+
    geom_errorbarh(aes(xmin=ydiv_mean-ydiv_sd, xmax=ydiv_mean+ydiv_sd))+
    geom_text_repel(label=df1$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Stability of annual maximum EVI (ln)') +
    scale_y_continuous(limits = c(1.62,3.2),breaks = c(seq(1.6,3.2, by = 0.4)))+
    scale_x_continuous(limits = c(2,5),breaks = c(seq(2,5, by = 1)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

summary(lm(STA_max_EVI~phe_var_based_EVI,df))#p-value: 0.0001054,Adjusted R-squared:  0.09141 
(sta_phe <- ggplot(df1, aes(x=phe_var_based_EVI_mean, y=STA_max_EVI_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df1,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=STA_max_EVI_mean-STA_max_EVI_sd, ymax=STA_max_EVI_mean+STA_max_EVI_sd))+
    geom_errorbarh(aes(xmin=phe_var_based_EVI_mean-phe_var_based_EVI_sd, xmax=phe_var_based_EVI_mean+phe_var_based_EVI_sd))+
    geom_text_repel(label=df1$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Phenological variability (ln)",y='Stability of annual maximum EVI (ln)') +
    scale_y_continuous(limits = c(1.58,3.2),breaks = c(seq(1.6,3.2, by = 0.4)))+
    scale_x_continuous(limits = c(-1.3,1.3),breaks = c(seq(-1,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)
dev.new()
figS5 <- sta_plot + sta_div+sta_phe+plot_annotation(tag_levels = 'A')
figS5
ggsave(figS5,file="figures/FigS5_Relationships between diversity, phenological variability and stability based onmax EVI1.pdf",width = 16 , height =8, dpi = 800)

#######################################################################################################
################FigS6: Partial regression models  ####################################################
###The relationships between diversity, phenological variability and stability of annual mean (max) EVI###
#######################################################################################################

## the results based on the linear partial regression extract the residuals from the linear model,
# we used MAP and MAT because both MAP and MAT has been generally recognized 
# as the predominant environmental factors along the climate gradients.

resid_plm.result <- c()
for (i in 2:5){
  each.depend <- dd[ , i]
  MAT_scale <- dd[ , 6]
  MAP_scale <- dd[ , 7]
  each.fit <- lm(each.depend ~ MAT_scale + MAP_scale)
  resdat <- cbind(each.fit$residuals)
  PI <- dd$PI
  resid_plm.result <- cbind(resid_plm.result, resdat)
}
colnames(resid_plm.result) <- c("ydiv_p","sta_max_EVI_p","sta_avg_EVI_p","phe_var_based_EVI_p")
resid_plm.result <- as.data.frame(resid_plm.result)
resid_plm.result$PI = dd$PI

summary(lm(phe_var_based_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 0.0003193,Adjusted R-squared:  0.07839 
summary(lm(sta_avg_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 0.5456,Adjusted R-squared:  -0.004295 
summary(lm(sta_avg_EVI_p ~ phe_var_based_EVI_p, data = resid_plm.result)) # p-value: 1.499e-08,Adjusted R-squared:  0.191

summary(lm(sta_max_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 0.04064,Adjusted R-squared:  0.02159 
summary(lm(sta_max_EVI_p ~ phe_var_based_EVI_p, data = resid_plm.result)) #p-value: 3.139e-06,Adjusted R-squared:  0.132 


df2 = resid_plm.result %>% 
  # select(PI,grid,ydiv,STA_avg_EVI,phe_var_based_EVI) %>% 
  group_by(PI) %>% 
  summarise_at(vars(ydiv_p:phe_var_based_EVI_p),list(mean = mean,sd = parameters::standard_error)) %>% 
  mutate(PI = factor(PI))

names(df2)
# [1] "PI"                       "ydiv_p_mean"              "sta_max_EVI_p_mean"       "sta_avg_EVI_p_mean"      
# [5] "phe_var_based_EVI_p_mean" "ydiv_p_sd"                "sta_max_EVI_p_sd"         "sta_avg_EVI_p_sd"        
# [9] "phe_var_based_EVI_p_sd" 

(plm_phe_div <- ggplot(df2, aes(x=ydiv_p_mean, y=phe_var_based_EVI_p_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df2,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=phe_var_based_EVI_p_mean-phe_var_based_EVI_p_sd, 
                      ymax=phe_var_based_EVI_p_mean+phe_var_based_EVI_p_sd))+
    geom_errorbarh(aes(xmin=ydiv_p_mean-ydiv_p_sd, xmax=ydiv_p_mean+ydiv_p_sd))+
    geom_text_repel(label=df2$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Phenological variability (ln)') +
    scale_y_continuous(limits = c(-1.5,1.5),breaks = c(seq(-1.5,1.5, by = 1)))+
    scale_x_continuous(limits = c(-1.25,1),breaks = c(seq(-1,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

(plm_avg_sta_div <- ggplot(df2, aes(x=ydiv_p_mean , y=sta_avg_EVI_p_mean )) + 
    geom_point(size=2) + 
    geom_smooth(data=df2,method="lm", formula=y~x, se=T, col="black", linewidth=1.5,linetype=2)+
    geom_errorbar(aes(ymin=sta_avg_EVI_p_mean -sta_avg_EVI_p_sd, ymax=sta_avg_EVI_p_mean +sta_avg_EVI_p_sd))+
    geom_errorbarh(aes(xmin=ydiv_p_mean -ydiv_p_sd , xmax=ydiv_p_mean +ydiv_p_sd ))+
    geom_text_repel(label=df2$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Stability of annual mean EVI (ln)') +
    # labs(x=" ",y='Stability of annual mean EVI (ln)') +
    scale_y_continuous(limits = c(-0.6,0.75),breaks = c(seq(-0.6,0.6, by = 0.3)))+
    scale_x_continuous(limits = c(-1.25,1),breaks = c(seq(-1,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

(plm_avg_sta_phe <- ggplot(df2, aes(x=phe_var_based_EVI_p_mean, y=sta_avg_EVI_p_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df2,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=sta_avg_EVI_p_mean-sta_avg_EVI_p_sd, ymax=sta_avg_EVI_p_mean+sta_avg_EVI_p_sd))+
    geom_errorbarh(aes(xmin=phe_var_based_EVI_p_mean-phe_var_based_EVI_p_sd,
                       xmax=phe_var_based_EVI_p_mean+phe_var_based_EVI_p_sd))+
    geom_text_repel(label=df2$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Phenological variability (ln)",y='Stability of annual mean EVI (ln)') +
    # labs(x=" ",y=' ') +
    scale_y_continuous(limits = c(-0.6,0.65),breaks = c(seq(-0.6,0.6, by = 0.3)))+
    scale_x_continuous(limits = c(-1.5,1),breaks = c(seq(-1.5,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

(plm_max_sta_div <- ggplot(df2, aes(x=ydiv_p_mean , y=sta_max_EVI_p_mean )) + 
    geom_point(size=2) + 
    geom_smooth(data=df2,method="lm", formula=y~x, se=T, col="black", linewidth=1.5,linetype=1)+
    geom_errorbar(aes(ymin=sta_max_EVI_p_mean -sta_max_EVI_p_sd, ymax=sta_max_EVI_p_mean +sta_max_EVI_p_sd))+
    geom_errorbarh(aes(xmin=ydiv_p_mean -ydiv_p_sd , xmax=ydiv_p_mean +ydiv_p_sd ))+
    geom_text_repel(label=df2$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Stability of annual maximum EVI (ln)') +
    scale_y_continuous(limits = c(-0.55,0.75),breaks = c(seq(-0.5,0.75, by = 0.25)))+
    scale_x_continuous(limits = c(-1.25,1),breaks = c(seq(-1,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

(plm_max_sta_phe <- ggplot(df2, aes(x=phe_var_based_EVI_p_mean, y=sta_max_EVI_p_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df2,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=sta_max_EVI_p_mean-sta_max_EVI_p_sd, ymax=sta_max_EVI_p_mean+sta_max_EVI_p_sd))+
    geom_errorbarh(aes(xmin=phe_var_based_EVI_p_mean-phe_var_based_EVI_p_sd,
                       xmax=phe_var_based_EVI_p_mean+phe_var_based_EVI_p_sd))+
    geom_text_repel(label=df2$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Phenological variability (ln)",y='Stability of annual maximum EVI (ln)') +
    scale_y_continuous(limits = c(-0.6,0.65),breaks = c(seq(-0.6,0.6, by = 0.3)))+
    scale_x_continuous(limits = c(-1.5,1),breaks = c(seq(-1.5,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)
dev.new()
figS6 <- plm_phe_div| wrap_plots(plm_avg_sta_div, plm_avg_sta_phe,plm_max_sta_div, plm_max_sta_phe,nrow = 2,byrow = TRUE)
figS6
# ggsave(figS6,file="figures/FigS6_partial regression models_The relationships between diversity, phenological variability and stability.pdf",width = 16 , height =8, dpi = 800)

#######################################################################################################
################FigS7: SEM based on STA_max_EVI ####################################################
#######################################################################################################
##the priori model########
df3 = df %>% select(STA_max_EVI,Adjusted_latitude,phe_var_based_EVI,ydiv,MAT_scale,MAP_scale)
SEM1 = psem(
  lm(STA_max_EVI ~ Adjusted_latitude + phe_var_based_EVI + ydiv + MAT_scale + MAP_scale,data = df3),
  lm(phe_var_based_EVI ~ Adjusted_latitude + ydiv + MAT_scale + MAP_scale ,data = df3),
  lm(ydiv ~ Adjusted_latitude + MAT_scale + MAP_scale,data = df3),
  lm(MAT_scale ~ Adjusted_latitude ,data = df3),
  lm(MAP_scale ~ Adjusted_latitude,data = df3)
)
summary(SEM1)## Fisher's C = 0.942  with P-value = 0.624  and on 2 degrees of freedom, AIC=48.942    
##the best model########
SEM2 = psem(
  lm(STA_max_EVI ~ Adjusted_latitude + phe_var_based_EVI + MAP_scale,data = df3),
  lm(phe_var_based_EVI ~  ydiv + MAT_scale,data = df3),
  lm(ydiv ~ MAP_scale,data = df3),
  lm(MAT_scale ~ Adjusted_latitude ,data = df3),
  lm(MAP_scale ~ Adjusted_latitude,data = df3)
)
summary(SEM2)#Fisher's C = 10.805  with P-value = 0.701  and on 14 degrees of freedom, AIC=46.805       
##Standardized and unstandardized direct effects in our final SEM 
coeff = as.data.frame(xtable(summary(SEM2)$coefficients)) %>% select(Response:Std.Estimate)
write.csv(coeff,'data/SEM2-unstandardized direct effects.csv',row.names = F)
######################################################################################
################Supp info:  table ####################################################
######################################################################################
#######################table######################
tableS1 = read.csv('environment data.csv')[,1:8] %>% 
  group_by(Continent,Country,pi) %>% 
  summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),Elevation=mean(Elevation)) %>% 
  ungroup() %>% 
  arrange(pi)
tableS1_type = read.csv('environment data.csv')[,1:8] %>% 
  group_by(Continent,Country,Community_type,pi) %>% 
  summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),Elevation=mean(Elevation)) %>% 
  ungroup() %>% 
  arrange(pi)
# write.csv(tableS1,'data/tableS1.csv')
# write.csv(tableS1_type,'data/tableS1_type.csv')

###TableS3:slope pvalue r2##
df = read.csv('1phenological paper/data/data for figures/data for figures.csv')
slope <- df %>%
  select(Adjusted_latitude,ydiv,STA_max_EVI,STA_avg_EVI,phe_var_based_EVI,MAT,MAP) %>%
  pivot_longer(.,-1,names_to = 'ind',values_to = 'value') %>%
  group_by(ind) %>%
  do(coefs(lm(value ~ Adjusted_latitude, data = .))) %>%
  select(-2,-6,-9,-10)
r2 <- df %>% 
  select(Adjusted_latitude,ydiv,STA_max_EVI,STA_avg_EVI,phe_var_based_EVI,MAT,MAP) %>% 
  pivot_longer(.,-1,names_to = 'ind',values_to = 'value') %>% 
  group_by(ind) %>%
  nest() %>% 
  mutate(model = map(data, ~ lm(value ~ Adjusted_latitude, .x))) %>% 
  mutate(result1 = map(model,glance)) %>% 
  unnest_wider(result1) %>% 
  select(ind,r.squared,statistic,df,df.residual,nobs)
mod_inf = full_join(slope,r2) %>% rename(Response=ind)

names(df)
MAT_MAP = coefs(lm(MAP~MAT,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(MAP~MAT,df))[,c(1,4,6,11,12)])
MAT_div = coefs(lm(ydiv~MAT_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(ydiv~MAT_scale,df))[,c(1,4,6,11,12)])
MAT_sta_avg = coefs(lm(STA_avg_EVI~MAT_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_avg_EVI~MAT_scale,df))[,c(1,4,6,11,12)])
MAT_sta_max = coefs(lm(STA_max_EVI~MAT_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_max_EVI~MAT_scale,df))[,c(1,4,6,11,12)])
MAT_phe = coefs(lm(phe_var_based_EVI~MAT_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(phe_var_based_EVI~MAT_scale,df))[,c(1,4,6,11,12)])
MAP_div = coefs(lm(ydiv~MAP_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(ydiv~MAP_scale,df))[,c(1,4,6,11,12)])
MAP_sta_avg = coefs(lm(STA_avg_EVI~MAP_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_avg_EVI~MAP_scale,df))[,c(1,4,6,11,12)])
MAP_sta_max = coefs(lm(STA_max_EVI~MAP_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_max_EVI~MAP_scale,df))[,c(1,4,6,11,12)])
MAP_phe = coefs(lm(phe_var_based_EVI~MAP_scale,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(phe_var_based_EVI~MAP_scale,df))[,c(1,4,6,11,12)])

phe_div = coefs(lm(phe_var_based_EVI~ydiv,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(phe_var_based_EVI~ydiv,df))[,c(1,4,6,11,12)])
sta_avg_div = coefs(lm(STA_avg_EVI~ydiv,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_avg_EVI~ydiv,df))[,c(1,4,6,11,12)])
sta_max_div = coefs(lm(STA_max_EVI~ydiv,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_max_EVI~ydiv,df))[,c(1,4,6,11,12)])
sta_avg_phe = coefs(lm(STA_avg_EVI~phe_var_based_EVI,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_avg_EVI~phe_var_based_EVI,df))[,c(1,4,6,11,12)])
sta_max_phe = coefs(lm(STA_max_EVI~phe_var_based_EVI,df)) %>% select(-5,-8,-9) %>% mutate(glance(lm(STA_max_EVI~phe_var_based_EVI,df))[,c(1,4,6,11,12)])
mod_inf1 = rbind(mod_inf,MAT_MAP,MAT_div,MAT_sta_avg,MAT_sta_max,MAT_phe,MAP_div,MAP_sta_avg,MAP_sta_max,MAP_phe,
                 phe_div,sta_avg_div,sta_max_div,sta_avg_phe,sta_max_phe)
# write.csv(mod_inf1,'1phenological paper/data/mod_inf11.csv')

names(resid_plm.result) 
plm_phe_div = coefs(lm(phe_var_based_EVI_p~ydiv_p,resid_plm.result)) %>% select(-5,-8,-9) %>% mutate(glance(lm(phe_var_based_EVI_p~ydiv_p,resid_plm.result))[,c(1,4,6,11,12)])
plm_sta_avg_div = coefs(lm(sta_avg_EVI_p~ydiv_p,resid_plm.result)) %>% select(-5,-8,-9) %>% mutate(glance(lm(sta_avg_EVI_p~ydiv_p,resid_plm.result))[,c(1,4,6,11,12)])
plm_sta_max_div = coefs(lm(sta_max_EVI_p~ydiv_p,resid_plm.result)) %>% select(-5,-8,-9) %>% mutate(glance(lm(sta_max_EVI_p~ydiv_p,resid_plm.result))[,c(1,4,6,11,12)])
plm_sta_avg_phe = coefs(lm(sta_avg_EVI_p~phe_var_based_EVI_p,resid_plm.result)) %>% select(-5,-8,-9) %>% mutate(glance(lm(sta_avg_EVI_p~phe_var_based_EVI_p,resid_plm.result))[,c(1,4,6,11,12)])
plm_sta_max_phe = coefs(lm(sta_max_EVI_p~phe_var_based_EVI_p,resid_plm.result)) %>% select(-5,-8,-9) %>% mutate(glance(lm(sta_max_EVI_p~phe_var_based_EVI_p,resid_plm.result))[,c(1,4,6,11,12)])
mod_inf2 = rbind(plm_phe_div,plm_sta_avg_div,plm_sta_max_div,plm_sta_avg_phe,plm_sta_max_phe)
# write.csv(mod_inf2,'mod_inf22.csv')
