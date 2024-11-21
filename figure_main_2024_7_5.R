rm(list = ls())

### libaray packages ###
pacman::p_load(char = c('tidyverse','vegan','readxl','openxlsx','ggpmisc','sjPlot','pracma','plotbiomes','piecewiseSEM',
                        'eoffice','ggrepel','broom','ggpubr','nlme','lsmeans','xtable','broom','patchwork','ggthemes'))

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

# AT = read.xlsx('1-157pre temp.xlsx',sheet = 1)
# AP = read.xlsx('1-157pre temp.xlsx',sheet = 2)
# 
# MAT = AT %>% group_by(grid) %>% summarise(MAT = mean(annual_temperature))
# MAP = AP %>% group_by(grid) %>% summarise(MAP = mean(annual_precipitation))
# 
# df = read.csv('data_all_ln.csv') %>% 
#   # mutate(hemisphere=if_else (.$Latitude>0, 'Northern', 'Southern')) %>% 
#   mutate(Adjusted_latitude = Elevation/111+abs(Latitude)) %>% 
#   select(PI,grid,Community_type,Latitude,Longitude,Adjusted_latitude,
#          Elevation,ydiv,STA_avg_EVI,STA_max_EVI,phe_var_based_EVI) %>%
#   left_join(.,MAT) %>% 
#   left_join(.,MAP) %>% 
#   filter(!PI %in% c('Mitchell')) %>% #### there is no species in some grids 
#   filter(!grid %in% c(141,142,143,144)) %>% #### [NAs = 166] the number of NA values exceeds 1/3 (166/253)
#   mutate(MAT_scale = my_scale(MAT),MAP_scale = my_scale(MAP))

##read data##
df = read.csv('data for figures.csv')
##############################################################
########      Fig1:    sampling map                  #########
##############################################################

###Figure1A
world <- map_data("world")
map_data = df %>% 
  group_by(PI) %>% 
  summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),Elevation=mean(Elevation),
            MAP = mean(MAP),MAT=mean(MAT))
world.map <- ggplot() +
  geom_map(data = world, map = world,
           aes(x=long, y=lat, map_id = region),color = "black", fill = "lightgray", size = 0.1) +
  theme_bw()+
  scale_y_continuous(expand = expansion(mult=c(0,0)))+
  scale_x_continuous(expand = expansion(add=c(0,0)))
world.map
###add my points
(my_map = world.map + 
    geom_point(data = map_data,
               aes(x=Longitude, y=Latitude, colour = Elevation),size=3.5,alpha=0.8)+
    scale_color_gradient(low = "blue", high = "orange", limits = c(0, 3100)) +
    labs(x="Longitude",y="Latitude") +
    geom_text_repel(data = map_data, aes(x = Longitude, y = Latitude, label = PI),
                    size = 3, color = "grey30",
                    box.padding = 1, #字到点的距离 
                    point.padding = 1,#字到点的距离，点周围的空白宽度 
                    # Draw an arrow from the label to the data point.
                    arrow = arrow(length = unit(0.02, 'npc')),
                    max.overlaps = 100) +
    theme_bw()+
    mytheme+
    theme(legend.position=c(.1,.25)))
# ggsave(my_map,file="my_map.pdf.pdf",width = 16 , height =8, dpi = 800)

###Figure1B-1G: Latitudinal gradient
summary(lm(MAT~Adjusted_latitude,df))##p-value: < 2.2e-16,Adjusted R-squared:  0.7312
(fig1b <- ggplot(df, aes(y = MAT, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color="#E69F00")+
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, colour ="#E69F00", se=T) +
    guides(colour = "none",size='none') +
    labs(x="Adjusted latitude (°)") +
    ylab(bquote(paste(MAT*" ("^o*C*")"))) +
    scale_y_continuous(limits = c(-10,30),breaks = c(seq(-10,30, by = 10)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)

summary(lm(MAP~Adjusted_latitude,df))##p-value: 0.007525 ,Adjusted R-squared:  0.04224  
(fig1c <- ggplot(df, aes(y = MAP/10, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color="#E69F00")+
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, colour ="#E69F00", se=T) +
    guides(colour = "none",size='none') +
    labs(x="Adjusted latitude (°)") +
    ylab("MAP (cm)") +
    scale_y_continuous(limits = c(0,150),breaks = c(seq(0,150, by = 50)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)

summary(lm(MAP/10~MAT,df))##p-value: 0.01122,Adjusted R-squared: 0.03744    
(fig1d <- ggplot(df, aes(y = MAP/10, x = MAT)) +
    geom_point(shape=19,size=3.5,color="#E69F00")+
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, colour ="#E69F00", se=T) +
    guides(colour = "none",size='none') +
    xlab(bquote(paste(MAT*" ("^o*C*")"))) +
    ylab("MAP (cm)") +
    scale_x_continuous(limits = c(-10,30),breaks = c(seq(-10,30, by = 10)))+
    scale_y_continuous(limits = c(0,150),breaks = c(seq(0,150, by = 50)))+
    theme_bw()+
    mytheme)

summary(lm(ydiv ~ Adjusted_latitude, data =df)) #p-value: 0.2734, Adjusted R-squared:  0.001448  
# summary(lm(ydiv ~ Adjusted_latitude+I(Adjusted_latitude^2), data =df))#p-value: 0.544
(ydiv_plot <- ggplot(df, aes(y = ydiv, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color='#009E73') +
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, linetype = 2,colour ="#009E73", se=T) +
    # geom_smooth(aes(group = 1), method = "lm", formula = y ~ poly(x, 2), linewidth = 1, colour ="red", se=F) +
    labs(x="Adjusted latitude (°)",y='Plant richness (ln)') +
    scale_y_continuous(limits = c(2,5),breaks = c(seq(2,5, by = 1)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)

summary(lm(phe_var_based_EVI ~ Adjusted_latitude, data =df))#p-value: 4.343e-11, Adjusted R-squared:  0.2577  
# summary(lm(phe_var_based_EVI ~ Adjusted_latitude+I(Adjusted_latitude^2), data =df))
(pheno_var_plot <- ggplot(df, aes(y = phe_var_based_EVI, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color="#009E73") +
    # geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.5, colour ="black") +
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, colour ="#009E73", se=T) +
    labs(x="Adjusted latitude (°)",y='Phenological variability (ln)') +
    scale_y_continuous(limits = c(-2,2),breaks = c(seq(-2,2, by = 1)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)


summary(lm(STA_avg_EVI ~ Adjusted_latitude, data =df))#p-value:  0.7656
# summary(lm(STA_avg_EVI ~ Adjusted_latitude+I(Adjusted_latitude^2), data =df))#p-value: 0.002935,adjR2:0.06587 
(sta_plot <- ggplot(df, aes(y = STA_avg_EVI, x = Adjusted_latitude)) +
    geom_point(shape=19,size=3.5,color="#009E73") +
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, colour ="#009E73", linetype = 2) +
    # geom_smooth(method = "lm", formula = y ~ poly(x, 2), linewidth = 2, colour ="#0072B2", se=T) +
    labs(x="Adjusted latitude (°)",y='Stability (ln)') +
    scale_y_continuous(limits = c(1,4),breaks = c(seq(1,4, by = 1)))+
    scale_x_continuous(limits = c(10,70),breaks = c(seq(10,70, by = 20)))+
    theme_bw()+
    mytheme)

fig1 <- (my_map)/(fig1b|fig1c|fig1d)/(ydiv_plot|pheno_var_plot|sta_plot)+ plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = c(2, 1, 1))
dev.new()
fig1
# ggsave(fig1,file="Sampling map.pdf",width = 10 , height =12, dpi = 800)

################################################################################################
#####Figure2 The relationships between diversity, phenological variability and stability########
################################################################################################
df1 = df %>% 
  select(PI,grid,ydiv,STA_avg_EVI,phe_var_based_EVI) %>% 
  group_by(PI) %>% 
  summarise_at(vars(ydiv:phe_var_based_EVI),
               list(mean = mean,sd = parameters::standard_error)) %>% 
  mutate(PI = factor(PI))
names(df1)
# [1] "PI"                     "ydiv_mean"              "STA_avg_EVI_mean"       "phe_var_based_EVI_mean" "ydiv_sd"                "STA_avg_EVI_sd"        
# [7] "phe_var_based_EVI_sd" 

summary(lm(phe_var_based_EVI~ydiv,df))##p-value: 1.38e-05 ***,Adjusted R-squared: 0.1181  
(phe_div <- ggplot(df1, aes(x=ydiv_mean, y=phe_var_based_EVI_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df1,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=phe_var_based_EVI_mean-phe_var_based_EVI_sd, ymax=phe_var_based_EVI_mean+phe_var_based_EVI_sd))+
    geom_errorbarh(aes(xmin=ydiv_mean-ydiv_sd, xmax=ydiv_mean+ydiv_sd))+
    geom_text_repel(label=df1$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Phenological variability (ln)') +
    scale_y_continuous(limits = c(-1.5,1.5),breaks = c(seq(-1.5,1.5, by = 1)))+
    scale_x_continuous(limits = c(2.3,5),breaks = c(seq(3,5, by = 1)))+
    guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

summary(lm(STA_avg_EVI~ydiv,df))#p-value: 0.01818,Adjusted R-squared: 0.03166  
(sta_div <- ggplot(df1, aes(x=ydiv_mean, y=STA_avg_EVI_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df1,method="lm", formula=y~x, se=T, col="black", linewidth=1.5,linetype=1)+
    geom_errorbar(aes(ymin=STA_avg_EVI_mean-STA_avg_EVI_sd, ymax=STA_avg_EVI_mean+STA_avg_EVI_sd))+
    geom_errorbarh(aes(xmin=ydiv_mean-ydiv_sd, xmax=ydiv_mean+ydiv_sd))+
    geom_text_repel(label=df1$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Plant richness (ln)",y='Stability (ln)') +
    scale_y_continuous(limits = c(1.75,3.5),breaks = c(seq(2,3.5, by = 0.5)))+
    scale_x_continuous(limits = c(2.3,5),breaks = c(seq(3,5, by = 1)))+
    guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)

summary(lm(STA_avg_EVI~phe_var_based_EVI,df))#p-value: 1.197e-05,Adjusted R-squared:  0.1197
(sta_phe <- ggplot(df1, aes(x=phe_var_based_EVI_mean, y=STA_avg_EVI_mean)) + 
    geom_point(size=2) + 
    geom_smooth(data=df1,method="lm", formula=y~x, se=T, col="black", linewidth=1.5)+
    geom_errorbar(aes(ymin=STA_avg_EVI_mean-STA_avg_EVI_sd, ymax=STA_avg_EVI_mean+STA_avg_EVI_sd))+
    geom_errorbarh(aes(xmin=phe_var_based_EVI_mean-phe_var_based_EVI_sd, xmax=phe_var_based_EVI_mean+phe_var_based_EVI_sd))+
    geom_text_repel(label=df1$PI, size=2.5,max.overlaps=20)+  #from "ggrepel", add labels for each point
    labs(x="Phenological variability (ln)",y='Stability (ln)') +
    scale_y_continuous(limits = c(1.5,3.5),breaks = c(seq(2,3.5, by = 0.5)))+
    scale_x_continuous(limits = c(-1.25,1.25),breaks = c(seq(-1,1, by = 0.5)))+
    guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)
################ Partial regression models  ####################################################
## the results based on the linear partial regression extract the residuals from the linear model,
# we used MAP and MAT because both MAP and MAT has been generally recognized 
# as the predominant environmental factors along the climate gradients.
dd <- df %>% select(PI,ydiv,STA_max_EVI,STA_avg_EVI,phe_var_based_EVI,MAT_scale,MAP_scale)
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

summary(lm(phe_var_based_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 3.754e-05,Adjusted R-squared:  0.1062  
summary(lm(sta_avg_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 0.1441,Adjusted R-squared:  0.007974  
summary(lm(sta_avg_EVI_p ~ phe_var_based_EVI_p, data = resid_plm.result)) # p-value: 5.812e-08,adjR2:0.1809

summary(lm(sta_max_EVI_p ~ ydiv_p, data = resid_plm.result)) #p-value: 0.0288,Adjusted R-squared:  0.02622  
summary(lm(sta_max_EVI_p ~ phe_var_based_EVI_p, data = resid_plm.result)) #p-value: 3.968e-06,adjR2:  0.1327  


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
    labs(x="Plant richness (ln) | Climate",y='Phenological variability (ln) | Climate') +
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
    labs(x="Plant richness (ln) | Climate",y='Stability(ln) | Cilmate') +
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
    labs(x="Phenological variability (ln) | Climate",y='Stability(ln) | Climate') +
    # labs(x=" ",y=' ') +
    scale_y_continuous(limits = c(-0.6,0.65),breaks = c(seq(-0.6,0.6, by = 0.3)))+
    scale_x_continuous(limits = c(-1.5,1),breaks = c(seq(-1.5,1, by = 0.5)))+
    # guides(color=guide_legend(nrow=1))+
    theme_bw()+
    mytheme)
dev.new()
fig2 = phe_div+sta_div+sta_phe+plm_phe_div+plm_avg_sta_div+plm_avg_sta_phe+plot_annotation(tag_levels = 'A')
# ggsave(fig2,file="The relationships between diversity, phenological variability and stability.pdf",width = 16 , height =8, dpi = 800)

###Figure3 SEM [Version:	2.1.0]#####
############### SEM based on STA_avg_EVI ############### 
##the priori model########
SEM1 = psem(
  lm(STA_avg_EVI ~ Adjusted_latitude + phe_var_based_EVI + ydiv + MAT_scale + MAP_scale,data = df),
  lm(phe_var_based_EVI ~ Adjusted_latitude + ydiv + MAT_scale + MAP_scale ,data = df),
  lm(ydiv ~ Adjusted_latitude + MAT_scale + MAP_scale,data = df),
  lm(MAT_scale ~ Adjusted_latitude ,data = df),
  lm(MAP_scale ~ Adjusted_latitude,data = df)
)
summary(SEM1)## Fisher's C = 0.942  with P-value = 0.624  and on 2  degrees of freedom, AIC=48.942 
##the best model########
SEM2 = psem(
  lm(STA_avg_EVI ~ phe_var_based_EVI  + MAT_scale + MAP_scale,data = df),
  lm(phe_var_based_EVI ~ ydiv + MAT_scale ,data = df),
  lm(ydiv ~   MAP_scale,data = df),
  lm(MAT_scale ~ Adjusted_latitude ,data = df),
  lm(MAP_scale ~ Adjusted_latitude,data = df)
)
summary(SEM2)#Fisher's C = 10.875  with P-value = 0.696  and on 14 degrees of freedom, AIC=46.875     
##Standardized and unstandardized direct effects in our final SEM 
coeff = as.data.frame(xtable(summary(SEM2)$coefficients)) %>% select(Response:Std.Estimate)
# write.csv(coeff,'SEM2-unstandardized direct effects.csv',row.names = F)
#######################################################################################################
################FigS7: SEM based on STA_max_EVI ####################################################
#######################################################################################################
##the priori model########
SEM3 = psem(
  lm(STA_max_EVI ~ Adjusted_latitude + phe_var_based_EVI + ydiv + MAT_scale + MAP_scale,data = df),
  lm(phe_var_based_EVI ~ Adjusted_latitude + ydiv + MAT_scale + MAP_scale ,data = df),
  lm(ydiv ~ Adjusted_latitude + MAT_scale + MAP_scale,data = df),
  lm(MAT_scale ~ Adjusted_latitude ,data = df),
  lm(MAP_scale ~ Adjusted_latitude,data = df)
)
summary(SEM3)## Fisher's C = 0.942  with P-value = 0.624  and on 2 degrees of freedom, AIC=48.942    
##the best model########
SEM4 = psem(
  lm(STA_max_EVI ~ Adjusted_latitude + phe_var_based_EVI + MAP_scale,data = df),
  lm(phe_var_based_EVI ~  ydiv + MAT_scale,data = df),
  lm(ydiv ~ MAP_scale,data = df),
  lm(MAT_scale ~ Adjusted_latitude ,data = df),
  lm(MAP_scale ~ Adjusted_latitude,data = df)
)
summary(SEM4)#Fisher's C = 10.805  with P-value = 0.701  and on 14 degrees of freedom, AIC=46.805    
##Standardized and unstandardized direct effects in our final SEM 
coeff = as.data.frame(xtable(summary(SEM4)$coefficients)) %>% select(Response:Std.Estimate)
# write.csv(coeff,'SEM4-unstandardized direct effects.csv',row.names = F)

##Calculate the total effects of plant richness, phenological variability, and climate effects on stability
SEM_color<-c("#b2b2b2","#b2b2b2","#b2b2b2","#009e73","#e69f00","#0072b2")
SEM_effect_size = read_xlsx('SEM effect.xlsx')
SEM_effect_size$`Total effects` = round(SEM_effect_size$`Total effects`,2)
SEM_effect_size$Variables = factor(SEM_effect_size$Variables,
                                   levels =c("Adjusted_latitude","MAT_scale","MAP_scale","ydiv","phe_var_based_EVI"))
SEM_main_effect = ggplot(subset(SEM_effect_size,SEM_effect_size$Type=='main'), 
       aes(x = Variables,y = `Total effects`,fill = Variables))+
  geom_bar(stat ="identity")+         
  scale_fill_manual(values = SEM_color)+
  geom_hline(yintercept= 0 )+ 
  labs(x = " ")+ 
  geom_text(aes(label = `Total effects`),size = 2.5,vjust = -0.25)+ 
  theme_bw()+
  mytheme+
  theme(legend.position = "none")
topptx(SEM_main_effect,filename="SEM_main_effect.pptx",height = 6,width = 6)
SEM_appendix_effect = ggplot(subset(SEM_effect_size,SEM_effect_size$Type=='appendix'), 
       aes(x = Variables,y = `Total effects`,fill = Variables))+
  geom_bar(stat ="identity")+         
  scale_fill_manual(values = SEM_color)+
  geom_hline(yintercept= 0 )+ 
  labs(x = " ")+ 
  geom_text(aes(label = `Total effects`),size = 2.5,vjust = -0.25)+ 
  theme_bw()+
  mytheme+
  theme(legend.position = "none")

