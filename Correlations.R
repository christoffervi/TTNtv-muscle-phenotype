#CORRELATIONS in TTNtv
library(GGally)
dat_df$bac

dat_df_scaled <- dat_df %>% select(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                                   MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex) %>%
  scale() %>% as_tibble() %>% rowwise() %>% 
  mutate(strength = mean(c(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                           MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex), 
                         na.rm =T),
          knee_strength = mean(c(MD_L_knee_ext, MD_L_knee_flex,MD_R_knee_ext, MD_R_knee_flex),na.rm=T),
         hip_strength = mean(c(MD_L_hip_ext, MD_L_hip_flex,MD_R_hip_ext, MD_R_hip_flex),na.rm=T))


p <- dat_df %>% mutate(strength = dat_df_scaled$strength) %>%  
  select(back_FF, thigh_FF, calf_FF, Age, BMI,LVEF_bp,blood_proBNP, strength, `ipaq-7d-mettotal`, Gruppe) %>% 
  mutate(Gruppe = factor(Gruppe, labels = c("TTNtv", "Healthy")),
         blood_proBNP = log2(blood_proBNP)) %>% 
  rename(proBNP=blood_proBNP,
         LVEF = LVEF_bp,
         Back = back_FF,
         Thigh = thigh_FF,
         Calf = calf_FF)

library(cowplot)
#############################################
# INFORMATION PLOT KENDALL
#############################################
  ggpairs(p,
          ggplot2::aes(color =Gruppe), 
          columnLabels = c("Back", "Thigh", "Calf", "Age","BMI", "LVEF","pro-BNP","Strength", "METS", "Group"),
          lower = list(continuous = "smooth",combo = wrap("facethist", binwidth = 1)),
          upper = list(continuous = wrap("cor", size = 2.6, method = "kendall", digits =2, alpha =1), combo = "dot_no_facet"),
          axisLabels = "none",
          #proportions = c(2,2,2,1,1,1,1,1,1),
          switch = "y"
         )+
    scale_fill_scico_d(palette = "batlow", alpha = 0.5)+
    scale_color_scico_d(palette = "batlow", alpha = 0.8)+
    theme(text = element_text(family = "serif", size = 10),
          axis.text.x = element_text(angle = 45),
          axis.text.y = element_text())
ggsave("correlation_kendall.png", units = "cm", height = 24, width =24, dpi = 1200)

#############################################
# INFORMATION PLOT pearson
#############################################
ggpairs(p,
        ggplot2::aes(color =Gruppe), 
        columnLabels = c("Back", "Thigh", "Calf", "Age","BMI", "LVEF","pro-BNP","Strength", "METS", "Group"),
        lower = list(continuous = "smooth",combo = wrap("facethist", binwidth = 1)),
        upper = list(continuous = wrap("cor", size = 2.6, method = "pearson", digits =2, alpha =1), combo = "dot_no_facet"),
        axisLabels = "none",
        #proportions = c(2,2,2,1,1,1,1,1,1),
        switch = "y"
)+
  scale_fill_scico_d(palette = "batlow", alpha = 0.5)+
  scale_color_scico_d(palette = "batlow", alpha = 0.8)+
  theme(text = element_text(family = "serif", size = 10),
        axis.text.x = element_text(angle = 45),
        axis.text.y = element_text())
ggsave("correlation_pearson.png", units = "cm", height = 24, width =24, dpi = 1200)

#############################################
# INFORMATION PLOT spearman
#############################################
ggpairs(p,
        ggplot2::aes(color =Gruppe), 
        columnLabels = c("Back", "Thigh", "Calf", "Age","BMI", "LVEF","pro-BNP","Strength", "METS", "Group"),
        lower = list(continuous = "smooth",combo = wrap("facethist", binwidth = 1)),
        upper = list(continuous = wrap("cor", size = 2.6, method = "spearman", digits =2, alpha =1), combo = "dot_no_facet"),
        axisLabels = "none",
        #proportions = c(2,2,2,1,1,1,1,1,1),
        switch = "y"
)+
  scale_fill_scico_d(palette = "batlow", alpha = 0.5)+
  scale_color_scico_d(palette = "batlow", alpha = 0.8)+
  theme(text = element_text(family = "serif", size = 10),
        axis.text.x = element_text(angle = 45),
        axis.text.y = element_text())
ggsave("correlation_spearman.png", units = "cm", height = 24, width =24, dpi = 1200)


cor(dat_df_ttn$thigh_FF,dat_df_ttn$LVEF_bp, use = "pairwise.complete.obs", method = "spearman")







library(tidyverse)

p <- dat_df %>% mutate(strength = dat_df_scaled$strength, knee_strength = dat_df_scaled$knee_strength,
                       hip_strength = dat_df_scaled$hip_strength) %>%  
  select(back_FF, thigh_FF, calf_FF, Age, BMI,LVEF_bp, blood_proBNP, strength, `ipaq-7d-mettotal`, Gruppe) %>% 
  mutate(Gruppe = factor(Gruppe, labels = c("TTNtv", "Healthy")),
         blood_proBNP = log2(blood_proBNP)) %>% 
  rename(proBNP=blood_proBNP,
         LVEF = LVEF_bp,
         Back = back_FF,
         Thigh = thigh_FF,
         Calf = calf_FF,
         Strength = strength,
         METS = `ipaq-7d-mettotal`)


library(RColorBrewer);library(Redmonder); library(pals)
p1 <- p %>%  filter(Gruppe=="TTNtv")
ggcorr(p1, geom= "tile", nbreaks = 8, 
       method = c("pairwise.complete.obs", "kendall"), 
       label = T, label_round = 2, label_color = "black",
       size=4)+
  scale_fill_brewer(palette = "RdBu", direction = -1)
   # scale_fill_scico(palette = "batlow", direction = -1, alpha = 0.85)
  #scale_fill_scico(palette = "roma", begin = 0.75, end = 0)
ggsave("correlation_Kendall_step.png", units = "cm", height = 20, width =20, dpi = 800)

ggcorr(p1, geom= "tile",
       method = c("pairwise.complete.obs", "pearson"), 
       label = T, label_round = 2, label_color = "black",
       size=4, high = "#7E1900", low = "#1A3399", mid = "#EAEDE9")
  scale_fill_brewer(palette = "RdBu", direction = -1)
ggsave("correlation_Pearson_step_3.png", units = "cm", height = 20, width =20, dpi = 800)

ggcorr(p, geom= "blank", 
       method = c("pairwise.complete.obs", "spearman"), 
       label = T, hjust=0.75, label_round = 2, size =5)+
  geom_point(size = 14, aes(color = coefficient >0, alpha= abs(coefficient) > 0.5), show.legend = F) +
  scale_alpha_manual(values = c("TRUE" = 0.4, "FALSE" = 0)) +
  scale_color_manual(values = c("blue", "red"))
ggsave("correlation_4_cont.png", units = "cm", height = 20, width =20, dpi = 1200)
label_round = 2, label_color = "black",
       size=4)
 pals::ocean.balance(3)
scico(2, palette = "batlow")

