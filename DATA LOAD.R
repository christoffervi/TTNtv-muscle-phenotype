library(tidyverse);
library(viridis);library(ggridges);library(readr);
library(hrbrthemes);library(scico)

dat_df<-read_csv("~/Dropbox/10 R/TTNtv-muscle-phenotype/ttntv.csv", na = "NA")



dat_df_scaled <- dat_df %>% select(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                                   MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex) %>%
  scale() %>% as_tibble() %>% rowwise() %>% 
  mutate(strength = mean(c(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                           MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex), 
                         na.rm =T),
         knee_strength = mean(c(MD_L_knee_ext, MD_L_knee_flex,MD_R_knee_ext, MD_R_knee_flex),na.rm=T),
         hip_strength = mean(c(MD_L_hip_ext, MD_L_hip_flex,MD_R_hip_ext, MD_R_hip_flex),na.rm=T))
