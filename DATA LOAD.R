library(tidyverse);
library(viridis);library(ggridges);library(readr);
library(hrbrthemes);library(scico)

dat_df<-read_csv("~/Desktop/PhD projekter/Projekt 06 - TTNtv muskelinvolvering/R/TTNtv_muscle/TTN_muscle.csv", na = "NA")
dat_df<- dat_df %>% 
  dplyr::mutate(thigh_FF = (thigh_ant_fat+thigh_post_fat+thigh_med_fat)/(thigh_ant_total+thigh_post_total+thigh_med_total)*100,
                calf_FF=  (calf_ant_fat+calf_post_fat+calf_medsup_fat+calf_latsup_fat+calf_lat_fat)/(calf_ant_total+calf_post_total+calf_medsup_total+calf_latsup_total+calf_lat_total)*100,
                s2_FF = (s2_gmax_fat+s2_gmed_fat+s2_gmin_fat)/(s2_gmax_total+s2_gmed_total+s2_gmin_total)*100,
                mf_FF = (l4_mf_fat+th12_mf_fat)/(l4_mf_total+th12_mf_total)*100,
                es_FF = (l4_es_fat+th12_es_fat)/(l4_es_total+th12_es_total)*100,
                l4_FF = (l4_es_fat+l4_mf_fat+l4_pm_fat)/(l4_es_total+l4_mf_total+l4_pm_total)*100,
                th12_FF = (th12_es_fat+th12_mf_fat)/(th12_es_total+th12_mf_total)*100,
                back_FF = (s2_gmax_fat+s2_gmed_fat+s2_gmin_fat+l4_mf_fat+th12_mf_fat+l4_es_fat+th12_es_fat+l4_pm_fat)/(s2_gmax_total+s2_gmed_total+s2_gmin_total+l4_mf_total+th12_mf_total+l4_es_total+th12_es_total+l4_pm_total)*100,
                BSA = 0.20247*(Height/100)^0.725*Weight^0.425,
                LVIDd_index = LVIDd/BSA,
                LV_pred = LVIDd/(45.3*BSA^(1/3)-0.03*Age-7.2))



dat_df_scaled <- dat_df %>% select(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                                   MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex) %>%
  scale() %>% as_tibble() %>% rowwise() %>% 
  mutate(strength = mean(c(MD_L_elbow_flex, MD_L_elbow_ext, MD_L_hip_ext, MD_L_hip_flex, MD_L_ankle_dflex,MD_L_ankle_pflex, MD_L_knee_ext, MD_L_knee_flex,
                           MD_R_elbow_flex, MD_R_elbow_ext, MD_R_hip_ext, MD_R_hip_flex, MD_R_ankle_dflex,MD_R_ankle_pflex, MD_R_knee_ext, MD_R_knee_flex), 
                         na.rm =T),
         knee_strength = mean(c(MD_L_knee_ext, MD_L_knee_flex,MD_R_knee_ext, MD_R_knee_flex),na.rm=T),
         hip_strength = mean(c(MD_L_hip_ext, MD_L_hip_flex,MD_R_hip_ext, MD_R_hip_flex),na.rm=T))
