library(tidyverse);library(viridis);library(ggridges);library(readxl);library(hrbrthemes);library(scico)
dat_df<-read_excel("L:/LovbeskyttetMapper/TTNtv muscle phenotype/Resultater/Data for R.xlsx", na = "NA")
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


lm(back_FF~Gruppe, data = dat_df) %>% broom::tidy()


lm(thigh_FF~Gruppe+Age+BMI+Sex, data = dat_df %>% filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))) %>% broom::tidy(conf.int=T)


lm(back_FF~Gruppe+Age+BMI+Sex, data = dat_df %>% filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))) %>% broom::tidy(conf.int=T)


lm(calf_FF~Gruppe+Age+BMI+Sex, data = dat_df %>% filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))) %>% broom::tidy()


rbind(
lm(thigh_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex, data = 
                dat_df %>% 
                filter(Gruppe %in% c(1,3)) %>% 
                mutate(Gruppe = factor(Gruppe))
              ) %>% broom::tidy(conf.int=T) %>% mutate(muscle = 'thigh'),
lm(s2_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex, data = 
     dat_df %>% 
     filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))
) %>% broom::tidy(conf.int=T) %>% mutate(muscle = 'S2'),
lm(l4_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex, data = 
     dat_df %>% 
     filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))
) %>% broom::tidy(conf.int=T) %>% mutate(muscle = 'l4'),
lm(calf_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex, data = 
     dat_df %>% 
     filter(Gruppe %in% c(1,3)) %>% 
     mutate(Gruppe = factor(Gruppe))
) %>% broom::tidy(conf.int=T) %>% mutate(muscle = 'calf')) %>% 
  filter(term=="Gruppe3") %>% 
  ggplot(aes(x=estimate, xmin = conf.low, xmax = conf.high, y=muscle))+
  geom_errorbar(width =0)+
  geom_point(size = 4)+
  geom_vline(aes(xintercept = 0))+
  theme_classic()








dat_df %>% select(ends_with("FF"), BMI, Gruppe, Age,Sex) %>% 
  filter(Gruppe %in% c(1,2,3)) %>% 
  mutate(Gruppe = factor(Gruppe)) %>% 
  pivot_longer(ends_with("FF"), names_to = 'muscle', values_to = 'FF') %>% 
  lm(FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex,data = . ) %>% 
  broom::tidy()

dat_df %>% select(c(thigh_FF, calf_FF, back_FF), BMI, Gruppe, Age,Sex) %>% 
  filter(Gruppe %in% c(1,2,3)) %>%
  mutate(Gruppe = factor(Gruppe)) %>% 
  pivot_longer(c(thigh_FF, calf_FF, back_FF), names_to = 'muscle', values_to = 'FF') %>% 
  lm(FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI, 3)+Sex,data = . ) %>% 
  broom::tidy(conf.int=T)

library(lme4)
model <- 
  dat_df %>% 
  select(ends_with("FF"), BMI, Gruppe, Age,Sex, ID) %>% 
  filter(Gruppe %in% c(1,2,3)) %>% 
  mutate(Gruppe = factor(Gruppe)) %>% 
  pivot_longer(ends_with("FF"), names_to = 'muscle', values_to = 'FF') %>% 
  lmer(FF~Gruppe+Age+BMI+Sex+(Gruppe|ID),data = . )
anova(model, test = "Chisq")
fixef(model)
confint(model)
  lmer(composite ~ valsartan+comp2b+mlvwtb_z+puberty+
                lvef_cmrb+bmib+ageb+gender+(1 | site), 
              data = vanish_2)

# Extract the fixed effects estimates
fixed_effects <- fixef(model)





dat_df<- dat_df %>% 
  dplyr::mutate(thigh_ant_fat,
                thigh_post_fat,
                thigh_med_fat)/(thigh_ant_total+thigh_post_total+thigh_med_total)*100,
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