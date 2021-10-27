library(tidymodels);library(tidyverse); library(scico); library(readxl)
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
                LV_pred = LVIDd/(45.3*BSA^(1/3)-0.03*Age-7.2),
                DCM_time = time_length(interval(start = DCM_date, end = Date_MR), unit = "years"),
                phenotype_now = case_when(
                  DCM_phenotype %in% c("DCM", "HTx") ~ "DCM",
                  LVEF_bp < 45 & LV_pred>1.12 ~ "DCM",
                  LVEF_3D < 45 & LV_pred>1.12 ~ "DCM",
                  LVEF_bp < 50 ~ "HNDC",
                  LVEF_3D < 50 ~ "HNDC",
                  LV_pred>1.17 ~ "HNDC",
                  TRUE~ "Healthy"),
                CM_time = case_when(DCM_time > 0 ~ DCM_time,
                                    phenotype_now %in% c("HNDC", "DCM")~ 0,
                                    T~NA_real_),
                delta_LVEF_3D = LVEF_3D-echoFDC_LVEF)

d <- dat_df %>% 
  select(thigh_ant:s2_gmin, back_FF, thigh_FF, calf_FF) %>%
  scale() %>%
  as_tibble()
d
x<- dat_df %>% select(ID) %>%  bind_cols(d) %>% 
  mutate(ID = fct_recode(ID, CTR01 = "IM01", CTR02 = "IM11",CTR03 ="KMR11", CTR04="KMR19", CTR05="KMR21", CTR06="KMR22", CTR07 = "KMR23", CTR08= "KMR26",  CTR09="KMR28", 
                         CTR10= "KMR29", CTR11= "KMR30",  CTR12="KMR32", CTR13 = "KMR35", CTR14  ="KMR38",  CTR15 ="KMR4", CTR16= "KMR42", CTR17="KMR43", 
                         CTR18="KMR47", CTR19= "KMR48", CTR20= "KMR5", CTR21="KMR50", CTR22="KMR52", CTR23= "KMR7", CTR24= "SBMAC -08", CTR25= "SBMAC-14") )%>% 
  pivot_longer(cols = thigh_ant:calf_FF, names_to = "muscle") %>% 
  mutate(muscle = fct_relevel(muscle, 
                              levels = c("c6_deep",    
                                         "th12_es",     "th12_multi",  
                                         "l4_es",       "l4_multi",    "l4_pm",       
                                         "s2_gmax",     "s2_gmed",     "s2_gmin",
                                         "thigh_ant", "thigh_med","thigh_post", 
                                         "calf_ant","calf_lat","calf_post","calf_latsup","calf_medsup", 
                                         "back_FF" ,    "thigh_FF", "calf_FF")),
         muscle = fct_rev(muscle),
         muscle = fct_relabel(muscle, ~gsub("_", " ", .x)),
         muscle = fct_relabel(muscle, ~gsub("es", "erector spinae", .x)),
         muscle = fct_relabel(muscle, ~gsub("FF", "overall", .x)),
         muscle = fct_relabel(muscle, ~gsub("multi", "multifidus", .x)),
         muscle = fct_relabel(muscle, ~gsub("PM", "psoas major", .x)),
         muscle = fct_relabel(muscle, ~gsub("thigh", "Thigh", .x)),
         muscle = fct_relabel(muscle, ~gsub("c6", "C6", .x)),
         muscle = fct_relabel(muscle, ~gsub("th12", "Th12", .x)),
         muscle = fct_relabel(muscle, ~gsub("l4", "L4", .x)),
         muscle = fct_relabel(muscle, ~gsub("s2", "S2", .x)),
         muscle = fct_relabel(muscle, ~gsub("calf", "Calf", .x)),
         muscle = fct_relabel(muscle, ~gsub("back", "Back", .x))) %>% 
  ggplot()+
  geom_tile(aes(y=muscle, x=ID, fill = value), color = "black")+
  coord_fixed()+
  scale_fill_scico(palette = "devon", direction = -1, begin =  0)+
  #scale_fill_scico(palette = "batlow", direction = 1, begin =  0)+
  #scale_fill_gradient(low = "white", high = "#2C194C")+
  #scale_fill_gradient2(low = "#1A3399", mid ="white",  high = "#7E1900")+
  #scale_fill_scico(palette = "romaO", direction = -1, begin =  0)+

  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 5, title = "Z-score"))+
  labs(x = "", y="", title = "Normalized muscle fat fractions")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 6), legend.title = element_text(size = 8, hjust = 0.1),
        title = element_blank())
x
