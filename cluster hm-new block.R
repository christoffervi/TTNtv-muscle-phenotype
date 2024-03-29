library(tidymodels);library(tidyverse); library(scico); library(readxl); library(lubridate)
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
x<- dat_df %>% filter(row_number()!=55)%>% select(ID) %>%  bind_cols(d%>% filter(row_number()!=55)) %>% 
  mutate(ID = fct_recode(ID, CTR01 = "IM01", CTR02 = "IM11",CTR03 ="KMR11", CTR04="KMR19", CTR05="KMR21", CTR06="KMR22", CTR07 = "KMR23", CTR08= "KMR26",  CTR09="KMR28", 
                         CTR10= "KMR29", CTR11= "KMR30",  CTR12="KMR32", CTR13 = "KMR35", CTR14  ="KMR38",  CTR15 ="KMR4", CTR16= "KMR42", CTR17="KMR43", 
                         CTR18="KMR47", CTR19= "KMR48", CTR20= "KMR5", CTR21="KMR50", CTR22="KMR52", CTR23= "KMR7", CTR24= "SBMAC -08", CTR25= "SBMAC-14") )%>% 
  pivot_longer(cols = thigh_ant:calf_FF, names_to = "muscle") %>% tibble() %>% 
  mutate(muscle = fct_relevel(factor(muscle), 
                              "c6_deep",
                              "th12_es","th12_multi",  
                              "l4_es","l4_multi",    "l4_pm",       
                              "s2_gmax","s2_gmed",     "s2_gmin",
                              "thigh_ant", "thigh_med","thigh_post", 
                              "calf_ant","calf_lat","calf_post","calf_latsup","calf_medsup", 
                              "back_FF" ,"thigh_FF", "calf_FF"),
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
         muscle = fct_relabel(muscle, ~gsub("back", "Back", .x))) 
x  
# Run clustering

ttn_matrix <- x %>% 
  pivot_wider(names_from = muscle, values_from = value) %>% 
  select(!c(1,
            #  'Calf overall','Thigh overall','Back overall' 
  )) %>% 
  as.matrix()
rownames(ttn_matrix) <- x %>% pivot_wider(names_from = muscle, values_from = value) %>% 
  select(ID) %>% pull()

ttn_dendro <- as.dendrogram(
  hclust(d = dist(x = ttn_matrix), 
         method = "ward.D2"#, height = 3
  ))

testhclust <-   hclust(d = dist(x = ttn_matrix), 
                       method = "ward.D2"#, height = 3
)

grp <- cutree(testhclust,k=2) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("ID")

grp<-
  grp %>% rename("vuf"='.') %>% 
  left_join(dat_df %>% 
              mutate(ID = fct_recode(ID, CTR01 = "IM01", CTR02 = "IM11",CTR03 ="KMR11", CTR04="KMR19", CTR05="KMR21", CTR06="KMR22", CTR07 = "KMR23", CTR08= "KMR26",  CTR09="KMR28", 
                                     CTR10= "KMR29", CTR11= "KMR30",  CTR12="KMR32", CTR13 = "KMR35", CTR14  ="KMR38",  CTR15 ="KMR4", CTR16= "KMR42", CTR17="KMR43", 
                                     CTR18="KMR47", CTR19= "KMR48", CTR20= "KMR5", CTR21="KMR50", CTR22="KMR52", CTR23= "KMR7", CTR24= "SBMAC -08", CTR25= "SBMAC-14") 
                     ,
                     alders_racisme = case_when(Age>50~"Føj",
                                                T~"Lækkert")))

grp %>% janitor::tabyl(alders_racisme, vuf)
#ttn_dendro <- as.dendrogram(hclust(d = dist(x = ttn_matrix), method = "average"#, height = 3
#))
#ttn_dendro <- as.dendrogram(hclust(d = dist(x = ttn_matrix), method = "average"#, height = 3
#))
library(dendextend)
# dend <- color_branches(ttn_dendro, k=2)
# dend
# labels_colors(dend) <-
#   rainbow_hcl(3)[sort_levels_values(
#     as.numeric([order.dendrogram(dend)]
#   )]
# 
labels(ttn_dendro)
order.dendrogram(ttn_dendro)
dendro_plot <- ggdendrogram(data = ttn_dendro, rotate = F)#+theme_void()
library(ggdendro)
ggdendrogram(data = dend, rotate = F)#+theme_void()
dendro_plot

ggdendrogram(data = dend, rotate = F)#+theme_void()

x_wide <- x %>% 
  pivot_wider(names_from = muscle, values_from = value) %>% 
  mutate(id = order.dendrogram(ttn_dendro),
         id = factor(ID, levels = labels(ttn_dendro)))
#ID = fct_reorder(ID, id))

x_long <- x_wide %>% 
  pivot_longer(cols = 2:21, names_to = "muscle") %>% 
  mutate(muscle =
           fct_relevel(muscle,"C6 deep",    
                       "Th12 erector spinae",     "Th12 multifidus",  
                       "L4 erector spinae",       "L4 multifidus",    "L4 pm",       
                       "S2 gmax",     "S2 gmed",     "S2 gmin",
                       "Thigh ant", "Thigh med","Thigh post", 
                       "Calf ant","Calf lat","Calf post","Calf latsup","Calf medsup", 
                       "Back overall" ,    "Thigh overall", "Calf overall"))
#x_long
library(scico)
xp<-
  x_long %>% 
  mutate(group = case_when(str_detect(ID, "car")~ "DCM",
                           str_detect(ID, "TTN")~ "TTNtv",
                           
                           T~"Healthy"),
         group = factor(group, levels = c('Healthy','TTNtv','DCM')),
         muscle = fct_rev(muscle)) %>% 
  ggplot(aes(y=muscle, x=id, fill = value))+
  geom_tile(color = "black")+
  geom_point(aes(y=-.8,color = group), shape = "I", size = 8, show.legend = F)+
  coord_fixed()+
  #scale_color_scico_d(palette = "roma", direction = -1)+
  scale_color_manual(values =c("gray79",  "blue", "red"))+
  #scale_fill_scico(palette = "oleron", direction = -1, begin =  0)+
  #scale_fill_gradient2()+
  #scale_fill_scico(palette = "batlow", direction = 1, begin =  0)+
  #scale_fill_gradient(low = "white", high = "#2C194C")+
  #scale_fill_gradient2(low = "#1A3399", mid ="white",  high = "#7E1900")+
  #scale_fill_gradient2(low = "#7E1900", mid ="white",  high = "#1A3399")+
  scale_fill_gradient2(low = scico(1, palette = "bam", begin = 0), mid ="white",  high = scico(1, palette = "bam", begin = 1))+
  
  #scale_fill_scico(palette = "romaO", direction = -1, begin =  0)+
  
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 5, title = "Z-score"))+
  labs(x = "", y="", title = "Normalized muscle fat fractions")+
  theme_void()+
  theme(axis.text.x = element_blank(),
        #element_text(angle = 90, vjust = 0.5, size = 6, family = "Roboto"),
        axis.text.y = element_text(size = 6, family = "Roboto"), 
        legend.title = element_text(size = 8, hjust = 0.1, family = "Roboto"),
        title = element_blank(),
  )
xp

#hclust(dist())
library(patchwork)
dendro_plot+xp
(dendro_plot+
    theme_void()+
    coord_cartesian(expand = F, xlim = c(0,58)))/
  (xp+coord_cartesian(expand = F, xlim = c(0,58)))+
  plot_layout(heights = c(1,4))
ggsave("Clustered_hm.pdf", device = cairo_pdf, width = 16, height = 10, dpi = 3200, units = "cm")

ggsave("Clustered_heatmap_4.png", width = 16, height = 10, dpi = 1200, units = "cm")
ggsave("Clustered_heatmap_4.tiff", compression = "lzw", width = 16, height = 10, dpi = 1200, units = "cm")
tiff("Clustered_heatmap_4.tiff", compression = "lzw", width = 16, height = 10, res = 1200, units = "cm")


x %>% pivot_wider(names_from = muscle, values_from = value) %>% 
  heatmaply::heatmaply(color = scico(n= 50, palette ="devon", direction = -1), 
                       file = "cluster_heatmap.png")
ggsave
x  
library(tidyHeatmap)
xp<-
  x %>% 
  ggplot(aes(y=muscle, x=ID, fill = value))+
  geom_tile(color = "black")+
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
ggsave("heatmap_ftw_4.png", units = "cm", width = 20, height = 8, dpi = 1600)
scico_palette_show()
library(rayshader)  
x
##############
rayshader::plot_gg(xp, height = 4, width = 8, multicore = T, 
                   zoom = 0.6, phi = 30, theta = 45, sunangle = 225)
render_highquality(filename = "HEAT_3D_HQ_4.png")
render_highquality(filename = "HEAT_3D_HQ_5.png", lightdirection = 60)
library(gifski)
render_movie("Heat_movie_1.gif")
####


rayshader::plot_gg(x, width = , multicore = T, zoom = 0.6, phi = 30, theta = 45, sunangle = 225)

rayshader::plot_gg(x, height = 8, width = 20, multicore = T, zoom = 0.6, phi = 30, theta = 45, sunangle = 225)
render_snapshot(filename = "HEAT_3D_2.png")
render_highquality(filename = "HEAT_3D_HQ_3.png", lightdirection = 95, lightaltitude = 60)
snapshot3d("3Dplot_1.png", fmt = "png", width = 800, height = 400)
ggsave("HEAT_3D_FTW__.png", units = "cm", height = 10, width = 20, dpi = 1200)


#######################



x_long %>% 
  mutate(group = if_else(str_detect(ID, "TT"), "TTNtv", "healthy"),
         group = factor(group),
         muscle = fct_rev(muscle)) %>% 
  group_by(id) %>% 
  mutate(m = mean(value)) %>% ungroup() %>% 
  mutate(ids = fct_reorder(id, if_else(group=="TTNtv", m+10,m))) %>% 
  ggplot(aes(y=muscle, x=ids, fill = value))+
  geom_tile(color = "black")+
  geom_point(aes(y=-.8,color = group), shape = "I", size = 8, show.legend = F)+
  coord_fixed()+
  scale_color_manual(values =c("gray79",  "black"))+
  scale_fill_gradient2(low = scico(1, palette = "bam", begin = 0), mid ="white",  high = scico(1, palette = "bam", begin = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 5, title = "Z-score"))+
  labs(x = "", y="", title = "Normalized muscle fat fractions")+
  theme_void()+
  theme(axis.text.x = element_blank(),
        #element_text(angle = 90, vjust = 0.5, size = 6, family = "Roboto"),
        axis.text.y = element_text(size = 6, family = "Roboto"), 
        legend.title = element_text(size = 8, hjust = 0.1, family = "Roboto"),
        title = element_blank(),
  )
