fig2_df <- dat_df %>% filter(Gruppe %in% c(1,2))
summary.lm(lm(back_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy"))) )) %>% broom::tidy() 
summary.lm(lm(thigh_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy"))) )) %>% broom::tidy() 
summary.lm(lm(calf_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy"))) )) %>% broom::tidy()

p1 <- fig2_df %>% 
  pivot_longer(cols = c(thigh_FF,calf_FF, s2_FF, c6_deep, l4_FF, th12_FF), names_to = "muscle") %>%  
  
  mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy")),
         muscle = factor(muscle, 
                         levels = c("calf_FF", "thigh_FF","s2_FF", "l4_FF","th12_FF", "c6_deep" ),
                         labels = c("Calf", "Thigh", "S2", "L4", "Th12", "C6")))%>%
  ggplot(aes(x = value, y=muscle, fill = group))+
  ggridges::geom_density_ridges(alpha = 0.5, jittered_points=T,point_alpha=2,point_shape=21, 
                      #bandwith = 0.1,
                      #rel_min_height = 0.01,
                      #scale = .5,
                      #position = "raincloud"
                      #position = position_points_jitter(width = 0.05, height = 0), point_shape = "|",
                      seed = 42)+
  stat_summary(color = "black", fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3, shape = 23, alpha = 1, stroke = 1, show.legend = F)+
  annotate("text", x=50, y=6.25, label = paste0("p= ", round(summary.lm(lm(c6_deep~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=5.25, label = paste0("p= ", round(summary.lm(lm(th12_FF~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=4.25, label = paste0("p= ", round(summary.lm(lm(l4_FF~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=3.25, label = paste0("p= ", round(summary.lm(lm(s2_FF~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=2.25, label = paste0("p= ", round(summary.lm(lm(thigh_FF~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=1.25, label = paste0("p= ", round(summary.lm(lm(calf_FF~Gruppe+Age+BMI+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  scale_fill_scico_d(palette = "batlow")+
  guides(color = F)+
  scale_x_continuous(name = "Fat fraction (%)", limits = c(0,55), breaks = seq(0,60,10))+
  labs(title = "Muscle groups of the back and leg", x = "Fat fraction in %", caption = "p corrected for age, BMI and sex")+
  ggridges::theme_ridges(font_family = "Roboto")+theme(legend.position = c(0.9,.9), legend.background = element_blank(), legend.title = element_blank())
p1



#########

fig2_df <- dat_df %>% filter(Gruppe %in% c(1,2)) %>% filter(HTx_c!=1)
p1 <- fig2_df %>% 
  pivot_longer(cols = c(thigh_FF,calf_FF, s2_FF, c6_deep, l4_FF, th12_FF), names_to = "muscle") %>%  
  
  mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy")),
         muscle = factor(muscle, 
                         levels = c("calf_FF", "thigh_FF","s2_FF", "l4_FF","th12_FF", "c6_deep" ),
                         labels = c("Calf", "Thigh", "S2", "L4", "Th12", "C6")))%>%
  ggplot(aes(x = value, y=muscle, fill = group))+
  ggridges::geom_density_ridges(alpha = 0.5, jittered_points=T,point_alpha=2,point_shape=21, 
                                #bandwith = 0.1,
                                #rel_min_height = 0.025,
                                #scale = .5,
                                #position = "raincloud"
                                #position = position_points_jitter(width = 0.05, height = 0), point_shape = "|",
                                seed = 42)+
  stat_summary(color = "black", fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3, shape = 23, alpha = 1, stroke = 1, show.legend = F)+
  annotate("text", x=50, y=6.25, label = paste0("p= ", round(summary.lm(lm(c6_deep~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=5.25, label = paste0("p= ", round(summary.lm(lm(th12_FF~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=4.25, label = paste0("p= ", round(summary.lm(lm(l4_FF~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=3.25, label = paste0("p= ", round(summary.lm(lm(s2_FF~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=2.25, label = paste0("p= ", round(summary.lm(lm(thigh_FF~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=1.25, label = paste0("p= ", round(summary.lm(lm(calf_FF~Gruppe+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  scale_fill_scico_d(palette = "batlow")+
  guides(color = F)+
  scale_x_continuous(name = "Fat fraction (%)", limits = c(0,55), breaks = seq(0,60,10))+
  labs(title = "Muscle groups of the back and leg", 
       x = "Fat fraction in %", 
       caption = "P corrected for age, BMI and sex \n Sensitivity analysis excluding transplanted patients")+
  ggridges::theme_ridges(font_family = "Roboto")+theme(legend.position = c(0.9,.9), legend.background = element_blank(), legend.title = element_blank())
p1
ggsave(filename = 'figure2_sens.pdf', device = cairo_pdf , height = 24, width = 24, units = "cm", dpi =3000)





#########

fig2_df <- dat_df %>% filter(Gruppe %in% c(1,3))
fig2_dfp <- dat_df %>% filter(Gruppe %in% c(1,2#,3
                                            )
                              ) %>% 
  mutate(Gruppe = factor(Gruppe),
         lvef = if_else(is.na(lvef),NA,lvef))
#p1 <- 
  fig2_df %>% 
  pivot_longer(cols = c(thigh_FF,calf_FF, s2_FF, c6_deep, l4_FF, th12_FF), names_to = "muscle") %>%  
  
  mutate(group = factor(Gruppe, labels = c("TTNtv", "DCM")),
         muscle = factor(muscle, 
                         levels = c("calf_FF", "thigh_FF","s2_FF", "l4_FF","th12_FF", "c6_deep" ),
                         labels = c("Calf", "Thigh", "S2", "L4", "Th12", "C6")))%>%
  ggplot(aes(x = value, y=muscle, fill = group))+
  ggridges::geom_density_ridges(alpha = 0.5, jittered_points=F,point_alpha=2,point_shape=21, 
                                #bandwith = 0.1,
                                rel_min_height = 0.01,
                                #scale = 1,
                                position = "raincloud",
                                #position = position_points_jitter(width = 0.05, height = 0), point_shape = "|",
                                seed = 42)+
  stat_summary(color = "black", fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3, shape = 23, alpha = 1, stroke = 1, show.legend = F)+
  annotate("text", x=50, y=6.25, label = paste0("p= ", round(summary.lm(lm(c6_deep~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  annotate("text", x=50, y=5.25, label = paste0("p= ", round(summary.lm(lm(th12_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  annotate("text", x=50, y=4.25, label = paste0("p= ", round(summary.lm(lm(l4_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  annotate("text", x=50, y=3.25, label = paste0("p= ", round(summary.lm(lm(s2_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  annotate("text", x=50, y=2.25, label = paste0("p= ", round(summary.lm(lm(thigh_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  annotate("text", x=50, y=1.25, label = paste0("p= ", round(summary.lm(lm(calf_FF~Gruppe+rms::rcs(Age,3)+rms::rcs(BMI,3)+Sex, data = fig2_dfp ))$coefficients[3,4],3)))+
  scale_fill_scico_d(palette = "batlow", end = .5)+
  guides(color = F)+
  scale_x_continuous(name = "Fat fraction (%)", limits = c(0,55), breaks = seq(0,60,10))+
  labs(title = "Muscle groups of the back and leg", 
       x = "Fat fraction in %", y = 'Muscle location',
       caption = "P corrected for age, BMI and sex")+
  ggridges::theme_ridges(font_family = "Roboto")+theme(legend.position = c(0.9,.9), legend.background = element_blank(), legend.title = element_blank())
p1

ggsave(filename = 'figure2_dcm_controls.pdf', device = cairo_pdf , height = 24, width = 24, units = "cm", dpi =3000)






#########

fig2_df <- dat_df %>% filter(Gruppe %in% c(1,2,3))
p1 <- fig2_df %>% 
  pivot_longer(cols = c(thigh_FF,calf_FF, s2_FF, c6_deep, l4_FF, th12_FF), names_to = "muscle") %>%  
  
  mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM")),
         muscle = factor(muscle, 
                         levels = c("calf_FF", "thigh_FF","s2_FF", "l4_FF","th12_FF", "c6_deep" ),
                         labels = c("Calf", "Thigh", "S2", "L4", "Th12", "C6")))%>%
  ggplot(aes(x = value, y=muscle, fill = group))+
  ggridges::geom_density_ridges(alpha = 0.5, jittered_points=T,point_alpha=2,
                                point_shape=21, 
                                #bandwith = 0.1,
                                #rel_min_height = 0.01,
                                #scale = .5,
                                #position = "raincloud"
                                #position = position_points_jitter(width = 0.05, height = 0), point_shape = "|",
                                seed = 42)+
  stat_summary(color = "black", fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3, shape = 23, alpha = 1, stroke = 1, show.legend = F)+
  annotate("text", x=50, y=6.25, label = paste0("p= ", round(summary.lm(lm(c6_deep~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=5.25, label = paste0("p= ", round(summary.lm(lm(th12_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=4.25, label = paste0("p= ", round(summary.lm(lm(l4_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=3.25, label = paste0("p= ", round(summary.lm(lm(s2_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=2.25, label = paste0("p= ", round(summary.lm(lm(thigh_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  annotate("text", x=50, y=1.25, label = paste0("p= ", round(summary.lm(lm(calf_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df ))$coefficients[2,4],3)))+
  scale_fill_scico_d(palette = "batlow")+
  guides(color = F)+
  scale_x_continuous(name = "Fat fraction (%)", limits = c(0,55), breaks = seq(0,60,10))+
  labs(title = "Muscle groups of the back and leg", x = "Fat fraction in %", caption = "p corrected for age, BMI and sex")+
  ggridges::theme_ridges(font_family = "Roboto")+theme(legend.position = c(0.9,.9), legend.background = element_blank(), legend.title = element_blank())
p1




summary.lm(lm(c6_deep~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df%>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy()
summary.lm(lm(th12_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df%>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy() 
summary.lm(lm(l4_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df%>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy()
summary.lm(lm(s2_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df%>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy()
summary.lm(lm(back_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy() 
summary.lm(lm(thigh_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy() 
summary.lm(lm(calf_FF~group+rms::rcs(Age,4)+rms::rcs(BMI,4)+Sex, data = fig2_df %>% mutate(group = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM"))) )) %>% broom::tidy()
  