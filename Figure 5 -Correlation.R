p <- dat_df %>% mutate(strength = dat_df_scaled$strength, knee_strength = dat_df_scaled$knee_strength,
                       hip_strength = dat_df_scaled$hip_strength) %>%  
  select(back_FF, thigh_FF, calf_FF, Age, BMI,LVEF_bp, blood_proBNP, strength, `ipaq-7d-mettotal`, Gruppe) %>% 
  mutate(Gruppe = factor(Gruppe, labels = c("TTNtv", "Healthy", "DCM")),
         blood_proBNP = log2(blood_proBNP)) %>% 
  rename(proBNP=blood_proBNP,
         LVEF = LVEF_bp,
         Back = back_FF,
         Thigh = thigh_FF,
         Calf = calf_FF,
         Strength = strength,
         METS = `ipaq-7d-mettotal`)

p1 <- p %>%  filter(Gruppe=="TTNtv")

GGally::ggcorr(p1, geom= "tile",
       method = c("pairwise.complete.obs", "pearson"), 
       label = T, label_round = 2, label_color = "black",
       size=4, high = "#7E1900", low = "#1A3399", mid = "#EAEDE9")


ggsave("correlation_Pearson_step_3.png", units = "cm", height = 20, width =20, dpi = 800)
