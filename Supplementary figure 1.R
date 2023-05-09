dat_df %>% filter(Gruppe==1) %>% 
  group_by(ID) %>% 
  mutate(lvef = mean(c(LVEF_bp, LVEF_3D), na.rm=T)) %>% ungroup() %>% 
  mutate(lvef1 = case_when(is.na(echoFDC_LVEF)~lvef,
                           T~echoFDC_LVEF),
         phen = case_when(HTx_c == 1~"HTx",
                          T~phenotype_now),
         phen = factor(phen, levels = c("Healthy", "HNDC", "DCM", "HTx"))) %>% 
  ggplot2::ggplot()+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_point(aes(x=lvef1, y= lvef-lvef1,fill = phen), shape = 21, size = 4, alpha = .8)+
  scale_y_continuous(limits = c(-20,50), breaks = seq(-50,60,5))+
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60,5))+
  scale_fill_scico_d(palette = "devon", direction = -1)+
  labs(#title = "Left ventricular ejection fraction",
    #caption = "CAG",
    x = "LVEF at first evaluation",
    y = "Change in LVEF from initial evaluation")+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color = "gray79"),
        axis.text = element_text(size = 10, family = "Roboto"),
        plot.caption =element_text(size = 10, family = "Roboto"),
        axis.title = element_text(size = 10, family = "Roboto"),
        title = element_text(size = 12, family = "Roboto"),
        legend.title = element_blank())

ggsave("LVEF_CAG.png", units = "cm", height =  12*.75, width = 18*.75, dpi = 1800)


########

dat_df %>% filter(Gruppe==1) %>%
  group_by(ID) %>% 
  mutate(lvef = mean(c(LVEF_bp, LVEF_3D), na.rm=T)) %>% ungroup() %>% 
  mutate(lvef1 = case_when(is.na(echoFDC_LVEF)~lvef,
                           T~echoFDC_LVEF),
         phen = case_when(HTx_c == 1~"HTx",
                          T~phenotype_now),
         phen = factor(phen, levels = c("Healthy", "HNDC", "DCM", "HTx"))) %>% 
  ggplot2::ggplot(aes(x=phen, y= lvef,fill = phen))+
  geom_half_point(shape = 21, size = 4, alpha = .8, show.legend = F)+
  #geom_half_violin(alpha = 0.1, show.legend = F)+
  scale_y_continuous(limits = c(0,60), breaks = seq(-50,60,5))+
  scale_fill_scico_d(palette = "devon", direction = -1)+
  labs(#title = "Left ventricular ejection fraction",
    #caption = "CAG",
    x = "",
    y = "Current LVEF")+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color = "gray79"),
        axis.text = element_text(size = 10, family = "Roboto"),
        plot.caption =element_text(size = 10, family = "Roboto"),
        axis.title = element_text(size = 10, family = "Roboto"),
        title = element_text(size = 12, family = "Roboto"),
        legend.title = element_blank())

ggsave("LVEF_CAG_2.png", units = "cm", height =  12*.75, width = 18*.75, dpi = 1800)
