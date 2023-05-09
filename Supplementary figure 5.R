library(tidyverse); library(patchwork)


x1<-
  dat_df %>% #lm(back_FF~Age, data = .) %>% 
  #broom::augment() %>% 
  ggplot(aes(x=Age))+
  geom_point(aes(y=back_FF, color = factor(Gruppe)), size = 3, show.legend = F)+ 
  geom_smooth(aes(y=back_FF),method = "lm", color = scico(1, palette = "batlow", begin = .5))+
  scale_color_scico_d(palette = "batlow")+
  scale_x_continuous(breaks = seq(0,80,5))+
  scale_y_continuous(breaks = seq(0,100,5))+
  theme_classic(base_family = "Roboto")+
  geom_richtext(aes(x= 28, y = 30, label= "**R^2 = 0.48**"), family = "Roboto",
                label.color = NA)+
  labs(y = "Fat-fraction (%)",
       title = "Back muscles")+
  theme(axis.text =  element_markdown(color = "black"))+
  coord_cartesian(xlim = c(20,81), y= c(0,36), expand = F)


x2<-
  dat_df %>% #lm(back_FF~Age, data = .) %>% 
  #broom::augment() %>% 
  ggplot(aes(x=Age))+
  geom_point(aes(y=thigh_FF, color = factor(Gruppe)), size = 3, show.legend = F)+ 
  geom_smooth(aes(y=thigh_FF),method = "lm", color = scico(1, palette = "batlow", begin = .5))+
  scale_color_scico_d(palette = "batlow")+
  scale_x_continuous(breaks = seq(0,80,5))+
  scale_y_continuous(breaks = seq(0,100,5))+
  theme_classic(base_family = "Roboto")+
  geom_richtext(aes(x= 28, y = 30, label= "**R^2 = 0.24**"), family = "Roboto",
                label.color = NA)+
  labs(y = "Fat-fraction (%)",
       title = "Thigh muscles")+
  theme(axis.text =  element_markdown(color = "black"))+
  coord_cartesian(xlim = c(20,81), y= c(0,36), expand = F)

x3<-
  dat_df %>% 
  mutate(Gruppe= factor(Gruppe, labels = c("TTNtv", "Control"))) %>% 
  ggplot(aes(x=Age))+
  geom_point(aes(y=calf_FF, color = factor(Gruppe)), size = 3, show.legend = T)+ 
  geom_smooth(aes(y=calf_FF),method = "lm", color = scico(1, palette = "batlow", begin = .5))+
  scale_color_scico_d(palette = "batlow")+
  scale_x_continuous(breaks = seq(0,80,5))+
  scale_y_continuous(breaks = seq(0,100,5))+
  theme_classic(base_family = "Roboto")+
  geom_richtext(aes(x= 28, y = 30, label= "**R^2 = 0.17**"), family = "Roboto",
                label.color = NA)+
  labs(y = "Fat-fraction (%)",
       title = "Calf muscles")+
  theme(axis.text =  element_markdown(color = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(family = "Roboto"))+
  coord_cartesian(xlim = c(20,81), y= c(0,36), expand = F)
x1/x2/x3

ggsave("supp5.png", dpi = 1200, units = "cm", width = 12, height = 12*2.1)
