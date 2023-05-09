library(tidyverse)
library(tidymodels)
library(janitor)

dat_df_var <- dat_df %>% 
  select(ID, Age, BMI, back_FF, calf_FF, thigh_FF, Sex, Gruppe) %>% 
  mutate(Gruppe = factor(Gruppe, levels = c(2,1), labels = c("Healthy", "TTNtv")),
         Sex= factor(Sex, levels = c("m", "f"))) %>% 
  rename(Group = Gruppe) %>% 
  pivot_longer(cols = c(back_FF, thigh_FF, calf_FF), names_to = "Muscle", values_to = "FF") %>% 
  mutate(Muscle = factor(Muscle, levels = c("thigh_FF", "back_FF", "calf_FF"), labels = c("Thigh", "Back", "Calf")))


lm_fit <- 
  lm(FF~Age+BMI+Sex+Group+Muscle, data = dat_df_var)

tidy(lm_fit)
set.seed(42)

FF_boot <- 
  dat_df_var %>%  bootstraps(times = 10000)

FF_boot

FF_models <- 
  FF_boot %>% 
  mutate(model= map(splits,~lm(FF~Age+BMI+Sex+Group+Muscle, data =.)),
         coef_info = map(model, tidy))

FF_coefs <- FF_models %>% 
  unnest(coef_info)
FF_coefs

plot_2<- 
  FF_coefs %>% 
  filter(term=="GroupTTNtv") %>% 
  ggplot(aes(estimate))+
  geom_histogram(alpha =0.7, fill ="#001959", bins = 50)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x=expression(Delta*"Fat fraction (%)"),
       title = "Histogram of bootstrap estimates (n=10,000)",
       subtitle = "Assessing the effect of carrying a TTNtv")+
  theme_bw()+
  scale_x_continuous(limits = c(-0.2,5), breaks = seq(0,5,0.5))+
  scale_y_continuous(limits = c(-1,800), breaks = seq(0,800,100))+
  coord_cartesian(clip = "off",expand = F )
plot_2
ggsave("Histogram_bootstrap estimates.png", units = "cm", width = 16, height = 12, dpi = 1200)


int_pctl(FF_models, coef_info)
FF_model_bs_est <- int_pctl(FF_models, coef_info)



FF_aug <- FF_models %>%
  sample_n(200) %>%
  mutate(augmented = map(model, augment)) %>%
  unnest(augmented)

FF_aug

ggplot(FF_aug, aes(Age, FF)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "cyan3") +
  geom_point()

FF_lm_bs <- 
  FF_model_bs_est %>% filter(!term=="(Intercept)")

FF_lm_bs %>% 
ggplot(aes(y=.estimate, ymin = .lower, ymax = .upper, x=term))+
  geom_errorbar(width = .2)+
  geom_point(color = "cyan3", fill= "cyan3", size = 3)+
  geom_hline(aes(yintercept = 0), linetype =2)+
  theme_bw()
FF_model_bs_est

FF_lm_bs <- 
  FF_model_bs_est %>% filter(!term=="(Intercept)") %>% 
  mutate(category = "Bootstrap") %>% 
  select(term, .lower, .estimate, .upper, category)

FF_lm <- tidy(lm_fit) %>% filter(!term=="(Intercept)") %>% 
  mutate(.lower = estimate-std.error*1.96,
         .estimate = estimate,
         .upper = estimate+std.error*1.96,
         category = "Normal")%>% 
  select(term, .lower, .estimate, .upper, category)
FF_lm

FF_combi <-
  bind_rows(FF_lm, FF_lm_bs)

#Plotting and comparing to original
FF_combi <-
  FF_combi %>% mutate(term = fct_recode(term, 
                                          "TTNtv" = "GroupTTNtv",
                                          "Age" = "Age", 
                                          "BMI"= "BMI",
                                          "Women"= "Sexf",
                                          "Calf" = "MuscleCalf",
                                          "Back"= "MuscleBack"))

library(scico)
plot_1<- 
  FF_combi %>% 
  ggplot(aes(y=.estimate, ymin = .lower, ymax= .upper, x=factor(term, levels = c("Calf","Back",  "Women","BMI",  "Age", "TTNtv")), 
             fill = category,group = category))+
  geom_errorbar(position = position_dodge(width = 1), width = 0.4)+
  geom_point(aes(color= category),position = position_dodge(width = 1), size = 3)+
  geom_hline(yintercept = 0, linetype =2)+
  scale_y_continuous(breaks = seq(-5,8,1))+
  scale_color_scico_d(palette= "batlow")+ 
  labs(title = "Normal & bootstrapped confidence intervals",
       y= expression(Delta*"Fat fraction (%)"),
       x="")+
  theme_minimal()+
  theme(text = element_text(family = "serif", color = "black", size = 11), 
        legend.background = element_blank(), legend.position = c(0.9,0.9), legend.title = element_blank(),
        axis.text = element_text(color = "black", family = "serif"))+
  coord_flip()
plot_1
ggsave("Boot_LM.png", units = "cm", dpi = 1000, height = 12, width = 16)


library(cowplot)
cowplot::plot_grid(plot_1, plot_2, nrow = 2, labels = "AUTO")
ggsave("Sup_1.png", units = "cm", dpi = 1000, height = 16, width = 12)
