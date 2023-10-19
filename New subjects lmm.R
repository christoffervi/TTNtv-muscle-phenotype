library(tidyverse)
library(tidymodels)
library(janitor)

dat_df_var <- dat_df %>% 
  select(ID, Age, BMI, back_FF, calf_FF, thigh_FF, Sex, Gruppe) %>% 
  mutate(gruppe = factor(Gruppe, levels = c(1,2,3), labels = c("TTNtv","Healthy", "Other DCM")),
         sex= factor(Sex, levels = c("m", "f"))) %>% 
  rename(group = gruppe,
         age =Age,
         bmi=BMI) %>% 
  pivot_longer(cols = c(back_FF, thigh_FF, calf_FF), names_to = "muscle", values_to = "FF") %>% 
  mutate(muscle = factor(muscle, levels = c("thigh_FF", "back_FF", "calf_FF"), labels = c("Thigh", "Back", "Calf")))

dat_df_var <- dat_df %>% 
  select(ID, Age, BMI, contains("_FF"), Sex, Gruppe) %>% 
  mutate(gruppe = factor(Gruppe, levels = c(2,1,3), labels = c("Healthy","TTNtv", "Other DCM")),
         sex= factor(Sex, levels = c("m", "f"))) %>% 
  rename(group = gruppe,
         age =Age,
         bmi=BMI) %>% 
  pivot_longer(cols = contains("_FF"), names_to = "muscle", values_to = "FF") %>% 
  mutate(muscle = factor(muscle))


dat_df_var <- dat_df %>% 
  select(ID, Age, BMI, contains("_FF"), Sex, Gruppe) %>% 
  mutate(gruppe = factor(Gruppe, levels = c(1,2,3), labels = c("TTNtv","Healthy", "Other DCM")),
         sex= factor(Sex, levels = c("m", "f"))) %>% 
  rename(group = gruppe,
         age =Age,
         bmi=BMI) %>% 
  pivot_longer(cols = contains("_FF"), names_to = "muscle", values_to = "FF") %>% 
  mutate(muscle = factor(muscle))

lm_fit <- 
  lm(FF~age+rms::rcs(bmi,3)+sex+group+muscle, data = dat_df_var)

tidy(lm_fit)
set.seed(42)

FF_boot <- 
  dat_df_var %>%  bootstraps(times = 10000, strata = group)

FF_boot

FF_models <- 
  FF_boot %>% 
  mutate(model= map(splits,~lm(FF~rms::rcs(age,3)+rms::rcs(bmi,3)+sex+group+muscle, data =.)),
         coef_info = map(model, tidy))


FF_models <- 
  FF_boot %>% 
  mutate(model= map(splits,~lme4::lmer(FF~group+rms::rcs(age,3)+rms::rcs(bmi,3)+sex+(1|muscle)+(1|ID), data =.)),
         coef_info = map(model, broom.mixed::tidy))


FF_coefs <- FF_models %>% 
  unnest(coef_info)

FF_coefs %>% select(id, effect, group, term, estimate, std.error, statistic) %>% 
  write_csv2("boot_lmermodel2.csv")

FF_coefs <- read_csv2('boot_lmermodel.csv')
plot_2<- 
  FF_coefs %>% 
  filter(term %in% c("groupHealthy","groupOther DCM")) %>% 
  ggplot(aes(estimate, group = term, fill = term))+
  geom_histogram(alpha =0.7,  bins = 50)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x=expression(Delta*"Fat fraction (%)"),
       title = "Histogram of bootstrap estimates (n=10,000)",
       subtitle = "Assessing the effect of carrying a TTNtv")+
  theme_bw()+
  #scale_x_continuous(limits = c(-0.2,5), breaks = seq(0,5,0.5))+
  #scale_y_continuous(limits = c(-1,800), breaks = seq(0,800,100))+
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

ggplot(FF_aug, aes(age, FF)) +
  geom_line(aes(y = .fitted, group = group), alpha = .2, col = "cyan3") +
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



FF_coefs %>% 
  filter(term %in% c("groupHealthy","groupOther DCM", "sexf"
                     )
         ) %>% 
  mutate(term = factor(term, levels=  c("groupHealthy","groupOther DCM", "sexf"),
                       labels = c("Healthy controls <br> vs <br> TTNtv", 
                                  "Other DCM <br> vs <br> TTNtv", "Females  <br> vs <br> Males"))) %>% 
  ggplot(aes(x=estimate, group = term, fill = term, y= term))+
  ggdist::stat_dist_slabinterval(show.legend = F, scale =.5)+
  scale_fill_manual(values = c("#F9CCF9","#808133",'#800020'))+
  geom_point(pch = "|", size = 2, position = position_nudge(y = -.15), show.legend = F, alpha = .1)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x=expression(Delta*"Fat fraction (%)"),
       title = "Linear mixed regression model of muscle fat fraction",
       subtitle = "Bootstrap distributions (n= 10,000)",
       caption = "Fixed effects include age, BMI and sex. Investigated muscle-group and patient ID were included as random effects"
       )+
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_markdown(family = "Roboto", color = "black"),
        plot.subtitle = element_markdown(family = "Roboto", color = "black"),
        plot.caption = element_markdown(family = "Roboto", color = "black", size = 6),
        axis.text.x = element_markdown(family = "Roboto", color = "black"),
  axis.text.y = element_markdown(family = "Roboto", color = "black", hjust = .5),
  axis.title.x = element_text(family = "Roboto", color = "black"),
  axis.title.y = element_blank(),
  panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  scale_x_continuous(breaks = seq(-5,5,0.5))

ggsave(filename = 'boot_lmm_point.pdf', device = cairo_pdf , height = 12, width = 18, units = "cm", dpi =3000)

ggsave("Boot_LMM.png", units = "cm", dpi = 1000, height = 12, width = 18)





FF_coefs %>% group_by(term) %>% 
summarise(
  pooled_estimate = mean(estimate),
  within_variance = mean(std.error^2),
  between_variance = var(estimate),
  total_variance = within_variance + (1 + 1/length(unique(FF_coefs$id))) * between_variance,  # Rubin's formula, assuming 5 imputations
  pooled_se = sqrt(total_variance),
  .est = exp(pooled_estimate),
  .lower = (pooled_estimate - 1.96 * pooled_se),
  .upper = (pooled_estimate + 1.96 * pooled_se),
  z_value = pooled_estimate / pooled_se,
  p = 2 * (1 - pnorm(abs(z_value)))   # two-tailed p-value
  
)










##################



FF_coefs %>% 
  filter(term %in% c("groupHealthy","groupOther DCM", "sexf"
  )
  ) %>% 
  mutate(term = factor(term, levels=  c("groupHealthy","groupOther DCM", "sexf"),
                       labels = c("Healthy controls <br> vs <br> TTNtv", 
                                  "Other DCM <br> vs <br> TTNtv", "Females  <br> vs <br> Males"))) %>% 
  ggplot(aes(x=estimate, group = term, fill = term, y= term))+
  ggdist::stat_dist_slabinterval(show.legend = F, scale =.5)+
  scale_fill_manual(values = c("#F9CCF9","#808133",'#800020'))+
  geom_point(pch = "|", size = 2, position = position_nudge(y = -.15), show.legend = F, alpha = .1)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x=expression(Delta*"Fat fraction (%)"),
       #title = "Linear mixed regression model of muscle fat fraction",
       #subtitle = "Bootstrap distributions (n= 10,000)",
       caption = "Bootstrap distributions of 10.000 resamples of a linear mixed regression model of muscle fat fraction <br>
       Fixed effects include age, BMI and sex. Random effects included muscle and patient ID"
  )+
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_markdown(family = "Roboto", color = "black"),
        plot.subtitle = element_markdown(family = "Roboto", color = "black"),
        plot.caption = element_markdown(family = "Roboto", color = "black", size = 6),
        axis.text.x = element_markdown(family = "Roboto", color = "black"),
        axis.text.y = element_markdown(family = "Roboto", color = "black", hjust = .5),
        axis.title.x = element_text(family = "Roboto", color = "black"),
        axis.title.y = element_blank(),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  scale_x_continuous(breaks = seq(-5,5,0.5))

ggsave(filename = 'boot_lmm_point.pdf', device = cairo_pdf , height = 14, width = 18, units = "cm", dpi =3000)
ggsave(filename = 'boot_lmm_point.tiff', compression = 'lzw' , height = 12, width = 18, units = "cm", dpi =3000)
