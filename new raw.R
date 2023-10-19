dat_df_var %>% filter(Gruppe %in% c(1,3)) %>% 
  #split(.$muscle) %>%
  group_by(muscle, .add = T) %>% 
  group_map(~ {
    t_test_result <- tidy(t.test(FF ~ Gruppe, data = .))
    t_test_result <- t_test_result
    t_test_result
  }
  
  ) %>% 
  bind_rows()



library(broom)
library(dplyr)
library(purrr)

dat_df_var %>%
  filter(Gruppe %in% c(1,3)) %>%
  group_by(muscle) %>%
  group_map(~ {
    t_test_result <- tidy(t.test(FF ~ Gruppe, data = .))
    
    # Combine t-test results with the group data 
    t_test_result %>%
      mutate(muscle_group = cur_group()$muscle)
  }) %>%
  bind_rows()


library(broom)
library(dplyr)
library(purrr)

dat_df_var %>%
  filter(Gruppe %in% c(1,3)) %>%
  group_by(muscle) %>%
  group_modify(~ {
    t_test_result <- tidy(t.test(FF ~ Gruppe, data = .))
    
    # Combine t-test results with the group data 
    bind_cols(.[1,], t_test_result) %>% select(.est =estimate,.low = conf.low,
                                               .high = conf.high,
                                               estimate1, estimate2, statistic, p.value
                                               )
  }) %>%
  ungroup()
