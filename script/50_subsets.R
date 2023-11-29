# subset 1 ----------------------------------------------
# without micro_rating, and selected vars included in microratings

df_wo_svars <- df_corr %>% 
  select(- c(Micro_rating, wgh_avg_sonnenklasse_per_egid, 
         dist_to_haltst, dist_to_highway, dist_to_main_stat, dist_to_train_stat,
         dist_to_lake, dist_to_river))

# subset 2 ----------------------------------------------------------
# without all micro_rating except main one

df_wo_mrs <- df_corr %>% 
  select(- c(Micro_rating_NoiseAndEmission, Micro_rating_Accessibility, Micro_rating_DistrictAndArea,
         Micro_rating_SunAndView, Micro_rating_ServicesAndNature))


df_wo_mrs_others <- df_wo_mrs %>% 
  select(-c(cabletv, dryer, oven ))
