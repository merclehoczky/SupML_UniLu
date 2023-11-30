# subset 1 ----------------------------------------------
# without micro_rating, and selected vars included in microratings

df_wo_svars <- df_corr %>% 
  select(- c(Micro_rating, wgh_avg_sonnenklasse_per_egid, 
         dist_to_haltst, dist_to_highway, dist_to_main_stat, dist_to_train_stat,
         dist_to_lake, dist_to_river))

# subset 2 ----------------------------------------------------------
# without all micro_rating except main one

df_wo_mrs <- df_corr %>% 
  select(- c( Micro_rating_Accessibility, Micro_rating_DistrictAndArea,
         Micro_rating_SunAndView, Micro_rating_ServicesAndNature))


df_wo_mrs_others <- df_wo_mrs %>% 
  select(-c(cabletv, dryer, oven ))

df_wo_variability <- df_corr %>% 
  select(-c(dryer, elevator, cabletv, minergie, playground, shared_flat, wheelchair, 
            oven, dist_to_haltst,  dist_to_highway,dist_to_lake,dist_to_main_stat, anteil_efh, 
            Anteil_auslaend,dishwasher,apoth_pix_count_km2 , pool, raised_groundfloor ,avg_bauperiode,wgh_avg_sonnenklasse_per_egid, 
            dist_to_school_1, dist_to_train_stat, dist_to_river ,Micro_rating, Avg_size_household))

# subset 3 ---------
df_new <- subset(df_corr, select = -c(dryer, elevator, cabletv, minergie, playground, shared_flat, wheelchair, 
                                         oven, dist_to_haltst,  dist_to_highway,dist_to_lake,dist_to_main_stat, anteil_efh, 
                                         KTKZ,furnished,Micro_rating_NoiseAndEmission ,Anteil_auslaend,dishwasher,apoth_pix_count_km2 , pool, raised_groundfloor ,avg_bauperiode,wgh_avg_sonnenklasse_per_egid, 
                                         dist_to_school_1, dist_to_train_stat, dist_to_river ,Micro_rating, Avg_size_household))
df_new$msregion <- training_data$msregion
df_new$msregion <- as.factor(df_new$msregion)
colnames(df_new)