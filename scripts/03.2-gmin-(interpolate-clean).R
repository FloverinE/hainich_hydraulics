



gmin_cal <- read_csv("outputs/gmin_cal.csv") 



gmin_data2 <- gmin_cal %>% 
#  filter(id == "2_6.3_August_leaf") %>% 
  group_by(id) %>% 
mutate(n_nonna = sum(!is.na(rwc), na.rm=TRUE)) 

gmin_data2 %>% 
  names()

write_csv(gmin_data2, "outputs/gmin_data2.csv")
  
check <- gmin_data2 %>% 
  filter(n_nonna < 5)


# fit with monotonic constraint
fit <- gmin_data2 %>%
  group_by(id) %>% 
  mutate(
    ma = max(real_time, na.rm=TRUE),
    # pick nr of inner knots to include quantile based knot selection
    nr_ik = ifelse(n_nonna[1] > 23, 10,
                   ifelse(n_nonna[1] < 15, 6, 8)),
    myknots = list(realtime=c(-rev(seq(10, 40, 10)) ,
                              unname(quantile(real_time,
                                              seq(0, 1, length=nr_ik[1]+2))),
                              ma[1]+seq(10, 40, 10))),
    model = list(
      scam::scam(leaf_mass_r ~ s(real_time, bs="mpd", 
                                 k=nr_ik[1]+2+3, m=c(3, 0)),
                 knots=myknots[1],
                 data=cur_data(), optimizer="efs")
    )
  )



# what time to interpolate?
# use all times with gaps of more than max_gap min
max_gap <- 5
gmin_data2$leadtime <- dplyr::lead(gmin_data2$real_time)

# create a sequence of new times that are interpolated
create_seq <- function(x, y, interval=30){
  myseq <- seq(x, y, length.out=round((y - x) / interval, 0)+2)
  myseq <- c(NA, round(myseq[2:(length(myseq)-1)], 0))
  
  return(myseq)
}

gmin_data_interpolate <- gmin_data2 %>% 
  group_by(id) %>% 
  ungroup() %>% 
  mutate(newtime = map2(real_time, leadtime, ~ 
                          if(!is.na(.x)&!is.na(.y)&(.y-.x)>max_gap) 
                            create_seq(.x, .y, interval=max_gap)
                        else NA)) %>% 
  select(id, campaign, real_time,  newtime, leaf_mass_r) %>% 
  unnest(c(id, campaign,  real_time, leaf_mass_r)) %>%  
  distinct(.keep_all = TRUE) %>% 
  unnest(newtime) %>% 
  mutate(interpolated = factor(ifelse(!is.na(newtime), 1, 0)),
         real_time = ifelse(interpolated==1, NA, real_time),
         leaf_mass_r = ifelse(interpolated==1, NA, leaf_mass_r),
         time_min = coalesce(real_time, newtime))

gmin_data_interpolate %>% 
  #filter(campaign == "1st") %>% 
  ggplot(aes(real_time, leaf_mass_r, col = campaign,
             group = interaction(id, campaign))) +
  geom_point() +
  geom_line() +
  facet_wrap(~id, scales = "free") +
  theme_bw()


# take fitted models and merge to data set (also include rwc for later analysis)
fit_id <- fit %>% 
  group_by(id) %>% 
  summarise(model=model[1], real_time=real_time, rwc=rwc)


gmin_fit_interpolate <- left_join(gmin_data_interpolate, fit_id, by=c("id", "real_time"))

# interpolate values
gmin_data_predict <- gmin_fit_interpolate %>% 
  group_by(id) %>% 
  mutate(pred=predict(model[1][[1]], data.frame(real_time=newtime)),
         leaf_mass_new = coalesce(leaf_mass_r, pred)) 

gmin_data_predict %>% 
  select(!c(model, pred, interpolated, leaf_mass_r, real_time)) %>% 
  write.csv("outputs/gmin_mass_interpolated.csv")



# export the data so that we do not have to read this script before
# and making the other scripts independent of this script



# plot model results (grid per species)
plts <- map(.x = unique(data$campaign), ~ gmin_data_predict %>% 
                   filter(campaign == .x) %>% 
                   group_by(id) %>% 
                   ggplot(aes(x=time_min, y=leaf_mass_new, col=interpolated)) +
                   geom_point(alpha=0.8, size=0.5) +
                   facet_wrap(~id, scales = "free") +
                   theme_bw() +
                   ggtitle(.x))

plts[[1]]
plts[[2]]
plts[[3]]
plts[[4]]
