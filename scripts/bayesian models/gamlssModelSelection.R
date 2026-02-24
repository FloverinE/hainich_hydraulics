
# Script for choosing what distribution for the conditional response is a good choice
library(gamlss)

# Bound the response rwc to live in (0,1) interval, so we can use the Beta regression model
df_pv_params$rwc_bounded <- df_pv_params$rwc_tot_tlp/100

# Fit an initial model, the same used in brms. Where the family argument is just a starting guess
m1 <- gamlss(rwc_bounded ~ species * campaign * year + re(random=~1 | sample_id),
             family = NO,
             data = df_pv_params)

# We now choose from the 'type' argument what is the support of the response distribution. See the documentation here https://www.gamlss.com/wp-content/uploads/2023/06/DistributionsForModellingLocationScaleandShape-1.pdf
# The range (or support) tells us what are the posible values that the response distribution could potentially take. 
# The parallel argument is to speed up the fitting procedure.
t1 <- chooseDist(m1, type="real0to1", parallel = "snow", ncpus = 4)

# After fitting many distribution, we select the one with the lowest Information criterion. Some distribution are super technical, so we balance between fit and simplicity 
getOrder(t1, 3)

# Then update the model and compare, based on AIC/BIC, how much we improved
fm<-update(m1, family=names(getOrder(t1,3)[1]))

summary(m1)
summary(fm)
AIC(m1, fm)
BIC(m1, fm)
plot(fm)

# After selecting a "good" distribution we passe it to brms
rwc_bounded.prior <- default_prior(rwc_bounded ~ species * campaign * year + (1 | sample_id),
                               family = Beta(link = "logit"),
                               data = df_pv_params)
blme_rwc_bounded <- brm(rwc_bounded ~ species * campaign * year + (1 | sample_id),
                    family = Beta(link= "logit"),
                    data = df_pv_params ,
                    prior = rwc_tlp.prior,
                    chains = 8,
                    cores = 8,
                    iter = 10000,
                    warmup = 2000,
                    thin=5,
                    
                    # seed for reproducibility
                    seed= 2300, 
                    
                    # Tuning parameters for the NUTS algorithm
                    control = list(adapt_delta = 0.95, max_treedepth = 12),
                    
                    # Saving model parameters and stan code
                    #save_model = "blme_rwc_tlp.stan",
                    save_pars = save_pars(all = TRUE)
)
blme_rwc_bounded |> tidybayes::summarise_draws("mean", ~quantile(.x, probs=c(.05, .25, .5, .75, .95)))
blme_rwc_bounded |> bayes_R2()
blme_rwc_bounded |> loo_R2()
