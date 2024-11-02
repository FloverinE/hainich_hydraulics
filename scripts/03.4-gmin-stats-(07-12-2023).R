###############################################################################
#
# Goal: stats stuff for gmin
#   1. find a model to explain gmin data
#   2. extract tidy output



# Script: Sharath Paligi (sharath.paligi@uni-goettingen.de) 
#         Jens Lichter (jens.lichter@uni-goettingen.de)
#      

###############################################################################

# 1. Preparations -------------------------------------------------------------

# load libraries
pacman::p_load(tidyverse, janitor, conflicted, flextable, cowplot, webshot,
               broom, gtools, emmeans, performance)


# load data

gmin_data_mod <- read_csv("outputs/gmin_data_clean.csv") %>% 
  mutate(tree = str_sub(id, 1, 7))

unique(gmin_data_mod$species)

gmin_data_mod %>% 
  names()

# emmeans at model scale -----------------------------------------------
# this is just to visualise all in one
# this does not make a main difference from what we wanted to say earlier,
# but just useful to have it this way!

gmin_glm <- gmin_data_mod %>% 
  mutate(across(c(species, campaign, tree), as.factor)) %>% 
  lmer(gmin ~ species * campaign + (1|species/tree), data = .)

gmin_emmean <- emmeans(gmin_glm,  ~ species * campaign, 
                       adjust = "mvt",
                             type = "response")


# emmeans at nutrient level -----------------------------------------------

gmin_campaign_emmean <- emmeans(gmin_glm,  ~ species * campaign, adjust = "none",
                           type = "response")


(gmin_campaign_emmean_tidy <- pairs(regrid(gmin_campaign_emmean), 
                                 simple = "campaign", 
                                 type = "response", adjust = "mvt") %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(signif = stars.pval(p.value)) %>% 
    separate(contrast, c("campaign", "campaign2")) %>% 
    select(species, everything()) 
) 


gmin_campaign_emmtidy <- gmin_campaign_emmean %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  arrange(species) %>% 
  ungroup() 

gmin_campaign_emmtidy %>% 
  clean_names() %>% 
  mutate(species = mgsub::mgsub(as.character(species), c("FASY", "FREX"), c("beech", "fraxinus"))) %>% 
  ggplot(aes(x = species, y = emmean,  col = campaign)) +
  geom_pointrange(aes(y = emmean, ymin = lower_cl, ymax = upper_cl),
                  position = position_dodge(width = 0.4), size = 0.0001) +
  scale_colour_manual(values = campaign_col,
                      name = "Campaign") +
  labs(x = "Neighborhood", 
       y = expression(paste(G[min], ~"(mmol", ~m^-2, ~s^-1, ")" ))) +
  theme_bw(base_size = 6) +
  theme(legend.position = "right", # c(0.8, 0.8),
        line = element_blank(),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 5),
        legend.key.size = unit(0.3, "cm"))

ggsave("figures/3.1-gmin-campaign.png", height = 45,
       width = 65, units = "mm", dpi = 300)


gmin_campaign_emmtidy_output <- gmin_campaign_emmtidy %>% 
  select(1:3) %>% 
  right_join(gmin_campaign_emmean_tidy) %>% 
  rename(response = emmean) %>% 
  mutate(response2 = response - estimate) %>% 
  unite("Contrast", c("campaign", "campaign2"), sep = " : ") %>% 
  select(Species = species, Contrast,
         response1 = response, response2, everything()) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  group_by( Species, Contrast) %>% 
  mutate('% diff' = 100 * (1 - (min(response1, response2))/(max(response1, response2)))) %>% 
  mutate(across(c('% diff'), ~round(., 1))) %>%
  relocate('% diff', .after = estimate) %>%
  arrange(Species)




gmin_flex <- gmin_campaign_emmtidy_output %>% 
  relocate(Species) %>% 
  group_by(Species) 

(gmin_flex_OUT <- gmin_flex %>% 
  select(!c(estimate, df))  %>% 
  mutate(across(c('t.ratio'), ~round(., 2)),
         Species = mgsub::mgsub(as.character(Species), c("FASY", "FREX"), c("beech", "fraxinus"))) %>% 
  rename('Diff (%)' = '% diff', t = t.ratio, Significance = signif,
         'P-value' = 'p.value') %>%
  flextable() %>% 
  align_nottext_col(align = "right") %>% 

  merge_v(j = ~Species) %>% 
  separate_header() %>% 
  # mk_par(part = "header", j = c(3), i = 1, value = as_paragraph(as_b("G"),
  #                                                               as_sub("min"),
  #                                                               as_b(" (mmol m"),
  #                                                               as_sup("-2"),
  #                                                               as_b(" s"),
  #                                                               as_sup("-1"),
  #                                                               as_b(")"))) %>%
  align_nottext_col(align = "right") %>% 
  vline(j = c( 1, 2) ) %>% 
  hline(i = c(6)) %>% 
  vline_left() %>% 
  vline_right() %>% 
  fontsize(part = "header", size = 9) %>% 
  fontsize(part = "body", size = 8) %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  line_spacing(part = "body", space = .15) %>% 
  line_spacing(part = "header", space = .3) %>% 
  fix_border_issues() %>% 
  set_table_properties(layout = "autofit")
)
 
save_as_image(gmin_flex_OUT, path = "tables/gmin-campaign.png",
              webshot = "webshot2")





