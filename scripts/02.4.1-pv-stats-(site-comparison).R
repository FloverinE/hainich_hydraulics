



#install.packages("lme4", type = "source")

pacman::p_load(tidyverse, lme4, flextable, Matrix, gtools, broom.mixed, performance, emmeans, cowplot)

pv_parameters_clean_stat <- read_csv("outputs/pv_parameters_clean.csv") %>% 
  filter(species == "FASY") %>% 
  filter(campaign %in% c("2nd", "3rd")) %>% 
  mutate(ID = gsub("try2", "", ID),
         tree = str_sub(ID, 5, 11))

unique(pv_parameters_clean_stat$tree)

pv_parameters_clean_stat %>% 
  names()

tlp_lm1 <- lm(elasticity_tot ~ site * campaign, pv_parameters_clean_stat)

tlp_1 <- lme4::lmer(elasticity_tot ~ site * campaign + 
                      (1|tree) + (1|campaign), pv_parameters_clean_stat)


tidy(tlp_1)

tlp_lme <- lmer(psi_tlp ~ site * campaign +
                  (1|tree), pv_parameters_clean_stat)


ft_lme <- lmer(psi_ft ~ site * campaign +
                 (1|tree), pv_parameters_clean_stat)

rwc_lme <- lmer(rwc_tot_tlp ~ site * campaign +
                  (1|tree), pv_parameters_clean_stat)


elas_lme <- lmer(elasticity_tot ~ site * campaign +
                   (1|tree), pv_parameters_clean_stat)



# emmean only for campaign ------------------------------------------------

pv_emmean_camp <- function(mod) {
  pv_emmean_camp <- emmeans(mod, type = "response",
                            pairwise ~ site * campaign,
                            bias.adj = T)
  return(pv_emmean_camp)
}

pv_emmean_camp(tlp_lme)

pv_emmean_camp_est <- function(mod) {
  pv_emmean_camp_est <- pv_emmean_camp(mod)$emmeans %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(site, contains("emmean"), campaign, contains("CL")) %>% 
    select(site, contains("emmean"), campaign)  %>% 
    # mutate(across(c(emmean_First, emmean_Second), funs(. * -1))) %>% 
    # select(!c(emmean_First, emmean_Second)) %>% 
    set_names(gsub("[_*]", "", fixed = F, names(.)))
  return(pv_emmean_camp_est)
}

pv_emmean_camp_est(tlp_lme)


pv_emmean_camp_tidy <- function(mod, parameter_name) {
  pv_emmean_camp_tidy <- pairs(regrid(pv_emmean_camp(mod)),
                               simple = "campaign",
                               combine = T, adjust = "mvt") %>% 
    as_tibble() %>% 
    mutate(signif = stars.pval(p.value)) %>% 
    separate(contrast, c("campaign", "campaign2")) %>% 
    select(site, everything()) 
  return(pv_emmean_camp_tidy)
}

pv_emmean_camp_tidy(tlp_lme)

pv_moisture_table <- function(mod, parameter_name) {
  pv_moisture_table <- pv_emmean_camp_est(mod) %>% 
    right_join(pv_emmean_camp_tidy(mod)) %>% 
    rename(response = emmean) %>%
    mutate(response2 = response - estimate) %>% 
    unite("Contrast", c("campaign", "campaign2"), sep = " : ") %>% 
    select(Site = site, Contrast,
           response1 = response, response2, everything()) %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    group_by( Site, Contrast) %>% 
    mutate('% diff' = 100 * (1 - (min(response1, response2))/(max(response1, response2))),
           Parameter = parameter_name) %>% 
    mutate(across(c('% diff'), ~round(., 1))) %>%
    relocate('% diff', .after = estimate) %>%
    arrange(Site)
  return(pv_moisture_table)
}

pv_moisture_table(tlp_lme, "tlp_lme")


tlp_emmean_camp_tidy <- pv_moisture_table(tlp_lme, "TLP") 
ft_emmean_camp_tidy <- pv_moisture_table(ft_lme, "FT" ) 
rwc_emmean_camp_tidy <- pv_moisture_table(rwc_lme, "RWC" ) %>% 
  mutate(across(c(response1, response2), abs)) 
elas_emmean_camp_tidy <- pv_moisture_table(elas_lme, "Elast" ) %>% 
  mutate(across(c(response1, response2), abs)) 



# put all pv parameters together and make a table out ---------------------


pv_table_camp <- list(tlp_emmean_camp_tidy,
                      ft_emmean_camp_tidy,
                      rwc_emmean_camp_tidy,
                      elas_emmean_camp_tidy) %>% 
  reduce(full_join) %>%
  rename(Response1 = response1,
         Response2 = response2) %>% 
  mutate(across(c('% diff'), ~round(., 1))) %>% 
  mutate(Parameter = fct_relevel(Parameter, "TLP", "FT", "RWC", "Elast")
  ) %>% 
  relocate(Parameter, Site, Contrast) %>% 
  relocate(Response1, Response2, .after = Contrast) %>% 
  mutate(across(c(Response1, Response2), ~round(., 2))) 

(pv_flex_camp <- pv_table_camp %>% 
    select(!c(df))  %>% 
    filter(!Parameter == "Elast") %>% 
    mutate(across(c('t.ratio'), ~round(., 2)),
           Site = mgsub::mgsub(as.character(Site), c("H", "Leinefelde"), c("Hainich", "Leinefelde"))) %>% 
    rename(Campaign = Contrast, t = t.ratio, Significance = signif,
           'P-value' = 'p.value', 'Diff (%)'  = '% diff') %>% 
    flextable() %>% 
    
    mk_par(part = "body", j = c(1), i = c(1:2), value = as_paragraph(as_b("Ψ"),
                                                                      as_sub("TLP"),
                                                                      as_b(" (MPa)"))) %>%
    mk_par(part = "body", j = c(1), i = c(3:4), value = as_paragraph(as_b("Ψ"),
                                                                       as_sub("FT"),
                                                                       as_b(" (MPa)"))) %>%
    mk_par(part = "body", j = c(1), i = c(5:6), value = as_paragraph(as_b("RWC"),
                                                                       as_sub("TLP"),
                                                                       as_b(" (%)"))) %>%
    # mk_par(part = "body", j = c(1), i = c(37:48), value = as_paragraph(as_b("Elasticity"),
    #                                                                    # as_sub("TLP"),
    #                                                                    as_b(" (MPa)"))) %>%
    merge_v(j = ~Parameter) %>% 
    merge_v(j = ~Site) %>% 
    align_nottext_col(align = "right") %>% 
    vline(j = c( 1, 2, 3)) %>% 
    hline(i = c(2, 4, 6)) %>% 
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

save_as_image(pv_flex_camp, path = "tables/pv_flex_moisture.png",
              webshot = "webshot2")

# Figures for important PV parameters -------------------------------------

# emmean for all at once to look at the plots -----------------------------------------------------
pacman::p_load(janitor)

pv_plot <- function(mod) {
  emmeans(mod, type = "response", 
          pairwise ~ site * campaign,
          bias.adj = T)[1] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    set_names(gsub("emmeans.", "", names(.))) %>% 
    clean_names() %>% 
    mutate(site = mgsub::mgsub(as.character(site), c("H", "Leinefelde"), c("Hainich", "Leinefelde"))) %>% 
    #   mutate(across(c(emmean, lower_cl, upper_cl), funs(. * xx), .names = "{.col}")) %>% 
    ggplot(aes(site, emmean, group = campaign, col = campaign)) +
    geom_pointrange(aes(y = emmean, ymin = lower_cl, ymax = upper_cl),
                    position = position_dodge(width = 0.4), size = 0.001) +
    # geom_line(aes(group = stand), linetype = 2, alpha = 0.1) +
    #    facet_wrap(~moisture) +
    labs(x = "Species") +
    scale_color_manual(values = campaign_col,
                       name = "Campaign") +
    theme_bw(base_size = 8) 
}

(tlp_plot <- pv_plot(tlp_lme) +
    ylim(-3.0, -2.4) +
    labs(x = "", y = expression(paste(~Ψ[TLP], ~"(MPa)"))) +
    theme(legend.position = "none",
          line = element_blank())
)

(ft_plot <- pv_plot(ft_lme) +
  ylim(-2.5, -1.8) +
  labs(x = "", y = expression(paste(~Ψ[FT], ~"(MPa)"))) +
  theme(legend.position = "none",
        line = element_blank())
)

(rwc_plot <- pv_plot(rwc_lme) +
  labs(y = expression(paste(RWC[TLP], ~"(%)")))  +
  theme(line = element_blank())
)

(elas_plot <- pv_plot(elas_lme) +
  labs(y = expression(paste(~Elasticity, ~"(MPa)"))) +
  theme(legend.position = "none",
        line = element_blank())
)

legend_pv <- get_legend(rwc_plot + 
                          theme(legend.box.margin = margin(0,0,0,12)) +
                          theme(legend.position = c(0.2, 0.65),
                                legend.text = element_text(size = 6),
                                legend.key.size = unit(0.4, "cm")))


plot_grid(tlp_plot, ft_plot,
          rwc_plot + theme(legend.position = "none",
                           line = element_blank()), 
          legend_pv,
          labels = c("a", "b", "c", ""), label_size = 10, hjust = -0.8,
          ncol = 2,
          rel_widths = c(1, 1, 0.4, 1, 1))


ggsave("figures/2.2-pv-traits-beech.png", height = 100,
       width = 150, units = "mm", dpi = 300)

