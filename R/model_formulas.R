main_formula <- bf(n_amr_events ~ ln_livestock_consumption_kg_per_capita +
                     ln_migrant_pop_per_capita + ln_tourism_inbound_per_capita + #ln_tourism_outbound_per_capita +
                     ln_ab_export_per_capita + #ab_export_bin + 
                     health_expend_perc +
                     human_consumption_ddd + english_spoken +
                     ln_pubcrawl_per_capita + ln_promed_mentions_per_capita + ln_gdp_per_capita + offset(ln_population),
                   zi ~ ln_pubcrawl_per_capita + ln_promed_mentions_per_capita  + ln_gdp_per_capita + ln_population + english_spoken)

remove_outbound_tourism_formula <- bf(n_amr_events ~ ln_livestock_consumption_kg_per_capita +
                                        ln_migrant_pop_per_capita + ln_tourism_inbound_per_capita +
                                        ln_ab_export_per_capita + ab_export_bin + health_expend_perc +
                                        human_consumption_ddd + english_spoken +
                                        ln_pubcrawl_per_capita + ln_promed_mentions_per_capita + ln_gdp_per_capita + offset(ln_population),
                                      zi ~ ln_pubcrawl_per_capita + ln_promed_mentions_per_capita  + ln_gdp_per_capita + ln_population + english_spoken)

remove_livestock_biomass_formula <- bf(n_amr_events ~  ln_livestock_consumption_kg_per_pcu + ln_livestock_pcu +
                                         ln_migrant_pop_per_capita + ln_tourism_inbound_per_capita + ln_tourism_outbound_per_capita +
                                         ln_ab_export_per_capita + ab_export_bin + health_expend_perc + 
                                         human_consumption_ddd + english_spoken + 
                                         ln_pubcrawl_per_capita + ln_promed_mentions_per_capita + ln_gdp_per_capita + offset(ln_population),
                                       zi ~ ln_pubcrawl_per_capita + ln_promed_mentions_per_capita  + ln_gdp_per_capita + ln_population + english_spoken)
