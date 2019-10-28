library(tidyverse)
library(htmltools)
library(webshot)    
library(formattable)

h <- here::here
country_trans <- read_csv(h("data/country-level-amr-transformed.csv")) 
variable_lookup <- read_csv(h("data/variable-lookup.csv")) 

# Sum stats
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

sstats <- read_csv(h("data/country-level-amr.csv")) %>%
  filter(iso3c %in% country_trans$iso3c) %>%
  dplyr::select(-iso3c, -continent, -region, -country, -gdp_dollars, -pubs_sum, -promed_mentions) %>%
  gather() %>%
  group_by(key) %>%
  summarize(`Mean (SD)` = paste0(formatC(mean(value, na.rm = T), format = "e", digits = 1), " (", formatC(sd(value, na.rm = T), format = "e", digits = 1), ")"),
            Maximum = formatC(max(value, na.rm = T), format = "e", digits = 1),
            Minumum = formatC(min(value, na.rm = T), format = "e", digits = 1),
            `Countries with available data` = length(value[!is.na(value)])
  ) %>%
  left_join(variable_lookup) %>%
  mutate_all(~replace_na(., "")) %>%
  mutate(key = factor(key, levels = variable_lookup$key)) %>%
  arrange(key) %>%
  select(" " = label, `Measurement Units`, everything(), -key, -Citation) 

sstats <- sstats %>%
  select(-`Mean (SD)`, - Maximum, - Minumum)

sstats_tbl <- formattable(sstats, 
                          align =c("l","c","c","c","c","c"), 
                          list(`Indicator Name` = formatter(
                            "span", style = ~ style(color = "grey",font.weight = "bold")) 
                          ))

export_formattable(sstats_tbl, h("plots", "parameter_summary.png"))
xlsx::write.xlsx(sstats,h("plots", "parameter_summary.xlsx"))


