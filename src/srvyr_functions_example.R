library(tidyverse)
library(survey)
library(srvyr)
library(magrittr)

muestra_estratos <- read_csv("muestra_estratos.csv")
muestra_2006 <- read_csv("muestra_2006.csv")

muestra_estratos %<>% mutate(factor = N/n)
muestra_2006 %<>% left_join(muestra_estratos)

muestra = muestra_2006 %>% gather(key = Partido, 
                                  value = Votos, -c(casilla_id, edo_id, total, estrato, n, N, factor ))

design <- muestra %>% 
  as_survey_design(ids = casilla_id, weights = factor, strata = estrato)

set.seed(08112018)
elect_boot <- design %>% 
  as_survey_rep(type = "subbootstrap", replicates = 100)

design %>%
  group_by(Partido) %>% 
  summarise(Total = survey_total(Votos),
            Proporción = survey_ratio(Votos,total, vartype = c("se","ci","cv"), deff = T)) %>% 
  arrange(desc(Proporción))


elect_boot %>%
  group_by(Partido) %>% 
  summarise(Total = survey_total(Votos),
            Proporción = survey_ratio(Votos,total, vartype = c("se","ci","cv"), deff = T)) %>% 
  arrange(desc(Proporción))

svyquantile(~pri_pvem, elect_boot, quantiles = seq(0.1, 1, 0.1), interval.type = "quantile")