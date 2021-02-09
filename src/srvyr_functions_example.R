library(tidyverse)
library(survey)
library(srvyr)
library(magrittr)
library(tidyr)
library(foreign)
library(Hmisc)

muestra_estratos <- read_csv("data/muestra_estratos.csv")
muestra_2006 <- read_csv("data/muestra_2006.csv")

muestra_estratos %<>% mutate(factor = N/n)
muestra_2006 %<>% left_join(muestra_estratos)

muestra = muestra_2006 %>% 
  gather(key = Partido,
         value = Votos, -c(casilla_id, edo_id, total, estrato, n, N, factor ))

design <- muestra %>% 
  as_survey_design(ids = casilla_id, 
                   weights = factor, 
                   strata = estrato)

set.seed(08112018)
elect_boot <- design %>% 
  srvyr::as_survey_rep(type = "subbootstrap", replicates = 100)

design %>%
  group_by(Partido) %>% 
  summarise(Total = survey_total(Votos),
            Proporción = survey_ratio(Votos,total, 
                                      vartype = c("se","ci","cv"), deff = T)) %>% 
  arrange(desc(Proporción))


elect_boot %>%
  srvyr::group_by(Partido) %>% 
  srvyr::summarise(Total = srvyr::survey_total(Votos),
            Proporcion = srvyr::survey_ratio(numerator = Votos, 
                                             denominator = total,
                                             vartype = c("ci","cv")
                                             )
            ) %>% 
  arrange(desc(Proporcion))

svyquantile(~Votos, elect_boot, 
            quantiles = seq(0.1, 1, 0.1), 
            interval.type = "quantile")


######################### EJEMPLO ENIGH ######################### 

concentrado_hogar <- read_csv("data/concentradohogar.csv")

hogar <- concentrado_hogar %>% 
  dplyr::select(
    folioviv, 
    foliohog, 
    est_dis, 
    upm, 
    ocupados,
    sexo_jefe,
    factor_hog, 
    ing_cor, 
    alimentos,
    vestido, 
    vivienda, 
    salud, 
    transporte, 
    comunica, 
    educacion, 
    esparci
    )

enigh_design <- hogar %>% 
  as_survey_design(
    ids = upm, 
    weights = factor_hog, 
    strata = est_dis
    )

set.seed(12345)
enigh_boot <- enigh_design %>% 
  as_survey_rep(
    type = "subbootstrap", 
    replicates = 100
    )

deciles <- enigh_boot %>% 
  srvyr::summarise(
    deciles = survey_quantile(ing_cor, 
                              quantiles = seq(0.1, 1, 0.1),
                              deff = T,
                              interval_type = "quantile")) 

deciles %>% 
  pivot_longer(cols = starts_with("deciles"), 
               names_to = "decil") 




enigh_boot <- enigh_boot %>% 
  srvyr::mutate(
    sexo_jefe = ifelse(sexo_jefe == 1, "Hombre", "Mujer"),
    decil = cut2(ing_cor, g = 10),
    gasto = alimentos + vestido + vivienda + salud + 
      transporte + comunica + educacion + esparci,
    pct_gasto = ifelse(ing_cor != 0, gasto / ing_cor, NA),
    var_x = sample(c("categ_a","categ_b","categ_c"), nrow(concentrado_hogar), T, prob = c(0.3,0.5,0.2)),
    tot = 1
    )

enigh_boot$variables %>% glimpse()

# Estimación de variable numérica
pct_gasto_estimado <- enigh_boot %>% 
  srvyr::summarise(
    mean_pct_gasto = survey_mean(
      pct_gasto, na.rm = T, 
      vartype = c("se","ci","cv"),
      deff = T)
  )
pct_gasto_estimado

# Estimación agrupada de variable numérica
pct_gasto_sex <- enigh_boot %>% 
  group_by(sexo_jefe) %>% 
  srvyr::summarise(
    mean_pct_gasto = survey_mean(
      pct_gasto, 
      na.rm = T, 
      vartype = c("se","ci","cv"),
      deff = T)
  )
pct_gasto_sex %>% 
  as.data.frame()

# Estimación agrupada de variable numérica
pct_gasto_x_decil <- enigh_boot %>% 
  group_by(decil) %>% 
  srvyr::summarise(
    mean_pct_gasto = survey_mean(
      pct_gasto, 
      na.rm = T, 
      vartype = c("se","ci","cv"),
      deff = T)
  )
pct_gasto_x_decil %>% 
  as.data.frame()


# Estimación (media y total) de variable categórica
sexo_ocup <- enigh_boot %>% 
  srvyr::group_by(sexo_jefe) %>% 
  srvyr::summarize(
    prop = survey_mean(
    na.rm = T, 
    vartype = c("se","ci","cv"),
    deff = T),
    total = survey_total(
      na.rm = T, 
      vartype = c("se","ci","cv"),
      deff = T
      )
    )
  
sexo_ocup %>% 
  as.data.frame()




# Estimación agrupada de variable categórica
sexo_categ_x <- enigh_boot %>% 
  srvyr::group_by(sexo_jefe, var_x) %>% 
  srvyr::summarize(
    prop = survey_mean(
      na.rm = T, 
      vartype = c("se","ci","cv"),
      deff = T),
    total = survey_total(),
    .groups = "drop"
  )

sexo_categ_x %>% 
  as.data.frame()

sexo_categ_x %>% 
  select(sexo_jefe, var_x, prop, total) %>% 
  pivot_wider(names_from = var_x, values_from = c(prop, total)) %>% 
  rowwise() %>% 
  mutate(total = sum(across(starts_with("total")))) %>% 
  select(-starts_with("total_")) %>% 
  ungroup() %>%
  mutate(pct = round(total/sum(total)*100,2))
  



ggplot(pct_gasto_x_decil, aes(x = decil, y = mean_pct_gasto)) +
  geom_hline(yintercept = pct_gasto_estimado$mean_pct_gasto, 
             col = "blue", linetype = 'dotted') +
  geom_point(col = "red") + 
  geom_errorbar(aes(ymin = mean_pct_gasto - qnorm(0.975) * mean_pct_gasto_se,
                    ymax = mean_pct_gasto + qnorm(0.975) * mean_pct_gasto_se), 
                width = 0.1) +
  geom_line() +
  ggtitle("Intervalos de confianza de gasto (%) por decil de ingreso") +
  ylab("Gasto (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(pct_gasto_x_decil, aes(x = decil, y = mean_pct_gasto)) +
  geom_col(fill = "gray") + 
  geom_errorbar(
    aes(
      ymin = mean_pct_gasto - qnorm(0.975) * mean_pct_gasto_se,
      ymax = mean_pct_gasto + qnorm(0.975) * mean_pct_gasto_se
        ),
    width = 0.1
    ) +
  geom_line() +
  geom_hline(yintercept = pct_gasto_estimado$mean_pct_gasto, 
             col = "blue", linetype = 'dotted') +
  ggtitle("Intervalos de confianza de gasto (%) por decil de ingreso") +
  ylab("Gasto (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))














