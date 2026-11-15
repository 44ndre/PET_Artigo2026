#analise só do primeiro questionário

#LIVRARIAS ============

library(tidyverse)
library(readxl)
library(survey)
library(here)


df <- read_xlsx(
  here("data", "survey-text-clean-20260413 (2).xlsx")
)

#LIMPAR BANCO ============

df <- df %>% filter(!is.na(q02_data_nascimento))
df <- df %>%
  separate(
    q24_modalidade_ingresso_usp,
    into = c("cota", "modalidade"),
    sep = ":\\s*"
  )

df <- df %>%
  filter(modalidade != "TRANFERÊNCIA INTERNA OU EXTERNA")

df <- df %>%
  mutate(cota = if_else(cota == "EP - PPI", "PPI", cota))

#pessoas que responderam ampla concorrencia no provao paulista oq é impossivel
df[56, 25] <- "EP"
df[30, 25] <- "EP"
df[33, 25] <- "EP"


#mutates variados pra análise dps

df <- df %>%
  mutate(voto_binario = ifelse(q37_voto_presidencia_hipotetico == "Lula (PT)", 1, 0))

df$q35_posicao_espectro_politico <- as.numeric(gsub(" .*$", "", df$q35_posicao_espectro_politico))


df <- df %>%
  mutate(
    # Transformar em fator com ordem correta
    avaliacao_lula_ord = factor(q39_avaliacao_governo_lula,
                                levels = c("Péssimo", "Ruim", "Regular", "Bom", "Ótimo"),
                                ordered = TRUE),
    # Converter para numérico (1 = Péssimo, 5 = Ótimo)
    avaliacao_lula_num = as.numeric(avaliacao_lula_ord)
  )

df <- df %>% 
  mutate(renda_numerica = case_when(q15_renda_familiar_mensal == "Até R$ 2.000" ~ 1000,
                                    q15_renda_familiar_mensal == "De R$ 2.001 a R$ 4.000" ~ 3000,
                                    q15_renda_familiar_mensal == "De R$ 4.001 a R$ 8.000" ~ 6000,
                                    q15_renda_familiar_mensal == "De R$ 8.001 a R$ 15.000" ~ 11500,
                                    q15_renda_familiar_mensal == "De R$ 15.001 a R$ 30.000" ~ 22500,
                                    q15_renda_familiar_mensal == "Acima de R$ 30.000" ~ 40000,
                                    .default = NA))

df <- df %>% mutate(cota2 = grepl("Não se aplica", q25_motivo_nao_cotista))
df <- df %>% mutate(papfe = grepl("Você solicitou", q26_situacao_auxilios_papfe))


df <- df %>% mutate(escola = grepl("pública", q18_tipo_ensino_medio)) #publica é true

#PACOTE SURVEY DESENHO DA PESQUISA ===========

pop_turno <- data.frame(
  modalidade = c(
    "FUVEST", "FUVEST",
    "ENEM USP", "ENEM USP",
    "PROVÃO PAULISTA", "PROVÃO PAULISTA"
  ),
  q32_periodo_matricula = c(
    "Vespertino", "Noturno",
    "Vespertino", "Noturno",
    "Vespertino", "Noturno"
  ),
  Freq = c(73, 81, 13, 11, 11, 15)
)


pop_cota <- data.frame(
  modalidade = c(
    "FUVEST", "FUVEST", "FUVEST",
    "ENEM USP", "ENEM USP", "ENEM USP",
    "PROVÃO PAULISTA", "PROVÃO PAULISTA", "PROVÃO PAULISTA"
  ),
  cota = c(
    "AC", "EP", "PPI",
    "AC", "EP", "PPI",
    "AC", "EP", "PPI"
  ),
  Freq = c(91, 40, 23, 12, 2+6, 2+2, 0, 11+4, 5+6)
)





des <- svydesign(
  ids = ~1,
  data = df
)

#ESSE É O OBJETO AGR (o de baixo)

des_raked <- rake(
  design = des,
  sample.margins = list(
    ~modalidade + q32_periodo_matricula,
    ~modalidade + cota
  ),
  population.margins = list(
    pop_turno,
    pop_cota
  )
)

#Filtrar lula samara

des_filtrado_voto <- subset(
  des_raked,
  q37_voto_presidencia_hipotetico %in% c("Lula (PT)", "Samara Martins (UP)")
)




#REGRESSÔES ===========

modelo <- svyglm(
  voto_binario ~ q15_renda_familiar_mensal + avaliacao_lula_num,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo)

modelo2 <- svyglm(
  voto_binario ~ log(renda_numerica) + avaliacao_lula_num,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo2)

modelo2.5 <- svyglm(
  voto_binario ~ q15_renda_familiar_mensal,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo2.5)

modelo4 <- svyglm(
  voto_binario ~ papfe,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo4)

modelo5 <- svyglm(
  voto_binario ~ avaliacao_lula_num,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo5)

#lembrar que escola true = publica
modelo6 <- svyglm(
  voto_binario ~ escola,
  design = des_filtrado_voto,
  family = quasibinomial()
)

summary(modelo6)
exp(coef(modelo6))



#esse de baixo é linear. notar q da bem baixo o coeficiente

modelo3 <- svyglm(
  avaliacao_lula_num ~ renda_numerica,
  design = des_raked
)

summary(modelo3)







#GRÁFICOS ===============
#como estamos usando o objeto do survey, n da pra usar ggplot nele direto
#ent tem q fazer esse role aí de criar a tabela resultado

#grafico 1
resultado <- svytable(
  ~voto_binario + escola,
  des_filtrado_voto
)

resultado <- prop.table(resultado, margin = 1)

resultado_df <- as.data.frame(resultado)

ggplot(resultado_df, aes(x = escola, y = Freq, fill = voto_binario)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("0" = "darkred", "1" = "orange"),
    labels = c("0" = "Samara", "1" = "Lula"),
    name = "Voto"
  ) +
  theme_minimal() +
  labs(
    y = "Frequência",
    x = "Escola ensino médio"
  ) +
  scale_x_discrete(
    labels = c(
      "FALSE" = "Particular",
      "TRUE" = "Pública"
    )
  )





#grafico 2
resultado2 <- svyby(
  ~voto_binario,
  ~renda_numerica,
  des_filtrado_voto,
  svymean,
  na.rm = TRUE
)

resultado_df2 <- as.data.frame(resultado2)

resultado_df2$outros <- 1 - resultado_df2$voto_binario


resultado_long <- resultado_df2 |>
  pivot_longer(
    cols = c(voto_binario, outros),
    names_to = "voto",
    values_to = "proporcao"
  )

ggplot(resultado_long, aes(
  x = renda_numerica,
  y = proporcao,
  color = voto
)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c(
      "voto_binario" = "blue",
      "outros" = "red"
    ),
    labels = c(
      "voto_binario" = "Lula",
      "outros" = "Samara"
    )
  ) +
  labs(
    x = "Renda familiar mensal",
    y = "Proporção",
    color = "Voto"
  ) +
  theme_minimal()


ggplot(resultado_long, aes(
  x = factor(renda_numerica),
  y = proporcao,
  fill = voto
)) +
  geom_col(position = "fill") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.2)
  ) +
  scale_fill_manual(
    values = c(
      "voto_binario" = "blue",
      "outros" = "red"
    ),
    labels = c(
      "voto_binario" = "Lula",
      "outros" = "Samara"
    )
  ) +
  labs(
    x = "Renda familiar mensal",
    y = "Proporção dos votos",
    fill = "Candidato"
  ) +
  theme_minimal()





resultado2 <- svyby(
  ~voto_binario,
  ~renda_numerica,
  des_filtrado_voto,
  svymean,
  na.rm = TRUE
)

resultado_df2 <- as.data.frame(resultado2)

resultado_df2$outros <- 1 - resultado_df2$voto_binario

# Contar observações válidas em cada categoria de renda
contagens <- des_filtrado_voto$variables |>
  dplyr::filter(
    !is.na(voto_binario),
    !is.na(renda_numerica)
  ) |>
  dplyr::count(renda_numerica, name = "n")

# Criar rótulos personalizados e incluir o número de observações
resultado_df2 <- resultado_df2 |>
  dplyr::left_join(contagens, by = "renda_numerica") |>
  dplyr::mutate(
    rotulo = dplyr::recode(
      as.character(renda_numerica),
      "1000"  = "Até R$ 2.000",
      "3000"  = "De R$ 2.001 a R$ 4.000",
      "6000"  = "De R$ 4.001 a R$ 8.000",
      "11500" = "De R$ 8.001 a R$ 15.000",
      "22500" = "De R$ 15.001 a R$ 30.000",
      "40000" = "Acima de R$ 30.000"
    ),
    rotulo = paste0(rotulo, "\n(n = ", n, ")")
  )

# Transformar os dados para o formato longo
resultado_long <- resultado_df2 |>
  tidyr::pivot_longer(
    cols = c(voto_binario, outros),
    names_to = "voto",
    values_to = "proporcao"
  )

# Gráfico
ggplot(
  resultado_long,
  aes(
    x = factor(
      renda_numerica,
      levels = resultado_df2$renda_numerica,
      labels = resultado_df2$rotulo
    ),
    y = proporcao,
    fill = voto
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.2)
  ) +
  scale_fill_manual(
    values = c(
      "voto_binario" = "orange",
      "outros" = "darkred"
    ),
    labels = c(
      "voto_binario" = "Lula",
      "outros" = "Samara"
    )
  ) +
  labs(
    x = "Renda familiar mensal",
    y = "Proporção dos votos",
    fill = "Candidato"
  ) +
  theme_minimal()

#grafico 3

resultados3 <- svyby(
  ~voto_binario,
  ~renda_numerica,
  des_filtrado_voto,
  svymean,
  na.rm = TRUE,
  vartype = "ci"
)

resultados3 <- as.data.frame(resultados3)

resultados3$outros <- 1 - resultados3$voto_binario
resultados3$outros_lwr <- 1 - resultados3$ci_u
resultados3$outros_upr <- 1 - resultados3$ci_l




ggplot(resultados3, aes(x = renda_numerica)) +
  geom_ribbon(
    aes(
      ymin = ci_l,
      ymax = ci_u
    ),
    fill = "blue",
    alpha = 0.15
  ) +
  geom_line(
    aes(y = voto_binario, color = "Lula"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = outros, color = "Samara"),
    linewidth = 1
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c(
      "Lula" = "blue",
      "Samara" = "red"
    )
  ) +
  labs(
    x = "Renda familiar mensal",
    y = "Proporção de voto",
    color = "Voto"
  ) +
  theme_minimal()



#Tabela pro artigo ============


library(knitr)
library(kableExtra)


table(df$q32_periodo_matricula)
table(df$cota)
table(df$modalidade)

svytable(~modalidade, des_raked)
svytable(~cota, des_raked)
svytable(~q32_periodo_matricula, des_raked)


library(knitr)
library(kableExtra)

dados <- data.frame(
  "Vespertino" = c(74, 97),
  "Noturno" = c(71, 107),
  "FUVEST" = c(107, 154),
  "ENEM USP" = c(17, 24),
  "Provão Paulista" = c(21, 26),
  "AC" = c(74, 103),
  "PPI" = c(25, 38),
  "EP" = c(46, 63)
)

rownames(dados) <- c("Respostas", "Universo")

# Calcula a diferença: Universo - Respostas
diferenca <- dados["Universo", ] - dados["Respostas", ]

# Adiciona a diferença como terceira linha
dados_final <- rbind(
  dados,
  "Diferença" = diferenca
)

dados_final |>
  kable(
    format = "html",
    align = "c",
    booktabs = TRUE
  ) |>
  add_header_above(c(
    " " = 1,
    "TURNO" = 2,
    "INGRESSO" = 3,
    "COTA" = 3
  )) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 14
  ) |>
  column_spec(1, bold = TRUE, width = "2.8cm") |>
  column_spec(2:9, width = "2.5cm") |>
  row_spec(0, bold = TRUE) |>
  row_spec(1:2, extra_css = "height: 35px;") |>
  row_spec(
    3,
    bold = TRUE,
    extra_css = "border-top: 2px solid #999; padding-top: 12px;"
  )

summary(weights(des_raked))


