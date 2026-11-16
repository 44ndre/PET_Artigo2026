#analise primeiro e segundo questionário 
#isso aq tudo é exploratório e uma gigantesca chapação vou fazer direito e organizar ainda
#teste trabalho


library(tidyverse)
library(readxl)
library(DescTools)
library(survey)
library(here)

ff <- read_xlsx(
  here("data", "survey-text-clean-20260413 (2).xlsx")
)

df2 <- read_xlsx(
  here("data", "PET politico 2026_2 (respostas).xlsx")
)





ff$q40_numero_usp <- as.numeric(ff$q40_numero_usp)
df2$nusp <- as.numeric(df2$nusp)

ff <- ff %>% filter(!is.na(q40_numero_usp))
df2 <- df2 %>% filter(!is.na(nusp))

df <- ff %>%
  left_join(df2, by = c("q40_numero_usp" = "nusp")) 


df <- df %>% filter(!is.na(df$`Carimbo de data/hora`))




ggplot(df, aes(x = q37_voto_presidencia_hipotetico, fill = voto)) +
  geom_bar()


ggplot(df, aes(x = avaliacao, fill = q39_avaliacao_governo_lula)) +
  geom_bar()

summary(df$escala_lula)
summary(df$escala)

df$q35_posicao_espectro_politico <- as.numeric(gsub(" .*$", "", df$q35_posicao_espectro_politico))
df$escala <- as.numeric(df$escala)



df <- df %>%
  mutate(mudanca = df$escala - df$q35_posicao_espectro_politico)



df$q35_posicao_espectro_politico <- as.numeric(df$q35_posicao_espectro_politico)





ggplot(df, aes(x = factor(escala), fill = factor(q35_posicao_espectro_politico))) +
  geom_bar()

summary(df$mudanca)


ggplot(df, aes(x = mudanca)) +
  geom_bar()








df <- df %>%
  mutate(
    avaliacao = case_when(
      avaliacao == "Péssimo" ~ 1,
      avaliacao == "Ruim"    ~ 2,
      avaliacao == "Regular" ~ 3,
      avaliacao == "Bom"     ~ 4,
      avaliacao == "Ótimo"   ~ 5,
      TRUE ~ NA_real_
    ),
    
    q39_avaliacao_governo_lula = case_when(
      q39_avaliacao_governo_lula == "Péssimo" ~ 1,
      q39_avaliacao_governo_lula == "Ruim"    ~ 2,
      q39_avaliacao_governo_lula == "Regular" ~ 3,
      q39_avaliacao_governo_lula == "Bom"     ~ 4,
      q39_avaliacao_governo_lula == "Ótimo"   ~ 5,
      TRUE ~ NA_real_
    ),
    
    diferenca_aval = avaliacao - q39_avaliacao_governo_lula
    
  )


summary(df$diferenca_aval)


ggplot(df, aes(x = diferenca_aval)) +
  geom_bar()







df <- df %>%
  mutate(
    mudou_voto = q37_voto_presidencia_hipotetico != voto
  )


df_mudou <- df %>% filter(mudou_voto == TRUE)


modelo <- glm(
  mudou_voto ~ q38_alistamento_eleitoral_18,
  data = df,
  family = binomial(link = "logit")
)

summary(modelo)

exp(coef(modelo))

exp(confint(modelo))






modelo <- glm(
  mudou_voto ~ q37_voto_presidencia_hipotetico,
  data = df,
  family = binomial(link = "logit")
)

summary(modelo)

exp(coef(modelo))

exp(confint(modelo))






modelo <- glm(
  mudou_voto ~ escala + mudanca,
  data = df,
  family = binomial(link = "logit")
)

summary(modelo)

exp(coef(modelo))

exp(confint(modelo))




modelo <- glm(
  mudou_voto ~ q35_posicao_espectro_politico,
  data = df,
  family = binomial(link = "logit")
)

summary(modelo)

exp(coef(modelo))

exp(confint(modelo))





ggplot(filter(df, mudou_voto == TRUE), aes(x = q37_voto_presidencia_hipotetico, fill = voto)) +
  geom_bar()



ggplot(df, aes(x = escala)) +
  geom_bar()


ggplot(filter(df, mudou_voto == TRUE), aes(x = escala)) +
  geom_bar()



summary(df$escala)
summary(df_mudou$escala)



summary(df$q35_posicao_espectro_politico)
summary(df_mudou$q35_posicao_espectro_politico)




#aq é umas chapação do chatgpt mas pd ser util


df %>%
  count(q35_posicao_espectro_politico, escala)

tab <- table(df$q35_posicao_espectro_politico, df$escala)

tab


df %>%
  filter(escala == 1) %>% #ir mudando sla deve ter jeito mais facil
  count(q35_posicao_espectro_politico) %>%
  mutate(prop = n / sum(n))

df <- df %>%
  mutate(
    mudou = q35_posicao_espectro_politico != escala
  )
table(df$mudou)
df %>%
  count(mudou) %>%
  mutate(prop = n / sum(n))


tab2 <- table(
  factor(df$q35_posicao_espectro_politico, levels = 0:10),
  factor(df$escala, levels = 0:10)
)


StuartMaxwellTest(tab2) #distribuicao das respostas nao muda parece. bate com grafico da mudança ser meio normal





tab3 <- df %>%
  count(q35_posicao_espectro_politico, escala)

ggplot(tab3, aes(x = escala, y = q35_posicao_espectro_politico, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "black") +
  scale_fill_viridis_c() +
  labs(
    x = "Tempo 2",
    y = "Tempo 1",
    fill = "N",
    title = "Matriz de transição"
  ) +
  theme_minimal()



#comparar o ff (banco completo) com o df (o so com qm respondeu os dois) e a proporcao verdadeira.
#provavlmente ta errado


ff <- ff %>% filter(!is.na(q02_data_nascimento))
ff <- ff %>%
  separate(
    q24_modalidade_ingresso_usp,
    into = c("cota", "modalidade"),
    sep = ":\\s*"
  )

ff <- ff %>%
  filter(modalidade != "TRANFERÊNCIA INTERNA OU EXTERNA")

ff <- ff %>%
  mutate(cota = if_else(cota == "EP - PPI", "PPI", cota))

#pessoas que responderam ampla concorrencia no provao paulista oq é impossivel
ff[56, 25] <- "EP"
ff[30, 25] <- "EP"
ff[33, 25] <- "EP"


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


verdadeiro <- data.frame(
  cota = c("AC", "EP", "PPI"),
  n = c(91 + 12, 40 + 8 + 15, 23 + 4 + 11)
)

verdadeiro$Freq <- verdadeiro$n / sum(verdadeiro$n)

antigo <- ff %>%
  count(cota) %>%
  mutate(prop = n / sum(n))

atual <- df %>%
  count(cota) %>%
  mutate(prop = n / sum(n))

antigo
atual
verdadeiro

tab4 <- rbind(
  Verdadeiro = verdadeiro$n,
  atual = atual$n
)

colnames(tab4) <- verdadeiro$cota

tab4

chisq.test(tab4)

#===========================================================================

verdadeiro2 <- data.frame(
  ingresso = c("ENEM USP", "FUVEST", "PROVAO PAULISTA"),
  n = c(12+8+4, 91+20+23, 15+11)
)

verdadeiro2$Freq <- verdadeiro2$n / sum(verdadeiro2$n)

antigo2 <- ff %>%
  count(modalidade) %>%
  mutate(prop = n / sum(n))

atual2 <- df %>%
  count(modalidade) %>%
  mutate(prop = n / sum(n))

antigo2
atual2
verdadeiro2

tab5 <- rbind(
  Verdadeiro2 = verdadeiro2$n,
  atual2 = atual2$n
)

colnames(tab5) <- verdadeiro2$modalidade

tab5

chisq.test(tab5)

#==============================================================


verdadeiro3 <- data.frame(
  q32_periodo_matricula = c("Noturno", "Vespertino"),
  n = c(81+11+15, 73+13+11)
)

verdadeiro3$Freq <- verdadeiro3$n / sum(verdadeiro3$n)

antigo3 <- ff %>%
  count(q32_periodo_matricula) %>%
  mutate(prop = n / sum(n))

atual3 <- df %>%
  count(q32_periodo_matricula) %>%
  mutate(prop = n / sum(n))

antigo3
atual3
verdadeiro3

tab6 <- rbind(
  Verdadeiro3 = verdadeiro3$n,
  atual3 = atual3$n
)

colnames(tab6) <- verdadeiro3$q32_periodo_matricula

tab6

chisq.test(tab6)







