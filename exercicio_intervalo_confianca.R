# ===================================================
# Análise Comparativa de Intervalos de Confiança
# ===================================================
# Autora: Sophia Araújo
#
# Descrição:
# Este script realiza uma análise comparativa entre o método de Intervalo de Confiança Computacionalmente Intensivo (ICCI) e o método clássico de intervalo de confiança (usando a função t.test) para diferentes distribuições e tamanhos de amostra.
#
# Funções:
# 1. ICCI: Calcula o intervalo de confiança usando o método jackknife para uma amostra fornecida.
# 2. teste_distribuicoes: Avalia a cobertura dos intervalos de confiança (ICCI e IC clássico) para diferentes distribuições (normal, exponencial, poisson) e tamanhos de amostra. Realiza 1000 repetições para estimar a cobertura.
#
# Resultados:
# - A cobertura do ICCI e do IC clássico é calculada e comparada para cada combinação de distribuição e tamanho de amostra.
#
# Uso:
# - Ajuste os parâmetros das distribuições e tamanhos de amostra desejados e execute o script para obter os resultados.
# ===================================================


ICCI <- function(amostra, alfa) {
  estamostra <- mean(amostra)
  n <- length(amostra)
  pseudov <- numeric(n)

  for (i in 1:n) {
    amostrajack <- amostra[-i]
    estjack <- mean(amostrajack)
    pseudov[i] <- n * estamostra - (n - 1) * estjack
  }

  estpont <- mean(pseudov)
  epjack <- sd(pseudov) / sqrt(n)
  LI <- estpont - qt(1 - alfa / 2, df = n - 1) * epjack
  LS <- estpont + qt(1 - alfa / 2, df = n - 1) * epjack

  # Retorna os limites calculados
  list("IC(mu, 1-alfa)" = c(LI, LS))
}

# Função para executar o teste para diferentes distribuições e tamanhos de amostra
teste_distribuicoes <- function(distribuicao, tamanho_amostra) {
  alfa <- 0.05 # Intervalo de confiança de 95%
  m <- 1  # Média
  n <- tamanho_amostra  # Tamanho da amostra

  # Inicializar contadores das coberturas dos ICs
  cob_ICCI <- 0 # Cobertura do método ICCI
  cob_ICCL <- 0 # Cobertura do método clássico

  for (i in 1:1000) {  # Realizar 1000 repetições para reduzir a aleatoriedade
    # Gerar amostra de acordo com a distribuição e tamanho desejados
    if (distribuicao == "normal") {
      amostra <- rnorm(n, mean = m, sd = 1)
    } else if (distribuicao == "exponencial") {
      amostra <- rexp(n, rate = 1 / m)
    } else if (distribuicao == "poisson") {
      amostra <- rpois(n, lambda = m)
    } else {
      stop("Distribuição não reconhecida.")
    }

    # Calcular o IC computacionalmente intensivo (ICCI)
    LimCI <- ICCI(amostra, alfa)

    # Calcular a cobertura do ICCI
    if ((LimCI$"IC(mu, 1-alfa)"[1] < m) & (m < LimCI$"IC(mu, 1-alfa)"[2])) cob_ICCI <- cob_ICCI + 1

    # Calcular o IC clássico (método t.test)
    t_test <- t.test(amostra)
    Li <- t_test$conf.int[1]
    Ls <- t_test$conf.int[2]

    # Calcular a cobertura do método clássico
    if ((Li < m) & (m < Ls)) cob_ICCL <- cob_ICCL + 1
  }

  # Escrever coberturas
  list("Distribuicao" = distribuicao, "Tamanho_Amostra" = tamanho_amostra,
       "Cobertura_ICCI" = cob_ICCI, "Cobertura_ICCL" = cob_ICCL)
}

# Testar para diferentes distribuições e tamanhos de amostra
resultados <- list()

# Loop sobre as distribuições e tamanhos de amostra desejados
distribuicoes <- c("normal", "exponencial", "poisson")
tamanhos_amostra <- c(10, 20, 50, 100, 200)

for (dist in distribuicoes) {
  for (tam in tamanhos_amostra) {
    resultado <- teste_distribuicoes(dist, tam)
    resultados[[length(resultados) + 1]] <- resultado
  }
}
