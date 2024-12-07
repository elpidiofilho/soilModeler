#' Install and Load Required Packages  
#'  
#' This function checks if the specified R packages are installed. If any of the  
#' packages are not installed, it installs them. Then, it loads all the specified  
#' packages into the current R session.  
#'  
#' @param packages A character vector of package names to be installed and loaded.  
#'  
#' @return Invisibly returns a list of logical values indicating whether each package  
#'         was successfully loaded (`TRUE`) or not (`FALSE`).  
#'  
#' @details  
#' The function first checks the installed packages using `utils::installed.packages()`.  
#' If any packages are missing, it installs them using `utils::install.packages()`.  
#' After ensuring all packages are installed, it loads them using `library()`.  
#' Suppresses startup messages when loading packages.  
#'  
#' @examples  
#' \dontrun{  
#' # Example usage:  
#' install_and_load_packages(c("ggplot2", "dplyr", "tidyr"))  
#' }  
#'  
#' @importFrom utils installed.packages install.packages  
#' @export  
install_and_load_packages <- function(packages) {  
  installed <- rownames(installed.packages())  
  to_install <- packages[!packages %in% installed]  
  if (length(to_install) > 0) {  
    install.packages(to_install)  
  }  
  suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))  
}  
#' Extrai o limite de um raster como um objeto sf
#'
#' Esta função carrega um arquivo raster, substitui valores não-NA por 1,
#' converte o raster em polígonos e retorna um objeto sf.
#'
#' @param raster_path Caminho para o arquivo raster.
#' @return Um objeto sf representando o limite do raster.
#' @importFrom terra rast as.polygons crs
#' @importFrom sf st_as_sf
#' @examples
#' # resultado <- extrai_limite_raster('./raster_covariaveis_full/nasadem.tif')
extrai_limite_raster <- function(raster_path) {
  # Verifica se o arquivo existe
  if (!file.exists(raster_path)) {
    stop("O arquivo raster especificado não existe.")
  }
  
  # Carrega o raster
  ext <- terra::rast(raster_path)
  
  # Plota o raster
  plot(ext)
  
  # Substitui valores não-NA por 1
  ext[] <- ifelse(is.na(ext[]), NA, 1)
  
  # Converte o raster em polígonos
  pol <- terra::as.polygons(ext)
  
  # Converte para um objeto sf
  s <- sf::st_as_sf(pol, crs = terra::crs(ext))
  
  return(s)
}

#' Gera amostras aleatórias e salva como shapefile
#'
#' Esta função gera amostras aleatórias dentro de uma área especificada e salva
#' as amostras como um shapefile.
#'
#' @param shape_area Objeto sf representando a área de amostragem.
#' @param nsample Número de amostras a serem geradas.
#' @param type Tipo de amostragem (e.g., "random", "hexagonal").
#' @param output_path Caminho para salvar o shapefile de saída.
#' @param seed Semente para o gerador de números aleatórios.
#' @return Um objeto sf com as amostras geradas.
#' @importFrom sf st_sample st_as_sf st_set_crs st_crs
#' @importFrom sf st_write
gerar_amostras <- function(shape_area, nsample, type, output_path, seed) {
  # Verifica se o diretório de saída existe
  if (!dir.exists(dirname(output_path))) {
    stop("O diretório de saída especificado não existe.")
  }
  
  # Trava gerador de números aleatórios para reprodutibilidade
  set.seed(seed)
  
  # Cria vetor de amostras
  am_rand <- st_sample(x = shape_area, size = nsample, type = type, exact = TRUE)
  
  # Converte para sf e define o CRS
  am_rand_sf <- st_as_sf(data.frame(ID = 1:length(am_rand)), geometry = am_rand) %>%
    st_set_crs(st_crs(shape_area))  # Usa o mesmo CRS do objeto original
  
  # Cria shapefile de pontos no disco com a amostragem
  st_write(am_rand_sf, output_path, append = FALSE)
  return(am_rand_sf)
}

#' Extrai dados de uma pilha de rasters usando um shapefile de pontos
#'
#' Esta função extrai valores de uma pilha de rasters em locais especificados
#' por um shapefile de pontos.
#'
#' @param raster_folder Caminho para a pasta contendo os arquivos raster.
#' @param points_shapefile Caminho para o shapefile de pontos.
#' @return Data frame com os valores extraídos dos rasters.
#' @importFrom terra rast vect extract
extrair_dados_rasters <- function(raster_folder, points_shapefile) {
  # Verifica se o diretório e o shapefile existem
  if (!dir.exists(raster_folder)) {
    stop("O diretório de rasters especificado não existe.")
  }
  if (!file.exists(points_shapefile)) {
    stop("O shapefile de pontos especificado não existe.")
  }
  
  # Lista todos os arquivos de raster na pasta
  raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Carrega a pilha de rasters
  rasters <- terra::rast(raster_files)
  
  # Carrega o shapefile de pontos
  pontos <- terra::vect(points_shapefile)
  
  # Extrai os valores dos rasters nos pontos
  valores_extraidos <- terra::extract(rasters, pontos, xy = TRUE)
  
  # Retorna os valores extraídos
  return(valores_extraidos)
}

#' Cria um boxplot comparando dois métodos de amostragem
#'
#' Esta função cria um boxplot para comparar variáveis entre dois métodos de amostragem.
#'
#' @param df_rand_selected Data frame com dados do método de amostragem aleatória.
#' @param df_hexa_selected Data frame com dados do método de amostragem hexagonal.
#' @return Um objeto ggplot com o boxplot comparativo.
#' @import ggplot2 
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
criar_boxplot_comparativo <- function(df_rand_selected, df_hexa_selected) {
  # Adicionar uma coluna para identificar o método de amostragem
  df_rand_selected$Method <- "Random"
  df_hexa_selected$Method <- "Hexagonal"
  
  # Combinar os dois data frames
  df_combined <- bind_rows(df_rand_selected, df_hexa_selected)
  
  # Converter para formato longo para ggplot
  df_long <- df_combined %>%
    pivot_longer(cols = -Method, names_to = "Variable", values_to = "Value")
  
  # Criar o boxplot
  plot <- ggplot(df_long, aes(x = Method, y = Value, fill = Method)) +
    geom_boxplot() +
    labs(title = "Comparação de Variáveis por Método de Amostragem",
         x = "Variável",
         y = "Valor") +
    scale_fill_manual(values = c("Random" = "blue", "Hexagonal" = "green")) +
    facet_wrap(vars(Variable), scales = "free_y") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),  # Remove o texto do eixo x
      legend.position = "bottom"      # Move a legenda para abaixo do gráfico
    )
  
  return(plot)
}



#' Seleciona Componentes da PCA com Base no Limite de Autovalores
#'
#' Esta função realiza uma Análise de Componentes Principais (PCA) em um conjunto
#' de dados numéricos e seleciona os componentes com base em um limite de autovalores
#' especificado. Retorna o número de componentes selecionados e a variância total
#' explicada por eles.
#'
#' @param data Um dataframe ou matriz numérica. Todas as colunas devem ser numéricas.
#' @param eigenvalue_threshold Um valor numérico que especifica o limite mínimo de
#' autovalores para selecionar os componentes. O padrão é 1.
#'
#' @return Uma lista contendo:
#' \item{num_selected_components}{O número de componentes da PCA selecionados com base no limite de autovalores.}
#' \item{total_variance_explained}{A variância total explicada pelos componentes selecionados.}
#'
#' @details A função utiliza \code{\link[stats]{prcomp}} para realizar a PCA e calcula
#' os autovalores como o quadrado dos desvios padrão dos componentes principais.
#' Os componentes com autovalores maiores que o limite especificado são selecionados.
#'
#' @importFrom stats prcomp
#' @importFrom stats var
#'
#' @examples
#' # Exemplo de uso:
#' data <- mtcars[, c("mpg", "disp", "hp", "wt")]
#' resultado <- select_pca_componentes(data, eigenvalue_threshold = 1)
#' print(resultado)
#'
#' @export
select_pca_componentes <- function(data, eigenvalue_threshold = 1) {
  # Ensure the data is numeric
  if (!all(sapply(data, is.numeric))) {
    stop("All columns in the dataframe must be numeric.")
  }
  
  # Run PCA
  pca_result <- prcomp(data, scale. = TRUE)
  
  # Calculate eigenvalues
  eigenvalues <- pca_result$sdev^2
  
  # Select components based on the eigenvalue threshold
  selected_components <- eigenvalues > eigenvalue_threshold
  
  # Number of selected components
  num_selected_components <- sum(selected_components)
  
  # Total variance explained by selected components
  variance_explained <- eigenvalues / sum(eigenvalues)
  total_variance_explained <- sum(variance_explained[selected_components])
  
  return(list(
    num_selected_components = num_selected_components,
    total_variance_explained = total_variance_explained
  ))
}

# Example usage:
# df <- data.frame(matrix(rnorm(100), nrow=10))
# result <- pca_with_eigenvalue_threshold(df)
# print(result)




#' Seleciona variáveis por carga em PCA
#'
#' Esta função realiza uma análise de componentes principais (PCA) e seleciona
#' variáveis com cargas absolutas maiores que um limiar especificado.
#'
#' @param data Data frame com os dados para PCA.
#' @param num_componentes Número de componentes principais a considerar.
#' @param limiar_carga Limiar para seleção de variáveis com base na carga.
#' @return Vetor de nomes de variáveis selecionadas.
#' @importFrom stats prcomp
selecionar_por_carga <- function(data, num_componentes, limiar_carga) {
  # Realiza a PCA
  pca_result <- prcomp(data, scale. = TRUE)
  
  # Obtém as cargas dos componentes
  cargas <- pca_result$rotation[, 1:num_componentes]
  
  # Seleciona variáveis com cargas absolutas maiores que o limiar
  variaveis_selecionadas <- rownames(cargas)[apply(abs(cargas), 1, max) > limiar_carga]
  
  return(variaveis_selecionadas)
}

#' Seleciona variáveis por correlação com componentes principais
#'
#' Esta função realiza uma análise de componentes principais (PCA) e seleciona
#' variáveis com correlação absoluta maior que um limiar especificado com os componentes principais.
#'
#' @param data Data frame com os dados para PCA.
#' @param num_componentes Número de componentes principais a considerar.
#' @param limiar_correlacao Limiar para seleção de variáveis com base na correlação.
#' @return Vetor de nomes de variáveis selecionadas.
#' @importFrom stats prcomp cor
selecionar_por_correlacao <- function(data, num_componentes, limiar_correlacao) {
  # Realiza a PCA
  pca_result <- prcomp(data, scale. = TRUE)
  
  # Calcula as pontuações dos componentes principais
  scores <- pca_result$x[, 1:num_componentes]
  
  # Calcula a correlação entre as variáveis originais e os componentes principais
  correlacoes <- cor(data, scores)
  
  # Seleciona variáveis com correlação absoluta maior que o limiar
  variaveis_selecionadas <- rownames(correlacoes)[apply(abs(correlacoes), 1, max) > limiar_correlacao]
  
  return(variaveis_selecionadas)
}

#' Plota o gráfico de scree para PCA
#'
#' Esta função cria um gráfico de scree mostrando a variância explicada por cada componente principal.
#'
#' @param data Data frame com os dados para PCA.
#' @param num_componentes Número de componentes principais a plotar.
#' @return Um objeto ggplot com o gráfico de scree.
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal scale_y_continuous
#' @importFrom stats prcomp
#' @importFrom scales percent
plot_scree <- function(data, num_componentes) {
  # Realizar a PCA
  pca_result <- prcomp(data, scale. = TRUE)
  
  # Calcular a variância explicada por cada componente
  variancia_explicada <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  variancia_explicada_acumulada <- cumsum(variancia_explicada)
  
  # Criar um data frame para plotagem
  df_variancia <- data.frame(
    Componente = seq_along(variancia_explicada),
    VarianciaExplicada = variancia_explicada,
    VarianciaExplicadaAcumulada = variancia_explicada_acumulada
  )
  
  # Filtrar para o número de componentes desejado
  df_variancia <- df_variancia[1:num_componentes, ]
  
  # Plotar o gráfico de scree
  ggplot(df_variancia, aes(x = Componente)) +
    geom_bar(aes(y = VarianciaExplicada), stat = "identity", fill = "skyblue") +
    geom_text(aes(y = VarianciaExplicada, label = scales::percent(VarianciaExplicada, accuracy = 0.1)),
              vjust = -0.5, size = 3) +
    labs(
      title = "Gráfico de Scree",
      x = "Componente Principal",
      y = "Proporção de Variância Explicada"
    ) +
    theme_minimal() +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Variância Explicada Acumulada"))
}

#' Seleciona rasters por vetor de strings e cria um stack
#'
#' Esta função seleciona arquivos raster em um diretório com base em um vetor de padrões de string
#' e cria um stack de rasters.
#'
#' @param diretorio Caminho para o diretório contendo os arquivos raster.
#' @param padroes Vetor de strings com padrões para seleção de arquivos.
#' @return Um objeto rast representando o stack de rasters selecionados.
#' @importFrom terra rast
selecionar_rasters_por_vetor <- function(diretorio, padroes) {
  # Verifica se o diretório existe
  if (!dir.exists(diretorio)) {
    stop("O diretório especificado não existe.")
  }
  
  # Lista todos os arquivos no diretório
  arquivos <- list.files(diretorio, pattern = "\\.tif$", full.names = TRUE)
  
  # Inicializa uma lista para armazenar os rasters selecionados
  rasters_selecionados <- list()
  
  # Itera sobre cada padrão no vetor de strings
  for (padrao in padroes) {
    # Encontra o primeiro arquivo que contém o padrão no nome
    arquivo_encontrado <- arquivos[grepl(padrao, arquivos)]
    
    # Verifica se algum arquivo foi encontrado
    if (length(arquivo_encontrado) > 0) {
      # Carrega o raster e adiciona à lista
      rasters_selecionados[[padrao]] <- rast(arquivo_encontrado[1])
    } else {
      warning(paste("Nenhum arquivo encontrado para o padrão:", padrao))
    }
  }
  
  # Cria um stack com os rasters selecionados
  stack_rasters <- rast(rasters_selecionados)
  
  return(stack_rasters)
}

#' Converte um data frame em um shapefile de pontos
#'
#' Esta função converte um data frame com coordenadas x e y em um shapefile de pontos.
#'
#' @param dataframe Data frame contendo as coordenadas.
#' @param campo_x Nome do campo de coordenadas x.
#' @param campo_y Nome do campo de coordenadas y.
#' @param crs Sistema de referência de coordenadas (CRS) para o shapefile.
#' @param caminho_saida Caminho para salvar o shapefile de saída.
#' @return Um objeto sf representando os pontos.
#' @importFrom sf st_as_sf write_sf
converter_data_frame_para_shapefile <- function(dataframe, campo_x, campo_y, crs, caminho_saida) {
  # Verifica se os campos x e y existem no data frame
  if (!(campo_x %in% names(dataframe)) || !(campo_y %in% names(dataframe))) {
    stop("Os campos x e y especificados não existem no data frame.")
  }
  
  # Cria um objeto sf a partir do data frame
  pontos_sf <- st_as_sf(dataframe, coords = c(campo_x, campo_y), crs = crs)
  
  # Salva o objeto sf como um shapefile
  write_sf(pontos_sf, caminho_saida, append = FALSE)
  
  return(pontos_sf)
}



#' Detect Potential Factor Variables
#'
#' This function identifies columns in a dataframe that are either factors or have a limited number of unique values, suggesting they could be converted to factors.
#'
#' @param data A dataframe to be analyzed for potential factor variables.
#' @param unique_threshold An integer specifying the maximum number of unique values a column can have to be considered a potential factor. Default is 10.
#'
#' @return A list where the names are the column names of potential factor variables, and the values indicate whether they are factors or possibly factors.
#' @examples
#' df <- data.frame(a = c(1, 2, 2, 3), b = factor(c("x", "y", "x", "z")), c = c("a", "b", "a", "a"))
#' detect_factors(df)
#'
#' @export
detect_factors <- function(data, unique_threshold = 10) {
  # Check if the input is a dataframe
  if (!is.data.frame(data)) {
    stop("The input must be a dataframe.")
  }
  
  # Check if the unique_threshold is a positive integer
  if (!is.numeric(unique_threshold) || unique_threshold <= 0 || unique_threshold != as.integer(unique_threshold)) {
    stop("The 'unique_threshold' must be a positive integer.")
  }
  
  # Initialize a list to store the names of potential factor variables
  factor_vars <- list()
  
  # Iterate over each column in the dataset
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    
    # Check if the column is a factor
    if (is.factor(col_data)) {
      factor_vars[[col_name]] <- "Factor"
    } else {
      # Check if the column has a limited number of unique values
      num_unique_values <- length(unique(col_data))
      if (num_unique_values <= unique_threshold) {
        factor_vars[[col_name]] <- paste("Possibly Factor (", num_unique_values, " unique values)", sep = "")
      }
    }
  }
  
  return(factor_vars)
}



#' Agrupar Níveis Raros de um Fator
#'
#' Esta função agrupa níveis raros de um vetor do tipo fator em um novo nível com um nome especificado.
#'
#' @param fator Um vetor do tipo fator. Este é o vetor cujos níveis serão modificados.
#' @param niveis_para_agrupar Um vetor de caracteres contendo os níveis do fator que devem ser agrupados.
#' @param novo_nome Um valor de caractere que será o nome do novo nível que substituirá os níveis agrupados.
#'
#' @return Um vetor do tipo fator com os níveis especificados agrupados no novo nível.
#'
#' @details A função verifica se o vetor fornecido é do tipo fator e se os níveis especificados em 
#' \code{niveis_para_agrupar} existem no fator. Caso contrário, retorna um erro. Os níveis especificados 
#' são substituídos pelo \code{novo_nome}, e os demais níveis permanecem inalterados.
#'
#' @examples
#' # Criar um fator de exemplo
#' fator_exemplo <- factor(c("A", "B", "C", "A", "D", "E", "B", "C"))
#'
#' # Agrupar os níveis "B", "C" e "D" em "Outros"
#' fator_modificado <- agrupar_niveis_raros(fator_exemplo, c("B", "C", "D"), "Outros")
#'
#' # Ver o resultado
#' print(fator_modificado)
#' # [1] A      Outros Outros A      Outros E      Outros Outros
#' # Levels: A Outros E
#'
#' @export
agrupar_niveis_raros <- function(fator, niveis_para_agrupar, novo_nome) {
  # Verificar se o vetor é um fator
  if (!is.factor(fator)) {
    stop("O vetor fornecido não é do tipo fator.")
  }
  
  # Obter os níveis atuais do fator
  niveis_atuais <- levels(fator)
  
  # Verificar se os níveis para agrupar existem no fator
  niveis_invalidos <- setdiff(niveis_para_agrupar, niveis_atuais)
  if (length(niveis_invalidos) > 0) {
    stop(paste("Os seguintes níveis não existem no fator:", paste(niveis_invalidos, collapse = ", ")))
  }
  
  # Substituir os níveis especificados pelo novo nome
  novos_niveis <- niveis_atuais
  novos_niveis[niveis_atuais %in% niveis_para_agrupar] <- novo_nome
  
  # Atualizar os níveis do fator
  levels(fator) <- novos_niveis
  
  return(fator)
}



#' Plotar Tolerância x Kappa para Modelos RFE
#'
#' Esta função cria um gráfico que relaciona a tolerância e o número de variáveis selecionadas
#' com a métrica de desempenho (por padrão, Kappa) em um modelo RFE (Recursive Feature Elimination).
#'
#' @param model_rfe Um modelo RFE gerado pelo pacote \code{caret}, contendo os resultados do processo de seleção de variáveis.
#' @param metric Uma string indicando a métrica de desempenho a ser usada no gráfico. O padrão é \code{'Kappa'}.
#' @param tol_seq Um vetor numérico indicando a sequência de valores de tolerância a serem avaliados. O padrão é \code{seq(0.1, 10, by = 0.1)}.
#' @param maximize Um valor lógico indicando se a métrica deve ser maximizada (\code{TRUE}) ou minimizada (\code{FALSE}). O padrão é \code{TRUE}.
#'
#' @return Um gráfico gerado com \code{ggplot2} que mostra a relação entre o número de variáveis selecionadas, a tolerância e a métrica de desempenho.
#'
#' @details A função utiliza a função \code{\link[caret]{pickSizeTolerance}} do pacote \code{caret} para determinar o número de variáveis
#' selecionadas com base em diferentes valores de tolerância. Em seguida, plota a métrica de desempenho (por padrão, Kappa) em função
#' do número de variáveis selecionadas.
#'
#' @importFrom caret pickSizeTolerance
#' @import ggplot2
#'
#' @examples
#' # Exemplo de uso:
#' library(caret)
#' library(ggplot2)
#'
#' # Criar um modelo RFE de exemplo
#' set.seed(123)
#' control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
#' model_rfe <- rfe(iris[, 1:4], iris$Species, sizes = c(1:4), rfeControl = control)
#'
#' # Plotar Tolerância x Kappa
#' plot_rfe_tolerance_kappa(model_rfe, metric = "Kappa", tol_seq = seq(0.1, 5, by = 0.5))
#'
#' @export
plot_rfe_tolerance_kappa <- function(model_rfe, 
                                     metric = 'Kappa', 
                                     tol_seq = seq(0.1, 10, by = 0.1),
                                     maximize = TRUE) {
  # Initialize variables
  md <- model_rfe$results
  stol <- length(tol_seq)
  dfvar <- data.frame(posicao = integer(stol), 
                      tolerancia = numeric(stol), 
                      kappa = numeric(stol))
  
  # Loop through tolerance values
  for (cont in seq_along(tol_seq)) {
    tol <- tol_seq[cont]
    vvar <- caret::pickSizeTolerance(md, tol = tol, maximize = maximize, metric = metric)
    posicao <- which(md$Variables == vvar)
    kappa <- md[[metric]][posicao]
    
    dfvar$posicao[cont] <- md$Variables[posicao]
    dfvar$tolerancia[cont] <- tol
    dfvar$kappa[cont] <- kappa
  }
  
  # Plot the results
  ggplot(dfvar, aes(x = posicao, y = kappa)) +
    geom_point() +
    geom_text(aes(label = round(kappa, 2)), hjust = +0.5, vjust = 0.95, size = 3) +
    geom_line(linewidth = 1, color = 'red') +
    geom_vline(aes(xintercept = posicao), linetype = 'dashed', color = 'blue') +
    scale_x_continuous(breaks = dfvar$posicao) +  
    labs(title = 'Tolerância x Kappa',
         x = 'Num variaveis',
         y = 'Kappa') +
    theme_minimal()
}


#' Criar Gráficos de Métricas de Modelos
#'
#' Esta função gera gráficos para visualizar as métricas de desempenho (Accuracy e Kappa) de diferentes modelos.
#' Os gráficos incluem barras com médias e intervalos de confiança, além de gráficos de violino para a distribuição
#' das métricas. Os gráficos podem ser salvos em arquivos PNG e PDF.
#'
#' @param metricas Um dataframe contendo as métricas de desempenho dos modelos. Deve conter, no mínimo, as colunas:
#' \code{modelo}, \code{Accuracy} e \code{Kappa}.
#' @param gravar_graficos Um valor lógico indicando se os gráficos devem ser salvos em arquivos. O padrão é \code{TRUE}.
#' @param pasta_graficos Uma string indicando o nome da pasta onde os gráficos serão salvos. O padrão é \code{"graficos"}.
#' @param resolucao_ppt Um valor numérico indicando a resolução (DPI) para os gráficos em PNG. O padrão é \code{96}.
#' @param resolucao_texto Um valor numérico indicando a resolução (DPI) para os gráficos em PDF. O padrão é \code{300}.
#'
#' @return Um objeto de gráfico combinado gerado com o pacote \code{patchwork}, contendo os gráficos de barras e violino
#' para as métricas de desempenho.
#'
#' @details A função verifica se os pacotes necessários (\code{ggplot2}, \code{dplyr} e \code{patchwork}) estão instalados.
#' Caso contrário, retorna um erro. Também valida o dataframe de entrada para garantir que ele contenha as colunas necessárias
#' e que os valores sejam válidos. Os gráficos gerados incluem:
#' \itemize{
#'   \item Gráficos de barras para as médias de Accuracy e Kappa, com intervalos de confiança.
#'   \item Gráficos de violino para a distribuição de Accuracy e Kappa.
#' }
#' Se \code{gravar_graficos = TRUE}, os gráficos são salvos em PNG e PDF na pasta especificada.
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise
#' @importFrom patchwork plot_layout
#'
#' @examples
#' # Exemplo de uso:
#' library(ggplot2)
#' library(dplyr)
#' library(patchwork)
#'
#' # Criar um dataframe de exemplo
#' metricas <- data.frame(
#'   modelo = rep(c("Modelo A", "Modelo B", "Modelo C"), each = 10),
#'   Accuracy = c(rnorm(10, 0.8, 0.05), rnorm(10, 0.75, 0.05), rnorm(10, 0.85, 0.05)),
#'   Kappa = c(rnorm(10, 0.6, 0.05), rnorm(10, 0.55, 0.05), rnorm(10, 0.65, 0.05))
#' )
#'
#' # Gerar os gráficos e salvá-los
#' criar_graficos_metricas(metricas, gravar_graficos = TRUE, pasta_graficos = "graficos")
#'
#' @export
criar_graficos_metricas <- function(metricas, 
                                    gravar_graficos = TRUE, 
                                    pasta_graficos = "graficos",
                                    resolucao_ppt = 96, 
                                    resolucao_texto = 300) {
  
  # Verificar se os pacotes necessários estão instalados
  required_packages <- c("ggplot2", "dplyr", "patchwork")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Pacotes necessários não instalados: ", paste(missing_packages, collapse = ", "))
  }
  

  # Verificar se o dataframe tem as colunas necessárias
  required_cols <- c("modelo", "Accuracy", "Kappa")
  if (!all(required_cols %in% colnames(metricas))) {
    stop("O dataframe deve conter as colunas: ", paste(required_cols, collapse = ", "))
  }
  
  # Verificar se os tipos de dados estão corretos
  if (!is.logical(gravar_graficos)) {
    stop("gravar_graficos deve ser TRUE ou FALSE")
  }
  if (!is.character(pasta_graficos)) {
    stop("pasta_graficos deve ser uma string")
  }
  if (!is.numeric(resolucao_ppt) || resolucao_ppt <= 0) {
    stop("resolucao_ppt deve ser um número positivo")
  }
  if (!is.numeric(resolucao_texto) || resolucao_texto <= 0) {
    stop("resolucao_texto deve ser um número positivo")
  }
  
  # Verificar se há dados suficientes
  if (nrow(metricas) == 0) {
    stop("O dataframe está vazio")
  }
  
  # Verificar se há valores NA
  if (any(is.na(metricas[, required_cols]))) {
    warning("O dataframe contém valores NA nas colunas de métricas")
  }
  
  tryCatch({
    # Calcular a média de Accuracy e Kappa para cada modelo
    medias <- metricas %>%
      group_by(modelo) %>%
      summarise(AccuracyMedia = mean(Accuracy), 
                KappaMedia = mean(Kappa))
    
    # Ordenar os modelos pelo valor médio de Accuracy
    metricas$modelo <- factor(metricas$modelo, 
                              levels = medias$modelo[order(medias$AccuracyMedia)])
    
    # Criar o gráfico de barras para a acurácia média de cada modelo
    grafico_acuracia <- ggplot(metricas, aes(x = modelo, y = Accuracy)) +
      stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
      stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
      labs(title = "Acurácia Média por Modelo", x = "Modelo", y = "Acurácia Média") +
      theme_minimal()
    
    # Ordenar os modelos pelo valor médio de Kappa
    metricas$modelo <- factor(metricas$modelo, 
                              levels = medias$modelo[order(medias$KappaMedia)])
    
    # Criar o gráfico de barras para o Kappa médio de cada modelo
    grafico_kappa <- ggplot(metricas, aes(x = modelo, y = Kappa)) +
      stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
      stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
      labs(title = "Kappa Médio por Modelo", x = "Modelo", y = "Kappa Médio") +
      theme_minimal()
    
    # Criar o gráfico de violino para a distribuição da acurácia
    grafico_violino_acuracia <- ggplot(metricas, aes(x = modelo, y = Accuracy)) +
      geom_violin(fill = "lightgreen", color = "black", alpha = 0.7) +
      geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "darkgreen") +
      labs(title = "Distribuição da Acurácia por Modelo", 
           x = "Modelo", y = "Acurácia") +
      theme_minimal()
    
    # Criar o gráfico de violino para a distribuição do Kappa
    grafico_violino_kappa <- ggplot(metricas, aes(x = modelo, y = Kappa)) +
      geom_violin(fill = "lightcoral", color = "black", alpha = 0.7) +
      geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "darkred") +
      labs(title = "Distribuição do Kappa por Modelo", 
           x = "Modelo", y = "Kappa") +
      theme_minimal()
    
    # Adicionar margens para criar separação visual
    grafico_acuracia <- grafico_acuracia + 
      theme(plot.margin = margin(10, 10, 10, 10))
    grafico_kappa <- grafico_kappa + 
      theme(plot.margin = margin(10, 10, 10, 10))
    grafico_violino_acuracia <- grafico_violino_acuracia + 
      theme(plot.margin = margin(10, 10, 10, 10))
    grafico_violino_kappa <- grafico_violino_kappa + 
      theme(plot.margin = margin(10, 10, 10, 10))
    
    # Combinar os gráficos usando patchwork com margens
    layout_combined <- (grafico_acuracia | grafico_kappa) / 
      (grafico_violino_acuracia | grafico_violino_kappa) +
      plot_layout(guides = "collect")
    
    # Se gravar_graficos for TRUE, salvar os arquivos
    if (gravar_graficos) {
      tryCatch({
        # Verificar se a pasta existe, se não, criá-la
        if (!dir.exists(pasta_graficos)) {
          dir.create(pasta_graficos, recursive = TRUE)
        }
        
        # Salvar o layout combinado como PNG para PowerPoint
        ggsave(file.path(pasta_graficos, "layout_combined_ppt.png"), 
               plot = layout_combined, 
               width = 10, 
               height = 8, 
               dpi = resolucao_ppt)
        
        # Salvar o layout combinado como PDF para relatórios
        ggsave(file.path(pasta_graficos, "layout_combined_pdf.pdf"), 
               plot = layout_combined, 
               width = 10, 
               height = 8, 
               dpi = resolucao_texto)
      }, error = function(e) {
        warning("Erro ao salvar os arquivos: ", e$message)
      })
    }
    
    # Retornar o layout combinado
    return(layout_combined)
    
  }, error = function(e) {
    stop("Erro ao criar os gráficos: ", e$message)
  })
}


#' Criar Abreviações para Nomes  
#'  
#' Esta função cria abreviações para uma lista de nomes com base em um dicionário  
#' de abreviações específicas. Se um nome não estiver no dicionário, ele será retornado  
#' sem alterações.  
#'  
#' @param nomes Um vetor de caracteres contendo os nomes que devem ser abreviados.  
#'  
#' @return Um vetor de caracteres com os nomes abreviados, de acordo com o dicionário.  
#'  
#' @details  
#' A função utiliza um dicionário interno de abreviações específicas (`dict_especifico`)  
#' para substituir nomes conhecidos por suas versões abreviadas. Caso um nome não esteja  
#' no dicionário, ele será retornado sem alterações.  
#'  
#' @examples  
#' \dontrun{  
#' # Exemplo de uso:  
#' nomes <- c("solar_diffuse", "geologia", "outro_nome")  
#' criar_abreviacoes(nomes)  
#' # Retorna: c("sol_dif", "geol", "outro_nome")  
#' }  
criar_abreviacoes <- function(nomes) {    
  # Dicionário de abreviações específicas    
  dict_especifico <- c(    
    "solar_diffuse" = "sol_dif",    
    "effective_air_flow_heights" = "air_flow",    
    "Mag_AMG_MAGIGRF_UP150_ASA" = "mag_asa",    
    "uso_cobertura" = "uso_cob",    
    "geomorfologia" = "geomorf",    
    "geologia" = "geol",    
    "Gama_AMG_eTh" = "gama_th"    
  )    
  
  # Substituir nomes com base no dicionário    
  sapply(nomes, function(nome) {    
    if (nome %in% names(dict_especifico)) {    
      dict_especifico[[nome]]    
    } else {    
      nome    
    }    
  })    
}  


#' Abreviar Nomes Não Específicos
#'
#' Esta função cria abreviações para nomes fornecidos. Se o nome estiver em um dicionário específico, 
#' a abreviação correspondente será usada. Caso contrário, uma abreviação automática será gerada.
#'
#' @param nome Um vetor de caracteres contendo o(s) nome(s) a ser(em) abreviado(s).
#'
#' @return Um vetor de caracteres contendo as abreviações dos nomes fornecidos.
#' 
#' @details A função verifica se o nome está presente em um dicionário específico (\code{dict_especifico}). 
#' Se estiver, retorna a abreviação correspondente. Caso contrário, cria uma abreviação automática:
#' \itemize{
#'   \item Se o nome contém underscores (\code{"_"}), a abreviação será formada pelas três primeiras letras de cada parte.
#'   \item Se o nome não contém underscores, a abreviação será formada pelos seis primeiros caracteres do nome.
#' }
#'
#' @examples
#' # Exemplo de uso:
#' dict_especifico <- c("nome_completo" = "n_c", "outro_nome" = "o_n")
#' abreviar_nome("nome_completo") # Retorna "n_c"
#' abreviar_nome("exemplo_nome")  # Retorna "exe_nom"
#' abreviar_nome("exemplo")       # Retorna "exempl"
#'
#' @export
abreviar_nome <- function(nome, dict_especifico = NULL) {
  # Se existe no dicionário específico, use essa abreviação
  if (nome %in% names(dict_especifico)) {
    return(dict_especifico[nome])
  }
  
  # Para outros casos, criar abreviação automática
  partes <- strsplit(nome, "_")[[1]]
  if (length(partes) > 1) {
    # Se tem underscore, pega as três primeiras letras de cada parte
    paste0(substr(partes, 1, 3), collapse = "_")
  } else {
    # Se não tem underscore, pega os primeiros seis caracteres
    substr(nome, 1, 6)
  }
}


#' Abreviar Nomes e Criar Tabela de Referência  
#'  
#' Esta função aplica abreviações a uma lista de nomes, cria uma tabela de referência  
#' com os nomes originais e abreviados, e retorna ambos como uma lista.  
#'  
#' @param nomes Um vetor de caracteres contendo os nomes que devem ser abreviados.  
#'  
#' @return Uma lista com dois elementos:  
#' \describe{  
#'   \item{abreviacoes}{Um vetor de caracteres com os nomes abreviados.}  
#'   \item{tabela_referencia}{Um data frame com duas colunas: `nome_original` e `nome_abreviado`.}  
#' }  
#'  
#' @details  
#' A função utiliza a função auxiliar `abreviar_nome` para aplicar abreviações a cada  
#' nome fornecido. Em seguida, cria uma tabela de referência que relaciona os nomes  
#' originais com suas respectivas abreviações. Essa tabela pode ser útil para rastrear  
#' as alterações realizadas.  
#'  
#' @examples  
#' # Exemplo de uso:  
#' nomes <- c("solar_diffuse", "geologia", "geomorfologia")  
#' resultado <- abrevia(nomes)  
#' print(resultado$abreviacoes)  
#' print(resultado$tabela_referencia)  
#'  
#' @seealso \code{\link{abreviar_nome}} para a função auxiliar usada para abreviações.  
#'  
#' @export  
abrevia <- function(nomes) {  
  # Aplicar abreviação a todos os nomes  
  abreviacoes <- sapply(nomes, abreviar_nome)  
  
  # Criar tabela de referência  
  ref_table <- data.frame(  
    nome_original = nomes,  
    nome_abreviado = abreviacoes,  
    stringsAsFactors = FALSE  
  )  
  
  # Retornar lista com abreviações e tabela de referência  
  return(list(  
    abreviacoes = abreviacoes,  
    tabela_referencia = ref_table  
  ))  
}  

#' Criar Gráficos de Importância das Variáveis
#'
#' @param dados_importancia DataFrame contendo as métricas de importância das variáveis
#' @param gravar_graficos Logical, indica se os gráficos devem ser salvos
#' @param pasta_graficos String, nome da pasta onde os gráficos serão salvos
#' @param resolucao_ppt Numeric, resolução para arquivos PPT (DPI)
#' @param resolucao_texto Numeric, resolução para arquivos de texto (DPI)
#' @param largura_grafico Numeric, largura do gráfico em polegadas
#' @param altura_grafico Numeric, altura do gráfico em polegadas
#'
#' @return Lista contendo os gráficos individuais e o layout combinado
#'
#' Criar Gráficos de Importância de Variáveis
#'
#' Esta função gera gráficos para analisar a importância das variáveis em diferentes modelos. 
#' Os gráficos incluem:
#' \itemize{
#'   \item Um gráfico de barras com a importância média das variáveis e seus desvios padrão.
#'   \item Um heatmap mostrando a frequência relativa das variáveis no top 10 de importância.
#' }
#' Os gráficos podem ser salvos em arquivos PNG com resoluções configuráveis.
#'
#' @param dados_importancia Um dataframe contendo as informações de importância das variáveis. 
#' Deve conter, no mínimo, as colunas:
#' \code{modelo}, \code{variavel_abrev}, \code{importancia_media}, \code{importancia_sd}, \code{freq_relativa_top10}.
#' @param gravar_graficos Um valor lógico indicando se os gráficos devem ser salvos em arquivos. O padrão é \code{TRUE}.
#' @param pasta_graficos Uma string indicando o nome da pasta onde os gráficos serão salvos. O padrão é \code{"graficos"}.
#' @param resolucao_ppt Um valor numérico indicando a resolução (DPI) para os gráficos em PNG para apresentações. O padrão é \code{96}.
#' @param resolucao_texto Um valor numérico indicando a resolução (DPI) para os gráficos em PNG para relatórios. O padrão é \code{300}.
#' @param largura_grafico Um valor numérico indicando a largura dos gráficos salvos (em polegadas). O padrão é \code{15}.
#' @param altura_grafico Um valor numérico indicando a altura dos gráficos salvos (em polegadas). O padrão é \code{8}.
#'
#' @return Uma lista contendo os seguintes elementos:
#' \itemize{
#'   \item \code{grafico_barras}: O gráfico de barras com a importância média das variáveis.
#'   \item \code{grafico_heatmap}: O heatmap com a frequência relativa das variáveis no top 10.
#'   \item \code{layout_combinado}: O layout combinado dos dois gráficos.
#' }
#'
#' @details A função verifica se os pacotes necessários (\code{ggplot2} e \code{patchwork}) estão instalados. 
#' Caso contrário, retorna um erro. Também valida o dataframe de entrada para garantir que ele contenha as colunas necessárias.
#' Se \code{gravar_graficos = TRUE}, os gráficos são salvos em PNG na pasta especificada.
#'
#' @import ggplot2 
#' @importFrom patchwork plot_layout
#'
#' @examples
#' # Exemplo de uso:
#' library(ggplot2)
#' library(patchwork)
#'
#' # Criar um dataframe de exemplo
#' dados_importancia <- data.frame(
#'   modelo = rep(c("Modelo A", "Modelo B"), each = 10),
#'   variavel_abrev = rep(paste0("Var", 1:10), 2),
#'   importancia_media = runif(20, 0.1, 1),
#'   importancia_sd = runif(20, 0.01, 0.1),
#'   freq_relativa_top10 = runif(20, 0, 1)
#' )
#'
#' # Gerar os gráficos e salvá-los
#' criar_graficos_importancia(dados_importancia, gravar_graficos = TRUE, pasta_graficos = "graficos")
#'
#' @export
criar_graficos_importancia <- function(dados_importancia, 
                                       gravar_graficos = TRUE, 
                                       pasta_graficos = "graficos",
                                       resolucao_ppt = 96, 
                                       resolucao_texto = 300,
                                       largura_grafico = 15,
                                       altura_grafico = 8) {
  
  # Verificar se os pacotes necessários estão instalados
  required_packages <- c("ggplot2", "patchwork")
  for(pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Pacote", pkg, "precisa ser instalado"))
    }
  }
  
  # Verificar se os dados contêm as colunas necessárias
  colunas_necessarias <- c("modelo", "variavel_abrev", "importancia_media", 
                           "importancia_sd", "freq_relativa_top10")
  if (!all(colunas_necessarias %in% colnames(dados_importancia))) {
    stop("Dados não contêm todas as colunas necessárias")
  }
  
  # Criar gráfico de barras
  p <- ggplot(dados_importancia, 
              aes(x = reorder(variavel_abrev, importancia_media), 
                  y = importancia_media)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_errorbar(aes(ymin = importancia_media - importancia_sd,
                      ymax = importancia_media + importancia_sd),
                  width = 0.2) +
    facet_wrap(~modelo, scales = "free") +
    coord_flip() +
    labs(title = "Top 10 Variáveis Mais Importantes por Modelo",
         x = "Variável",
         y = "Importância Média") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold"),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # Criar heatmap
  h <- ggplot(dados_importancia, 
              aes(x = modelo, y = variavel_abrev, fill = freq_relativa_top10)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = "Frequência das Variáveis no Top 10",
         fill = "Freq. Relativa",
         y = "Variável",
         x = "Modelo") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  # Combinar os gráficos
  layout_combined <- p + h + 
    plot_layout(widths = c(2, 1)) +
    plot_annotation(
      title = "Análise de Importância das Variáveis",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )
  
  # Gravar gráficos se solicitado
  if (gravar_graficos) {
    # Criar pasta se não existir
    if (!dir.exists(pasta_graficos)) {
      dir.create(pasta_graficos)
    }
    
    # Salvar versão para PPT
    ggsave(file.path(pasta_graficos, "importancia_variaveis_ppt.png"),
           layout_combined,
           width = largura_grafico,
           height = altura_grafico,
           dpi = resolucao_ppt)
    
    # Salvar versão para texto
    ggsave(file.path(pasta_graficos, "importancia_variaveis_texto.png"),
           layout_combined,
           width = largura_grafico,
           height = altura_grafico,
           dpi = resolucao_texto)
  }
  
  # Retornar lista com os gráficos
  return(list(
    grafico_barras = p,
    grafico_heatmap = h,
    layout_combinado = layout_combined
  ))
}


#' Adicionar Abreviações às Variáveis
#'
#' Esta função adiciona abreviações às variáveis de um objeto de resultados, utilizando uma tabela de referência
#' fornecida. As abreviações são adicionadas às tabelas \code{top_variaveis} e \code{resumo_importancia} do objeto.
#'
#' @param result Um objeto contendo os resultados, que deve incluir as tabelas \code{top_variaveis} e \code{resumo_importancia}.
#' @param resultado_abrev Um objeto contendo as abreviações e a tabela de referência. Deve incluir:
#' \itemize{
#'   \item \code{abreviacoes}: Um vetor de abreviações.
#'   \item \code{tabela_referencia}: Um dataframe com a coluna \code{nome_original}, que mapeia os nomes originais às abreviações.
#' }
#'
#' @return O objeto \code{result} atualizado, com as colunas \code{variavel_abrev} adicionadas às tabelas \code{top_variaveis} e \code{resumo_importancia}.
#'
#' @details A função utiliza a função \code{match} para mapear os nomes originais das variáveis às suas abreviações,
#' com base na tabela de referência fornecida em \code{resultado_abrev}.
#'
#' @examples
#' # Exemplo de uso:
#' result <- list(
#'   top_variaveis = data.frame(variavel = c("variavel1", "variavel2")),
#'   resumo_importancia = data.frame(variavel = c("variavel1", "variavel3"))
#' )
#'
#' resultado_abrev <- list(
#'   abreviacoes = c("var1", "var2", "var3"),
#'   tabela_referencia = data.frame(nome_original = c("variavel1", "variavel2", "variavel3"))
#' )
#'
#' result_atualizado <- add_variaveis_abreviadas(result, resultado_abrev)
#' print(result_atualizado)
#'
#' @export
add_variaveis_abreviadas <- function(result, resultado_abrev) {
  if (!"variavel" %in% colnames(result$top_variaveis)) {
    stop("A tabela 'top_variaveis' não contém a coluna 'variavel'.")
  }
  if (!"nome_original" %in% colnames(resultado_abrev$tabela_referencia)) {
    stop("A tabela de referência não contém a coluna 'nome_original'.")
  }
  
  result$top_variaveis$variavel_abrev <- resultado_abrev$abreviacoes[match(result$top_variaveis$variavel, 
                                                                           resultado_abrev$tabela_referencia$nome_original)]
  
  result$resumo_importancia$variavel_abrev <- resultado_abrev$abreviacoes[match(result$resumo_importancia$variavel, 
                                                                                resultado_abrev$tabela_referencia$nome_original)]
  
  return(result)
}


#' Processar Resultados de Treinamento
#'
#' Esta função processa os resultados de importância das variáveis de diferentes modelos, calculando estatísticas
#' descritivas e identificando as variáveis mais importantes (top 10) para cada modelo.
#'
#' @param importancias Uma lista de dataframes, onde cada dataframe contém as importâncias das variáveis para um modelo.
#' Deve incluir, no mínimo, as colunas \code{modelo} e \code{Overall}.
#' @param resultados (Opcional) Um objeto adicional que pode ser utilizado para complementar o processamento. 
#' Atualmente, não é utilizado diretamente na função.
#'
#' @return Uma lista contendo:
#' \itemize{
#'   \item \code{resumo_importancia}: Um dataframe com estatísticas descritivas das importâncias das variáveis por modelo.
#'   \item \code{top_variaveis}: Um dataframe com as 10 variáveis mais importantes para cada modelo.
#' }
#'
#' @details A função combina as importâncias de todas as variáveis em um único dataframe e calcula as seguintes estatísticas:
#' \itemize{
#'   \item \code{importancia_media}: Média da importância da variável.
#'   \item \code{importancia_sd}: Desvio padrão da importância da variável.
#'   \item \code{importancia_min}: Valor mínimo da importância da variável.
#'   \item \code{importancia_max}: Valor máximo da importância da variável.
#'   \item \code{n_vezes_top10}: Número de vezes que a variável aparece no top 10 (com base no percentil 90).
#'   \item \code{freq_relativa_top10}: Frequência relativa da variável no top 10.
#' }
#' Em seguida, identifica as 10 variáveis mais importantes para cada modelo.
#'
#' @importFrom dplyr group_by summarise arrange desc slice_head n
#' @importFrom stats quantile sd
#'
#' @examples
#' # Exemplo de uso:
#' library(dplyr)
#'
#' # Criar uma lista de dataframes de exemplo
#' importancias <- list(
#'   data.frame(modelo = "Modelo A", variavel = paste0("Var", 1:15), Overall = runif(15, 0, 1)),
#'   data.frame(modelo = "Modelo B", variavel = paste0("Var", 1:15), Overall = runif(15, 0, 1))
#' )
#'
#' # Processar os resultados
#' resultados <- processa_resultados_train(importancias = importancias)
#'
#' # Visualizar os resultados
#' print(resultados$resumo_importancia)
#' print(resultados$top_variaveis)
#'
#' @export
processa_resultados_train <- function(importancias = importancias, resultados = resultados) {
  # Verificar se há importâncias para analisar
  if (length(importancias) > 0) {
    # Combinar todas as importâncias em um único dataframe
    todas_importancias <- do.call(rbind, importancias)
    
    # Calcular frequência e estatísticas por modelo
    resumo_importancia <- todas_importancias %>%
      group_by(modelo, variavel) %>%
      summarise(
        importancia_media = mean(Overall),
        importancia_sd = sd(Overall),
        importancia_min = min(Overall),
        importancia_max = max(Overall),
        n_vezes_top10 = sum(Overall >= quantile(Overall, 0.9)),
        freq_relativa_top10 = n_vezes_top10 / n(),
        .groups = 'drop'
      ) %>%
      arrange(modelo, desc(importancia_media))
    
    # Visualizar top 10 variáveis mais importantes por modelo
    top_variaveis <- resumo_importancia %>%
      group_by(modelo) %>%
      slice_head(n = 10)
  }
  return(list(resumo_importancia = resumo_importancia, top_variaveis = top_variaveis))
}




#' Obter Significado de Variáveis Bioclimáticas
#'
#' Esta função retorna os significados das variáveis bioclimáticas com base em seus nomes padrão.
#' As variáveis bioclimáticas são amplamente utilizadas em estudos ambientais e ecológicos.
#'
#' @param nomes_variaveis Um vetor de caracteres contendo os nomes das variáveis bioclimáticas (ex.: \code{"bio_1"}, \code{"bio_12"}).
#'
#' @return Um vetor de caracteres com os significados correspondentes às variáveis fornecidas. 
#' Caso o nome da variável não seja reconhecido, o retorno será \code{"Variável desconhecida"}.
#'
#' @details A função utiliza a estrutura \code{switch} para mapear os nomes das variáveis bioclimáticas aos seus significados. 
#' Os nomes reconhecidos e seus significados são:
#' \itemize{
#'   \item \code{bio_1}: Temperatura Média Anual
#'   \item \code{bio_2}: Amplitude Térmica Diária Média
#'   \item \code{bio_3}: Isotermalidade (\code{bio_2/bio_7} * 100)
#'   \item \code{bio_4}: Sazonalidade da Temperatura (desvio padrão * 100)
#'   \item \code{bio_5}: Temperatura Máxima do Mês Mais Quente
#'   \item \code{bio_6}: Temperatura Mínima do Mês Mais Frio
#'   \item \code{bio_7}: Amplitude Térmica Anual (\code{bio_5-bio_6})
#'   \item \code{bio_8}: Temperatura Média do Trimestre Mais Úmido
#'   \item \code{bio_9}: Temperatura Média do Trimestre Mais Seco
#'   \item \code{bio_10}: Temperatura Média do Trimestre Mais Quente
#'   \item \code{bio_11}: Temperatura Média do Trimestre Mais Frio
#'   \item \code{bio_12}: Precipitação Anual
#'   \item \code{bio_13}: Precipitação do Mês Mais Úmido
#'   \item \code{bio_14}: Precipitação do Mês Mais Seco
#'   \item \code{bio_15}: Sazonalidade da Precipitação (Coeficiente de Variação)
#'   \item \code{bio_16}: Precipitação do Trimestre Mais Úmido
#'   \item \code{bio_17}: Precipitação do Trimestre Mais Seco
#'   \item \code{bio_18}: Precipitação do Trimestre Mais Quente
#'   \item \code{bio_19}: Precipitação do Trimestre Mais Frio
#' }
#'
#' @examples
#' # Exemplo de uso:
#' nomes <- c("bio_1", "bio_12", "bio_20")
#' significados <- bioclim(nomes)
#' print(significados)
#' # Resultado:
#' # [1] "Temperatura Média Anual" "Precipitação Anual" "Variável desconhecida"
#'
#' @export
bioclim <- function(nomes_variaveis) {
  # Função auxiliar para mapear um único nome de variável ao seu significado
  obter_significado <- function(nome_variavel) {
    significado <- switch(nome_variavel,
                          bio_1 = "Temperatura Média Anual",
                          bio_2 = "Amplitude Térmica Diária Média",
                          bio_3 = "Isotermalidade (bio_2/bio_7) (* 100)",
                          bio_4 = "Sazonalidade da Temperatura (desvio padrão * 100)",
                          bio_5 = "Temperatura Máxima do Mês Mais Quente",
                          bio_6 = "Temperatura Mínima do Mês Mais Frio",
                          bio_7 = "Amplitude Térmica Anual (bio_5-bio_6)",
                          bio_8 = "Temperatura Média do Trimestre Mais Úmido",
                          bio_9 = "Temperatura Média do Trimestre Mais Seco",
                          bio_10 = "Temperatura Média do Trimestre Mais Quente",
                          bio_11 = "Temperatura Média do Trimestre Mais Frio",
                          bio_12 = "Precipitação Anual",
                          bio_13 = "Precipitação do Mês Mais Úmido",
                          bio_14 = "Precipitação do Mês Mais Seco",
                          bio_15 = "Sazonalidade da Precipitação (Coeficiente de Variação)",
                          bio_16 = "Precipitação do Trimestre Mais Úmido",
                          bio_17 = "Precipitação do Trimestre Mais Seco",
                          bio_18 = "Precipitação do Trimestre Mais Quente",
                          bio_19 = "Precipitação do Trimestre Mais Frio",
                          "Variável desconhecida")
    return(significado)
  }
  
  # Aplicar a função auxiliar a cada elemento do vetor de entrada
  significados <- sapply(nomes_variaveis, obter_significado)
  
  return(significados)
}




#' Obter Nome Completo do Modelo
#'
#' Esta função retorna o nome completo de um modelo de machine learning com base no seu nome abreviado.
#' A função utiliza a lista de modelos disponíveis fornecida pela função \code{getModelInfo()} do pacote \code{caret}.
#'
#' @param nome_abreviado Uma string contendo o nome abreviado do modelo (ex.: \code{"rf"} para Random Forest).
#'
#' @return Uma string com o nome completo do modelo. Caso o nome abreviado não seja encontrado, retorna \code{"Modelo desconhecido"}.
#'
#' @details A função utiliza a função \code{getModelInfo()} do pacote \code{caret} para obter informações sobre os modelos disponíveis.
#' O nome completo do modelo é extraído do campo \code{label} correspondente ao nome abreviado fornecido.
#'
#' @importFrom caret getModelInfo
#'
#' @examples
#' # Exemplo de uso:
#' library(caret)
#'
#' # Obter o nome completo do modelo Random Forest
#' nome_completo <- obter_nome_completo_modelo("rf")
#' print(nome_completo)
#'
#' # Tentar obter o nome de um modelo inexistente
#' nome_invalido <- obter_nome_completo_modelo("modelo_invalido")
#' print(nome_invalido)
#'
#' @export
obter_nome_completo_modelo <- function(nome_abreviado) {
  # Obter informações sobre todos os modelos
  modelos_info <- getModelInfo()
  
  # Obter a lista de nomes abreviados
  nomes_abreviados <- names(modelos_info)
  
  # Encontrar a posição do nome abreviado
  posicao <- match(nome_abreviado, nomes_abreviados)
  
  # Verificar se o nome abreviado foi encontrado
  if (!is.na(posicao)) {
    # Retornar o label correspondente
    return(modelos_info[[posicao]]$label)
  } else {
    return("Modelo desconhecido")
  }
}




#' Criar Gráfico de Importância das Variáveis
#'
#' Esta função cria um gráfico de barras que exibe a importância média das variáveis para um modelo específico,
#' incluindo barras de erro que representam o desvio padrão da importância.
#'
#' @param sigla_modelo Uma string contendo a sigla do modelo (ex.: \code{"rf"} para Random Forest).
#' @param result_abrev Um objeto contendo os resultados processados, incluindo a tabela \code{resumo_importancia}.
#' Deve conter, no mínimo, as colunas \code{modelo}, \code{variavel}, \code{importancia_media} e \code{importancia_sd}.
#'
#' @return Um objeto \code{ggplot} representando o gráfico de importância das variáveis para o modelo especificado.
#'
#' @details A função utiliza a função \code{obter_nome_completo_modelo} para obter o nome completo do modelo com base na sigla fornecida.
#' Em seguida, filtra os dados de importância para o modelo especificado e cria um gráfico de barras com as importâncias médias
#' e os desvios padrão.
#'
#' @importFrom dplyr filter select arrange desc
#' @import ggplot2
#' @importFrom patchwork plot_annotation
#' @importFrom stats reorder
#' @examples
#' # Exemplo de uso:
#' library(ggplot2)
#' library(dplyr)
#'
#' # Criar um dataframe de exemplo
#' result_abrev <- list(
#'   resumo_importancia = data.frame(
#'     modelo = c("rf", "rf", "rf", "xgb", "xgb"),
#'     variavel = c("Var1", "Var2", "Var3", "Var1", "Var2"),
#'     importancia_media = c(0.8, 0.6, 0.4, 0.9, 0.7),
#'     importancia_sd = c(0.1, 0.2, 0.1, 0.15, 0.1)
#'   )
#' )
#'
#' # Criar o gráfico para o modelo Random Forest
#' grafico <- criar_grafico_importancia("rf", result_abrev)
#' print(grafico)
#'
#' @export
criar_grafico_importancia <- function(sigla_modelo, result_abrev) {
  # Obter o nome completo do modelo
  nome_completo <- obter_nome_completo_modelo(sigla_modelo)
  
  # Filtrar e preparar os dados
  imp_med <- result_abrev$resumo_importancia %>% 
    dplyr::filter(modelo == sigla_modelo) %>% 
    dplyr::select(variavel, importancia_media, importancia_sd) %>% 
    dplyr::arrange(desc(importancia_media))
  
  # Criar o gráfico
  ggplot(imp_med, aes(x = reorder(variavel, importancia_media), y = importancia_media)) +
    geom_bar(stat = 'identity', fill = 'skyblue') +
    geom_errorbar(aes(ymin = importancia_media - importancia_sd, ymax = importancia_media + importancia_sd), 
                  width = 0.2, 
                  color = 'black') +
    coord_flip() +
    labs(title = paste('Importância das variáveis -', nome_completo),
         x = 'Variável',
         y = 'Importância média') +
    theme_minimal()
}


#' Visualizar Árvore de Decisão de um Modelo Random Forest
#'
#' Esta função gera uma visualização gráfica de uma árvore de decisão específica de um modelo Random Forest.
#' A árvore é extraída do modelo e representada como um dendrograma, com informações sobre os nós de decisão,
#' pontos de divisão e previsões nas folhas.
#'
#' @param final_model Um modelo Random Forest treinado, criado com a função \code{randomForest()} do pacote \code{randomForest}.
#' @param tree_num Um número inteiro indicando o índice da árvore a ser visualizada no modelo Random Forest.
#'
#' @return Um objeto \code{ggplot} representando a árvore de decisão como um dendrograma.
#'
#' @details A função utiliza a função \code{getTree()} do pacote \code{randomForest} para extrair a estrutura da árvore.
#' Em seguida, a estrutura é convertida em um grafo utilizando o pacote \code{igraph}, e a visualização é gerada com o pacote \code{ggraph}.
#' Os nós de decisão, pontos de divisão e previsões nas folhas são exibidos no gráfico.
#'
#' @importFrom randomForest getTree
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom igraph graph_from_data_frame delete_vertices V
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text geom_node_label
#' @importFrom ggplot2 theme_bw theme element_blank element_rect element_text
#'
#' @examples
#' # Exemplo de uso:
#' library(randomForest)
#' library(tibble)
#' library(dplyr)
#' library(igraph)
#' library(ggraph)
#'
#' # Treinar um modelo Random Forest
#' set.seed(123)
#' data(iris)
#' rf_model <- randomForest(Species ~ ., data = iris, ntree = 10)
#'
#' # Visualizar a primeira árvore do modelo
#' tree_plot <- tree_func(rf_model, tree_num = 1)
#' print(tree_plot)
#'
#' @export
tree_func <- function(final_model, tree_num) {
  
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # Tornar os pontos de divisão das folhas como NA, para que os 0s não sejam plotados
    dplyr::mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  graph <- igraph::graph_from_data_frame(graph_frame) %>% igraph::delete_vertices("0")
  
  igraph::V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  igraph::V(graph)$leaf_label <- as.character(tree$prediction)
  igraph::V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE, size = 2) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white", size = 2) +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", 
                    show.legend = FALSE,  size = 2) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  return(plot)
}


#' Ajustar Rasters Categóricos com Base em Dados de Treino
#'
#' Esta função ajusta rasters categóricos com base nos níveis presentes em um dataframe de treino.
#' Valores no raster que não estão presentes nos dados de treino são substituídos por valores padrão
#' (\code{99} para valores positivos e \code{-99} para valores negativos). O raster ajustado é salvo
#' como um novo arquivo na mesma pasta.
#'
#' @param df_treino Um dataframe contendo os dados de treino. Cada coluna representa uma variável categórica.
#' @param pasta_rasters Um caminho para a pasta onde os rasters categóricos estão armazenados. Os arquivos devem ter o mesmo nome das colunas do dataframe, com extensão \code{.tif}.
#'
#' @return Nenhum valor é retornado. Os rasters ajustados são salvos como novos arquivos na mesma pasta, com o sufixo \code{"_ajustado"} adicionado ao nome do arquivo.
#'
#' @details A função itera sobre as colunas do dataframe de treino, ajustando os rasters categóricos correspondentes.
#' Para cada raster, os valores que não estão presentes nos níveis da variável categórica no dataframe de treino
#' são substituídos por \code{99} (valores positivos) ou \code{-99} (valores negativos). O raster ajustado é então
#' convertido em um raster categórico e salvo como um novo arquivo.
#'
#' @importFrom terra rast values writeRaster as.factor
#'
#' @examples
#' # Exemplo de uso:
#' library(terra)
#'
#' # Criar um dataframe de treino de exemplo
#' df_treino <- data.frame(
#'   var1 = c(1, 2, 3),
#'   var2 = c(0, 1, 0)
#' )
#'
#' # Criar rasters de exemplo
#' pasta_rasters <- tempdir()
#' r1 <- rast(nrows = 10, ncols = 10, vals = sample(1:5, 100, replace = TRUE))
#' r2 <- rast(nrows = 10, ncols = 10, vals = sample(0:2, 100, replace = TRUE))
#' writeRaster(r1, file.path(pasta_rasters, "var1.tif"), overwrite = TRUE)
#' writeRaster(r2, file.path(pasta_rasters, "var2.tif"), overwrite = TRUE)
#'
#' # Ajustar os rasters
#' ajustar_rasters_categoricos(df_treino, pasta_rasters)
#'
#' @export
ajustar_rasters_categoricos <- function(df_treino, pasta_rasters) {
  # Iterar sobre as colunas do dataframe (variáveis categóricas)
  for (coluna in colnames(df_treino)) {
    # Obter os níveis únicos da variável no dataframe de treino
    niveis_treino <- unique(df_treino[[coluna]])
    
    # Construir o caminho do raster correspondente
    raster_path <- file.path(pasta_rasters, paste0(coluna, ".tif"))
    
    # Verificar se o raster existe
    if (!file.exists(raster_path)) {
      message(paste("Raster", raster_path, "não encontrado. Pulando..."))
      next
    }
    
    # Ler o raster
    raster <- rast(raster_path)
    
    # Criar uma cópia do raster para ajuste
    raster_ajustado <- raster
    
    # Obter os valores únicos do raster, ignorando NaN
    valores_raster <- unique(values(raster))
    valores_raster <- valores_raster[!is.nan(valores_raster)]  # Remover NaN
    
    # Substituir valores não presentes no treino por 99 ou -99
    for (valor in valores_raster) {
      if (!(valor %in% niveis_treino)) {
        if (valor < 0) {
          # Caso especial: valores negativos não presentes no treino vão para -99
          raster_ajustado[raster == valor] <- -99
        } else {
          # Valores positivos não presentes no treino vão para 99
          raster_ajustado[raster == valor] <- 99
        }
      }
    }
    
    # Transformar o raster ajustado em categórico
    raster_ajustado <- as.factor(raster_ajustado)
    
    # Salvar o raster ajustado
    ajustado_path <- file.path(pasta_rasters, paste0(coluna, "_ajustado.tif"))
    writeRaster(raster_ajustado, ajustado_path, overwrite = TRUE)
    
    message(paste("Raster ajustado salvo em:", ajustado_path))
  }
}



#' Criar um Stack de Rasters Ajustados e Originais
#'
#' Esta função cria um stack de rasters combinando os rasters ajustados (com o sufixo \code{"_ajustado"})
#' e os rasters originais que não possuem uma versão ajustada. O stack resultante contém os rasters ajustados
#' quando disponíveis e os originais quando não há uma versão ajustada.
#'
#' @param pasta_rasters Um caminho para a pasta onde os rasters estão armazenados. Os arquivos devem ter extensão \code{.tif}.
#'
#' @return Um objeto \code{SpatRaster} contendo o stack dos rasters ajustados e originais.
#'
#' @details A função identifica os rasters ajustados (com o sufixo \code{"_ajustado"}) e os combina com os rasters
#' originais que não possuem uma versão ajustada. O stack resultante é criado utilizando a função \code{rast()} do pacote \code{terra}.
#'
#' @importFrom terra rast
#'
#' @examples
#' # Exemplo de uso:
#' library(terra)
#'
#' # Criar rasters de exemplo
#' pasta_rasters <- tempdir()
#' r1 <- rast(nrows = 10, ncols = 10, vals = sample(1:5, 100, replace = TRUE))
#' r2 <- rast(nrows = 10, ncols = 10, vals = sample(0:2, 100, replace = TRUE))
#' r3 <- rast(nrows = 10, ncols = 10, vals = sample(1:3, 100, replace = TRUE))
#' writeRaster(r1, file.path(pasta_rasters, "var1.tif"), overwrite = TRUE)
#' writeRaster(r2, file.path(pasta_rasters, "var2.tif"), overwrite = TRUE)
#' writeRaster(r3, file.path(pasta_rasters, "var3_ajustado.tif"), overwrite = TRUE)
#'
#' # Criar o stack
#' stack <- criar_stack_ajustado(pasta_rasters)
#' print(stack)
#'
#' @export
criar_stack_ajustado <- function(pasta_rasters) {
  # Listar todos os arquivos raster na pasta
  arquivos <- list.files(pasta_rasters, pattern = "\\.tif$", full.names = TRUE)
  
  # Identificar os rasters ajustados (com sufixo "_ajustado")
  ajustados <- arquivos[grepl("_ajustado\\.tif$", arquivos)]
  
  # Identificar os rasters originais (sem o sufixo "_ajustado")
  originais <- arquivos[!grepl("_ajustado\\.tif$", arquivos)]
  
  # Obter os nomes base (sem extensão e sem "_ajustado") dos ajustados
  nomes_ajustados <- gsub("_ajustado\\.tif$", "", basename(ajustados))
  
  # Filtrar os rasters originais que não possuem uma versão ajustada
  originais_sem_ajustados <- originais[!gsub("\\.tif$", "", basename(originais)) %in% nomes_ajustados]
  
  # Combinar os rasters ajustados com os originais que não têm versão ajustada
  rasters_para_incluir <- c(ajustados, originais_sem_ajustados)
  
  # Criar o stack com os rasters selecionados
  stack <- rast(rasters_para_incluir)
  
  # Retornar o stack
  return(stack)
}




#' Fazer Predições em Paralelo com Tiles
#'
#' Esta função realiza predições em um raster grande utilizando tiles (divisões do raster) para processamento paralelo.
#' Os tiles são processados em paralelo, e os resultados são mesclados em um único raster final.
#'
#' @param resultados Uma lista de modelos treinados (ex.: objetos do \code{caret}).
#' @param st Um objeto \code{SpatRaster} representando o raster de entrada para predição.
#' @param pasta_resultados Um caminho para a pasta onde os resultados serão salvos.
#' @param n_cores Número de núcleos a serem utilizados para processamento paralelo. O padrão é \code{4}.
#' @param n_tiles_x Número de divisões (tiles) no eixo X. O padrão é \code{4}.
#' @param n_tiles_y Número de divisões (tiles) no eixo Y. O padrão é \code{4}.
#' @param debug Um valor lógico indicando se mensagens de depuração devem ser exibidas. O padrão é \code{TRUE}.
#'
#' @return Nenhum valor é retornado. Os resultados são salvos na pasta especificada.
#'
#' @details A função divide o raster de entrada em tiles, processa cada tile em paralelo utilizando os modelos fornecidos,
#' e mescla os resultados em um único raster final. Os tiles são salvos temporariamente e removidos após o processamento.
#'
#' @importFrom terra rast crop ext writeRaster merge
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map
#'
#' @examples
#' \dontrun{  
#' # Exemplo de uso:
#' library(terra)
#' library(caret)
#' library(future)
#' library(furrr)
#'
#' # Criar um raster de exemplo
#' st <- rast(nrows = 100, ncols = 100, vals = runif(10000))
#' names(st) <- c("var1", "var2", "var3")
#'
#' # Criar um modelo de exemplo
#' set.seed(123)
#' df_treino <- data.frame(var1 = runif(100), var2 = runif(100), 
#'   var3 = runif(100), classe = sample(0:1, 100, replace = TRUE))
#' 
#' modelo <- train(classe ~ ., data = df_treino, method = "rf")
#'
#' # Lista de modelos
#' resultados <- list(modelo_rf = modelo)
#'
#' # Pasta de resultados
#' pasta_resultados <- tempdir()
#'
#' # Executar a função
#' fazer_predicoes_paralelo_tiles(resultados, st, pasta_resultados, n_cores = 2, 
#' n_tiles_x = 2, n_tiles_y = 2)
#' }
#' @export
fazer_predicoes_paralelo_tiles <- function(resultados, st, pasta_resultados, 
                                           n_cores = 4, n_tiles_x = 4, n_tiles_y = 4,
                                           debug = TRUE) {
  debug_log <- function(msg) {
    if(debug) message(Sys.time(), " - DEBUG: ", msg)
  }
  
  debug_log("Verificando e criando pastas...")
  pasta_predicao <- file.path(pasta_resultados, "predicao")
  pasta_tiles <- file.path(pasta_predicao, "tiles")
  pasta_temp <- file.path(pasta_predicao, "temp")
  dir.create(pasta_predicao, recursive = TRUE, showWarnings = FALSE)
  dir.create(pasta_tiles, recursive = TRUE, showWarnings = FALSE)
  dir.create(pasta_temp, recursive = TRUE, showWarnings = FALSE)
  
  # Função para processar um único tile
  processar_tile_arquivo <- function(caminho_tile, caminho_modelo, pasta_saida, 
                                     nome_modelo) {
    tryCatch({
      # Carregar dados
      st_tile <- rast(caminho_tile)
      modelo <- readRDS(caminho_modelo)
      
      # Obter variáveis do modelo
      variaveis_modelo <- colnames(modelo$trainingData)
      variaveis_modelo <- setdiff(variaveis_modelo, ".outcome")
      
      # Selecionar variáveis
      st_tile_selecionado <- st_tile[[variaveis_modelo]]
      
      # Fazer predição
      predicao_tile <- predict(st_tile_selecionado, modelo, na.rm = TRUE)
      
      # Salvar resultado
      nome_saida <- basename(caminho_tile)
      caminho_saida <- file.path(pasta_saida, nome_saida)
      writeRaster(predicao_tile, caminho_saida, overwrite = TRUE)
      
      return(list(status = "success", file = caminho_saida))
    }, error = function(e) {
      return(list(status = "error", error = conditionMessage(e)))
    })
  }
  
  # Para cada modelo
  nome_modelo = names(resultados)[1]
  for(nome_modelo in names(resultados)) {
    debug_log(sprintf("Processando modelo: %s", nome_modelo))
    
    # Salvar modelo
    caminho_modelo <- file.path(pasta_temp, paste0(nome_modelo, "_modelo.rds"))
    saveRDS(resultados[[nome_modelo]], caminho_modelo)
    
    # Criar e salvar tiles
    debug_log("Criando e salvando tiles...")
    ext_total <- ext(st)
    largura <- (ext_total[2] - ext_total[1]) / n_tiles_x
    altura <- (ext_total[4] - ext_total[3]) / n_tiles_y
    
    caminhos_tiles <- character()
    
    for(i in 1:n_tiles_x) {
      for(j in 1:n_tiles_y) {
        xmin <- ext_total[1] + (i-1) * largura
        xmax <- xmin + largura
        ymin <- ext_total[3] + (j-1) * altura
        ymax <- ymin + altura
        
        # Recortar e salvar tile
        ext_tile <- ext(xmin, xmax, ymin, ymax)
        st_tile <- crop(st, ext_tile)
        
        nome_tile <- sprintf("%s_tile_%d_%d.tif", nome_modelo, i, j)
        caminho_tile <- file.path(pasta_temp, nome_tile)
        writeRaster(st_tile, caminho_tile, overwrite = TRUE)
        
        caminhos_tiles <- c(caminhos_tiles, caminho_tile)
      }
    }
    
    # Configurar paralelismo
    debug_log("Configurando processamento paralelo...")
    future::plan(future::multisession, workers = n_cores)
    
    # Criar lista de argumentos para processamento
    args_list <- lapply(caminhos_tiles, function(caminho_tile) {
      list(
        caminho_tile = caminho_tile,
        caminho_modelo = caminho_modelo,
        pasta_saida = pasta_tiles,
        nome_modelo = nome_modelo
      )
    })
    
    # Processar tiles em paralelo
    debug_log("Processando tiles...")
    resultados_tiles <- future_map(args_list, function(args) {
      do.call(processar_tile_arquivo, args)
    }, .progress = TRUE)
    
    # Restaurar configuração do future
    future::plan(future::sequential)
    
    # Verificar resultados
    falhas <- Filter(function(x) x$status == "error", resultados_tiles)
    if(length(falhas) > 0) {
      for(falha in falhas) {
        warning(sprintf("Falha no processamento: %s", falha$error))
      }
    }
    
    # Mesclar tiles bem-sucedidos
    
    debug_log("Mesclando tiles...")
    tiles_sucesso <- Filter(function(x) x$status == "success", resultados_tiles)
    tiles_paths <- sapply(tiles_sucesso, function(x) x$file)
    
    if(length(tiles_paths) > 0) {
      # Mesclar em blocos
      raster_final <- NULL
      tamanho_bloco <- min(5, length(tiles_paths))
      
      for(i in seq(1, length(tiles_paths), by = tamanho_bloco)) {
        debug_log(sprintf("Mesclando bloco %d/%d", 
                          ceiling(i/tamanho_bloco), 
                          ceiling(length(tiles_paths)/tamanho_bloco)))
        
        fim_bloco <- min(i + tamanho_bloco - 1, length(tiles_paths))
        tiles_bloco <- tiles_paths[i:fim_bloco]
        
        rasters_bloco <- lapply(tiles_bloco, rast)
        raster_bloco <- do.call(merge, rasters_bloco)
        
        if(is.null(raster_final)) {
          raster_final <- raster_bloco
        } else {
          raster_final <- merge(raster_final, raster_bloco)
        }
        
        rm(rasters_bloco, raster_bloco)
        gc()
      }
      
      # Salvar resultado final
      debug_log("Salvando resultado final...")
      nome_raster_final <- paste0(nome_modelo, "_final.tif")
      caminho_raster_final <- file.path(pasta_predicao, nome_raster_final)
      writeRaster(raster_final, caminho_raster_final, overwrite = TRUE)
    }
    
    # Limpar arquivos temporários
    debug_log("Limpando arquivos temporários...")
    unlink(c(caminho_modelo, caminhos_tiles, tiles_paths))
  }
  
  # Limpar pasta temporária
  unlink(pasta_temp, recursive = TRUE)
  
  debug_log("Processamento concluído!")
}


#' Realizar Predições com Modelos Treinados
#'
#' Esta função realiza predições utilizando modelos treinados em um raster stack de entrada.
#' Os resultados das predições são salvos como arquivos raster (\code{.tif}) em uma pasta de saída.
#'
#' @param resultados Uma lista de modelos treinados (ex.: objetos do \code{caret}).
#' @param st Um objeto \code{SpatRaster} representando o raster stack de entrada para predição.
#' @param pasta_resultados Um caminho para a pasta onde os resultados das predições serão salvos.
#'
#' @return Nenhum valor é retornado. Os rasters de predição são salvos na pasta especificada.
#'
#' @details A função itera sobre os modelos fornecidos, seleciona as variáveis necessárias no raster stack de entrada,
#' realiza a predição para toda a área de trabalho e salva os resultados como arquivos raster (\code{.tif}).
#' A pasta de saída é criada automaticamente, caso não exista.
#'
#' @importFrom terra writeRaster predict
#'
#' @examples
#' # Exemplo de uso:
#' \dontrun{
#' library(terra)
#' library(caret)
#'
#' # Criar um raster stack de exemplo
#' st <- rast(nrows = 100, ncols = 100, nlyrs = 3, vals = runif(30000))
#' names(st) <- c("var1", "var2", "var3")
#'
#' # Criar um modelo de exemplo
#' set.seed(123)
#' df_treino <- data.frame(var1 = runif(100), var2 = runif(100), 
#' var3 = runif(100), classe = sample(0:1, 100, replace = TRUE))
#' modelo <- train(classe ~ ., data = df_treino, method = "rf")
#'
#' # Lista de modelos
#' resultados <- list(modelo_rf = modelo)
#'
#' # Pasta de resultados
#' pasta_resultados <- tempdir()
#'
#' # Executar a função
#' fazer_predicoes(resultados, st, pasta_resultados)
#'}
#'
#' @export
fazer_predicoes <- function(resultados, st, pasta_resultados) {
  # Criar a pasta "predicao" dentro da pasta "resultados"
  pasta_predicao <- file.path(pasta_resultados, "predicao")
  if (!dir.exists(pasta_predicao)) {
    dir.create(pasta_predicao, recursive = TRUE)
  }
  
  # Iterar sobre os modelos e suas repetições
  for (nome_modelo in names(resultados)) {
    # Obter o modelo da lista
    modelo <- resultados[[nome_modelo]]
    
    # Obter as variáveis utilizadas pelo modelo
    variaveis_modelo <- colnames(modelo$trainingData)
    variaveis_modelo <- setdiff(variaveis_modelo, ".outcome")
    
    # Selecionar apenas as variáveis necessárias no stack
    st_selecionado <- st[[variaveis_modelo]]
    
    # Fazer a predição para toda a área de trabalho
    raster_predicao <- predict(st_selecionado, modelo, na.rm = TRUE)
    plot(raster_predicao, main = nome_modelo)
    
    # Salvar o raster na pasta "predicao"
    nome_raster <- paste0(nome_modelo, ".tif")
    caminho_raster <- file.path(pasta_predicao, nome_raster)
    writeRaster(raster_predicao, caminho_raster, overwrite = TRUE)
    
    # Mensagem de progresso
    message(paste("Predição salva para o modelo:", nome_modelo, "em", caminho_raster))
  }
}

#' Gerar Estatísticas de Classe Modal a Partir de Rasters de Predição
#'
#' Esta função calcula estatísticas relacionadas à classe modal em um conjunto de rasters de predição
#' gerados por um classificador específico. As estatísticas incluem a classe modal, o número de classes
#' distintas por pixel e a frequência da classe modal.
#'
#' @param classificador Um nome de classificador (ex.: "rf", "svm") para identificar os rasters de predição.
#' @param pasta_predicao Um caminho para a pasta onde os rasters de predição estão armazenados.
#' @param pasta_saida Um caminho para a pasta onde os resultados das estatísticas serão salvos.
#'
#' @return Nenhum valor é retornado. Três rasters são gerados e salvos na pasta de saída:
#' \itemize{
#'   \item \code{<classificador>_classe_modal.tif}: Raster com a classe modal por pixel.
#'   \item \code{<classificador>_num_classes.tif}: Raster com o número de classes distintas por pixel.
#'   \item \code{<classificador>_frequencia_modal.tif}: Raster com a frequência da classe modal por pixel.
#' }
#'
#' @details A função utiliza os rasters de predição gerados por um classificador específico para calcular:
#' \itemize{
#'   \item A classe modal (classe mais frequente) por pixel.
#'   \item O número de classes distintas por pixel.
#'   \item A frequência da classe modal (proporção de ocorrências da classe modal em relação ao total de valores não nulos).
#' }
#' Os resultados são salvos como arquivos raster (\code{.tif}) na pasta de saída especificada.
#'
#' @importFrom terra rast modal app writeRaster
#' @importFrom stats na.omit
#'
#' @examples
#' # Exemplo de uso:
#' library(terra)
#'
#' # Criar rasters de exemplo
#' pasta_predicao <- tempdir()
#' r1 <- rast(nrows = 10, ncols = 10, vals = sample(1:3, 100, replace = TRUE))
#' r2 <- rast(nrows = 10, ncols = 10, vals = sample(1:3, 100, replace = TRUE))
#' r3 <- rast(nrows = 10, ncols = 10, vals = sample(1:3, 100, replace = TRUE))
#' writeRaster(r1, file.path(pasta_predicao, "rf_1.tif"), overwrite = TRUE)
#' writeRaster(r2, file.path(pasta_predicao, "rf_2.tif"), overwrite = TRUE)
#' writeRaster(r3, file.path(pasta_predicao, "rf_3.tif"), overwrite = TRUE)
#'
#' # Pasta de saída
#' pasta_saida <- tempdir()
#'
#' # Gerar estatísticas
#' gerar_estatistica_modal("rf", pasta_predicao, pasta_saida)
#'
#' @export
gerar_estatistica_modal <- function(classificador, pasta_predicao, pasta_saida) {
  # Listar os rasters do classificador especificado
  rasters <- list.files(
    path = pasta_predicao,
    pattern = paste0("^", classificador, "_.*\\.tif$"),  # Exemplo: "rf_1.tif", "rf_2.tif"
    full.names = TRUE
  )
  
  # Verificar se há rasters disponíveis
  if (length(rasters) == 0) {
    stop(paste("Nenhum raster encontrado para o classificador:", classificador))
  }
  
  # Criar um stack com os rasters
  stack_rasters <- rast(rasters)
  
  # 1. Determinar a classe modal por pixel
  classe_modal <- modal(stack_rasters, ties = "first")  # Classe mais frequente
  # Salvar o raster da classe modal
  writeRaster(classe_modal, file.path(pasta_saida, paste0(classificador, "_classe_modal.tif")), overwrite = TRUE)
  
  # 2. Contar o número de classes distintas por pixel
  num_classes <- app(stack_rasters, function(x) length(unique(na.omit(x))))
  writeRaster(num_classes, file.path(pasta_saida, paste0(classificador, "_num_classes.tif")), overwrite = TRUE)
  
  # 3. Determinar a frequência da classe modal por pixel
  frequencia_modal <- app(stack_rasters, function(x) {
    # Classe modal
    modal_class <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
    # Frequência da classe modal
    freq_modal <- sum(x == modal_class, na.rm = TRUE) / length(na.omit(x))
    return(freq_modal)
  })
  writeRaster(frequencia_modal, file.path(pasta_saida, paste0(classificador, "_frequencia_modal.tif")), overwrite = TRUE)
  
  # Mensagens de progresso
  message("Rasters gerados para o classificador: ", classificador)
  message("1. Classe modal: ", file.path(pasta_saida, paste0(classificador, "_classe_modal.tif")))
  message("2. Número de classes: ", file.path(pasta_saida, paste0(classificador, "_num_classes.tif")))
  message("3. Frequência da classe modal: ", file.path(pasta_saida, paste0(classificador, "_frequencia_modal.tif")))
}



#' Estratégia de Remoção de NAs em um Data Frame
#'
#' Esta função avalia diferentes estratégias de remoção de linhas e colunas com valores ausentes (\code{NA})
#' em um \code{data.frame}. Para cada combinação de remoção de linhas e colunas, calcula o número de linhas,
#' colunas e valores ausentes restantes.
#'
#' @param df Um \code{data.frame} contendo os dados a serem analisados.
#' @param max_rows Número máximo de linhas a serem removidas. O padrão é \code{10}.
#' @param max_cols Número máximo de colunas a serem removidas. O padrão é \code{10}.
#'
#' @return Um \code{data.frame} com as seguintes colunas:
#' \itemize{
#'   \item \code{RemovedRows}: Número de linhas removidas.
#'   \item \code{RemovedCols}: Número de colunas removidas.
#'   \item \code{RemainingRows}: Número de linhas restantes.
#'   \item \code{RemainingCols}: Número de colunas restantes.
#'   \item \code{RemainingNAs}: Número de valores ausentes (\code{NA}) restantes.
#' }
#'
#' @details A função remove iterativamente as linhas e colunas com o maior número de valores ausentes (\code{NA}),
#' até os limites especificados por \code{max_rows} e \code{max_cols}. Para cada combinação de remoção de linhas
#' e colunas, calcula o número de linhas, colunas e valores ausentes restantes, retornando os resultados em um
#' \code{data.frame}.
#'
#' @examples
#' # Exemplo de uso:
#' df <- data.frame(
#'   A = c(1, NA, 3, NA),
#'   B = c(NA, 2, 3, 4),
#'   C = c(1, 2, NA, NA)
#' )
#'
#' # Avaliar estratégias de remoção de NAs
#' resultados <- estrategia_remocao_na(df, max_rows = 2, max_cols = 2)
#' print(resultados)
#'
#' @export
estrategia_remocao_na <- function(df, max_rows = 10, max_cols = 10) {
  results <- data.frame(
    RemovedRows = integer(),
    RemovedCols = integer(),
    RemainingRows = integer(),
    RemainingCols = integer(),
    RemainingNAs = integer()
  )
  
  total_rows <- nrow(df)
  total_cols <- ncol(df)
  
  # Iterar sobre o número de linhas a serem removidas
  for (rows_to_remove in 0:max_rows) {
    # Remover as linhas com mais valores NA primeiro
    df_temp <- df[order(rowSums(is.na(df)), decreasing = TRUE), ]
    df_temp <- df_temp[(rows_to_remove + 1):total_rows, , drop = FALSE]
    
    # Iterar sobre o número de colunas a serem removidas
    for (cols_to_remove in 0:max_cols) {
      # Remover as colunas com mais valores NA primeiro
      df_temp2 <- df_temp[, order(colSums(is.na(df_temp)), decreasing = TRUE), drop = FALSE]
      df_temp2 <- df_temp2[, (cols_to_remove + 1):total_cols, drop = FALSE]
      
      # Calcular o número de linhas, colunas e NAs restantes
      remaining_rows <- nrow(df_temp2)
      remaining_cols <- ncol(df_temp2)
      remaining_nas <- sum(is.na(df_temp2))
      
      # Armazenar os resultados
      results <- rbind(
        results,
        data.frame(
          RemovedRows = rows_to_remove,
          RemovedCols = cols_to_remove,
          RemainingRows = remaining_rows,
          RemainingCols = remaining_cols,
          RemainingNAs = remaining_nas
        )
      )
    }
  }
  
  return(results)
}


#' Remover Linhas e Colunas com Base em Estratégia de NAs
#'
#' Esta função remove um número especificado de linhas e colunas de um \code{data.frame},
#' priorizando aquelas com o maior número de valores ausentes (\code{NA}).
#'
#' @param df Um \code{data.frame} contendo os dados a serem processados.
#' @param rows_to_remove Número de linhas a serem removidas. As linhas com mais valores \code{NA} são removidas primeiro.
#' @param cols_to_remove Número de colunas a serem removidas. As colunas com mais valores \code{NA} são removidas primeiro.
#'
#' @return Um \code{data.frame} com as linhas e colunas especificadas removidas.
#' Se ainda houver valores \code{NA} restantes, um aviso será emitido.
#'
#' @details A função ordena as linhas e colunas com base no número de valores ausentes (\code{NA}),
#' removendo iterativamente as que possuem mais valores ausentes. Após a remoção, verifica se ainda
#' existem valores \code{NA} no \code{data.frame} resultante e emite um aviso, se necessário.
#'
#' @examples
#' # Exemplo de uso:
#' df <- data.frame(
#'   A = c(1, NA, 3, NA),
#'   B = c(NA, 2, 3, 4),
#'   C = c(1, 2, NA, NA)
#' )
#'
#' # Remover 1 linha e 1 coluna com mais NAs
#' df_reduzido <- remove_na_estrategia(df, rows_to_remove = 1, cols_to_remove = 1)
#' print(df_reduzido)
#'
#' @export
remove_na_estrategia <- function(df, rows_to_remove, cols_to_remove) {
  # Ordenar as linhas pelo número de valores NA e remover o número especificado de linhas
  df_ordered_rows <- df[order(rowSums(is.na(df)), decreasing = TRUE), ]
  df_reduced_rows <- df_ordered_rows[(rows_to_remove + 1):nrow(df_ordered_rows), , drop = FALSE]
  
  # Ordenar as colunas pelo número de valores NA e remover o número especificado de colunas
  df_ordered_cols <- df_reduced_rows[, order(colSums(is.na(df_reduced_rows)), decreasing = TRUE), drop = FALSE]
  df_reduced_cols <- df_ordered_cols[, (cols_to_remove + 1):ncol(df_ordered_cols), drop = FALSE]
  
  # Verificar se ainda existem valores NA
  remaining_nas <- sum(is.na(df_reduced_cols))
  if (remaining_nas > 0) {
    warning(sprintf("Ainda existem %d valores NA no data.frame resultante.", remaining_nas))
  } else {
    cat("Todos os valores NA foram removidos com sucesso.\n")
  }
  
  return(df_reduced_cols)
}
