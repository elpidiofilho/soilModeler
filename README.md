# soilModeler

## **Pacote R: Ferramentas para Análise de Dados Espaciais e Modelagem**

Este pacote fornece um conjunto de funções para facilitar a análise de dados espaciais, modelagem estatística e manipulação de dados em R. Ele é projetado para pesquisadores e analistas que trabalham com dados geoespaciais, aprendizado de máquina e estatísticas descritivas. Abaixo estão os principais recursos e funcionalidades do pacote:

---

## **Principais Funcionalidades**

### **1. Manipulação de Dados Espaciais**
- **`extrai_limite_raster`**: Extrai o limite de um raster como um objeto `sf`.
- **`gerar_amostras`**: Gera amostras aleatórias dentro de uma área especificada e salva como shapefile.
- **`extrair_dados_rasters`**: Extrai valores de uma pilha de rasters com base em um shapefile de pontos.
- **`selecionar_rasters_por_vetor`**: Seleciona rasters com base em padrões de string e cria um stack.
- **`converter_data_frame_para_shapefile`**: Converte um data frame com coordenadas em um shapefile de pontos.

### **2. Modelagem Estatística e Machine Learning**
- **`select_pca_componentes`**: Seleciona componentes principais com base em autovalores.
- **`criar_boxplot_comparativo`**: Cria boxplots para comparar métodos de amostragem.
- **`plot_scree`**: Gera gráficos de scree para análise de componentes principais.
- **`processa_resultados_train`**: Processa resultados de modelos treinados, identificando variáveis mais importantes.
- **`fazer_predicoes`**: Realiza predições em rasters com modelos treinados.

### **3. Visualização de Dados**
- **`criar_graficos_metricas`**: Gera gráficos de métricas de desempenho (Accuracy e Kappa) para modelos.
- **`criar_grafico_importancia`**: Cria gráficos de importância de variáveis para modelos específicos.
- **`tree_func`**: Visualiza árvores de decisão de modelos Random Forest.

### **4. Manipulação de Dados**
- **`estrategia_remocao_na`**: Avalia estratégias de remoção de valores ausentes em data frames.
- **`remove_na_estrategia`**: Remove linhas e colunas com base em valores ausentes.
- **`detect_factors`**: Identifica variáveis que podem ser convertidas em fatores.
- **`agrupar_niveis_raros`**: Agrupa níveis raros de um fator em um novo nível.

### **5. Ferramentas para Variáveis Bioclimáticas**
- **`bioclim`**: Retorna os significados de variáveis bioclimáticas com base em seus nomes padrão.

---

## **Exemplo de Uso**

### **Extração de Limite de Raster**
```r
resultado <- extrai_limite_raster('./raster_covariaveis_full/nasadem.tif')
print(resultado)
```

### **Geração de Amostras Aleatórias**
```r
amostras <- gerar_amostras(shape_area = area_sf, nsample = 100, type = "random", output_path = "amostras.shp", seed = 42)
```

### **Análise de Componentes Principais**
```r
data <- mtcars[, c("mpg", "disp", "hp", "wt")]
resultado_pca <- select_pca_componentes(data, eigenvalue_threshold = 1)
print(resultado_pca)
```

### **Criação de Gráficos de Importância**
```r
grafico <- criar_grafico_importancia("rf", result_abrev)
print(grafico)
```

---

## **Instalação**

Para instalar o pacote diretamente do GitHub, use o seguinte comando:

```r
# Instalar o pacote remotes, se necessário
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Instalar o pacote a partir do GitHub
remotes::install_github("usuario/repositorio")
```

Substitua `"usuario/repositorio"` pelo nome do repositório GitHub onde o pacote está hospedado.

---

## **Contribuição**

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou enviar pull requests no repositório GitHub.

---

Se precisar de mais informações ou ajuda, consulte a documentação das funções no R ou entre em contato com o mantenedor do pacote. 😊



<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/elpidiofilho/soilModeler/graph/badge.svg)](https://app.codecov.io/gh/elpidiofilho/soilModeler)
<!-- badges: end -->

The goal of soilModeler is to ...

## Installation

You can install the development version of soilModeler from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("elpidiofilho/soilModeler")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(soilModeler)
## basic example code
```

