# 📊 Análise da COVID-19 no Brasil

Este repositório contém meu **primeiro projeto de Análise de Dados**, desenvolvido no âmbito da disciplina **Análise de Dados para Economia**, do curso de **Economia da PUC-Rio**, no ano de **2023**.

O projeto tem como objetivo aplicar técnicas de **integração, tratamento e análise exploratória de dados** ao contexto da pandemia da COVID-19 no Brasil, explorando informações em níveis **municipal, estadual e regional**, bem como sua representação espacial.

O trabalho está disponível em **duas versões**:
- Um **script em R** (`AnaliseCovid.R`)
- Um **notebook Jupyter** (`AnáliseCovid.ipynb`)

Ambas as versões implementam a mesma lógica analítica, permitindo comparar abordagens e facilitar a reprodutibilidade.

---

## 🎯 Objetivos do Projeto

- Integrar múltiplas bases de dados relacionadas à COVID-19
- Tratar dados ausentes e inconsistências
- Analisar a evolução de casos e óbitos ao longo do tempo
- Comparar indicadores nos níveis municipal, estadual e regional
- Explorar visualizações gráficas e geográficas para extração de insights
- Aplicar conceitos de análise de dados ao contexto econômico e social da pandemia

---

⚠️ **Observação:** devido ao tamanho do arquivo principal de dados, ele não está incluído diretamente neste repositório (ver seção abaixo).

---

## 📂 Dados Utilizados

O projeto utiliza dados de casos e óbitos de COVID-19 no Brasil, além de bases auxiliares para informações regionais e geográficas.

### 🔗 Arquivo principal de casos

O arquivo `casos.csv`, necessário para a execução do projeto, está disponível no link abaixo:

👉 **Download do arquivo `casos.csv`:**  
https://drive.google.com/file/d/155Sb6EDalfO_eNfrbvOmI0rU8DdTpN8N/view?usp=sharing

### 📌 Instruções

Após o download:
1. Salve o arquivo `casos.csv` dentro da pasta DadosCOVID
   
---

## 🧠 Metodologia

O projeto segue as seguintes etapas:

- Integração de dados a partir de múltiplas fontes (CSV e base SQLite)
- Padronização de identificadores regionais (UF, municípios e regiões)
- Tratamento e imputação de dados ausentes
- Cálculo de:
  - Casos e óbitos acumulados
  - Casos e óbitos diários
  - Taxas de mortalidade e letalidade
- Comparação entre dados reportados nos níveis municipal e estadual
- Análise de tendências temporais
- Cálculo de médias móveis para avaliação de dinâmica da mortalidade

---

## 📈 Visualizações

O projeto utiliza diferentes tipos de visualizações para comunicação dos resultados:

- Mapas coropléticos (Brasil e Estado de São Paulo)
- Gráficos de dispersão e linhas conectadas
- Gráficos de setores (pizza)
- Tabelas analíticas de casos, óbitos e taxas

As visualizações são construídas com **ggplot2** e **sf**, permitindo análise espacial e temporal integrada.

---

## 🛠️ Tecnologias e Pacotes

### Linguagens
- **R**
- **Python** (no notebook Jupyter)

### Principais pacotes utilizados
- tidyverse
- readr
- DBI / RSQLite
- lubridate
- sf
- ggplot2
- scales
- zoo
- stargazer

---

## ▶️ Como Executar o Projeto

### Opção 1 — Script em R
1. Baixe o arquivo `casos.csv` conforme instruções acima
2. Ajuste os caminhos dos arquivos, se necessário
3. Execute o script `AnaliseCovid.R`

### Opção 2 — Notebook Jupyter
1. Baixe o arquivo `casos.csv`
2. Abra o arquivo `AnáliseCovid.ipynb`
3. Execute as células sequencialmente

---

## 📌 Observações Finais

Este projeto possui caráter **estritamente educacional** e foi desenvolvido com o objetivo de consolidar conceitos fundamentais de **Análise de Dados aplicados à Economia**, no contexto da pandemia da COVID-19.

Os resultados apresentados dependem da qualidade dos dados disponíveis e das hipóteses adotadas ao longo da análise.

---





