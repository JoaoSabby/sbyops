# sbyops

`sbyops` é o pacote de **operações computacionais de base** do ecossistema
`sabby`. Ele concentra ferramentas reutilizáveis para dados tabulares,
seleção de variáveis, escrita analítica, perfilamento estatístico, diagnóstico
de relações e rotinas robustas de apoio à modelagem. A organização do pacote é
orientada por módulos de ferramentas para permitir crescimento incremental sem
misturar responsabilidades.

## Princípios de desenho

- **Ferramentas pequenas e composáveis**: cada função externa executa uma tarefa
  clara e pode ser combinada em pipelines de preparação, auditoria ou modelagem.
- **API estável por famílias**: funções públicas usam o prefixo `sby_` e são
  agrupadas por domínio operacional.
- **Backends eficientes**: rotinas críticas usam C, C++, Fortran, BLAS, OpenMP
  ou bibliotecas vetorizadas quando isso reduz tempo de execução.
- **Execução reprodutível**: amostragens usadas para limitar custo são
  determinísticas, não aleatórias.
- **Controle explícito de recursos**: funções com `num_treads` aplicam limite
  temporário de threads e restauram o contexto da sessão ao final.
- **Saídas auditáveis**: ferramentas de perfilamento retornam `tibble` com
  nomes de colunas descritivos para facilitar registro, inspeção e persistência.

## Famílias de ferramentas

### Configuração de execução

| Função | Finalidade |
| --- | --- |
| `sby_config()` | Define limiares globais de estratégia e limite padrão de threads. |

### Seleção e filtragem de colunas

| Função | Finalidade |
| --- | --- |
| `sby_select_non_constant()` | Remove colunas constantes entre as colunas selecionadas. |
| `sby_select_modal_frequency()` | Remove colunas dominadas por uma única moda acima de um limiar. |
| `sby_select_correlation()` | Remove colunas numéricas redundantes por correlação de Pearson. |

### Escrita tabular e Parquet

| Função | Finalidade |
| --- | --- |
| `sby_table_optimize_scheme()` | Infere um schema Arrow compacto para escrita Parquet. |
| `sby_table_write()` | Escreve `data.frame` ou `tibble` em Parquet com schema otimizado. |

### Perfilamento estatístico e qualidade de dados

| Função | Finalidade |
| --- | --- |
| `sby_profile_data_set()` | Resume dimensões, memória, nulidade e duplicidade do dataset. |
| `sby_profile_data_catalog()` | Cria catálogo por coluna com estatísticas, qualidade e sinais Oracle. |
| `sby_profile_missing_patterns()` | Lista padrões de nulidade mais frequentes em nível de linha. |
| `sby_profile_data_relations()` | Mede relações bivariadas, redundância e dependências funcionais. |

### Modelagem auxiliar e diagnóstico robusto

| Função | Finalidade |
| --- | --- |
| `sby_distribution_candidates()` | Lista famílias contínuas candidatas para ajuste GAMLSS. |
| `sby_identify_distribution()` | Ajusta e ranqueia distribuições candidatas para um vetor numérico. |
| `sby_detect_multivariate_outliers()` | Detecta outliers multivariados com distância robusta MCD. |

## Entradas suportadas

As funções de seleção e perfilamento aceitam objetos tabulares usuais, conforme
a ferramenta:

- `data.frame`;
- `tibble`;
- relações/tabelas DuckDB (materializadas automaticamente como `data.frame`);
- `matrix`, quando a função documenta suporte matricial.

Seleções de colunas usam semântica `tidyselect` quando a função recebe `...`.
Quando `...` é omitido, cada ferramenta aplica seu padrão documentado: todas as
colunas, ou somente colunas numéricas quando a estatística exige geometria
numérica.

## Exemplo rápido

```r
library(sbyops)

sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)

base_limpa <- dados |>
  sby_select_non_constant() |>
  sby_select_modal_frequency(threshold = 0.99)

catalogo <- sby_profile_data_catalog(base_limpa)
relacoes <- sby_profile_data_relations(base_limpa, max_rows = 100000L)
padroes <- sby_profile_missing_patterns(base_limpa)
```

## Configuração oficial de threads

O pacote usa a option `sby_config_max_threads` como limite global de threads.
Funções que aceitam `num_treads` podem sobrescrever esse valor apenas durante a
chamada corrente.

```r
sby_config(sby_config_max_threads = 4L)

catalogo <- sby_profile_data_catalog(
  dados,
  num_treads = 1L
)
```

Em pipelines com paralelismo externo, por exemplo `targets`, recomenda-se usar
`num_treads = 1L` dentro de cada worker, salvo quando a alocação de recursos do
pipeline reservar explicitamente múltiplas threads por tarefa.

## Como funciona o paralelismo automático

Durante operações intensivas, o pacote executa um ciclo de contexto:

1. captura variáveis de ambiente de OpenMP/BLAS, options R relevantes e estado
   disponível em `RhpcBLASctl`;
2. aplica temporariamente o limite solicitado por `num_treads` ou por
   `sby_config_max_threads`;
3. executa o backend selecionado;
4. restaura o estado original mesmo quando ocorre erro.

Esse ciclo evita efeitos colaterais permanentes na sessão do usuário.

## Estratégia de backends

`sby_select_correlation()` escolhe automaticamente entre estratégias de acordo
com o tamanho do problema e os limiares configurados:

- `streaming` para cargas menores;
- `fortran` para cargas intermediárias;
- `blas` para cargas maiores, conforme disponibilidade do ambiente.

O backend efetivo de BLAS depende de como o R foi compilado e linkado. oneMKL,
OpenBLAS, BLIS, Accelerate ou BLAS de referência podem responder de maneiras
diferentes ao ajuste de threads.

## Escrita de tabelas Arrow/Parquet

`sby_table_optimize_scheme()` e `sby_table_write()` implementam:

- inferência automática de tipos Arrow por coluna;
- compactação de inteiros e inteiros empíricos quando segura;
- `dictionary encoding` para fatores e caracteres elegíveis;
- escolha automática de `chunk_size`;
- compressão configurável por `options()` com fallback seguro.

O pacote `arrow` é requerido apenas no momento em que as ferramentas de escrita
são chamadas.

## Perfilamento e diagnóstico

As ferramentas de perfilamento foram desenhadas para auditoria técnica e apoio à
modelagem. Elas não substituem validação de negócio, análise causal, verificação
temporal ou avaliação de workload SQL. Em especial, campos de recomendação
Oracle são **sinais de triagem derivados dos dados**, não comandos físicos a
serem aplicados automaticamente.

## Dependências opcionais

- `arrow`: necessário para escrita Parquet e schema Arrow.
- `RhpcBLASctl`: usado quando disponível para leitura e ajuste de threads
  BLAS/OpenMP em tempo de execução.
- `gamlss` e `gamlss.dist`: necessários para `sby_identify_distribution()`.

## Documentação de ajuda

Todas as funções públicas possuem páginas em `man/*.Rd`. Em uma sessão R com o
pacote instalado, os comandos abaixo devem abrir os respectivos tópicos:

```r
?sby_profile_data_catalog
help("sby_profile_data_relations", package = "sbyops")
help(package = "sbyops")
```

## Benchmarks locais

Arquivos de benchmark podem ser mantidos em diretórios próprios, conforme a
família de ferramenta avaliada:

- `inst/benchmarks/benchmark_correlation.R`;
- `tools/benchmarks/benchmark-modal-frequency.R`.
