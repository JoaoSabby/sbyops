# sbyops

`sbyops` implementa operações de seleção de colunas para pré-processamento tabular em R, com núcleos nativos em Fortran e OpenMP.

## API pública

As funções públicas atuais são:

- `sby_select_modal_frequency()`
- `sby_select_correlation()`

Exemplo básico:

```r
preprocessed <- data |>
  sby_select_modal_frequency(threshold = 0.95) |>
  sby_select_correlation(threshold = 0.95)
```

## Seleção por frequência modal

` sby_select_modal_frequency()` remove colunas selecionadas cuja frequência modal é maior ou igual a `threshold`.

- `threshold` é inclusivo.
- Sem seletores, todas as colunas suportadas são avaliadas.
- Com seletores, apenas colunas selecionadas são avaliadas e as demais são preservadas.

Tipos suportados para avaliação modal: fator, caractere, inteiro, lógico e numérico.

## Seleção por correlação de Pearson

` sby_select_correlation()` remove colunas numéricas altamente correlacionadas usando correlação de Pearson em valor absoluto.

- `threshold` é inclusivo.
- Sem seletores, todas as colunas numéricas são avaliadas.
- Colunas não numéricas são preservadas.
- A correlação usa tratamento pairwise quando há `NA`, `NaN` ou `Inf`.

## OpenMP

Os núcleos nativos usam OpenMP com controle por variáveis de ambiente. O pacote não fixa número de threads no código.

### Controle de threads

Exemplo em Linux:

```bash
export OMP_NUM_THREADS=8
export OMP_DYNAMIC=FALSE
export OMP_PROC_BIND=spread
export OMP_PLACES=cores
```

Exemplo em R:

```r
Sys.setenv(
  OMP_NUM_THREADS = "8",
  OMP_DYNAMIC = "FALSE",
  OMP_PROC_BIND = "spread",
  OMP_PLACES = "cores"
)
```

### Oversubscription

Quando houver paralelismo externo (`future`, `parallel`, `foreach`, `mirai`, `targets`), recomenda-se reduzir `OMP_NUM_THREADS` para evitar oversubscription.

### Quando OpenMP pode piorar desempenho

Em bases pequenas, o custo de criação/sincronização de threads pode superar o ganho de paralelismo.

## Núcleos nativos

- Frequência modal: processamento por coluna com códigos inteiros.
- Correlação de Pearson: matriz densa por pares de colunas, com tratamento de não finitos.

## Ferramentas opcionais de diagnóstico

VTune e Advisor são opcionais para desenvolvimento, não dependências do pacote.

```bash
vtune -collect hotspots -- Rscript benchmark_sbyops.R
vtune -collect threading -- Rscript benchmark_sbyops.R
vtune -collect memory-access -- Rscript benchmark_sbyops.R

advisor --collect=survey --project-dir=advisor_sbyops -- Rscript benchmark_sbyops.R
advisor --collect=roofline --project-dir=advisor_sbyops_roofline -- Rscript benchmark_sbyops.R
```
