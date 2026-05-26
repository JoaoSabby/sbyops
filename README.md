# sbyops

`sbyops` é um pacote de tools para operações tabulares de alto desempenho em R.

## API pública

- `sby_select_modal_frequency()`
- `sby_select_correlation()`
- `sby_config()`

A API não expõe backend, `finite_mode`, `block_rows` ou `column_names`: essas decisões são internas.

## Entradas suportadas

- `data.frame`
- `tibble`
- `matrix`

### Nomes de colunas

- Se existirem, são preservados.
- Em `matrix` sem `colnames`, nomes determinísticos são gerados internamente.
- Nomes vazios/duplicados são reparados internamente de forma determinística.

## Seleção por frequência modal

` sby_select_modal_frequency()` remove colunas com frequência modal `>= threshold` (limite inclusivo), usando backend nativo por tipo (logical, integer/factor, numeric e character).

- Suporte de avaliação: factor, character, integer, logical, numeric.
- Colunas não avaliáveis permanecem no resultado.

## Seleção por correlação de Pearson

` sby_select_correlation()` remove colunas numéricas com correlação absoluta `>= threshold` (limite inclusivo).

- Correlação absoluta (`abs`).
- Com `NA`, `NaN` ou `Inf`, usa caminho robusto pairwise.
- Colunas constantes recebem correlação efetiva zero no motor atual.
- Em pares altamente correlacionados, remove a coluna com maior correlação média absoluta com o conjunto ativo.
- A ordem das colunas pode influenciar desempates por varredura.

## Estratégia automática interna

A função escolhe internamente entre:

- caminho simples robusto em Fortran/OpenMP,
- caminho BLAS para dados finitos e memória suficiente,
- caminho streaming para reduzir cópias em matrizes altas.

Sem necessidade de usuário escolher backend.

## OpenMP

```bash
export OMP_NUM_THREADS=8
export OMP_DYNAMIC=FALSE
export OMP_PROC_BIND=spread
export OMP_PLACES=cores
```

OpenMP pode piorar desempenho em bases pequenas por overhead.

## MKL, TBB e SYCL

- MKL **pode** acelerar caminho BLAS apenas quando o R estiver linkado a MKL.
- MKL não é dependência direta do pacote.
- TBB e SYCL não são usados na implementação atual.

## Benchmark local

Script: `inst/benchmarks/benchmark_correlation.R`.


## Configuração de execução

Use `sby_config()` para ajustar os limites de troca de engine e threads:

- `sby_config_start_fortran` default `10000L`
- `sby_config_start_blas` default `100000L`
- `sby_config_max_threads` default `2L`

Exemplo:

```r
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
```
