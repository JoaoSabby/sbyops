# sbyops

`sbyops` é um pacote para seleção de colunas em dados tabulares com backends nativos (C/Fortran), OpenMP e BLAS, com controle automático de paralelismo por execução.

## API pública

- `sby_select_modal_frequency()`
- `sby_select_correlation()`
- `sby_select_non_constant()`
- `sby_config()`
- `sby_table_optimize_scheme()`
- `sby_table_write()`

## Escrita de tabelas (Arrow/Parquet)

As funções `sby_table_optimize_scheme()` e `sby_table_write()` implementam:

- inferência automática de tipos Arrow por coluna,
- heurística de `dictionary encoding` para fatores e `character`,
- escolha automática de `chunk_size` (row group),
- compressão configurável por `options()` com fallback seguro.

## Pacotes externos considerados para otimização

O módulo tabular foi estruturado para coexistir com pacotes de alto desempenho:

- `data.table`, `dtplyr`, `tidytable`, `tidyfast`, `fastplyr` para pré-processamento tabular;
- `kit`, `cheapr`, `Rfast`, `coop` para operações vetorizadas auxiliares;
- `vctrs` para padronização/coerção de tipos antes de escrita;
- `fastmap` para cache de metadados de schema em pipelines repetitivos.

O núcleo de inferência crítico permanece em C++ e backend Arrow nativo.

## Entradas suportadas

- `data.frame`
- `tibble`
- `matrix`

## Configuração oficial de threads

O pacote usa `sby_config_max_threads` como parâmetro global oficial para limite de threads. Funções que aceitam `num_treads` podem sobrescrever esse valor apenas para a chamada corrente.

- `sby_config_max_threads`
- `num_treads` por chamada, quando disponível

Configuração:

```r
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
```

## Como funciona o paralelismo automático

Durante operações intensivas, o pacote executa um ciclo de contexto:

1. **Captura** estado atual da sessão
   - variáveis de ambiente de OpenMP/BLAS (como `OMP_NUM_THREADS`, `MKL_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, `BLIS_NUM_THREADS`),
   - opções R relevantes (`mc.cores`, `Ncpus`),
   - estado de threads via `RhpcBLASctl` quando disponível.
2. **Aplica** temporariamente `num_treads` quando informado pela função externa; caso contrário, usa `sby_config_max_threads`
   - OpenMP: define limites e desativa dinâmica (`OMP_DYNAMIC=FALSE`),
   - BLAS: define variáveis de backend e tenta aplicar setters via `RhpcBLASctl`,
   - sessão R: `options(mc.cores=..., Ncpus=...)`.
3. **Restaura** integralmente o estado original ao final
   - inclusive em erro,
   - preservando diferença entre variável ausente e variável definida como string vazia.

Esse processo evita efeitos colaterais permanentes na sessão do usuário.

## O que é alterado para OpenMP

Quando a estratégia ativa OpenMP, o pacote ajusta temporariamente:

- `OMP_NUM_THREADS`
- `OMP_THREAD_LIMIT`
- `OMP_DYNAMIC=FALSE`
- `OMP_MAX_ACTIVE_LEVELS=1`

Além disso, rotinas nativas podem usar `num_threads(...)` explicitamente no código C/OpenMP.

## O que é alterado para BLAS

Quando a estratégia ativa BLAS, o pacote ajusta temporariamente variáveis como:

- `MKL_NUM_THREADS`, `MKL_DYNAMIC`
- `OPENBLAS_NUM_THREADS`
- `GOTO_NUM_THREADS`
- `BLIS_NUM_THREADS`
- `VECLIB_MAXIMUM_THREADS`

Também tenta aplicar controle em tempo de execução com `RhpcBLASctl` quando o pacote está instalado e as funções estão disponíveis no namespace.

## Estratégia automática em `sby_select_correlation()`

A função escolhe internamente entre:

- `streaming` para cargas menores,
- `fortran` para cargas intermediárias,
- `blas` para cargas maiores.

A escolha usa os limiares configurados em `sby_config_start_fortran` e `sby_config_start_blas`.

## Comportamento de backend

- O backend BLAS efetivo depende de como o R foi compilado e linkado.
- oneMKL, OpenBLAS, BLIS ou Accelerate podem responder de forma diferente ao ajuste de threads.
- O pacote não assume que um backend específico está ativo sem detecção.

## Benchmark local

- `inst/benchmarks/benchmark_correlation.R`
- `tools/benchmarks/benchmark-modal-frequency.R`
