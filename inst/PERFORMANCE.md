# Desempenho técnico do sbyops

## Arquitetura adaptativa

A seleção por correlação usa estratégia interna automática, sem parâmetros públicos de backend:

- **fortran**: caminho robusto para dados com `NA`, `NaN` ou `Inf`.
- **blas**: caminho para dados finitos com memória temporária considerada segura.
- **streaming**: caminho para matrizes altas com restrição de memória, reduzindo cópias grandes.

A decisão é baseada em perfil interno (`n_rows`, `n_cols`, pares, finitude e estimativa de memória).

## Caso de produção 4.000.000 x 250

- Matriz principal double aproximada: `~7,45 GiB`.
- Matriz de correlação final: pequena (`250 x 250`).
- Risco principal: cópias temporárias.
- Recomendação: evitar objetos duplicados no ambiente e executar benchmark fora do CI.

## OpenMP

Os núcleos nativos em Fortran utilizam OpenMP internamente.

Exemplo:

```bash
export OMP_NUM_THREADS=8
export OMP_DYNAMIC=FALSE
export OMP_PROC_BIND=spread
export OMP_PLACES=cores
```

Em paralelismo externo (por processo), reduzir `OMP_NUM_THREADS` para evitar oversubscription.

## Benchmark

Script: `inst/benchmarks/benchmark_correlation.R`

Cenários leves/médios no script:

- 10.000 x 250
- 250.000 x 250
- 50.000 x 1.000

Cenários pesados (manual/local):

- 1.000.000 x 250
- 4.000.000 x 250

O benchmark não integra `R CMD check`.

## Ferramentas opcionais

VTune e Advisor podem ser usados em desenvolvimento para hotspot, threading e memória.

## Limites conhecidos

- O caminho BLAS e streaming atuais exigem dados finitos.
- A heurística de memória é conservadora e ajustável por opções internas.
