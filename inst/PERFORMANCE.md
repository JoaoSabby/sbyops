# Desempenho e OpenMP em servidor NUMA

## Configuração recomendada inicial

Servidor informado:

- 2 sockets NUMA
- 48 núcleos físicos
- 96 CPUs lógicas via Hyper-Threading
- Intel Xeon Platinum 8260
- AVX2 e AVX-512
- 1,4 TB DDR4-2933

A rotina `sby_nzv()` é predominantemente limitada por memória. Por isso,
usar 96 threads lógicas nem sempre é melhor do que usar 48 threads físicas.

Configuração inicial recomendada no shell antes de abrir o R:

```bash
export OMP_PROC_BIND=spread
export OMP_PLACES=cores
export OMP_DYNAMIC=false

export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
```

Uso recomendado no R:

```r
res <- sby_nzv(data, threshold = 0.95, n_threads = 48)
```

## Por que MKL_NUM_THREADS=1?

O núcleo Fortran de `sby_nzv()` não usa BLAS/MKL para a contagem modal.
Se outras partes do pipeline usarem MKL ao mesmo tempo, deixar MKL também
paralelizar pode causar oversubscription: OpenMP do pacote cria threads e MKL
cria mais threads por cima. Isso tende a piorar desempenho e estabilidade.

## Como testar

Testar pelo menos:

```r
for (nt in c(12, 24, 36, 48, 72, 96)) {
  gc()
  cat("threads =", nt, "\n")
  print(system.time(sby_nzv(data, threshold = 0.95, n_threads = nt)))
}
```

Em muitos casos, o melhor ponto ficará entre 24 e 48 threads. Em bases muito
largas, 48 pode ser o melhor. Em bases pequenas ou estreitas, valores menores
podem ganhar por reduzir overhead.
