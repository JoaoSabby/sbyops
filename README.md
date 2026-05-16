# sbyops

`sbyops` é um pacote R para identificar colunas com baixa variabilidade por
frequência modal. A interface pública valida e codifica colunas no R, enquanto o
núcleo nativo em Fortran calcula, por coluna, o valor mais frequente, sua
contagem absoluta e sua frequência relativa.

## Instalação

```r
# A partir da raiz do repositório local
install.packages(".", repos = NULL, type = "source")
```

O pacote usa OpenMP no núcleo Fortran. Em sistemas sem OpenMP disponível, a
compilação deve ser validada com a toolchain R local.

## Exemplo rápido

```r
library(sbyops)

df <- data.frame(
  constante = c(1, 1, 1, 1, 1),
  quase_constante = c("x", "x", "x", "x", "y"),
  variavel = c(1, 2, 3, 4, 5),
  ausente_modal = c(NA, NA, NA, NA, 10)
)

sby_fe_filter_nzv(df, threshold = 0.8)
```

A função retorna uma linha para cada coluna cujo conteúdo modal aparece em
proporção maior ou igual ao `threshold`.

## Critério estatístico

Para cada coluna, o pacote calcula:

```text
ratio = frequência do valor mais comum / número de linhas
```

A coluna é retornada quando:

```text
ratio >= threshold
```

Consequências práticas:

- `threshold = 1` retorna apenas colunas completamente constantes.
- `threshold = 0.95` retorna colunas em que algum conteúdo representa pelo menos
  95% das linhas.
- `threshold = 0` retorna toda coluna não vazia.

## Tipos suportados

As colunas devem ser dos tipos:

- `factor`
- `character`
- `integer`
- `logical`
- `numeric`

Outros tipos, como `Date`, `POSIXct`, listas e colunas matriciais, são recusados
para evitar semântica ambígua de comparação.

## Tratamento de valores ausentes

`NA` é tratado como conteúdo. Portanto, uma coluna com 95% de `NA` possui `NA`
como valor modal e será retornada quando `threshold <= 0.95`.

## Valores numéricos

Colunas `numeric` são comparadas por igualdade exata após codificação por
`factor()`. Valores numericamente próximos, mas não idênticos, não são agrupados.
Se for necessário agrupar por tolerância, arredonde ou transforme os dados antes
de chamar `sby_fe_filter_nzv()`.

## Paralelismo

Use `n_threads` para solicitar um número de threads OpenMP:

```r
sby_fe_filter_nzv(df, threshold = 0.95, n_threads = 4)
```

Quando `n_threads = NULL`, o runtime OpenMP usa a configuração do ambiente, como
`OMP_NUM_THREADS`. Quando `n_threads` é informado, o núcleo nativo chama
`omp_set_num_threads()`; dependendo do runtime OpenMP, essa configuração pode
persistir para chamadas seguintes no mesmo processo R.

Em servidores compartilhados, evite oversubscription combinando muitas threads do
pacote com BLAS/MKL também paralelos. Uma configuração comum é limitar BLAS/MKL a
uma thread durante benchmarks do filtro.

## Objeto retornado

O retorno é um objeto `sby_fe_filter_nzv_result`, baseado em `tibble` quando o
pacote `tibble` estiver instalado, ou em `data.frame` base caso contrário. A
tabela possui as colunas:

- `column`: nome da coluna filtrada;
- `ratio`: frequência relativa do valor modal;
- `value`: valor modal como representação textual;
- `count`: frequência absoluta do valor modal.

Os atributos `n_rows`, `n_cols`, `threshold` e `n_threads` armazenam metadados da
análise.

## Limitações

- O filtro mede dominância modal, não variância estatística clássica.
- Não usa os critérios adicionais de alguns filtros near-zero variance, como
  razão entre a primeira e a segunda frequência mais comum.
- Não há suporte especializado a matrizes esparsas.
- O desempenho depende da cardinalidade das colunas, do número de colunas e da
  configuração OpenMP.
