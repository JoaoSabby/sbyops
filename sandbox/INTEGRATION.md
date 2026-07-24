# Integração no sbyops

O módulo foi alinhado ao padrão atual do repositório `JoaoSabby/sbyops`:

- API pública com prefixo `sby_`;
- auxiliares internos com prefixo `sby_internal_`;
- variáveis em `camelCase`;
- documentação roxygen;
- ausência de operador de escopo duplo nos scripts R;
- retorno em tibble;
- controle de recursos por `num_treads` e pelo contexto de threads do pacote;
- nenhuma configuração global de `future`;
- entrada convertida para um contêiner `data.table` privado, sem `setDT` e sem alteração por referência.

## Arquivos

Copie os arquivos de `R/` para o diretório `R/` do pacote e os arquivos de
`tests/testthat/` para o diretório correspondente. O SQL de diagnóstico pode
permanecer em `inst/sql/`.

Aplique `DESCRIPTION.patch`. Em seguida, regenere `NAMESPACE` e os arquivos
`man/` com roxygen. `NAMESPACE-additions.txt` mostra as entradas esperadas e
serve como verificação.

## oneAPI e oneMKL

O perfil univariado reutiliza:

- `sby_internal_table_detect_integer_type()`;
- `sby_internal_table_detect_numeric_type()`;
- rotinas C de `collapse`;
- agregação eficiente de `data.table`.

Essas etapas são predominantemente limitadas por largura de banda de memória.
Forçar BLAS nelas não produz uma otimização válida. O módulo usa o contexto
OpenMP do pacote, mas desativa o contexto BLAS nessa parte.

As funções que usam `data.table` aplicam `num_treads` somente durante a chamada
e restauram a configuração anterior ao sair, inclusive em caso de erro.

O perfil bivariado usa amostragem determinística, discretização por quantis e
agregações esparsas para evitar tabelas de contingência densas em colunas de
alta cardinalidade. Ele retorna correlações, informação mútua, V de Cramer,
eta quadrado, dependências funcionais, redundância para modelagem e sinais para
extended statistics do Oracle.

O detector MCD ativa o contexto BLAS. Assim, decomposições e soluções matriciais
podem usar o backend oneMKL já ligado ao R e respeitam `num_treads`. O ajuste de
distribuições também entra no contexto controlado, sem deixar variáveis de
threads alteradas após a chamada.

## Inferência e modelagem

O catálogo informa o posterior e o intervalo de credibilidade da nulidade sob
um modelo Binomial com prior Beta de Jeffreys. Para vetores numéricos, informa
intervalos da média e de uma nova observação sob modelo Normal iid com prior de
Jeffreys. As hipóteses aparecem em colunas próprias.

`sby_identify_distribution()` compara 50 famílias compatíveis com o suporte,
devolve AIC, AICc, BIC, pesos de Akaike, testes de aderência e uma probabilidade
posterior aproximada por BIC. Esta probabilidade é condicionada aos modelos
que convergiram e não representa um Bayes factor exato.

## targets

Em ramificações dinâmicas, use uma thread por branch para evitar
oversubscription. A alocação paralela deve ser feita pelo `targets` e pelo
controlador `crew`, não dentro das funções de perfil.

```r
library(crew)
library(sbyops)
library(targets)

tar_option_set(
  packages = c(
    "sbyops",
    "gamlss",
    "gamlss.dist"
  )
)

list(
  tar_target(
    dadosTreino,
    preparar_dados_treino()
  ),
  tar_target(
    catalogoDados,
    sby_profile_data_catalog(
      .data = dadosTreino,
      num_treads = 1L
    )
  ),
  tar_target(
    relacoesDados,
    sby_profile_data_relations(
      .data = dadosTreino,
      num_treads = 1L
    )
  ),
  tar_target(
    outliersMultivariados,
    sby_detect_multivariate_outliers(
      .data = dadosTreino,
      num_treads = 1L
    )
  ),
  tar_target(
    colunaContinua,
    catalogoDados$NOME_COLUNA[
      catalogoDados$FLAG_CONTINUA %in% TRUE
    ],
    iteration = "vector"
  ),
  tar_target(
    distribuicaoColuna,
    sby_identify_distribution(
      x = dadosTreino[[colunaContinua]],
      num_treads = 1L
    ),
    pattern = map(colunaContinua)
  )
)
```

Limites de outlier, imputação, transformação e escolha de distribuição usados
como features devem ser aprendidos somente no conjunto de treino de cada
ressample. O catálogo puramente descritivo pode ser calculado sobre a fonte
bruta, desde que seus resultados não sejam usados para ajustar o modelo fora
dos folds.

## Oracle

As colunas Oracle do catálogo são heurísticas baseadas apenas nos dados. Não
constituem DDL automático. A decisão final de índice exige:

- frequência de leitura e DML;
- predicados de filtro, junção e agrupamento;
- estatísticas do otimizador;
- planos com linhas estimadas e observadas;
- particionamento real;
- fator de clusterização dos índices;
- licenciamento aplicável a AWR e ASH.

Use `inst/sql/sby_oracle_performance_metadata.sql` para coletar os metadados que
não existem em um tibble.
