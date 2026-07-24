# Revisão técnica do script original

## Correções necessárias

- `collapse::fnth()` não recebe um vetor de probabilidades. O módulo usa
  `fquantile()` para calcular todos os quantis.
- `Date` e `POSIXt` usam armazenamento numérico no R. As classes de data agora
  são avaliadas antes das regras numéricas do Oracle.
- `setDT()` poderia modificar o tibble recebido por referência. A conversão
  atual usa um contêiner `data.table` local e não altera a entrada.
- A configuração fixa de `future::plan()` e o número fixo de 46 workers foram
  removidos. O módulo usa o contexto de threads do `sbyops`.
- `NA`, `NaN`, infinito positivo e infinito negativo agora são medidos
  separadamente.
- Uma coluna totalmente nula não é mais classificada como constante.
- A escala decimal de um `double` não é inferida por sua representação textual.
  Valores contínuos sugerem `BINARY_DOUBLE`; `NUMBER(P,S)` exige escala de
  negócio conhecida.
- A sugestão de índice bitmap foi limitada a um sinal baseado em dados. Ela
  exige validação de workload, baixa taxa de DML e contexto de data warehouse.
- Limiares de outlier, transformações e distribuições não devem ser estimados
  fora dos folds de treino.

## Informações acrescentadas

O catálogo univariado inclui:

- classes, tipo base, modo de armazenamento, atributos, rótulo e unidade;
- tamanho em memória e custo aproximado por registro;
- nulidade com posterior Beta de Jeffreys e intervalo de credibilidade;
- cardinalidade, duplicação, moda, concentração, entropia, HHI e Gini;
- soma, média, mediana, variância, desvio, erro padrão, MAD, assimetria,
  curtose e quantis;
- intervalo bayesiano para a média e intervalo preditivo sob modelo Normal
  iid com prior de Jeffreys;
- monotonicidade, zeros, positivos, negativos e precisão inteira observada;
- Tukey, Tukey extremo, z-score, z-score robusto e boxplot ajustado por
  medcouple;
- comprimento em caracteres e bytes, Unicode, espaços, controle, quebras de
  linha e padrões de inteiro, decimal, lógico, data, UUID, e-mail, URL e JSON;
- métricas temporais, granularidade, intervalo e fuso horário;
- sinais de identificador, quase zero variância, alta nulidade, alta
  cardinalidade e ordem temporal;
- tipo Oracle, cardinalidade, chave candidata, bitmap, B-tree, histograma,
  particionamento, compressão e validade sintática do nome.

O perfil bivariado inclui:

- Pearson, Spearman e Kendall;
- V de Cramer corrigido;
- eta quadrado;
- informação mútua normalizada;
- associação entre padrões de nulidade;
- dependências funcionais direcionais;
- duplicação e redundância potencial para modelos;
- candidatos a extended statistics do Oracle.

As análises adicionais incluem:

- 50 distribuições contínuas GAMLSS, filtradas por suporte;
- log-verossimilhança, AIC, AICc, BIC, pesos de Akaike, KS, Cramer-von Mises
  e Anderson-Darling;
- probabilidade posterior aproximada por BIC, condicionada ao conjunto de
  modelos e aos priors informados;
- outliers nas caudas da distribuição ajustada;
- outliers multivariados por Minimum Covariance Determinant e distância de
  Mahalanobis robusta;
- dimensões, memória, células nulas, linhas completas, duplicadas e padrões
  conjuntos de nulidade.

## Limites estatísticos

A função de distribuição retorna o melhor modelo entre os candidatos que
convergiram. Isso não prova que a distribuição geradora verdadeira pertence à
lista. Os pesos BIC são uma aproximação de Laplace, não um Bayes factor exato.
Uma segunda etapa exata deve definir priors de parâmetros e ajustar somente os
melhores modelos com MCMC e bridge sampling ou comparação preditiva fora da
amostra.

Os intervalos bayesianos univariados declaram suas hipóteses no próprio
resultado. Eles não substituem modelos hierárquicos, dependência temporal,
pesos amostrais ou mecanismos informativos de ausência.

## Validação Oracle

O arquivo `inst/sql/sby_oracle_performance_metadata.sql` coleta:

- estatísticas de tabela, coluna, partição e histogramas;
- tamanho físico dos segmentos;
- índices, expressões, seletividade, BLEVEL e fator de clusterização;
- constraints e extended statistics existentes;
- modificações desde a última coleta de estatísticas;
- uso conjunto de colunas observado por `DBMS_STATS`;
- planos em cache e plano real do cursor;
- conflitos com palavras reservadas, quando houver privilégio de dicionário.

Nenhuma recomendação física deve gerar DDL automaticamente.

## Referências principais

- collapse:
  <https://cran.r-project.org/web/packages/collapse/collapse.pdf>
- distribuições GAMLSS:
  <https://cran.r-project.org/web/packages/gamlss.dist/gamlss.dist.pdf>
- estatísticas do otimizador Oracle:
  <https://docs.oracle.com/en/database/oracle/oracle-database/19/tgsql/optimizer-statistics-concepts.html>
- histogramas Oracle:
  <https://docs.oracle.com/en/database/oracle/oracle-database/19/tgsql/histograms.html>
- extended statistics Oracle:
  <https://docs.oracle.com/en/database/oracle/oracle-database/19/tgsql/managing-extended-statistics.html>
- branching dinâmico no targets:
  <https://books.ropensci.org/targets/dynamic.html>
