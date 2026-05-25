# Plano pré-implementação: compilação Intel ultra-agressiva (Fortran + C/C++)

Branch alvo: `feature/intel-ifx-icx-ultra-aggressive-optflags`

## Objetivo
Permitir que o pacote compile com as flags mais agressivas práticas para os compiladores Intel atuais (ifx/ifort para Fortran e icx/icpx para C/C++), com fallback seguro para toolchains não Intel.

## Alterações necessárias antes da implementação

1. **Reestruturar flags por compilador em `src/Makevars` e `src/Makevars.win`**
   - Detectar toolchain Intel via macros de `make`/`R` (ex.: nome do compilador em `$(FC)`, `$(CC)`, `$(CXX)` com `ifx|ifort|icx|icpx`).
   - Manter flags atuais para GCC/Clang como fallback.
   - Introduzir variáveis separadas para baseline e perfil agressivo (evita hardcode no `PKG_*FLAGS`).

2. **Definir perfil “ultra-agressive Intel” para Fortran**
   - Candidatas usuais para ifx/ifort (validar disponibilidade por versão):
     - `-O3` ou `-Ofast`
     - `-ipo` (interprocedural optimization)
     - `-xHost` (ou alvo explícito quando apropriado)
     - `-qopenmp` (ou integração correta com `SHLIB_OPENMP_FCFLAGS`)
     - `-fp-model fast=2` (avaliar impacto numérico)
     - `-funroll-loops` / `-unroll` (quando suportado)
   - Confirmar compatibilidade com `-ffree-line-length-none` (gfortran) e equivalente Intel se necessário.

3. **Definir perfil “ultra-agressive Intel” para C/C++**
   - Candidatas usuais para icx/icpx:
     - `-O3` ou `-Ofast`
     - `-ipo`
     - `-xHost`
     - `-qopenmp`
     - `-fno-math-errno` e afins (quando suportado)

4. **Fixar política de precisão numérica (sem fast-math)**
   - Manter a diretriz atual do projeto: não usar `-ffast-math` no perfil Intel ultra-agressivo.
   - Priorizar desempenho com `-O3`/`-Ofast` (quando aplicável), `-ipo`, vetorização e OpenMP, preservando comportamento numérico mais previsível em dados estatísticos.
   - Documentar explicitamente que qualquer experimento com fast-math fica fora do escopo oficial do pacote.

5. **Estratégia de portabilidade binária**
   - `-xHost` gera binário otimizado para CPU local; pode quebrar portabilidade.
   - Definir política:
     - builds locais/HPC: permitir `-xHost`.
     - distribuição ampla/CRAN: desabilitar por padrão.

6. **Adicionar mecanismo explícito de ativação do perfil agressivo**
   - Ex.: `SBYOPS_INTEL_ULTRA=1` no `Makevars` para habilitar perfil agressivo.
   - Sem variável, manter perfil conservador atual.

7. **Compatibilidade OpenMP cruzada**
   - Garantir coerência entre `SHLIB_OPENMP_*` do R e flags Intel (`-qopenmp`), evitando duplicidade/conflito.
   - Ajustar `PKG_LIBS` se necessário para runtime OpenMP Intel em ambientes específicos.

8. **Validação de versões de compilador Intel**
   - Documentar versão mínima testada (oneAPI 2024/2025/2026, por exemplo).
   - Opcional: checks em tempo de build para emitir aviso quando flags não suportadas forem detectadas.

9. **Atualização de documentação**
   - `README.md`: seção “Intel ultra-aggressive build profile” com exemplos de export de variáveis.
   - `inst/PERFORMANCE.md`: detalhar trade-offs (desempenho x reprodutibilidade x portabilidade).
   - `DESCRIPTION`/`SystemRequirements`: complementar menção a toolchain Intel como opcional.

10. **Matriz de testes e validação antes de merge**
    - Build matrix mínima:
      - GCC (fallback)
      - Clang (fallback)
      - Intel ifx/icx (perfil conservador)
      - Intel ifx/icx (perfil ultra-agressivo)
    - Rodar:
      - `R CMD check`
      - testes `testthat`
      - benchmark local de correlação
    - Critérios de aceite:
      - sem regressão funcional;
      - ganho de performance mensurável no perfil agressivo;
      - comportamento documentado para eventuais diferenças numéricas.

11. **Possível arquivo auxiliar de configuração**
    - Se `Makevars` ficar complexo, criar `src/Makevars.intel` e incluir condicionalmente para manter manutenção simples.

## Riscos técnicos já mapeados
- Divergência numérica potencial ao usar perfis de ponto flutuante agressivos (ex.: `-fp-model fast=2`).
- Falhas em ambientes sem runtime Intel/OpenMP compatível.
- Fragilidade de flags entre versões ifort (legado) e ifx (LLVM-based).

## Proposta de execução (após sua confirmação)
1. Implementar detecção condicional de compilador e modo opt-in agressivo.
2. Adicionar conjunto inicial de flags Intel “safe aggressive” sem `-ffast-math`.
3. Atualizar documentação e comandos de uso.
4. Rodar checks e reportar benchmark comparativo.
