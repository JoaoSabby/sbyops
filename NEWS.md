# sbyops 0.0.1

- Versão inicial do filtro de baixa variabilidade por frequência modal com
  interface R e núcleo Fortran/OpenMP.

- Otimização: `sby_select_modal_frequency()` agora usa backend nativo em C por tipo, removendo dependência de codificação `factor()` no caminho crítico e adicionando parada antecipada por limite modal.
