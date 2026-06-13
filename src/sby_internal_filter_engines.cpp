// Este arquivo foi esvaziado.
// Os backends C++ gerados pelo CODEX (sby_internal_correlation_removed_columns_cpp
// e sby_internal_modal_frequency_removed_columns_cpp) foram removidos pois
// introduziram regressao severa de performance:
//
// - modal_frequency: unordered_map<std::string> com make_column_key gerava ~1B
//   alocacoes de heap para datasets de 4M linhas x 250 colunas. Substituido por
//   kit::countOccur, que opera em C nativo sobre ponteiros diretos sem boxing.
//
// - correlation: passagem de data.frame sem conversao para matriz densa, ausencia
//   de OpenMP em build_z_score_buffer, varredura serial completa em
//   has_complete_z_score_buffer e float32 com risco de instabilidade numerica.
//   Restaurado o pipeline original: data.matrix() + storage.mode double +
//   dispatch para Fortran/BLAS/streaming.
//
// Os simbolos correspondentes foram removidos de src/init.c.
