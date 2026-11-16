# Bounded rerun of the unusually slow pixel lasso; convergence is audited by report.R.
code = readLines("run.R")
end = which(startsWith(code, "# Independent configurations")) - 1L
eval(parse(text = code[seq_len(end)]))
fit_config(configs[[1]], alphas = 1, overwrite = TRUE,
           solver_control = list(thresh = 1e-5, maxit = 3000))
