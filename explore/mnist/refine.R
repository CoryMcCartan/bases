# Run after the main grid. Refit the preliminary pixel ridge on the final grid,
# then extend any lambda paths whose CV minimum is at an endpoint.
code = readLines("run.R")
end = which(startsWith(code, "# Independent configurations")) - 1L
eval(parse(text = code[seq_len(end)]))

pixel = readRDS("models/pixels_alpha0.rds")
if (nrow(pixel$cv) != 30L || nzchar(pixel$summary$warnings)) {
    fit_config(configs[[1]], alphas = 0, overwrite = TRUE)
}
for (attempt in 1L) {
    files = list.files("models", pattern = "[.]csv$", full.names = TRUE)
    rows = do.call(rbind, lapply(files, function(file) {
        row = read.csv(file)
        row$group = NULL
        row
    }))
    # Spend extra computation on the best ridge candidate at an endpoint, not
    # on arbitrarily weak lasso tails that cannot improve the family winner.
    winners = vapply(split(rows, rows$id), function(x) {
        x$key[which.min(x$cv_log_loss)]
    }, "")
    rows = rows[rows$lambda_endpoint & rows$alpha == 0 & rows$key %in% winners &
        !grepl("converg|numerical", rows$warnings, ignore.case = TRUE) &
        rows$key != "pixels_alpha1", , drop = FALSE]
    if (nrow(rows) == 0L) break
    jobs = split(rows, seq_len(nrow(rows)))
    workers = as.integer(Sys.getenv("MNIST_REFINE_WORKERS", "4"))
    parallel::mclapply(jobs, function(row) {
        old = readRDS(file.path("models", paste0(row$key, ".rds")))
        lower = min(old$cv$lambda)
        upper = max(old$cv$lambda)
        if (which.min(old$cv$deviance) == nrow(old$cv)) lower = lower / 100 else {
            upper = upper * 100
        }
        path = exp(seq(log(upper), log(lower), length.out = 40))
        cfg = configs[[match(row$id, vapply(configs, `[[`, "", "id"))]]
        backup = file.path("models", "refinement_backup")
        dir.create(backup, showWarnings = FALSE)
        files = file.path("models", paste0(row$key, c(".rds", ".csv")))
        file.copy(files, backup, overwrite = TRUE)
        fit_config(cfg, alphas = row$alpha, overwrite = TRUE, lambda_path = path,
                   solver_control = list(thresh = 1e-6, maxit = 10000))
        updated = read.csv(files[2])
        if (grepl("converg|numerical", updated$warnings, ignore.case = TRUE)) {
            rejected = file.path("models", "refinement_rejected")
            dir.create(rejected, showWarnings = FALSE)
            file.copy(files, rejected, overwrite = TRUE)
            file.copy(file.path(backup, basename(files)), files, overwrite = TRUE)
            log_progress("Kept converged original fit for", row$key)
        }
    }, mc.cores = min(workers, nrow(rows)), mc.preschedule = FALSE, mc.set.seed = FALSE)
}
source("report.R")
