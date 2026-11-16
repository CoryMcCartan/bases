# Run from explore/mnist. All output paths are relative to this directory.
# Uses installed dependencies; does not install packages or rebuild bases.
Sys.setenv(VECLIB_MAXIMUM_THREADS = 1, OMP_NUM_THREADS = 1)
source("helpers.R")
basis = load_basis_code()
for (path in c("features", "models")) dir.create(path, showWarnings = FALSE)
data = readRDS("data/subset.rds")
images = image_list(rbind(data$train_pixels, data$test_pixels))
n_train = length(data$train_labels)
train_rows = seq_len(n_train)
test_rows = n_train + seq_along(data$test_labels)
y_train = factor(data$train_labels, levels = 0:9)
y_test = as.character(data$test_labels)

configs = list(list(id = "pixels", family = "pixels"))
for (size in c(3, 5, 9)) for (p in c(128, 512, 1024)) {
    configs[[length(configs) + 1L]] = list(
        id = sprintf("conv_k%d_p%d", size, p), family = "conv", size = size, p = p
    )
}
configs[[length(configs) + 1L]] = list(
    id = "conv_local_k5_p128", family = "conv", size = 5, p = 128, local = TRUE
)
for (pooling in c("global", "local")) for (scales in 1:3) {
    configs[[length(configs) + 1L]] = list(
        id = sprintf("scatter_%s_s%d", pooling, scales), family = "scatter",
        scales = scales, pooling = pooling,
        pad = scatter_padding(basis, scales, pooling)
    )
}
saveRDS(configs, "configurations.rds")

# Time real batches so filter setup is largely amortized. Backend selection uses
# training images and elapsed time only; it does not consult test labels.
pilot_path = "backend_pilot.csv"
pilot = if (file.exists(pilot_path)) read.csv(pilot_path) else NULL
for (cfg in configs) {
    if (cfg$family != "scatter" || (!is.null(pilot) && cfg$id %in% pilot$id)) next
    log_progress("backend pilot", cfg$id, "padded side", 28 + 2 * cfg$pad)
    batch = images[seq_len(32)]
    mark = bench::mark(
        direct = scatter_features(basis, batch, cfg$scales, cfg$pooling, FALSE, cfg$pad),
        fft = scatter_features(basis, batch, cfg$scales, cfg$pooling, TRUE, cfg$pad),
        iterations = 2, check = FALSE, filter_gc = FALSE
    )
    pilot = rbind(pilot, data.frame(
        id = cfg$id, method = as.character(mark$expression),
        seconds_per_image = as.numeric(mark$median) / length(batch),
        allocated_MiB = as.numeric(mark$mem_alloc) / 1024^2
    ))
    write.csv(pilot, pilot_path, row.names = FALSE)
    print(tail(pilot, 2), row.names = FALSE)
}

results = if (file.exists("results.csv")) read.csv("results.csv") else NULL
if (!is.null(results)) results$group = NULL
if (!is.null(results)) {
    for (i in seq_len(nrow(results))) {
        path = file.path("models", paste0(results$key[i], ".csv"))
        if (!file.exists(path)) write.csv(results[i, ], path, row.names = FALSE)
    }
}
make_features <- function(cfg) {
    feature_path = file.path("features", paste0(cfg$id, ".rds"))
    if (file.exists(feature_path)) {
        return(feature_path)
    } else {
        log_progress("features", cfg$id)
        metadata = cfg
        start = proc.time()[["elapsed"]]
        if (cfg$family == "pixels") {
            raw = rbind(data$train_pixels, data$test_pixels)
            X = matrix(as.double(raw) / 255, nrow(raw), ncol(raw))
        } else if (cfg$family == "conv") {
            # Banks are nested across filter counts at each kernel width.
            set.seed(9000 + cfg$size)
            kernels = matrix(rnorm(cfg$size^2 * 1024), cfg$size^2, 1024)
            metadata$kernels = kernels[, seq_len(cfg$p), drop = FALSE]
            activation = if (isTRUE(cfg$local)) local_max_pool(cfg$size) else max
            z = basis$b_conv(images, kernels = metadata$kernels,
                            activation = activation, stdize = "none")
            X = matrix(as.numeric(z), nrow(z), ncol(z))
        } else {
            rows = pilot[pilot$id == cfg$id, ]
            metadata$fft = rows$method[which.min(rows$seconds_per_image)] == "fft"
            # Batching limits storage of padded images; each batch still reuses
            # filter arrays across 250 images of identical dimensions.
            chunks = split(seq_along(images), ceiling(seq_along(images) / 250))
            parts = lapply(seq_along(chunks), function(i) {
                if (i %% 8 == 1) log_progress(cfg$id, "feature batch", i, "/", length(chunks))
                scatter_features(basis, images[chunks[[i]]], cfg$scales,
                                 cfg$pooling, metadata$fft, cfg$pad)
            })
            X = do.call(rbind, parts)
        }
        feature_seconds = proc.time()[["elapsed"]] - start
        stopifnot(nrow(X) == length(images), all(is.finite(X)))
        saveRDS(list(x = X, feature_seconds = feature_seconds, metadata = metadata),
                feature_path, compress = FALSE)
    }
    rm(X); gc()
    feature_path
}

fit_config <- function(cfg, alphas = c(0, 1), overwrite = FALSE, lambda_path = NULL,
                       solver_control = NULL) {
    completed = file.path("models", paste0(cfg$id, "_alpha", alphas, ".csv"))
    if (!overwrite && all(file.exists(completed))) return(TRUE)
    saved = readRDS(make_features(cfg))
    X = saved$x
    feature_seconds = saved$feature_seconds
    metadata = saved$metadata
    rm(saved)
    # Drop columns that are constant in the training sample only.
    keep = vapply(seq_len(ncol(X)), function(j) {
        diff(range(X[train_rows, j])) > 0
    }, logical(1))
    x_train = X[train_rows, keep, drop = FALSE]
    x_test = X[test_rows, keep, drop = FALSE]
    log_progress(cfg$id, "columns", ncol(X), "retained", ncol(x_train),
                 "feature seconds", round(feature_seconds, 2))
    for (alpha in alphas) {
        key = paste0(cfg$id, "_alpha", alpha)
        result_path = file.path("models", paste0(key, ".csv"))
        if (file.exists(result_path) && !overwrite) next
        log_progress("fit", key)
        # Explicit paths avoid the very different automatic lambda.max rule
        # used by glmnet for alpha=0; all models use identical training folds.
        lambda = if (!is.null(lambda_path)) lambda_path else if (alpha == 0) {
            10^seq(1, -3, length.out = 30)
        } else {
            exp(seq(log(0.3), log(0.0003), length.out = 30))
        }
        warnings = character()
        control = solver_control
        if (is.null(control)) {
            control = if (cfg$id == "pixels" && alpha == 1) {
                list(thresh = 1e-5, maxit = 3000)
            } else list(thresh = 1e-6, maxit = 100000)
        }
        start = proc.time()[["elapsed"]]
        fit = withCallingHandlers(glmnet::cv.glmnet(
            x_train, y_train, family = "multinomial", alpha = alpha,
            lambda = lambda, foldid = data$foldid, type.measure = "deviance",
            standardize = TRUE, intercept = TRUE,
            control = control, parallel = FALSE
        ), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        fit_seconds = proc.time()[["elapsed"]] - start
        probs = predict(fit, newx = x_test, s = "lambda.min", type = "response")[, , 1]
        predicted = colnames(probs)[max.col(probs, ties.method = "first")]
        chosen = which.min(fit$cvm)
        row = data.frame(
            key = key, id = cfg$id, family = cfg$family,
            kernel_size = if (cfg$family == "conv") cfg$size else NA,
            filters = if (cfg$family == "conv") cfg$p else NA,
            scales = if (cfg$family == "scatter") cfg$scales else NA,
            pooling = if (cfg$family == "scatter") cfg$pooling else if (cfg$family == "conv") {
                if (isTRUE(cfg$local)) "local" else "global"
            } else NA_character_,
            fft = if (cfg$family == "scatter") metadata$fft else NA,
            padded_side = if (cfg$family == "scatter") 28 + 2 * cfg$pad else 28,
            features = ncol(X), retained = ncol(x_train), alpha = alpha,
            lambda = fit$lambda.min, lambda_endpoint = chosen %in% c(1, length(fit$lambda)),
            cv_log_loss = fit$cvm[chosen] / 2,
            accuracy = mean(predicted == y_test),
            log_loss = -mean(log(pmax(probs[cbind(seq_along(y_test),
                                                     match(y_test,colnames(probs)))],1e-15))),
            feature_seconds = feature_seconds, fit_seconds = fit_seconds,
            warnings = paste(unique(warnings), collapse = " | ")
        )
        saveRDS(list(
            configuration = metadata, keep = keep, coefficients = coef(fit, s = "lambda.min"),
            cv = data.frame(lambda = fit$lambda, deviance = fit$cvm, se = fit$cvsd),
            lambda = fit$lambda.min, alpha = alpha, predicted = predicted,
            probabilities = probs, truth = y_test, summary = row,
            solver_control = control
        ), file.path("models", paste0(key, ".rds")))
        write.csv(row, result_path, row.names = FALSE)
        log_progress(key, "test accuracy", round(row$accuracy, 4),
                     "CV loss", round(row$cv_log_loss, 4),
                     "fit seconds", round(fit_seconds, 1))
        if (length(warnings)) log_progress("warnings:", row$warnings)
        if (row$lambda_endpoint) log_progress("NOTE: selected lambda is a path endpoint")
        rm(fit, probs); gc()
    }
    rm(X, x_train, x_test); gc()
    TRUE
}

# Independent configurations run in separate processes; folds within each
# cv.glmnet call remain serial. Per-model files make the run resumable.
workers = as.integer(Sys.getenv("MNIST_WORKERS", "4"))
out = parallel::mclapply(configs, fit_config, mc.cores = workers,
                         mc.preschedule = FALSE, mc.set.seed = FALSE)
if (any(vapply(out, inherits, logical(1), "try-error"))) {
    stop("At least one configuration failed; inspect the log and rerun")
}
paths = list.files("models", pattern = "[.]csv$", full.names = TRUE)
results = do.call(rbind, lapply(paths, function(path) {
    row = read.csv(path)
    row$group = NULL
    row
}))
write.csv(results, "results.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
log_progress("All configurations completed")
