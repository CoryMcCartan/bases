# Aggregate checkpoints, select configurations using training CV, and report
# held-out performance. Can also be run while run.R is still producing results.
paths = list.files("models", pattern = "[.]csv$", full.names = TRUE)
stopifnot(length(paths) > 0)
results = do.call(rbind, lapply(paths, function(path) {
    row = read.csv(path)
    row$group = NULL
    row
}))
results$pooling[results$family == "conv" & is.na(results$pooling)] = "global"
results$group = ifelse(results$family == "pixels", "Pixels",
    ifelse(results$family == "conv", paste("Random convolution", results$pooling),
           paste("Scattering", results$pooling)))
results = results[order(results$group, results$id, results$alpha), ]
write.csv(results, "results.csv", row.names = FALSE)

eligible = results[!grepl("converg|numerical", results$warnings, ignore.case = TRUE), ]
eligible = eligible[!(eligible$key == "pixels_alpha1" & eligible$lambda_endpoint), ]
best_config = do.call(rbind, lapply(split(eligible, eligible$id), function(x) {
    x[which.min(x$cv_log_loss), , drop = FALSE]
}))
selected = do.call(rbind, lapply(split(best_config, best_config$group), function(x) {
    x[which.min(x$cv_log_loss), , drop = FALSE]
}))
rownames(selected) = NULL
write.csv(best_config, "configuration_summary.csv", row.names = FALSE)
write.csv(selected, "selected_models.csv", row.names = FALSE)

selected$errors = round(2000 * (1 - selected$accuracy))
print(selected[, c("group", "id", "alpha", "retained", "accuracy", "errors",
                    "log_loss", "cv_log_loss", "feature_seconds", "fit_seconds")],
      row.names = FALSE)

if (nrow(selected) > 1) {
    models = setNames(lapply(selected$key, function(key) {
        readRDS(file.path("models", paste0(key, ".rds")))
    }), selected$group)
    set.seed(701)
    comparisons = list()
    pairs = combn(names(models), 2, simplify = FALSE)
    for (pair in pairs) {
        a = models[[pair[1]]]
        b = models[[pair[2]]]
        stopifnot(identical(a$truth, b$truth))
        delta = as.numeric(a$predicted == a$truth) - as.numeric(b$predicted == b$truth)
        bootstrap = replicate(2000, mean(sample(delta, length(delta), replace = TRUE)))
        interval = quantile(bootstrap, c(0.025, 0.975))
        observed_loss = function(model) {
            p = model$probabilities
            -log(pmax(p[cbind(seq_along(model$truth), match(model$truth, colnames(p)))],1e-15))
        }
        loss_delta = observed_loss(a) - observed_loss(b)
        loss_bootstrap = replicate(2000, mean(sample(loss_delta, length(loss_delta), replace=TRUE)))
        loss_interval = quantile(loss_bootstrap,c(0.025,0.975))
        comparisons[[length(comparisons) + 1L]] = data.frame(
            first = pair[1], second = pair[2], accuracy_difference = mean(delta),
            lower = interval[1], upper = interval[2],
            first_minus_second_log_loss = mean(loss_delta),
            log_loss_lower = loss_interval[1], log_loss_upper = loss_interval[2]
        )
    }
    write.csv(do.call(rbind, comparisons), "paired_comparisons.csv", row.names = FALSE)
    for (name in names(models)) {
        m = models[[name]]
        write.csv(table(truth = m$truth, predicted = m$predicted),
                  paste0("confusion_", gsub(" ", "_", tolower(name)), ".csv"),
                  row.names = FALSE)
    }
}

draw_plot = function() {
    z = best_config[order(best_config$accuracy), ]
    label = ifelse(z$family == "pixels", "Pixels",
        ifelse(z$family == "conv",
               paste0("Conv ", z$kernel_size, "x", z$kernel_size,
                      ", ", z$filters, " filters, ", z$pooling),
               paste0("Scatter ", z$scales, " scales, ", z$pooling)))
    colors = c("Pixels" = "#555555", "Random convolution global" = "#2166AC",
               "Random convolution local" = "#67A9CF", "Scattering global" = "#B2182B",
               "Scattering local" = "#EF8A62")
    se = sqrt(z$accuracy * (1-z$accuracy) / 2000)
    par(mar = c(4, 15, 3, 1))
    plot(100*z$accuracy, seq_len(nrow(z)), pch=19, col=colors[z$group],
         yaxt="n", ylab="", xlab="Held-out accuracy (%)", xlim=c(40,100),
         main="MNIST: 8,000 training / 2,000 test images")
    segments(100*(z$accuracy-1.96*se),seq_len(nrow(z)),
             100*(z$accuracy+1.96*se),seq_len(nrow(z)),col=colors[z$group])
    axis(2,at=seq_len(nrow(z)),labels=label,las=1,cex.axis=.75)
    abline(v=100*best_config$accuracy[best_config$family=="pixels"],lty=3,col="gray60")
}
pdf("accuracy.pdf", width=10, height=7)
draw_plot()
dev.off()
png("accuracy.png", width=1500, height=1050, res=150)
draw_plot()
dev.off()

fmt = function(x, digits=2) formatC(x, format="f", digits=digits)
lines = c(
    "# MNIST feature comparison", "",
    "Balanced subset: 800 images per digit from the official training set and",
    "200 per digit from the official test set (8,000 training; 2,000 held out).",
    "The split and common stratified five-fold training assignments use seed 20261115.", "",
    "All classifiers are multinomial logistic regression in glmnet. Ridge and lasso",
    "penalties are compared; lambda and the reported representative configuration",
    "within each family are selected by training CV log loss, never test accuracy.",
    "Features are standardized by glmnet. Columns constant in training are removed.", "",
    "The pixel lasso candidate uses thresh=1e-5 and maxit=3000 because the original",
    "path was exceptionally slow. Fits with convergence warnings, and a pixel",
    "lasso minimum at its capped path endpoint, are excluded from model selection.", "",
    "## Models selected by training cross-validation", "",
    "| Family | Configuration | Penalty | Features retained | Test accuracy | Test log loss | Feature extraction (s) | CV fitting (s) |",
    "|---|---|---|---:|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(selected))) {
    z = selected[i, ]
    lines = c(lines,sprintf("| %s | %s | %s | %d | %s%% | %s | %s | %s |",
        z$group,z$id,if(z$alpha==0)"ridge" else "lasso",z$retained,
        fmt(100*z$accuracy),fmt(z$log_loss,3),fmt(z$feature_seconds,1),fmt(z$fit_seconds,1)))
}
lines = c(lines, "", "## Feature definitions", "",
    "- Pixels: original 784 intensities divided by 255.",
    "- Random convolution: Gaussian kernels of widths 3, 5, and 9; 128, 512,",
    "  and 1,024 filters; global max pooling. Banks are nested within each width",
    "  using seed 9000 + width. No pixel standardization is learned.",
    "- Spatial control: 128 random 5x5 filters with max pooling in a 4x4 grid",
    "  (2,048 features before removing constants).",
    "- Scattering: 1, 2, or 3 scales, 6 angles, order 2, global or local means.",
    "  The prototype uses valid convolution, so zero background is added around",
    "  each unchanged 28x28 digit. Padding is derived from the filter path supports.",
    "  Local summaries are cropped to centers inside the original 28x28 region.",
    "  Local grids have 14x14, 7x7, and 4x4 positions, respectively.",
    "  A 32-image training-only timing pilot chooses FFT or direct per configuration.",
    "  Backend timings are in backend_pilot.csv. Extraction batches contain 250 images.", "",
    "## Interpretation and timing", "",
    "This is one exploratory train/test split. The test set is shared, so paired",
    "bootstrap intervals for accuracy differences are in paired_comparisons.csv.",
    "Spatial pooling differs between global convolution and local scattering;",
    "the locally pooled random-convolution model helps assess that distinction.",
    "Feature extraction times cover all 10,000 images. CV fitting times include",
    "the full training fit and five folds for one penalty path. Configurations",
    "run in up to four processes, so these are elapsed times under contention,",
    "not isolated performance benchmarks. Pixel extraction was measured serially.",
    "Scattering uses the experimental package code, not a validated ScatNet replica.", "",
    "## Reproduction", "", "Run from explore/mnist:", "", "```sh",
    "Rscript download.R", "MNIST_WORKERS=4 Rscript run.R", "Rscript report.R", "```", "",
    "After the main grid, Rscript refine.R extends lambda paths selected at endpoints",
    "and regenerates this report. All individual CV curves are retained in models/.", "",
    "run.R resumes from per-configuration features and per-model checkpoints.",
    "The current package native library in ../../src/bases.so must already exist.",
    "No packages are installed or native sources regenerated by these scripts.",
    "Raw files, feature matrices, and fitted coefficients are ignored by git.",
    "MNIST mirror: https://github.com/cvdfoundation/mnist.", "",
    "Full results: results.csv; best penalty per configuration: configuration_summary.csv.",
    "Classifier coefficients, CV curves, and held-out probabilities are in models/."
)
if (any(results$lambda_endpoint)) {
    lines = c(lines,"", "Some fits selected a lambda-path endpoint; inspect results.csv before treating the grid as final.")
}
if (file.exists("selected_feature_bench.csv")) {
    timing = read.csv("selected_feature_bench.csv")
    lines = c(lines, "", "## Isolated feature extraction", "",
        "Medians from bench::mark(), three iterations on 100 training images,",
        "with no model-fitting workers running and GC iterations retained.",
        "The 10,000-image column extrapolates the batch timings; it excludes loading and fitting.", "",
        "| Family | Milliseconds per image | Estimated seconds for 10,000 |",
        "|---|---:|---:|")
    for (i in seq_len(nrow(timing))) {
        z = timing[i, ]
        lines = c(lines,sprintf("| %s | %s | %s |",z$group,
            fmt(1000*z$seconds_per_image,3),fmt(z$estimated_seconds_10000,2)))
    }
}
if (file.exists("paired_comparisons.csv")) {
    paired = read.csv("paired_comparisons.csv")
    contrast = paired[paired$first == "Random convolution local" &
                           paired$second == "Scattering local", ]
    if (nrow(contrast) == 1L) {
        lines = c(lines,"",sprintf(
            "The local-scattering accuracy gain over local random convolution is %s percentage points; its paired bootstrap 95%% interval is [%s, %s] percentage points.",
            fmt(-100*contrast$accuracy_difference),fmt(-100*contrast$upper),
            fmt(-100*contrast$lower)))
    }
}
writeLines(lines, "README.md")
