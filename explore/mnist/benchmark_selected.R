# Run after fitting has finished, with no concurrent fitting workers.
source("helpers.R")
basis = load_basis_code()
selected = read.csv("selected_models.csv")
data = readRDS("data/subset.rds")
pixels = data$train_pixels[seq_len(100), , drop = FALSE]
images = image_list(pixels)
out = list()
for (i in seq_len(nrow(selected))) {
    row = selected[i, ]
    cfg = readRDS(file.path("models", paste0(row$key, ".rds")))$configuration
    feature_fun = if (cfg$family == "pixels") {
        function() matrix(as.double(pixels) / 255, nrow(pixels), ncol(pixels))
    } else if (cfg$family == "conv") {
        activation = if (isTRUE(cfg$local)) local_max_pool(cfg$size) else max
        function() basis$b_conv(images, kernels = cfg$kernels,
                                activation = activation, stdize = "none")
    } else {
        function() scatter_features(basis, images, cfg$scales, cfg$pooling,
                                    cfg$fft, cfg$pad)
    }
    log_progress("isolated feature timing", row$id)
    b = bench::mark(feature_fun(), iterations = 3, check = FALSE, filter_gc = FALSE)
    out[[i]] = data.frame(
        id = row$id, group = row$group, batch_size = length(images),
        seconds_per_image = as.numeric(b$median) / length(images),
        estimated_seconds_10000 = 10000 * as.numeric(b$median) / length(images),
        allocated_MiB_per_image = as.numeric(b$mem_alloc) / 1024^2 / length(images)
    )
    print(out[[i]], row.names = FALSE)
}
write.csv(do.call(rbind,out), "selected_feature_bench.csv", row.names = FALSE)
