# Download the official IDX files from the CVDF mirror and retain a reproducible
# 8,000-image training subset and 2,000-image official-test subset.
dir.create("data", showWarnings = FALSE)
base_url = "https://storage.googleapis.com/cvdf-datasets/mnist/"
files = c(
    "train-images-idx3-ubyte.gz", "train-labels-idx1-ubyte.gz",
    "t10k-images-idx3-ubyte.gz", "t10k-labels-idx1-ubyte.gz"
)
md5 = c(
    "f68b3c2dcbeaaa9fbdd348bbdeb94873", "d53e105ee54ea40749a09fcbcd1e9432",
    "9fb629c4189551a2d022fa330f9573f3", "ec29112dd5afa0611ce80d1b7f02629c"
)
for (i in seq_along(files)) {
    dest = file.path("data", files[i])
    if (!file.exists(dest)) {
        download.file(paste0(base_url, files[i]), dest, mode = "wb", quiet = FALSE)
    }
    stopifnot(unname(tools::md5sum(dest)) == md5[i])
}

read_idx = function(path) {
    con = gzfile(path, "rb")
    on.exit(close(con))
    magic = readBin(con, "integer", n = 1, size = 4, endian = "big")
    n = readBin(con, "integer", n = 1, size = 4, endian = "big")
    if (magic == 2049L) return(readBin(con, "integer", n = n, size = 1, signed = FALSE))
    stopifnot(magic == 2051L)
    dims = readBin(con, "integer", n = 2, size = 4, endian = "big")
    pixels = readBin(con, "raw", n = n * prod(dims))
    stopifnot(length(pixels) == n * prod(dims))
    matrix(pixels, nrow = n, ncol = prod(dims), byrow = TRUE)
}

train_labels = read_idx(file.path("data", files[2]))
test_labels = read_idx(file.path("data", files[4]))
set.seed(20261115)
train_ids = sort(unlist(lapply(0:9, function(y) sample(which(train_labels == y), 800))))
test_ids = sort(unlist(lapply(0:9, function(y) sample(which(test_labels == y), 200))))
train_pixels = read_idx(file.path("data", files[1]))[train_ids, , drop = FALSE]
test_pixels = read_idx(file.path("data", files[3]))[test_ids, , drop = FALSE]
foldid = integer(length(train_ids))
for (y in 0:9) {
    ids = which(train_labels[train_ids] == y)
    foldid[ids] = sample(rep(1:5, length.out = length(ids)))
}
saveRDS(list(
    train_pixels = train_pixels, test_pixels = test_pixels,
    train_labels = train_labels[train_ids], test_labels = test_labels[test_ids],
    train_ids = train_ids, test_ids = test_ids, foldid = foldid,
    seed = 20261115L, source = base_url, file_md5 = setNames(md5, files)
), "data/subset.rds")
cat("Saved balanced 8,000 training / 2,000 test images to data/subset.rds\n")
