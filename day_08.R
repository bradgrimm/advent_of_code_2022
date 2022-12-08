rows <- readLines("~/dev/advent_of_code_2022/data/day_08.txt")

n_rows <- length(rows)
n_cols <- nchar(rows[[1]])
flat_mat <- strsplit(paste(rows, collapse='', sep=''), '')[[1]]
data <- matrix(as.integer(flat_mat), nrow=n_rows, byrow=TRUE)

# Part 1
is_visible <- matrix(FALSE, nrow=n_rows, ncol=n_cols)
check_visible <- function(data, rows, cols) {
    is_visible <- matrix(FALSE, nrow=n_rows, ncol=n_cols)
    for (i in rows) {
        low <- -1
        for (j in cols) {
            v <- data[i, j]
            if (v > low) {
                low <- v
                is_visible[i, j] <- TRUE
            }
        }
    }
    is_visible
}
d <- dim(data)
is_visible <- check_visible(data, 1:d[[1]], 1:d[[2]]) | check_visible(data, 1:d[[1]], d[[2]]:1)
is_visible_transposed <- check_visible(t(data), 1:d[[1]], 1:d[[2]]) | check_visible(t(data), 1:d[[1]], d[[2]]:1)
part1 <- sum(is_visible | t(is_visible_transposed))

# Part 2
check_visible <- function(data, rows, cols, low) {
    is_visible <- matrix(FALSE, nrow=n_rows, ncol=n_cols)
    for (i in rows) {
        for (j in cols) {
            if (i < 1 | j < 1 | i > n_rows | j > n_cols) break
            v <- data[i, j]
            is_visible[i, j] <- TRUE
            if (v >= low) break
        }
    }
    is_visible
}
scenic_score <- function(data, i, j) {
    val <- data[i, j]
    n_visible <- c(
        sum(check_visible(data, i, (j+1):n_rows, val)),
        sum(check_visible(data, i, (j-1):1, val)),
        sum(check_visible(t(data), j, (i+1):n_cols, val)),
        sum(check_visible(t(data), j, (i-1):1, val))
    )
    prod(n_visible)
}
all_grid_pts <- expand.grid(1:n_cols, 1:n_rows)
part2 <- max(apply(all_grid_pts, 1, function(x) scenic_score(data, x[1], x[2])))

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)