height_lookup <- c(letters[1:26], 'S', 'E')

height_to_numeric <- function(x) {
    match(x, height_lookup)
}

parse_map_row <- function(x) {
    unlist(lapply(strsplit(x, '')[[1]], height_to_numeric))
}

map_rows <- strsplit(map_str, '\n')[[1]]
map <- simplify2array(lapply(map_rows, parse_map_row))

distance_to_end <- function(start) {
    distances <- matrix(, nrow = nrow(map), ncol = ncol(map))
    distances[start] <- 0
    dist <- 0
    while (is.na(distances[end])) {
        to_check <- which(distances == dist, arr.ind = TRUE)
        dist <- dist + 1
        for (row in 1:nrow(to_check)) {
            pos <- to_check[row, ]
            nearby <- list(
                pos + c(1, 0),
                pos + c(0, 1),
                pos + c(-1, 0),
                pos + c(0, -1)
            )

            for (loc in nearby) {
                if (any(loc < 1) | loc[1] > nrow(map) | loc[2] > ncol(map)) next
                if (!is.na(distances[loc[1], loc[2]])) next
                if (map[pos[1], pos[2]] < map[loc[1], loc[2]] - 1) next
                distances[loc[1], loc[2]] <- dist
            }
        }
    }
    distances[end]
}

start <- which(map == 27, arr.ind = TRUE)
end <- which(map == 28, arr.ind = TRUE)
map[map == 27] <- 1
map[map == 28] <- 26
part1 <- distance_to_end(start)

start <- which(map == 27 | map == 1, arr.ind = TRUE)
part2 <- distance_to_end(start)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)