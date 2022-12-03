df <- read.delim("~/dev/advent_of_code_2022/data/day_03.txt", sep=' ', header=FALSE)

priority_lookup <- c(letters[1:26], toupper(letters[1:26]))
find_shared <- function(s) {
    total <- nchar(s)
    mid <- total %/% 2
    left <- substr(s, 1, mid)
    right <- substr(s, mid + 1, total)
    left <- unlist(strsplit(left, ''))
    right <- unlist(strsplit(right, ''))
    shared_value <- intersect(left, right)
    match(shared_value, priority_lookup)
}
part1 <- sum(apply(df, 1, find_shared))

group_id <- (as.numeric(rownames(df)) + 2) %/% 3
find_shared_triplets <- function(x) {
    x1 <- unlist(strsplit(x[1], ''))
    x2 <- unlist(strsplit(x[2], ''))
    x3 <- unlist(strsplit(x[3], ''))
    shared_value <- intersect(intersect(x1, x2), x3)
    match(shared_value, priority_lookup)
}
part2 <- sum(aggregate(df, list(group_id), find_shared_triplets)$V1)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)