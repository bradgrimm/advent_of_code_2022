df <- read.delim("~/dev/advent_of_code_2022/data/day_04.txt", sep=',', header=FALSE)

to_range <- function(s) as.integer(strsplit(s, '-')[[1]])
full_overlap <- function(x) {
    left <- to_range(x['V1'])
    right <- to_range(x['V2'])
    left_in_right <- (left[1] >= right[1]) & (left[2] <= right[2])
    right_in_left <- (right[1] >= left[1]) & (right[2] <= left[2])
    left_in_right | right_in_left
}
part1 <- sum(apply(df, 1, full_overlap))

any_overlap <- function(x) {
    left <- to_range(x['V1'])
    right <- to_range(x['V2'])
    (right[2] >= left[1]) & (right[1] <= left[2])
}
part2 <- sum(apply(df, 1, any_overlap))

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)