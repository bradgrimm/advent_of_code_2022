move_list <- readLines("~/dev/advent_of_code_2022/data/day_09.txt")

move_lookup <- list('R'=c(1, 0), 'L'=c(-1, 0), 'U'=c(0, 1), 'D'=c(0, -1))
move_list <- lapply(move_list, function(x) {
    move <- strsplit(x, ' ')[[1]]
    as.integer(move[[2]]) * move_lookup[[move[1]]]
})

move_tail <- function(rope, r) {
    xy <- rope[[r-1]]
    tail <- rope[[r]]
    if (max(abs(xy - tail)) > 1) {
        dir <- tail - xy
        all_steps <- abs(dir) >= max(abs(dir))
        tail_step <- as.integer(sign(dir) * all_steps)
        rope[[r]] <- xy + tail_step
    }
    rope
}

find_all_unique_tail_spots <- function(move_list, tail_len) {
    rope <- list()
    for (i in 1:tail_len) rope[[i]] <- c(0, 0)

    all_spots <- list(rope[[i]])
    for (move in move_list) {
        max_val <- max(abs(move))
        step <- move / max_val
        for (i in 1:max_val) {
            rope[[1]] <- rope[[1]] + step
            for (r in 2:tail_len) {
                rope <- move_tail(rope, r)
            }
            all_spots <- append(all_spots, list(rope[[tail_len]]))
        }
    }
    length(all_spots[!duplicated(all_spots)])
}

part1 <- find_all_unique_tail_spots(move_list, 2)
part2 <- find_all_unique_tail_spots(move_list, 10)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)
