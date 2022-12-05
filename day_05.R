input_name <- "~/dev/advent_of_code_2022/data/day_05.txt"
raw_input <- readChar(input_name, file.info(input_name)$size)

# Parse Input
input_sections <- strsplit(raw_input, "\n\n")[[1]]
parse_moves <- function(s) {
    parts <- strsplit(s, ' ')[[1]]
    as.integer(c(parts[2], parts[4], parts[6]))
}
move_list <- lapply(strsplit(input_sections[2], '\n')[[1]], parse_moves)

parse_stacks <- function(s) {
    start_boxes <- strsplit(s, '\n')[[1]]
    start_boxes <- rev(start_boxes)[2:length(start_boxes)]

    stacks <- list()
    for (i in 1:length(start_boxes)) {
        col_data <- strsplit(gsub('    ', ' ', start_boxes[[i]]), ' ')[[1]]
        boxes <- lapply(col_data, function(s) substr(s, 2, 2))
        for (b in 1:length(boxes)) {
            if (boxes[[b]] == '') next
            if (b > length(stacks)) stacks[[b]] <- list(boxes[[b]])
            else stacks[[b]] <- append(stacks[[b]], boxes[[b]])
        }
    }
    stacks
}

part_1_move <- function(stacks, num, from_col, to_col) {
    for (i in 1:num) {
        to_move <- tail(stacks[[from_col]], n=1)
        stacks[[to_col]] <- append(stacks[[to_col]], to_move)
        stacks[[from_col]] <- head(stacks[[from_col]], n=-1)
    }
    stacks
}

part_2_move <- function(stacks, num, from_col, to_col) {
    to_move <- tail(stacks[[from_col]], n=num)
    stacks[[to_col]] <- append(stacks[[to_col]], to_move)
    stacks[[from_col]] <- head(stacks[[from_col]], n=-num)
    stacks
}

run_all_moves <- function(stacks, move_list, move_fn) {
    for (m in 1:length(move_list)) {
        stacks <- move_fn(stacks, move_list[[m]][1], move_list[[m]][2], move_list[[m]][3])
    }
    top_boxes = lapply(stacks, function(x) ifelse (length(x) > 0, tail(x, n=1)[[1]], ''))
    paste(top_boxes, collapse='')
}

stacks <- parse_stacks(input_sections[1])
part1 <- run_all_moves(stacks, move_list, part_1_move)

stacks <- parse_stacks(input_sections[1])
part2 <- run_all_moves(stacks, move_list, part_2_move)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)