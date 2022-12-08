data <- readLines("~/dev/advent_of_code_2022/data/day_07.txt")

# Find all sub directories, and all file sizes.
current_dir <- list('/')
file_sizes <- list()
sub_dirs <- list()
for (line in data) {
    if (startsWith(line, '$ cd')) {
        rhs <- strsplit(line, ' ')[[1]][3]
        if (rhs == '/') current_dir <- rhs
        else if (rhs == '..') current_dir <- current_dir[1:(length(current_dir)-1)]
        else current_dir <- append(current_dir, rhs)
    } else if (!startsWith(line, '$ ls')) {
        command <- strsplit(line, ' ')[[1]]
        path <- append(current_dir, command[2])
        dir_path <- paste(current_dir, collapse='/')
        file_path <- paste(path, collapse='/')
        sub_dirs[[dir_path]] <- append(sub_dirs[[dir_path]], file_path)
        if (!startsWith(line, 'dir')) {
            file_sizes[paste(file_path, collapse='/')] <- as.integer(command[1])
        }
    }
}

# Calculate total directory sizes.
all_dir_sizes <- list()
total_dirs <- function(dir) {
    total_sz <- 0
    for (sub_item in sub_dirs[[dir]]) {
        sz <- file_sizes[[sub_item]]
        if (!is.null(sz)) total_sz <- total_sz + sz
        else total_sz <- total_sz + total_dirs(sub_item)
    }
    all_dir_sizes[[dir]] <<- total_sz
    total_sz
}

total_dirs('/')

part1 <- sum(unlist(all_dir_sizes[all_dir_sizes <= 100000]))

space_needed <- 30000000 - (70000000 - all_dir_sizes[['/']])
part2 <- min(unlist(all_dir_sizes[all_dir_sizes >= space_needed]))

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)
