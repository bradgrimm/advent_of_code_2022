monkies <- paste(readLines("~/dev/advent_of_code_2022/data/day_11.txt"), collapse='\n')
monkies <- strsplit(monkies, '\n\n')[[1]]

last_part_as_int <- function(x) {
    parts <- strsplit(x, ' ')[[1]]
    as.integer(parts[[length(parts)]])
}

parse_monkey_data <- function (monkies) {
    monkey_data <- list()
    for (monkey_info in monkies) {
        m <- strsplit(monkey_info, '\n')[[1]]
        monkey_id <- strsplit(m[[1]], ' ')[[1]][[2]]
        monkey_id <- as.integer(sub(':', '', monkey_id))
        items <- strsplit(m[[2]], ': ')[[1]][[2]]
        items <- as.integer(strsplit(items, ', ')[[1]])
        operation <- strsplit(m[[3]], ': ')[[1]][[2]]
        test <- last_part_as_int(strsplit(m[[4]], ': ')[[1]][[2]])
        cond_true <- last_part_as_int(strsplit(m[[5]], ': ')[[1]][[2]]) + 1
        cond_false <- last_part_as_int(strsplit(m[[6]], ': ')[[1]][[2]]) + 1
        monkey_data[[monkey_id+1]] <- c(list(items), operation, test, cond_true, cond_false)
    }
    monkey_data
}

run_inspection <- function(monkey_data, inspection_count, mod_by) {
    for (m in 1:length(monkey_data)) {
        monkey <- monkey_data[[m]]
        inspection_count[[m]] <- inspection_count[[m]] + length(monkey[[1]])
        for (item in monkey[[1]]) {
            old <- item
            eval(parse(text=monkey[[2]]))
            value <- if (is.null(mod_by)) (new %/% 3) else (new %% mod_by)
            to_monkey <- if ((value %% monkey[[3]]) == 0) monkey[[4]] else monkey[[5]]
            monkey_data[[to_monkey]][[1]] <- append(monkey_data[[to_monkey]][[1]], value)
            # print(sprintf('(%s, %s): %s -> %s -- %s -- %s', old, new, value, to_monkey, monkey[[2]], is_part1))
        }
        monkey_data[[m]][[1]] <- list()
    }
    list('data' = monkey_data, 'counts' = inspection_count)
}

calculate_division_product <- function(monkey_data) {
    divisors <- c()
    for (data in monkey_data) divisors <- append(divisors, data[[3]])
    prod(divisors)
}

run_full_inspection <- function(n, mod_by_divisors) {
    monkey_data <- parse_monkey_data(monkies)
    inspection_count <- rep(0, length(monkey_data))
    mod_by <- if (mod_by_divisors) calculate_division_product(monkey_data) else NULL
    for (i in 1:n) {
        output <- run_inspection(monkey_data, inspection_count, mod_by = mod_by)
        monkey_data <- output$data
        inspection_count <- output$count
    }
    prod(head(sort(inspection_count, decreasing=TRUE), n=2))
}

part1 <- run_full_inspection(20, FALSE)
part2 <- run_full_inspection(10000, TRUE)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)