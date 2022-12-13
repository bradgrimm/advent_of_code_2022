packets <- readLines("~/dev/advent_of_code_2022/data/day_13.txt")

parse_packets <- function(raw_str) {
    stack <- list()
    for (p in strsplit(raw_str, '(?=[^\\d])', perl=TRUE)[[1]]) {
        n = length(stack)
        if (p == '[') {
            stack[[n + 1]] <- list()
        } else if (p == ']') {
            if (n > 1) {
                prev_n <- length(stack[[n - 1]])
                stack[[n - 1]][[prev_n + 1]] <- stack[[n]]
                stack <- head(stack, n=length(stack)-1)
            }
        } else if (p == ',') {
            next
        } else {
            stack[[n]] <- append(stack[[n]], as.integer(p))
        }
    }
    stack
}

compare_packets <- function(left, right)  {
    left_list <- typeof(left) == 'list'
    right_list <- typeof(right) == 'list'
    if (left_list & right_list) {
        n_left <- length(left)
        n_right <- length(right)
        n <- min(n_left, n_right)
        if (n > 0) {
            for (i in 1:n) {
                c <- compare_packets(left[[i]], right[[i]])
                if (!is.null(c)) return (c)
            }
        }
        if (n_left == n_right) return (NULL)
        return (n_left < n_right)
    } else if (left_list) {
        return (compare_packets(left, list(right)))
    } else if (right_list) {
        return (compare_packets(list(left), right))
    } else {
        if (left == right) return (NULL)
        return (left < right)
    }
}

is_valid <- list()
for (i in seq(1, length(packets), 3)) {
    left <- parse_packets(packets[[i]])
    right <- parse_packets(packets[[i+1]])
    idx <- (i %/% 3) + 1
    is_valid[[idx]] <- compare_packets(left, right)
}
part1 <- sum(which(is_valid == TRUE))

all_packets <- list('[[2]]', '[[6]]')
for (i in 1:length(packets)) {
    if (packets[[i]] == '') next
    all_packets <- append(all_packets, packets[[i]])
}

order_cache <- list()
ordered_packets <- list()
while (length(ordered_packets) < length(all_packets)) {
    found_one <- FALSE
    for (left_idx in 1:length(all_packets)) {
        if (left_idx %in% ordered_packets) next
        has_valid <- FALSE
        for (right_idx in 1:length(all_packets)) {
            if (right_idx %in% ordered_packets | left_idx == right_idx) next
            if (!left_idx %in% order_cache) {
                left <- parse_packets(all_packets[[left_idx]])
                right <- parse_packets(all_packets[[right_idx]])
                is_smaller <- compare_packets(left, right)
                order_cache[[left_idx]] <- is_smaller
                order_cache[[right_idx]] <- !is_smaller
            }
            if (order_cache[[left_idx]]) {
                has_valid <- TRUE
                break
            }
        }
        if (!has_valid) {
            ordered_packets <- append(ordered_packets, left_idx)
            found_one <- TRUE
            break
        }
    }
}

ordered_packets <- rev(ordered_packets)
part2 <- which(ordered_packets == 1) * which(ordered_packets == 2)

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)