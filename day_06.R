data <- readLines("~/dev/advent_of_code_2022/data/day_06.txt")
data <- strsplit(data, '')[[1]]

find_unique_packet <- function(data, n) {
  n <- n - 1
  for (i in 1:(length(data) - n)) {
    packet <- data[i:(i+n)]
    has_duplicates <- any(duplicated(packet))
    if (!has_duplicates) break
  }
  i + n
}

part1 <- find_unique_packet(data, 4)
part2 <- find_unique_packet(data, 14)
sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)