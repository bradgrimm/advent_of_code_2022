all_lines = readLines("~/dev/advent_of_code_2022/data/day_01.txt")
all_lines = paste(all_lines, collapse="\n")
elf_packs = unlist(strsplit(all_lines, "\n\n"))

cast_sum <- function(x) {
    values = strsplit(x, "\n")
    int_values = as.integer(values[[1]])
    sum(int_values)
}
elf_calories = unlist(lapply(elf_packs, cast_sum))
part1 <- max(elf_calories)
part2 <- sum(tail(sort(elf_calories), n=3))
sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)