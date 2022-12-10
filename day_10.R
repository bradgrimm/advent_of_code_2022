instructions <- readLines("~/dev/advent_of_code_2022/data/day_10.txt")

op <- 0
signal_strength <- 0
display <- ''

update_display <- function() {
    c <- if(abs((op %% 40) - x) <= 1) '#' else '.'
    display <<- paste(display, c, sep='')
    op <<- op + 1
    if (op %% 40 == 20)
        signal_strength <<- signal_strength + (x * op)
    if (op %% 40 == 0)
        display <<- paste(display, '\n', sep='')
}

x <- 1
for (instr in instructions) {
    command <- strsplit(instr, ' ')[[1]]
    if (command[1] == 'addx') {
        update_display()
        update_display()
        x <- x + as.integer(command[2])
    } else if (command[1] == 'noop') {
        update_display()
    }
}

print(sprintf('Part 1: %s', signal_strength))
print('Part 2: ')
print(strsplit(display, '\n')[[1]])