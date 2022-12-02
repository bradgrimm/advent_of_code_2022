df <- read.delim("~/dev/advent_of_code_2022/data/day_02.txt", sep=' ', header=FALSE)

score_lookup <- c(X=1, Y=2, Z=3)
win_lookup <- c(X='C', Y='A', Z='B')
draw_lookup <- c(X='A', Y='B', Z='C')
choice_lookup <- list(
  X=c(A='Z', B='X', C='Y'),
  Y=c(A='X', B='Y', C='Z'),
  Z=c(A='Y', B='Z', C='X')
)

score_column <- function(df, column) {
    score_round <- function(row) {
        if (win_lookup[row[column]] == row['V1']) return(6)
        if (draw_lookup[row[column]] == row['V1']) return(3)
        return (0)
    }

    df$shape_score <- sapply(df[column], function(x) score_lookup[x])
    df$outcome_score <- apply(df, 1, score_round)
    sum(df$shape_score + df$outcome_score)
}

pick_move <- function(row) choice_lookup[[row['V2']]][row['V1']]
df$selected_move <- apply(df, 1, pick_move)

part1 <- score_column(df, 'V2')
part2 <- score_column(df, 'selected_move')

sprintf("Part 1 = %s\nPart 2 = %s", part1, part2)