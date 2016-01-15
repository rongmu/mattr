# An alternative implementation of MATTR algorithm in R
# author: Shaoyun YU <eric.rongmu@gmail.com>
# ref: Covington & Mcfall (2010) Cutting the Gordian Knot: The Moving-Average Type-Token Ratio
# usage: mattr(vector_of_tokens, window_size)

window_types <- function(index, win_size, data) {
  i_start <- index
  i_end   <- i_start + win_size - 1

  win <- data[i_start:i_end]

  length(unique(win))
}


mattr <- function(x, win_size) {
  n_win <- length(x) - win_size + 1

  types <- vapply(1:n_win, window_types, integer(1),
                  win_size = win_size, data = x)

  mean(types) / win_size
}