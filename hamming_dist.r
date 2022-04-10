get_mod_hamming <- function(string1, string2) {
    if ((class(string1) != "character") | (class(string2) != "character")) {
        return(stop("Please pass two character arguments"))
    } else if (length(string1) != 1 | length(string2) != 1) {
        return(stop("Please pass two single-element vectors"))
    } else {
        if (nchar(string1) != nchar(string2)) {
            return(stop("Please pass two strings of equal length"))
        } else if (string1 == string2) {
            return(list(dist = 0, dist_norm = 0))
        } else if (string1 != string2) {
            string_mat <- cbind(unlist(strsplit(string1, split = "")),
                                unlist(strsplit(string2, split = "")))
            dist <- 0
            for (i in seq_len(nrow(string_mat))) {
                if ((string_mat[i, 1] == string_mat[i, 2]) |
                    (toupper(string_mat[i, 1]) %in% c("S", "Z") &
                     toupper(string_mat[i, 2]) %in% c("S", "Z"))) {
                    dist <- dist + 0
                } else {
                        if ((i != 1) & ((toupper(string_mat[i, 1]) ==
                                         toupper(string_mat[i, 2])) &
                                        (string_mat[i, 1] %in% LETTERS !=
                                         string_mat[i, 2] %in% LETTERS)) |
                                        ((toupper(string_mat[i, 1]) !=
                                          toupper(string_mat[i, 2])) &
                                        (string_mat[i, 1] %in% LETTERS !=
                                         string_mat[i, 2] %in% LETTERS))) {
                        dist <- dist + 0.5
                    }
                        if ((i == 1) & (toupper(string_mat[i, 1]) ==
                                        toupper(string_mat[i, 2]))) {
                            dist <- dist + 0
                    }
                        if (toupper(string_mat[i, 1]) !=
                            toupper(string_mat[i, 2])) {
                            dist <- dist + 1
                    }
                }
            }
        dist_norm <- dist / nrow(string_mat)
        dists <- list(dist = dist, dist_norm = dist_norm)
        return(dists)
        }
    }
}