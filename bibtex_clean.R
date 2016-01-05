#!/usr/bin/Rscript --vanilla

# (C) 2013-2016, Marek Gagolewski, http://gagolewski.rexamine.com

#####################################################################

bibtex_clean <- function(fin, fout=paste(fin, "out", sep="."), ident="   ")
{
   library('stringi')

   f <- stri_trim(stri_read_lines(fin))
   s1 <- which(stri_detect_regex(f, "^@"))
   s2 <- which(stri_detect_regex(f, "^\\}"))
   stopifnot(s1 < s2)
   stopifnot(s1[-1] > s2[-length(s2)])

   # extract entry ids
   ids <- stri_match_first_regex(f, "^@[^{]+\\{([^,]+),")[,2]
   stopifnot(which(!is.na(ids)) == s1)
   ids <- ids[!is.na(ids)]

   # sort w.r.t. identifier
   o <- stri_order(ids)
   ids <- ids[o]
   s1 <- s1[o]
   s2 <- s2[o]

   g <- file(fout, open="w")
   for (i in seq_along(s1)) {
      cat(f[s1[i]], "\n", sep="", file=g)
      contents <- f[(s1[i]+1):(s2[i]-1)]
#       contents <- stri_replace_first_regex(contents, '^([a-z]+)[ ]*=[ ]*["{](.*)["}](,?)$', "$1 = {$2}$3")
      cat(stri_paste(ident, contents), sep="\n", file=g)
      cat(f[s2[i]], "\n", sep="", file=g)
      cat("\n", file=g)
   }
   close(g)

   stri_match_first_regex(f, '^title[ ]*=[ ]*["{](.*)["}],?$')[,2]
}

# TO DO: extract title duplicates (adist)

#####################################################################

files <- dir("~/Publications/Bibliography", pattern="^.*\\.bib$", full.names=TRUE)
files <- files[!stringi::stri_detect_fixed(files, "cena_")]
titles <- character(0)
for (f in files) {
   cat(f, ": ", sep="")
   title <- bibtex_clean(f)
   title <- title[!is.na(title)]
   names(title) <- rep(basename(f), length(title))
   titles <- c(titles, title)
   cat("OK\n")
}


dst <- adist(titles)
cutoff <- 5
for (i in 1:(nrow(dst)-1)) {
   for (j in (i+1):ncol(dst)) {
      if (dst[i,j] < cutoff) {
         cat(sprintf("%s (%s) | %s (%s) | %g\n", titles[i], names(titles)[i], titles[j], names(titles)[j], dst[i,j]))
      }
   }
}
