# (C) 2013-2014, Marek Gagolewski, http://gagolewski.rexamine.com

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
      cat(stri_paste(ident, f[(s1[i]+1):(s2[i]-1)]), sep="\n", file=g)
      cat(f[s2[i]], "\n", sep="", file=g)
      cat("\n", file=g)
   }
   close(g)
   invisible(NULL)
}


#####################################################################

files <- dir("~/Publikacje/bibliography", pattern=glob2rx("*.bib"), full.names=TRUE)
for (f in files) {
   cat(f, ": ", sep="")
   bibtex_clean(f)
   cat("OK\n")
}
