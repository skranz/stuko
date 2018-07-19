remove.null = function(li) {
  if (length(li)==0) return(li)
  nu = sapply(li, is.null)
  li[!nu]
}

example.replace.umlaute = function() {
  str = c("äan")
  for (f in list.files(getwd(), full.names=TRUE)) {
    cat("\n",f)
    parse(f)
  }
  replace.umlaute(str)
}

replace.umlaute = function(str) {
  stringi::stri_replace_all_fixed(
    str,
    c("\U00e4", "\U00f6", "ü", "Ä", "Ö", "Ü","ß"),
    c("ae", "oe", "ue", "Ae", "Oe", "Ue","ss"),
    vectorize_all = FALSE
  )

}
