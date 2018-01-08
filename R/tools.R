remove.null = function(li) {
  if (length(li)==0) return(li)
  nu = sapply(li, is.null)
  li[!nu]
}

example.replace.umlaute = function() {
  str = c("üßan","Äüfffnö")
  replace.umlaute(str)
}

replace.umlaute = function(str) {
  stringi::stri_replace_all_fixed(
    str,
    c("ä", "ö", "ü", "Ä", "Ö", "Ü","ß"),
    c("ae", "oe", "ue", "Ae", "Oe", "Ue","ss"),
    vectorize_all = FALSE
  )

}
