example.replace.umlaute = function() {
  str = c("��an","��fffn�")
  replace.umlaute(str)
}

replace.umlaute = function(str) {
  stringi::stri_replace_all_fixed(
    str,
    c("�", "�", "�", "�", "�", "�","�"),
    c("ae", "oe", "ue", "Ae", "Oe", "Ue","ss"),
    vectorize_all = FALSE
  )

}