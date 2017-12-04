examples.data.frame.to.simple.form.yaml = function() {
  setwd("D:/libraries/stuko")
  db = get.stukodb()
  kurs = dbGet(db,"kurs")
  kurs = dbGet(db,"kursperson")

  kurs = dbGet(db,"modul")

  yaml = data.frame.to.simple.form.yaml(kurs, lang="de")
}

data.frame.to.simple.form.yaml = function(df, lang="en", file=NULL) {
  fields = lapply(colnames(df), function(col) {
    vals = unique(df[[col]])
    if (length(vals)<=10) {
      if (is.character(vals))
        vals = paste0("'", vals,"'")
      choices = paste0("\n        ",vals,": ",vals, collapse="")
      choices = paste0("\n      choices:", choices)
    } else {
      choices = NULL
    }

    paste0(
"  ",col,":
    lang_",lang,":
      label: '",col,"'", choices)
  })
  fields = paste0(fields, collapse="\n")
  cat(fields)
  str = paste0("fields:\n", fields)
  if (!is.null(file))
    writeLines(str, file)
  writeClipboard(str)
  str
}
