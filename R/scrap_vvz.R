examples.scrap_vvz = function() {
  setwd("D:/libraries/stuko/vvz")
  df = scrap_all_vvz()
  readr::write_csv(df, "D:/libraries/stuko/vvz_wiwi.csv")

}



scrap_all_vvz = function(dir=getwd()) {
  setwd(dir)

  pdf.files = list.files(dir, glob2rx("*.pdf"))

  li = lapply(pdf.files, function(file) {
    basename = tools::file_path_sans_ext(file)
    scrap_vvz(basename)
  })
  bind_rows(li)
}

scrap_vvz = function(basename, semester=str.between(basename,"-","-"), overwrite.txt = FALSE) {
  restore.point("scrap_vvz")

  cat("\nscrap ", basename,"...")
  if (!file.exists(paste0(basename,".txt")) | overwrite.txt) {
    library(pdftools)
    txt = pdf_text(paste0(basename,".pdf"))
    writeUtf8(merge.lines(txt), paste0(basename,".txt"))
  }
  txt = readUtf8(paste0(basename,".txt"))

  start.line = which(txt == "Wirtschaftswissenschaften")
  txt = txt[start.line:length(txt)]

  codes = c("WiWi","MATH", "ZSP-","CS")

  start.lines = NULL
  for (code in codes) {
    start.lines = c(start.lines,which(str.starts.with(txt,code)))
  }

  end.lines = c(start.lines[-1]-1,length(txt))
  end.lines = pmin(end.lines, start.lines+5)

  num = 10
  li = lapply(seq_along(start.lines), function(num) {
    scrap.vvz.info(txt[start.lines[num]:end.lines[num]])
  })

  df = bind_rows(li)
  df$semester = semester
  colnames(df) = lowercase(colnames(df))

  readr::write_csv(df, paste0(basename,".csv"))
  invisible(df)
}

scrap.vvz.info = function(str) {
  restore.point("scrap.vvz.info")


  vnum = str.left.of(str[1], " ")
  kurs = str.trim(str.right.of(str[1]," "))
  if (!str.starts.with(str[2],"Veranstalter:") & !str.starts.with(str[2],"Art:")) {
    kurs = paste0(kurs," ", str.trim(str[2]))
  }


  row = which(str.starts.with(str, "Veranstalter:"))[1]
  s = str.trim(str.right.of(str[row],"Veranstalter:"))
  dozenten = s
  #dozenten = strsplit(s, ";", fixed=TRUE)[[1]]

  row = which(str.starts.with(str, "Art:"))[1]
  s = str.trim(str.between(str[row],"Art:","("))
  art = s
  s = str.trim(str.between(str[row],"(","SWS)"))
  sws = s

  nlist(vnum, kurs,dozenten, art, sws)

}
