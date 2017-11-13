combine.modules = function() {
  setwd("D:/libraries/stuko")
  library(readr)
  df1 = read_csv("Module BA WiWI 2017_utf8.csv")
  df2 = read_csv("Module MA WiWI 2017_utf8.csv")
  df1$bama = "BA"
  df2$bama = "MA"
  df = bind_rows(df1, df2)
  write_csv(df,"modules.csv")
}

examples.scrap_all.modulhandbuch = function() {
  setwd("D:/libraries/stuko/module")
  dat = scrap_all_modulhandbuch()
  readr::write_csv(dat, "D:/libraries/stuko/module.csv")

}

scrap_all_modulhandbuch = function(dir=getwd()) {
  setwd(dir)

  pdf.files = list.files(dir, glob2rx("*.pdf"))

  li = lapply(pdf.files, function(file) {
    basename = tools::file_path_sans_ext(file)
    scrap_modulhandbuch(basename)
  })
  bind_rows(li)
}

scrap_modulhandbuch = function(basename,overwrite.txt = FALSE) {
  restore.point("scrap_modulhandbuch")
  library(pdftools)

  handbuch = str.between(basename,"-","-")
  is.nuf = str.starts.with(handbuch,"NUF")

  cat("\nscrap ", basename,"...")
  if (!file.exists(paste0(basename,".txt")) | overwrite.txt) {
    library(pdftools)
    txt = pdf_text(paste0(basename,".pdf"))
    writeUtf8(merge.lines(txt), paste0(basename,".txt"))
  }
  txt = readUtf8(paste0(basename,".txt"))

  if (!is.nuf) {
    ignore =
      str.starts.with(txt, "Bachelor of Science Wirtschaftswissenschaften Druckdatum:") |
      str.starts.with(txt, "Master of Science Wirtschaftswissenschaften")
  } else {
    ignore =
      str.starts.with(txt,"Master of Science Nachhaltige") |
      (str.starts.with(lag(txt),"Master of Science Nachhaltige") &
      str.starts.with(txt,"Unternehmensführung"))
  }


  txt = txt[!ignore]
  #line.dist = if (is.nuf) {1} else {2}
  line.dist = 1
  start.lines = which(str.starts.with(txt, "Modul zugeordnet zu"))-line.dist
  end.lines = c(start.lines[-1]-1,length(txt))

  txt = scraped.modules.normalize.title.lines(txt, start.lines,line.dist=line.dist)

  mod.num = 2
  li = lapply(seq_along(start.lines), function(mod.num) {
    scrap.module.info(txt[start.lines[mod.num]:end.lines[mod.num]], line.dist=line.dist)
  })

  li[[1]]
  names = sapply(li, function(el) el$Title)
  names(li) = names

  df = bind_rows(li)

  df = clean.scraped.module.info.df(df)

  df$handbuch = handbuch
  colnames(df) = tolower(colnames(df))

  readr::write_csv(df, paste0(basename,".csv"))
  invisible(df)
}

scraped.modules.normalize.title.lines = function(txt, start.lines, line.dist=2) {
  restore.point("scraped.modules.normalize.title.lines")

  # Deal with titles over three lines
  tripple = str.trim(txt[start.lines-line.dist]) != "" & str.trim(txt[start.lines-2*line.dist]) != ""

  rows = start.lines[tripple]
  txt[rows] = paste0(str.trim(txt[rows-2*line.dist])," ",str.trim(txt[rows-line.dist])," ", str.trim(txt[rows]))
  txt[rows-line.dist] = ""
  txt[rows-2*line.dist] = ""

  # Deal with titles over two lines
  double = str.trim(txt[start.lines-line.dist]) != ""

  rows = start.lines[double]
  txt[rows] = paste0(str.trim(txt[rows-line.dist])," ", str.trim(txt[rows]))
  txt[rows-line.dist] = ""

  txt
}

clean.scraped.module.info.df = function(df) {
  restore.point("clean.scraped.module.info.df")

  df$id = paste0(df$Code, ": ", df$Titel)

  # Verify that only "Zuordnung" is duplicated
  if (FALSE) {
    not.dupl = function(d) {
      if (NROW(d)==1) return(data_frame(cols=""))
      ind = sapply(d, function(vals) {
        any(duplicated(vals))
      })
      data_frame(cols=paste0(colnames(d)[!ind],collapse=", "))
    }
    nd = dat %>% group_by(id) %>%
      do(not.dupl(.)) %>%
      filter(cols != "")
    which(nd != "Zur")
    unique(nd$cols)

  }


  # Remove rows that only differ by Zuordnung
  # but store all Zuordnungen
  # comma separated
  mypaste = function(vec) {
    paste(unique(vec), collapse=", ")
  }

  dat = df %>%
    arrange(Zuordnung) %>%
    group_by(id) %>%
    mutate(Zuordnung = mypaste(Zuordnung)) %>%
    ungroup()
  dat = dat[!duplicated(dat),]


  if ("Studiengänge" %in% colnames(df)) {
    df$Studiengang = paste0(df$Studiengang," ", df$Studiengänge)
    df = df[,setdiff(colnames(df,"Studiengänge"))]
  }

  # Rename columns
  #cat(paste0('"',colnames(dat),'"', collapse=", "))
  colnames(dat) = c(
    "Titel", "Zuordnung", "Code", "ECTS", "SWS", "Sprache", "Dauer", "Turnus", "Modulkoordinator", "Dozent", "Studiengang", "Vorkenntnisse", "Lernergebnisse", "Inhalt", "Literatur", "Lehrform", "Arbeitsaufwand", "Bewertungsmethode", "Notenbildung", "Grundlage_Fuer", "id"
  )

  dat = dat %>% arrange(Modulkoordinator, Dozent, Titel)

  dat
}

scrap.module.info = function(str, line.dist=2) {
  restore.point("scrap.module.info")

  Titel = str[1]

  if (Titel=="Bachelor") stop()

  row = 1 + line.dist
  Zuordnung = str.right.of(str[row],"Modul zugeordnet zu ")

  str = str[-(1:row)]
  str = str[str.trim(str)!=""]

  row = which(str.starts.with(str,"Einordnung in die"))
  str[row] = paste0("Studiengang",str.right.of(str[row],"Einordnung in die"))
  str[row+1] = str.right.of(str[row+1],"Studiengänge")
  str[row+2] = str.right.of(str[row+2],"Studiengänge",not.found = str[row+2])

  row = which(str.starts.with(str,"Lehr- und"))
  str[row] = paste0("Lehrform",str.right.of(str[row],"Lehr- und"))
  str[row+1] = str.right.of(str[row+1],"Lernformen")
  #str[row+2] = str.right.of(str[row+2],"Lernformen")


  key.rows = which(!str.starts.with(str, " ") & !str.starts.with(str,"\t") & !str.trim(str) == "")

  str[key.rows]
  keys = str.left.of(str[key.rows],"  ")
  row = str.starts.with(keys,"Bewertungsmethode")
  keys[row] = "Bewertungsmethode"


  start.lines = key.rows
  end.lines = c(start.lines[-1]-1, length(str))

  li = lapply(seq_along(key.rows), function(i) {
    s = merge.lines(str.trim(str[start.lines[i]:end.lines[i]]))
    s = str.trim(str.right.of(s, keys[i]))
    s
  })
  names(li) = keys

  if ("Studieng?nge" %in% keys)
    stop("Have Studieng?nge")

  restore.point("sfhdfhidjfojdif")
  li = c(nlist(Titel, Zuordnung), li)
  li
}
