example.scrap.pruefnr = function() {
  setwd("D:/libraries/stuko/ulm")

  files = list.files("D:/libraries/stuko/modulpruefnr",pattern = glob2rx("*.pdf"),full.names = TRUE)
  dat = bind_rows(lapply(files, scrap.pruefungnr.get.raw))

  db = get.stukodb()
  scrap.pruefungnr.raw.to.db(dat, db=db)
}

scrap.pruefungnr.get.raw = function(pdf.file = "Module und Prüfungsnr WiWi 2017.pdf") {
  restore.point("scrap.pruefungnr.get.raw")
  library(pdftools)
  library(stuko)

  basename = tools::file_path_sans_ext(pdf.file)
  cat("\nscrap ", pdf.file,"...")
  if (!file.exists(paste0(basename,".txt"))) {
    library(pdftools)
    txt = pdf_text(paste0(basename,".pdf"))
    writeUtf8(merge.lines(txt), paste0(basename,".txt"))
  }
  txt = readUtf8(paste0(basename,".txt"))

  txt = str.trim(txt)
  num = str.left.of(txt," ")

  dat = data_frame(txt=txt, num=num)
  dat= filter(dat,  nchar(num) == 5 , !is.na(as.integer(num)))

  return(dat)
}

scrap.pruefungnr.raw.to.db = function(dat, db = get.stukodb()) {
  restore.point("scrap.pruefungnr.raw.to.db")

  # Modulcode starts with 7
  # pruefungsnummer starts with 1
  unique(substr(dat$txt,1,1))

  # Nummern die nicht mit 1 oder 7 starten
  dat$txt[!substr(dat$txt,1,1) %in% c("1","7")]

  dat = mutate(dat,
    first.digit = substr(dat$txt,1,1),
    is.modul = first.digit == "7",
    is.pruefung = first.digit == "1"
  ) %>%
    filter(is.modul | is.pruefung) %>%
    mutate(modul.row = cumsum(is.modul))

  md = filter(dat, is.modul) %>%
    transmute(modulcode = num, modul=str.right.of(txt," "),modul.row =modul.row )

  pd = filter(dat, is.pruefung) %>%
    transmute(pruefungsnr = num, pruefung=str.right.of(txt," "),modul.row =modul.row )


  df = left_join(pd, md, by="modul.row")

  df$is.vorleistung = has.substr(df$pruefung,"(Vorl")
  df$is.vorleistung



  df = df %>%
    arrange(modulcode,is.vorleistung, pruefungsnr) %>%
    group_by(modulcode) %>%
    #mutate(pruefungsnr = ifelse(!is.vorleistung,pruefungsnr, paste0(pruefungsnr," (Vorleistung)"))) %>%
    mutate(dupl = duplicated(pruefungsnr)) %>%
    filter(!dupl) %>%
    mutate(num_pruefungsnr = n()) %>%
    ungroup()

  ds = df %>% group_by(modulcode) %>%
    mutate(pruefungsnr =
      if (sum(!is.vorleistung)>1) {
        paste0(pruefungsnr,": ", pruefung)
      } else {
        ifelse(!is.vorleistung,pruefungsnr, paste0(pruefungsnr," (Vorleistung)"))
      }
    ) %>%
    summarize(
      pruefungsnr = paste0(unique(pruefungsnr), collapse=", "),
      modul = first(modul),
      pruefung = paste0(unique(pruefung), collapse=", ")
    )


  if (!is.null(db)) {
    dbWithTransaction(db, {
      modul = dbGet(db,"modul")
      modul = left_join(select(modul, -pruefungsnr), select(ds, code=modulcode, pruefungsnr),by="code") %>%
        mutate(pruefungsnr = ifelse(is.na(pruefungsnr),"", pruefungsnr))
      dbDelete(db, "modul", params=list())
      dbInsert(db, "modul",modul)
    })

  }

  invisible(ds)
}
