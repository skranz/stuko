# Diagnosen zum Lehrangebot

examples.pruefung.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")

  semester = 185
  pruefung.report(semester, db)

}

pruefung.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/pruefungen.docx"), sets=getApp()$glob$sets) {
  restore.point("pruefung.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")

  sd = get.sem.data(semester)
  ku = sd$kurse
  mo = sd$module

  tpl.file = system.file("report_tpl/pruefung_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)

  ku = filter(ku,
    is.true(extern==FALSE),
    pruefungsform != "se"
  )

  unique(ku$extern)
  unique(ku$zuordnung)
  unique(ku$bama)
  ku = mutate(ku,
    BA_PFLICHT = has.substr(zuordnung,"BA Pflicht"),
    BA_WP = !BA_PFLICHT & bama == "BA",
    BA_MA = ku$bama %in% c("BA MA","MA, BA"),
    NUF_PFLICHT = has.substr(zuordnung,"NUF Pflicht")
  ) %>% arrange(
    -BA_PFLICHT, NUF_PFLICHT, -BA_WP,-BA_MA, dozent
  )

  klausuren = filter(ku, pruefungsform %in% c("k","km"))
  andere = filter(ku, pruefungsform %in% c("k","km"))


  # Change bookmarks
  doc = doc %>%
    body_replace_text_at_bkm("sem_label",paste0(sem_label," ")) %>%
    body_replace_text_at_bkm("date_label", date_label)

  doc = doc %>%
    body_add_par("Klausuren", style = "heading 1")

  df = transmute(klausuren,Kurs=kursname,Dozent=dozent,Zuordnung=zuordnung,Pruefungsnummern=pruefungsnr,Dauer=pruefung_dauer,Plaetze=pruefung_teilnehmer)

  doc = doc %>%
    body_add_table(df, style="Plain Table 1")

  doc = doc %>%
    body_add_par("Vorlesungen mit anderen Pruefungsformen", style = "heading 1")


  print(doc, target = out.file)

  invisible(doc)
}



