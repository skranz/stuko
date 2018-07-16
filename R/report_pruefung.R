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
    pruefung != "se"
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
  ) %>%
    mutate(bereich = ifelse(
      BA_PFLICHT, "BA Pflicht",
      ifelse(NUF_PFLICHT, paste0("NUF Pflicht, ",bama),
        bama
      )
    )) %>%
    mutate(anmerkung =
      case_when(
        pruefung_selbst == "s1" ~ "Nur 2. Termin",
        pruefung_selbst == "s2" ~ "Nur 1. Termin",
        TRUE ~ ""
      )
    )


  klausuren = filter(ku, pruefung %in% c("k","km"), pruefung_selbst != "s")

  add.text= function(old, add) {
    if (length(old)==0) return(old)
    add.rows = nchar(old)>0
    old[add.rows] = paste0(old[add.rows],", ",add)
    old[!add.rows] = add
    old
  }
  klausuren = mutate(klausuren,
    anmerkung = ifelse(pruefung=="km",add.text(anmerkung,"evtl. m\U00FCndlich"), anmerkung)
  )

  selbst = filter(ku, pruefung %in% c("k","km"), pruefung_selbst == "s")
  andere = filter(ku, !pruefung %in% c("k","km"))


  # Change bookmarks
  doc = doc %>%
    body_replace_text_at_bkm("sem_label",paste0(sem_label," ")) %>%
    body_replace_text_at_bkm("date_label", date_label)

  doc = doc %>%
    body_add_par("Klausuren", style = "heading 1")

  df = transmute(klausuren,Kurs=kursname,Dozent=dozent,Zuordnung=bereich,Pruefungsnummern=pruefungsnr,Dauer=pruefung_dauer,Plaetze=pruefung_teilnehmer, `*`=anmerkung)

  doc = doc %>%
    body_add_table(df, style="Plain Table 1")

  doc = doc %>%
    body_add_par("Von Instituten zu beiden Terminen eigenstaendig organisierte Klausuren ausserhalb des Pruefungszeitraums", style = "heading 1")

  df = transmute(selbst,Kurs=kursname,Dozent=dozent,Zuordnung=bereich,Pruefungsnummern=pruefungsnr,Anmerkung="Ausserhalb Pruefungszeitraum. Vom Institut organisiert.")

  if (NROW(df)>0) {
    doc = doc %>%
      body_add_table(df, style="Plain Table 1")
  } else {
    doc = doc %>%
      body_add_par("--- Kein Eintrag ---")
  }

  pf.fun = function(pf) {
    multi.rows = has.substr(pf,",")

    li = strsplit(pf[multi.rows],",")
    multi = lapply(li, function(vec) {
      paste0(to.label(str.trim(vec),sets$pruefung), collapse=", ")
    })
    pf[multi.rows] = multi
    pf[!multi.rows] = to.label(pf[!multi.rows], sets$pruefung)
    pf
  }
  doc = doc %>%
    body_add_par("Vorlesungen ohne Klausuren", style = "heading 1")

  df = transmute(andere,Kurs=kursname,Dozent=dozent,Zuordnung=bereich,Pruefungsnummern=pruefungsnr,Pruefungsform=pf.fun(pruefung))

  if (NROW(df)>0) {
    doc = doc %>%
      body_add_table(df, style="Plain Table 1")
  } else {
    doc = doc %>%
      body_add_par("--- Kein Eintrag ---")
  }


  print(doc, target = out.file)

  invisible(doc)
}



