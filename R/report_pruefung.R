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



  kumo = sd$kumo
  kumo = left_join(kumo, sd$mo, by=c("semester","modulid"))
  mopr = kumo %>%
    group_by(kursid) %>%
    summarize(modulpruef = paste0("[",code,"] ", pruefungsnr, collapse="\n\n"))

  ku = inner_join(ku, select(mopr,kursid,modulpruef),by="kursid")

  ku = filter(ku,
    is.true(extern==FALSE),
    pruefung != "se"
  )


  mo = sd$module

  tpl.file = system.file("report_tpl/pruefung_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)


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
    ) %>%
    mutate(Kurs=paste0(kursname," [",bereich,"]"))

  add.text= function(old, add, sep=", ") {
    if (length(old)==0) return(old)
    add.rows = nchar(old)>0
    if (length(add)==1) {
      old[add.rows] = paste0(old[add.rows],sep,add)
      old[!add.rows] = add
    } else {
      old[add.rows] = paste0(old[add.rows],sep,add[add.rows])
      old[!add.rows] = add[!add.rows]
    }

    old
  }
  ku = mutate(ku, anmerkung = add.text(pruefung_anmerkung, anmerkung,sep=". "))
  ku = mutate(ku, anmerkung = ifelse(pruefung=="m2",add.text(anmerkung,"1. Termin Klausur, 2. Termin m\U00FCndlich",sep=". "), anmerkung))


  klausuren = filter(ku, pruefung %in% c("k","km","m2"), pruefung_selbst != "s")

  klausuren = mutate(klausuren,
    anmerkung = ifelse(pruefung=="km",add.text(anmerkung,"evtl. m\U00FCndlich"), anmerkung)
  )


  selbst = filter(ku, pruefung %in% c("k","km","m2"), pruefung_selbst == "s")
  andere = filter(ku, !pruefung %in% c("k","km","m2"))


  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)

  doc = doc %>%
    body_add_par("Klausuren im Pruefungszeitraum", style = "heading 1")

  df = transmute(klausuren,Kurs=Kurs,Dozent=dozent,og=offen,`[Modul]  Pruefungen`=modulpruef,Dauer=pruefung_dauer,Plaetze=pruefung_teilnehmer, `*`=anmerkung)
  colnames(df)[3] = " "

  doc = doc %>%
    body_add_table(df, style="Plain Table 1")

  doc = doc %>%
    body_add_par("Kurse mit beiden Klausuren ausserhalb des Pruefungszeitraums", style = "heading 1")

  df = transmute(selbst,Kurs=Kurs,Dozent=dozent,og=offen,`[Modul]  Pruefungen`=modulpruef,Dauer=pruefung_dauer, Anmerkung=add.text(anmerkung, pruefung_teilnehmer, sep=";\n "))
  colnames(df)[3] = " "

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

  df = transmute(andere,Kurs=Kurs,Dozent=dozent,og=offen,`[Modul]  Pruefungen`=modulpruef,Pruefungsform=pf.fun(pruefung), Anmerkung=add.text(anmerkung, pruefung_teilnehmer, sep=";\n "))
  colnames(df)[3] = " "

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



