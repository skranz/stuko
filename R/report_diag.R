# Diagnosen zum Lehrangebot

examples.lehrangebot.diagnostik.report = function() {
  setwd("D:/libraries/stuko")
  db = get.stukodb("D:/libraries/stuko/db")
  semester = 190
  lehrangebot.diagnostik.report(semester, db)

}

lehrangebot.diagnostik.report = function(semester, db = get.stukodb(), tpl.dir = getwd(), out.dir = getwd(), out.file = paste0(out.dir,"/lehrangebot_diag_", semester,".docx")) {
  restore.point("lehrangebot.diagnostik.report")

  sem = semester
  semp = semester -10
  sem_lab = semester_name(semester)
  semp_lab = semester_name(semp)
  date_label = format(Sys.Date(),"%d.%m.%Y")

  kup = load.kurse.for.lehrangebot(semester=semp, db=db)
  kup = mutate(kup, Kurs=kursname, Dozent=dozent, SWS=sws_kurs+sws_uebung, is_sem=kursform=="se")
  kup = filter(kup, aktiv)

  ku = load.kurse.for.lehrangebot(semester=semester, db=db)
  ku = mutate(ku, Kurs=kursname, Dozent=dozent, SWS=sws_kurs+sws_uebung, is_sem=kursform=="se")

  na_ku = filter(ku, !aktiv)
  ku = filter(ku, aktiv)


  ku = ku %>% arrange(kursname)
  kup = kup %>% arrange(kursname)

  doc = read_docx(file.path(tpl.dir,"lehrang_diag_tpl.docx"))
  doc = doc %>%
    body_replace_at("sem_label",semester_name(semester)) %>%
    body_replace_at("date_label", lang_datum())


  doc = doc %>% body_add_par("Kurse ohne Module", style = "heading 1")
  dat = filter(ku,!has.modul)
  doc = add.lad.comments(doc, dat=dat, "Sie sollten dies auf jeden Fall korrigieren.")
  doc = add.lad.table(doc,dat)


  doc = doc %>% body_add_par("Kurse ohne Dozenten", style = "heading 1")
  dat = filter(ku,is.na(dozent) | is.true(dozent == "") | is.true(dozent=="NN")) %>% rename("Uebungsleiter"="ul")

  doc = add.lad.comments(doc,dat=dat,
    if (any(dat$ul != "")) "Ggf. wurden in einigen Kursen die Dozenten fehlerhaft als Uebungsleiter klassifiziert..."
  )

  doc = add.lad.table(doc,dat, cols=c("Kurs","Uebungsleiter"))


  doc = doc %>% body_add_par("Kurse mit Uebungen mit 0 SWS Uebung", style = "heading 1")
  dat = filter(ku,kursform=="vu", sws_uebung==0)
  doc = add.lad.comments(doc, dat=dat, "Hier wurde noch keine SWS Aufteilung zwischen Kurs und Uebung angegeben")

  doc = add.lad.table(doc,dat, cols=c("Kurs","Dozent","SWS"))




  doc = doc %>% body_add_par("Kurse mit Kommentaren fuer dieses Semester", style = "heading 1")
  ku$kommentar[is.na(ku$kommentar)] = ""
  dat = filter(ku, nchar(str.trim(kommentar))>0)
  doc = add.lad.comments(doc, dat=dat, "")

  doc = add.lad.table(doc,dat, cols=c("Kurs","Dozent","kommentar"))


  # Statistiken
  staku = c(Kurse=NROW(ku),"BA-Pflicht"=sum(ku$ba_pflicht),"BA-WP"=sum(ku$ba_wp), "MA WiWi"=sum(ku$ma_wp),"NUF"=sum(ku$nuf_wp | ku$nuf_pflicht),"Nicht Zugeordnet"=sum(!ku$has.modul))
  stakup = c(Kurse=NROW(kup),"BA-Pflicht"=sum(kup$ba_pflicht),"BA-WP"=sum(kup$ba_wp), "MA WiWi"=sum(kup$ma_wp),"NUF"=sum(kup$nuf_wp | kup$nuf_pflicht),"Nicht Zugeordnet"=sum(!kup$has.modul))

  df = as.data.frame(cbind(names(staku),staku, stakup, staku-stakup))
  colnames(df) = c("Kategorie",semester_name(semester,TRUE), semester_name(semp,TRUE),"Veraenderung")
  rownames(df) = NULL

  doc = doc %>% body_add_par("Anzahl der Kurse", style = "heading 1") %>%
    body_add_table(df)

  # Kursveraenderungen
  doc = add.lad.diff.table(doc, label="Bachelor WiWi Pflicht", filter="ba_pflicht", ku, kup, sem, semp)

  doc = add.lad.diff.table(doc, label="Bachelor WiWi Wahlpflicht", filter="ba_wp", ku, kup, sem, semp, cols=c("Kurs", "Dozent","sp"))

  doc = add.lad.diff.table(doc, label="Master WiWi", filter="ma_wp", ku, kup, sem, semp,cols=c("Kurs", "Dozent","sp"))

  doc = add.lad.diff.table(doc, label="NUF Pflicht", filter="nuf_pflicht", ku, kup, sem, semp)

  doc = add.lad.diff.table(doc, label="NUF Wahlpflicht", filter="nuf_pflicht", ku, kup, sem, semp)

  doc = add.lad.diff.table(doc, label="Seminare", filter="is_sem", ku, kup, sem, semp)


  doc = doc %>% body_add_par("Nichtaktivierte Kurse", style = "heading 1")
  dat = na_ku
  doc = add.lad.comments(doc, dat=dat, "Beachten Sie, dass die vorherigen Statistiken nur aktivierte Kurse beruecksichtigt haben.")

  doc = add.lad.table(doc,dat, cols=c("Kurs","Dozent"))


  print(doc, target = out.file)

  invisible(doc)
}

add.lad.diff.table = function(doc, label, filter, ku, kup, sem, semp, cols=c("Kurs","Dozent")) {
  restore.point("add.lad.diff.table")
  doc = doc %>% body_add_par(paste0("Veraenderungen ", label," von ", semester_name(semp), " nach ", semester_name(sem)) , style = "heading 1")

  df1 = s_filter(ku, filter)
  df2 = s_filter(kup, filter)
  if (NROW(df1)>0)
    df1$Kurs = paste0(df1$Kurs, " (Turnus: ", df1$turnus,")")
  if (NROW(df2)>0)
    df2$Kurs = paste0(df2$Kurs, " (Turnus: ", df2$turnus,")")

  td = tables.diff(df1,df2, by="kursid")

  doc = doc %>% body_add_par(paste0(NROW(td$same), " gleiche Kurse."), style = "heading 3") %>%
    body_add_par(paste0(NROW(td$added), " neue Kurse:"),style = "heading 3") %>%
    body_add_table(td$added[,cols], header=FALSE) %>%
    body_add_par(paste0(NROW(td$removed), " weggefallene Kurse:"),style = "heading 3") %>%
    body_add_table(td$removed[,cols],header = FALSE)

  doc

}

add.lad.comments = function(doc, ...,dat=NULL) {
  if (!is.null(dat) & NROW(dat)==0) return(doc)
  args = list(...)
  is.null = sapply(args, is.null)
  args = args[!is.null]
  if (length(args)>0) {
    comments = paste0("  - ",unlist(args))
    for (comment in comments) {
      doc = body_add_par(doc, comment)
    }
  }
  doc = body_add_par(doc, "")

}

add.lad.table = function(doc, df, cols=c("Kurs","Dozent"),header=TRUE,...) {

  if (NROW(df)==0) {
    return(body_add_par(doc,"--- Keine Eintraege ---"))
  }
  df = df[,cols]


  doc %>% body_add_table(df, header=header)
  return(doc)

}

