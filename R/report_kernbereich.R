examples.kernbereich.report = function() {
  setwd("C:/libraries/stuko")
  db = get.stukodb("C:/libraries/stuko/ulm/db")
  semester = 225
  tpl.dir = system.file("report_tpl",package="stuko")
  kernbereich.report(semester, db)

}

kernbereich.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/kernbereiche_und_aqmt.docx"), sets=getApp()$glob$sets, just.english=FALSE) {
  restore.point("kernbereich.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")

  num.sem = 4
  sems = seq(semester, semester+(num.sem-1)*5,by=5)
  li = lapply(sems, function(sem) {
    sd = get.sem.data(sem)
    ku = sd$kurse
    ku = filter(ku,kursform %in% c("v","vu","kol"),aktiv, extern==FALSE | TRUE)
    ku
  })

  org.ku =  do.call(rbind,li)

  tpl.file = system.file("report_tpl/kernbereich_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  add.kernbereiche.to.report(org.ku, doc, sems, num.sem)
  add.aqmt.to.report(org.ku, doc, sems, num.sem)

  print(doc, target = out.file)

  invisible(doc)
}





# Zeige 4 Semesterplanung für Kernbereiche

# Zeige 4 Jahresplanung für Mathe / Info WP
add.kernbereiche.to.report = function(org.ku, doc, sems, num.sem) {
  restore.point("add.kernbereiche.to.report")
  #stop()
  ku=org.ku
  ku$sp = strsplit(ku$zuordnung, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp) %>%
    filter(startsWith(sp,"Kern"))

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("unsicher")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(
      Modulcode=first(code),
      LP=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))

  d = arrange(d, sp) %>% filter(nchar(sp)>0)
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Kurs"

  #d$bama = substring(d$sp,1,2)
  #d$sp = substring(d$sp,4)
  kernbereiche = unique(d$sp)

  for (sp in kernbereiche) {

    rows = d$sp %in% sp
    dbm = d[rows,]
    doc = doc %>% officer::body_add_break()

    lab = gsub("Kern", "Module im Kernbereich", sp)
    doc = doc %>%
      body_add_par(lab, style = "heading 1")
    df = dbm %>%
        ungroup() %>%
        select(-sp)
    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }
  return(invisible())
}


# Zeige 4 Jahresplanung für Mathe / Info WP
add.aqmt.to.report = function(org.ku, doc, sems, num.sem) {
  restore.point("add.kernbereiche.to.report")
  #stop()
  ku=org.ku
  ku$sp = strsplit(ku$zuordnung, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp) %>%
    filter(sp == "AQMT")

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("unsicher")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(
      Modulcode=first(code),
      LP=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))

  d = arrange(d, sp) %>% filter(nchar(sp)>0)
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Kurs"

  #d$bama = substring(d$sp,1,2)
  #d$sp = substring(d$sp,4)
  kernbereiche = unique(d$sp)

  for (sp in kernbereiche) {

    rows = d$sp %in% sp
    dbm = d[rows,]
    doc = doc %>% officer::body_add_break()

    doc = doc %>%
      body_add_par("Kurse im Advanced Quantitative Methods Track (AQMT)", style = "heading 1")
    df = dbm %>%
        ungroup() %>%
        select(-sp)
    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }
  return(invisible())
}
