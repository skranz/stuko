examples.kernbereich.report = function() {
  setwd("C:/libraries/stuko")
  db = get.stukodb("C:/libraries/stuko/ulm/db")
  semester = 225
  tpl.dir = system.file("report_tpl",package="stuko")
  kernbereich.report.engl(semester, db)

}

kernbereich.report.engl = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/plan_english_courses.docx"), sets=getApp()$glob$sets) {
  restore.point("kernbereich.report.engl")

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

  org.ku = filter(org.ku, sprache %in% c("en","de_en"))


  tpl.file = system.file("report_tpl/kernbereich_en_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)

  doc = add.all.courses.to.report.engl(org.ku, doc, sems, num.sem)
  doc = add.kernbereiche.to.report.engl(org.ku, doc, sems, num.sem)
  doc = add.aqmt.to.report.engl(org.ku, doc, sems, num.sem)

  print(doc, target = out.file)

  invisible(doc)
}




# Zeige 4 Jahresplanung für all englischen MA Kurse
add.all.courses.to.report.engl = function(org.ku, doc, sems, num.sem) {
  restore.point("add.all.courses.to.report.engl")
  #stop()
  ku=org.ku

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("uncertain")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname) %>%
    transmute(
      bama = bama,
      lang = sprache,
      ECTS=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4)) %>%
    filter(!has.substr(tolower(kursname), "business english"))

  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Course"


  unique(d$bama)

  for (bama in c("BA","BA MA","MA")) {
    dbm = d[d$bama == bama,]
    doc = doc %>% officer::body_add_break()

    if (bama == "BA") {
      prefix = "Bachelor courses"
    } else if (bama == "MA") {
      prefix = "Master courses"
    } else {
      prefix = "Courses open for both BA and MA"
    }
    doc = doc %>%
      body_add_par(paste0(prefix," that are (possibly) taught in English"), style = "heading 1")
    df = dbm %>%
        ungroup() %>%
        filter(!duplicated(Course)) %>%
        arrange(desc(lang)) %>%
        select(-bama) %>%
        add.report.ects.sum() %>%
        rename(Lang = lang) %>%
        mutate(Lang = case_when(
          Lang=="en"~ "EN",
          Lang=="" ~ "",
          TRUE ~ "EN or DE"))

    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }

  doc

}



# Zeige 4 Semesterplanung für Kernbereiche

add.kernbereiche.to.report.engl = function(org.ku, doc, sems, num.sem) {
  restore.point("add.kernbereiche.to.report.engl")
  #stop()
  ku=org.ku
  ku$sp = strsplit(ku$zuordnung, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp) %>%
    filter(startsWith(sp,"Kern"))

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("uncertain")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(
      lang = first(sprache),
      ECTS=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))

  d = arrange(d, sp) %>% filter(nchar(sp)>0)
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Course"

  #d$bama = substring(d$sp,1,2)
  #d$sp = substring(d$sp,4)
  kernbereiche = unique(d$sp)

  for (sp in kernbereiche) {

    rows = d$sp %in% sp
    dbm = d[rows,]
    doc = doc %>% officer::body_add_break()

    lab = gsub("Kern", "Modules in Core Area", sp)
    doc = doc %>%
      body_add_par(lab, style = "heading 1")
    df = dbm %>%
        ungroup() %>%
        select(-sp) %>%
        arrange(desc(lang)) %>%
        add.report.ects.sum() %>%
        rename(Lang = lang) %>%
        mutate(Lang = case_when(
          Lang=="en"~ "EN",
          Lang=="" ~ "",
          TRUE ~ "EN or DE"))

    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }
  doc
}


# Zeige 4 Jahresplanung für Mathe / Info WP
add.aqmt.to.report.engl = function(org.ku, doc, sems, num.sem) {
  restore.point("add.kernbereiche.to.report.engl")
  #stop()
  ku=org.ku
  ku$sp = strsplit(ku$zuordnung, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp) %>%
    filter(sp == "AQMT")

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("uncertain")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(
      lang=first(sprache),
      ECTS=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))

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
      body_add_par("Courses in the Advanced Quantitative Methods Track (AQMT)", style = "heading 1")
    df = dbm %>%
        ungroup() %>%
        select(-sp) %>%
        arrange(desc(lang)) %>%
        add.report.ects.sum() %>%
        rename(Lang = lang) %>%
        mutate(Lang = case_when(
          Lang=="en"~ "EN",
          Lang=="" ~ "",
          TRUE ~ "EN or DE"))

    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }
  doc
}
