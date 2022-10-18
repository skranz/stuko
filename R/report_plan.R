# Diagnosen zum Lehrangebot

examples.plan.report = function() {
  setwd("C:/libraries/stuko/")
  db = get.stukodb("C:/libraries/stuko/ulm/db")

  semester = 225
  planung.schwerpunkt.report(semester, db, just.english=!TRUE)

}

get.modulhandbuch.url = function(titel) {
  url = paste0("https://campusonline.uni-ulm.de/qislsf/rds?state=change&nextdir=change&type=3&moduleParameter=pordpos&next=TableSelect.vm&subdir=pord&searchOrder=y&P.sort=&pord.stg=184&pord.pltxt1=",htmltools::htmlEscape(titel),"&orderby=Apvers&orderdesc=DESC")
  #url = htmltools::htmlEscape(url)
  url
}

planung.schwerpunkt.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/planung_schwerpunkt.docx"), sets=getApp()$glob$sets, just.english=FALSE) {
  restore.point("planung.schwerpunkt.report")

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

  ku = org.ku =  do.call(rbind,li)

  if (just.english) {
    ku = filter(ku, sprache %in% c("en","de_en"))
  }

  #test = filter(ku, has.substr(ku$dozent,"Marten"))

  ku$sp = strsplit(ku$schwerpunkt, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp)

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("unsicher")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(LP=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))



  d = arrange(d, sp) %>% filter(nchar(sp)>0)
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[4:(3+num.sem)] = sem.labs
  colnames(d)[1] = "Kurs"

  d$bama = substring(d$sp,1,2)
  d$sp = substring(d$sp,4)

  if (just.english) {
    tpl.file = system.file("report_tpl/plan_en_tpl.docx",package="stuko")
  } else {
    tpl.file = system.file("report_tpl/plan_tpl.docx",package="stuko")
  }
  doc = read_docx(tpl.file)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)

  bm = "BA"

  for (bm in c("BA","MA")) {
    rows = d$bama %in% bm
    dbm = d[rows,]

    if (bm == "BA") {
      doc = doc %>%
        body_add_par("Profile / Schwerpunkte Bachelor", style = "heading 1")

    } else {
      doc = doc %>%
        body_add_par("Schwerpunkte Master", style = "heading 1")

    }

    for (csp in unique(dbm$sp)) {
      if (bm == "BA") {
        doc = doc %>%
          body_add_par(paste0("Profil / Schwerpunkt: ", csp, " (",bm,")"), style = "heading 2")
      } else {
          doc = doc %>%
            body_add_par(paste0("Schwerpunkt: ", csp, " (",bm,")"), style = "heading 2")
      }

      df = filter(dbm, sp %in% csp) %>%
        select(-sp,-bama)
      doc = doc %>%
         body_add_table(df, style="Plain Table 1")
    }
  }


  # Fuege Mathe / Info WP hinzu
  if (!just.english) {
    add.mathe.wp.to.report(li, doc, sems, num.sem)
  }

  #add.kernbereiche.to.report(org.ku, doc, sems, num.sem)

  print(doc, target = out.file)

  invisible(doc)
}



# Zeige 4 Jahresplanung für Mathe / Info WP
add.mathe.wp.to.report = function(li, doc, sems, num.sem) {
  ku = do.call(rbind,li)
  ku$sp = strsplit(ku$zuordnung, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp) %>%
    filter(sp %in% c("WP Mathe/Info"))

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("unsicher")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp, bama) %>%
    summarize(LP=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))



  d = arrange(d, sp) %>% filter(nchar(sp)>0)
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Kurs"

  #d$bama = substring(d$sp,1,2)
  #d$sp = substring(d$sp,4)
  bm = "BA"
  for (bm in c("BA","MA")) {

    rows = d$bama %in% bm
    dbm = d[rows,]
    doc = doc %>% officer::body_add_break()
    if (bm == "BA") {
      doc = doc %>%
        body_add_par("Wahlpficht Mathe/Info (BA alte PO)", style = "heading 1")

    } else {
      doc = doc %>%
        body_add_par("Wahlpficht Mathe/Info (MA)", style = "heading 1")

    }
    df = dbm %>%
        ungroup() %>%
        select(-sp,-bama)
    doc = doc %>%
        body_add_table(df, style="Plain Table 1")
  }



}



# Zeige 4 Semesterplanung für Kernbereiche

# Zeige 4 Jahresplanung für Mathe / Info WP
old.add.kernbereiche.to.report = function(org.ku, doc, sems, num.sem) {
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
