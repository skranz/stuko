# Diagnosen zum Lehrangebot

examples.plan.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")

  semester = 185
  planung.schwerpunkt.report(semester, db)

}

planung.schwerpunkt.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/planung_schwerpunkt.docx"), sets=getApp()$glob$sets) {
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

  ku = do.call(rbind,li)

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

  tpl.file = system.file("report_tpl/plan_tpl.docx",package="stuko")
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


  print(doc, target = out.file)

  invisible(doc)
}



