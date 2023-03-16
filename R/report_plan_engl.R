# Diagnosen zum Lehrangebot

examples.plan.report.engl = function() {
  setwd("C:/libraries/stuko/")
  db = get.stukodb("C:/libraries/stuko/ulm/db")

  semester = 225
  planung.profile.report.engl(semester, db)

}

planung.profile.report.engl = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/plan_profiles_engl.docx"), sets=getApp()$glob$sets) {
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


  ku = filter(ku, sprache %in% c("en","de_en"))

  #test = filter(ku, has.substr(ku$dozent,"Marten"))

  ku$sp = strsplit(ku$schwerpunkt, ", ", fixed=TRUE)

  ku = tidyr::unnest(ku, sp)

  # Transformiere in Planungstabelle

  xfun = function(semester, findet_statt, ind) {
    row = which(semester == sems[ind])
    if (length(row) == 0) return("")
    if (any(findet_statt[row]=="u")) return("uncertain")
    return("X")
  }

  #ku$kursname = paste0(ku$kursname, ", ",ku$dozent)
  d = ku %>% group_by(kursname, sp) %>%
    summarize(lang=first(sprache),ECTS=first(ects),sem1 = xfun(semester, findet_statt, 1) ,sem2=xfun(semester,findet_statt,2),sem3=xfun(semester,findet_statt,3),sem4=xfun(semester,findet_statt,4))



  d = arrange(d, sp, desc(lang)) %>% filter(nchar(sp)>0, sp!="MA TPM")
  sem.labs = sapply(sems, knapp_semester_name)
  colnames(d)[5:(4+num.sem)] = sem.labs
  colnames(d)[1] = "Course"

  d$bama = substring(d$sp,1,2)
  d$sp = substring(d$sp,4)

  tpl.file = system.file("report_tpl/plan_en_tpl.docx",package="stuko")
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
        body_add_par("Profiles Bachelor", style = "heading 1")

    } else {
      doc = doc %>%
        body_add_par("Profiles Master", style = "heading 1")

    }

    for (csp in unique(dbm$sp)) {
      if (bm == "BA") {
        doc = doc %>%
          body_add_par(paste0("Profile: ", csp, " (",bm,")"), style = "heading 2")
      } else {
          doc = doc %>%
            body_add_par(paste0("Profile: ", csp, " (",bm,")"), style = "heading 2")
      }

      df = filter(dbm, sp %in% csp) %>%
        select(-sp,-bama) %>%
        add.report.ects.sum() %>%
        rename(Lang = lang) %>%
        mutate(Lang = case_when(
          Lang=="en"~ "EN",
          Lang=="" ~ "",
          TRUE ~ "EN or DE"))

      doc = doc %>%
         body_add_table(df, style="Plain Table 1")

    }
  }


  print(doc, target = out.file)

  invisible(doc)
}

add.report.ects.sum = function(df) {
  restore.point("add.lp.sum")

  df$ECTS = as.integer(df$ECTS)
  max.sum = sum(df$ECTS)
  min.sum = sum(df$ECTS[df$lang == "en"])

  sem.min.sums = sapply(4:7, function(sem.col) {
    rows = df$lang == "en" & df[[sem.col]] == "X"
    sum(df$ECTS[rows])
  })
  sem.max.sums = sapply(4:7, function(sem.col) {
    rows = df[[sem.col]] != ""
    sum(df$ECTS[rows])
  })

  min_max_str = function(min, max) {
    str = paste0(min, " to ", max)
    str[min == max] = min[min == max]
    str
  }

  empty.row = rep("", 7)
  sum.row = c("Sum of ECTS", "", min_max_str(min.sum,max.sum), min_max_str(sem.min.sums,sem.max.sums))
  res = do.call(rbind,list(as.data.frame(df), empty.row, sum.row))
  as_tibble(res)
}


