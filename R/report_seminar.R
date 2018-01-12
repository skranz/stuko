# Diagnosen zum Lehrangebot

examples.seminar.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")
  semdb.dir = "D:/libraries/stuko/semdb"
  semdb = dbConnect(RSQLite::SQLite(), file.path(semdb.dir, "semDB.sqlite"))

  semester = 180
  seminar.report(semester, db, semdb)

}

seminar.report = function(semester, db = get.stukodb(), semdb, out.dir = getwd(), out.file = paste0(out.dir,"/seminare.docx")) {
  restore.point("seminar.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")


  semester
  csemester = semdb_semester_name(semester)

  sems = dbGet(semdb, "seminars", list(semester=csemester, active=TRUE))

  sems$matched = 0

  .semester = semester
  sd = get.sem.data(semester)
  kuse = filter(sd$kurse,semester==.semester, kursform=="se",aktiv, extern==FALSE)


  koos = unique(kuse$koordinator)

  tpl.file = system.file("report_tpl/seminar_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)
  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)



  ko = koos[1]

  for (ko in koos) {
    names = substring(str.trim(strsplit(ko,",",fixed = TRUE)[[1]]),4)

    kku = filter(kuse, koordinator == ko)

    ma = rep(0, NROW(sems))
    for (na in names) {
      ma = ma + has.substr(tolower(sems$teacher), tolower(na))
    }

    ma = ma >= length(names)

    dim = filter(amatch.with.rel.dist(kku$kursname, sems$semname), dist <= 0.3)
    rows = sems$semname %in% dim$key
    ma[rows] = TRUE
    sems$matched = sems$matched + ma

    kse = sems[ma,] %>% arrange(semBAMA, semname)

    inds = string.approx.pair.match(kku$kursname, kse$semname, return.index = TRUE)
    kku = kku[inds[[1]],]
    kse = kse[inds[[2]],]

    kkus = paste0(seq_along(kku$kursname),". ", kku$kursname, ", ", kku$dozent)
    kses = paste0(seq_along(kse$semname),". ",kse$semname, ", ", kse$teacher)
    n = max(NROW(kkus), NROW(kses))
    nku = NROW(kkus); nse = NROW(kses)
    empty = rep("", n)
    kkus = c(c(kkus, empty)[1:n])
    kses = c(c(kses, empty)[1:n])
    doc = doc %>% body_add_par(paste0("Koordinator ", ko), style = "heading 1")

    tab = data_frame("Lehrangebot"=kkus, "Seminarvergabesoftware"=kses)
    doc = doc %>%  body_add_table(tab)

    kse$semBAMA

    if (FALSE) {
      doc = doc %>% body_add_par(paste0("Seminare Lehrangebot"), style = "heading 3")
      for (i in seq_len(NROW(kku))) {
        doc = doc %>% body_add_par()
      }
      doc = doc %>% body_add_par(paste0("Seminarsoftware"), style = "heading 3")
      for (i in seq_len(NROW(kse))) {
        doc = doc %>% body_add_par(paste0("  - ", kse$semname[[i]], " ", kse$teacher[[i]]))
      }

    }

  }

  kse = filter(sems, matched == 0)
  if (NROW(kse) > 0) {
    doc = doc %>% body_add_par(paste0("Nichtzugeordnete Seminare aus Vergabesoftware"), style = "heading 1")
    for (i in seq_len(NROW(kse))) {
      doc = doc %>% body_add_par(paste0("  - ", kse$semname[[i]], " ", kse$teacher[[i]]))
    }
  }


  print(doc, target = out.file)

  invisible(doc)
}




semdb_semester_name = function(semester) {
  sose = semester %% 10 == 0
  jahr = substring(semester, 1,2)

  if (sose) {
    return(paste0("SS", jahr))
  } else{
    jahr = paste0(jahr,as.integer(jahr)+1)
    return(paste0("WS",jahr))
  }
}


