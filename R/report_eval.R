# Diagnosen zum Lehrangebot

examples.seminar.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")

  semester = 185
  evaluierung.report(semester, db)

}

evaluierung.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/evaluierung.docx"), sets=getApp()$glob$sets) {
  restore.point("evaluierung.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")

  sd = get.sem.data(semester)

  .semester = semester
  ku = filter(sd$kurse,semester==.semester, kursform !="se",aktiv, extern==FALSE)

  ku = ku %>%
    mutate(do_eval = eval_was != "-" & !is.na(eval_was) & nchar(eval_was)>0) %>%
    mutate(is_stuko = has.substr(koid, "studiendekan_")) %>%
    arrange(is_stuko, koid, dozent)

  dupl = duplicated(select(ku, koid, kursname))
  ku = ku[!dupl,]

  ku = mutate(ku, ul = ifelse(eval_was == "vu",ifelse(ul=="","NN",ul),""))

  ku$eval_was = to.label(ku$eval_was, sets$eval_was)

  #kue = filter(ku, do_eval)
  #kun = filter(ku, !do_eval)
  kue = ku


  tpl.file = system.file("report_tpl/evaluierung_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)
  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  tab = select(kue, koordinator,kursname,vnum,  dozent, ul, eval_was, sprache)
  colnames(tab) = c("Koord.",  "Kurs", "LSF","Dozent", "\u00dcbung", "Evaluation", "Sprache")

  #ft = regulartable(tab)
  doc = doc %>%
    #body_add_par("Kurse", style = "heading 1") %>%
    #body_add_par(" ") %>%
    body_add_table(tab, style="Plain Table 1")


  print(doc, target = out.file)

  invisible(doc)
}



