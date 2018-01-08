examples.make.job = function() {
  restore.point.options(display.restore.point=TRUE)
  stuko.dir = "D:/libraries/stuko/ulm"
  setwd(stuko.dir)
  sem = 180
  app = stukoApp(stuko.dir, sem=sem, init.userid = "sebastian.kranz@uni-ulm.de",need.password = FALSE,need.userid = FALSE)

  db = get.stukodb()
  dbmisc::dbCreateSchemaTables(db)
  make.lehrangebot.job(semester=sem, giverid="studiendekan_wiwi", wishdate=as.Date("2017-12-31"))
}

make.lehrangebot.job = function(semester, wishdate=NA, db=get.stukodb(), receiver=filter(glob$person, koordinator), glob=app$glob, app=getApp(), giverid="", jobgroup=random.string(1,14), send.email = TRUE) {
  restore.point("make.lehrangebot.job")

  sem_name =semester_name(semester, kurz=FALSE)
  job = list(
    jobid = NA,
    jobgroup = jobgroup,
    jobtitle = paste0("Lehrangebot ", sem_name, " ueberpruefen"),
    jobemail=if(send.email) 0 else -1,
    givetime = Sys.time(),
    giverid = giverid,
    givername = to.label(giverid, glob$sets$person),
    receiverid = NA,
    jobtype = "la",
    jobstate = "o",
    wishdate = wishdate,
    descr = "",
    comment = "",
    semester = semester
  )

  dbWithTransaction(db, {
    for (row in seq_len(NROW(receiver))) {
      rec = receiver[row, ]
      job$receiverid = rec$personid
      job$jobid = paste0(job$jobgroup,"_",row)
      dbInsert(db, "job", job)
    }
  })

}

job.receiver.ui = function(job, app=getApp(), glob=app$glob) {
  restore.point("job.receiver.ui")

  default.job.receiver.ui(job)

}

default.job.receiver.ui = function(job,app=getApp(), glob=app$glob, ...) {
  df = transmute(job,Fuer=to.label(job$receiverid, glob$sets$person), Von=givername, Datum=format(givetime,"%d.%m.%y"), Wunschtermin=ifelse(is.na(wishdate),"-",format(wishdate,"%d.%m.%y")))
  ns = NS(job$jobid)
  ui = tagList(
    h4(paste0(job$jobtitle)),
    HTML(html.table(df)),
    p(job$descr),
    job_kurse(),
    selectInput("jobstateSelect","Status", glob$sets$jobstate[1:3], selected=job$jobstate),
    textAreaInput("jobcommentSelect","Kommentar",value = job$comment),
    simpleButton("saveJobBtn","Aenderungen Speichern", form.ids = c("jobstateSelect","jobcommentSelect"))
  )
  buttonHandler("saveJobBtn", function(...) {
    save.job.click(...,job=job)
  })

  ui
}

save.job.click = function(formValues,job, ..., app=getApp(), glob=app$glob) {
  restore.point("save.job.click")
  jobstate = formValues$jobstateSelect
  comment = formValues$jobcommentSelect

  if (jobstate == job$jobstate & comment == job$comment) return()

  log = paste0("Job-Update durch Empfaenger ", job$receiverid, " (id=", job$jobid,")\n", job$jobtitle,":\n")
  if (jobstate != job$jobstate) {
    labs = to.label(c(job$jobstate,jobstate), glob$sets$jobstate)
    log = paste0(log,"\n- Status von ", labs[1], " zu ", labs[2])
  }
  if (comment != job$comment) {
    labs = to.label(c(job$jobstate,jobstate), glob$sets$jobstate)
    log = paste0(log,"\n- Alter Kommentar:\n", job$comment, "\n\n- Neuer Kommentar:\n", comment)
  }



  dbUpdate(glob$db, "job", nlist(jobstate, comment),where = job["jobid"])
  row = which(glob$jobs$jobid == job$jobid)
  glob$jobs$jobstate[row] = jobstate
  glob$jobs$comment[row]  = comment
  write.stuko.log(log,"job_receiver")
  update.app.jobs()


}

update.app.jobs = function(app=getApp(), glob=app$glob) {
  app$rjobs = filter(glob$jobs, receiverid %in% app$all.bossid)
  app$gjobs = filter(glob$jobs, giverid %in% app$all.bossid)
  update.jobs.ui(app = app)
}

frist.days = function(dates, today=Sys.Date()) {
  restore.point("frist.dates")
  if (length(dates)==0) return(integer(0))
  #dates = as.Date(c("2018-01-05","2017-12-12",NA))
  #as.integer(dates)-as.integer(today)
  as.integer(difftime(dates, today, units="days"))
}

job_kurse = function(job=app$job, is.email=isTRUE(app$is.email), ..., app=getApp(), glob=app$glob) {
  restore.point("job_kurse")
  sem = job$semester
  if (is.null(sem)) return(HTML("Kein Semester spezifiziert."))

  sd = get.sem.data(sem)
  kupe = filter(sd$kupe, personid == job$receiverid, rolle %in% c("dk","do"))
  kurse = semi_join(sd$kurse, kupe, by="kursid") %>% arrange(kursname)

  str = paste0(kurse$kursname, " (", kurse$bama, ", ", kurse$dozent,")")
  html = paste0("<h5>Von Ihnen koordinierte Kurse fuer ", semester_name(sem),":</h5><ul>", paste0("<li>", str,"</li>", collapse="\n"),"</ul>")
  HTML(html)

}
