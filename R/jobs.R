examples.make.job = function() {
  restore.point.options(display.restore.point=TRUE)
  stuko.dir = "D:/libraries/stuko/ulm"
  setwd(stuko.dir)
  app = stukoApp(stuko.dir, sem=180, init.userid = "sebastian.kranz@uni-ulm.de",need.password = FALSE,need.userid = FALSE)

  db = get.stukodb()
  dbmisc::dbCreateSchemaTables(db)
  make.lehrangebot.job(semester=app$sem)
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
