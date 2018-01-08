jobs.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("jobs.ui")
  ui= tagList(
    h4("Erhaltene Jobs:"),
    div(id="jobsTableDiv",
      dataTableOutput("jobsTable")
    ),
    uiOutput("editJobUI")
  )
  ui
}

update.jobs.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.jobs.ui")

  df = make.jobs.datatable.df(sd)
  if (is.null(df)) {
    shinyEvents::setDataTable("jobsTable", NULL)
    return()
  }

  dt = datatable(df,selection = 'none',escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=FALSE),
    class="display compact",
#    style="bootstrap",
    autoHideNavigation = TRUE, extensions = c('Select'),options = list(
    dom = 't<"bottom"irp><"clear">',
    select = FALSE,
    columnDefs = list(
      list(width="4em", targets=c(0)),
      list(width="8em", targets=c(1)),
      list(width="11em", targets=c(3,6)),
      list(width="6em", targets=c(4,5))
    ),
    autoWidth=TRUE,
    fixedColumns = list(leftColumns = 3)))

  dt = dt %>% formatStyle("Status", target="row",fontWeight = styleEqual(names(glob$sets$jobstate), c("bold","bold","normal", "normal")))

  shinyEvents::setDataTable("jobsTable", dt,server=TRUE)

  classEventHandler("editJobBtn",event = "click",function(data=NULL, ...) {
    restore.point("editJobBtn")
    job = filter(app$rjobs, jobid==data$jobid)
    app$new.job = FALSE
    show.edit.job(job)
    cat("\neditJobs clicked...")
  })
}


make.jobs.datatable.df = function(..., app=getApp(), glob=app$glob) {
  restore.point("make.kurse.table.df")
  jobs = app$rjobs
  if (NROW(jobs)==0) return(NULL)

  jobs$stateorder = ifelse(jobs$jobstate %in% c("o","w","b"), 1, 2)
  jobs = arrange(jobs, stateorder, givetime)

  ids = jobs$jobid

  btns= paste0(
    simpleButtonVector(paste0("editJobBtn_",ids),icon=icon(name = "pencil"), size="sm",extra.class = "editJobBtn",extra.head=paste0('data-jobid="',ids,'"'))
  )

  df = transmute(jobs,
    Aktion=btns, Status=to.label(jobstate, glob$sets$jobstate), Titel=jobtitle, Empfaenger=to.label(receiverid,glob$sets$person) ,
    #Datum=format(givetime, "%d.%m.%y"),
    Frist = ifelse(is.na(frist) | stateorder==2,"-", paste0(frist, " Tage")),
    Datum=as.Date(givetime),
    Sender=givername
  )

  df
}


show.edit.job = function(job,..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.job")
  app$job = job

  ui = job.receiver.ui(job)
  setUI("editJobUI", ui)
}
