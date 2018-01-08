gjobs.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("gjobs.ui")
  ui= tagList(
    h4("Erteilte Jobs:"),
    div(id="gjobTableDiv",
      dataTableOutput("gjobTable")
    ),
    uiOutput("editgjobUI")
  )
  ui
}

update.gjobs.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.gjobs.ui")

  df = make.gjobs.datatable.df(sd)
  if (is.null(df)) {
    shinyEvents::setDataTable("gjobTable", NULL)
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

  shinyEvents::setDataTable("gjobsTable", dt,server=TRUE)

  classEventHandler("editgjobBtn",event = "click",function(data=NULL, ...) {
    restore.point("editgjobBtn")
    gjob = filter(app$rgjobs, jobid==data$jobid)
    app$new.gjob = FALSE
    show.edit.gjob(gjob)
    cat("\neditgjobs clicked...")
  })
}


make.gjobs.datatable.df = function(..., app=getApp(), glob=app$glob) {
  restore.point("make.kurse.table.df")
  gjobs = app$gjobbs
  if (NROW(gjobs)==0) return(NULL)

  gjobs$stateorder = ifelse(gjobs$jobstate %in% c("o","w","b"), 1, 2)
  gjobs = arrange(gjobs, stateorder, givetime)

  ids = gjobs$jobid

  btns= paste0(
    simpleButtonVector(paste0("editgjobBtn_",ids),icon=icon(name = "pencil"), size="sm",extra.class = "editgjobBtn",extra.head=paste0('data-jobid="',ids,'"'))
  )

  df = transmute(gjobs,
    Aktion=btns, Status=to.label(jobstate, glob$sets$jobstate), Titel=jobtitle, Empfaenger=to.label(receiverid,glob$sets$person) ,
    #Datum=format(givetime, "%d.%m.%y"),
    Frist = ifelse(is.na(frist) | stateorder==2,"-", paste0(frist, " Tage")),
    Datum=as.Date(givetime),
    Sender=givername
  )

  df
}


show.edit.gjob = function(gjob,..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.gjob")
  app$gjob = gjob

  ui = gjob.receiver.ui(gjob)
  setUI("editgjobUI", ui)
}
