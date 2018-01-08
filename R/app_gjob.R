gjobs.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("gjobs.ui")
  ui= tagList(
    h4("Erteilte Jobs:"),
    div(id="gjobTableDiv",
      dataTableOutput("gjobsTable")
    ),
    uiOutput("editgjobUI")
  )
  ui
}

update.gjobs.ui = function(...,app=getApp(), glob=app$glob) {
  restore.point("update.gjobs.ui")

  df = make.gjobs.datatable.df(app=app)
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

  #dt = dt %>% formatStyle("Status", target="row",fontWeight = styleEqual(names(glob$sets$jobstate), c("bold","bold","normal", "normal")))

  shinyEvents::setDataTable("gjobsTable", dt,server=TRUE)

  classEventHandler("editjgBtn",event = "click",function(data=NULL, ...) {
    restore.point("editjgBtn")
    gjobs = filter(app$gjobs, jobgroup==data$jobgroup)
    app$new.gjob = FALSE
    show.edit.gjob(gjob)
    cat("\neditgjobs clicked...")
  })
}


make.gjobs.datatable.df = function(..., app=getApp(), glob=app$glob) {
  restore.point("make.kurse.table.df")
  gjobs = app$gjobs
  if (NROW(gjobs)==0) return(NULL)

  jgs = gjobs %>% group_by(giverid, givername, jobgroup, jobtype, givetime, wishdate, frist, semester) %>%
    summarize(jobtitle=first(jobtitle),num.job=n(), num.finished = sum(jobstate=="a"), num.open = num.job-num.finished,  num.comment = sum(nchar(str.trim(comment))>0), receiverids = paste0(receiverid, collapse=","), receivernames = jobgroup.receiver.string(receiverid)) %>%
    ungroup()

  jgs$stateorder = ifelse(jgs$num.open > 0, 1, 2)
  jgs = arrange(jgs, stateorder, givetime)

  ids = jgs$jobgroup

  btns= paste0(
    simpleButtonVector(paste0("editjgBtn_",ids),icon=icon(name = "folder-open-o"), size="sm",extra.class = "editjgBtn",extra.head=paste0('data-jobgroup="',ids,'"'))
  )

  df = transmute(jgs,
    Aktion=btns, Offen=paste0(num.open, " / ", num.job), Titel=jobtitle,
    #Datum=format(givetime, "%d.%m.%y"),
    Empfaenger=receivernames,
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

jobgroup.receiver.string = function(receiverids, glob=getApp()$glob) {
  if (length(receiverids)<=7) return(paste0(paste0(to.label(receiverid,glob$sets$person), collapse=", ")))

  koord = filter(glob$person, koordinator)$personid
  plus.koord = setdiff(receiverids, koord)
  neg.koord = setdiff(koord, receiverids)
  if (length(plus.koord)+length(neg.koord)>5) {
    return(paste0(length(receiverids), " Empfaenger"))
  } else {
    str = "Koordinatoren"
    if (length(plus.koord)>0) {
      str = paste0(str, " plus ", paste0(to.label(plus.koord,glob$sets$person), collapse=", "))
    }
    if (length(neg.koord)>0) {
      str = paste0(str, " ohne ", paste0(to.label(neg.koord,glob$sets$person), collapse=", "))
    }
    return(str)
  }

}
