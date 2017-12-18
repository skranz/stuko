vertreter.ui = function(..., app=getApp(), glob=app$glob) {
  pe = filter(glob$person, personid %in% app$admin.for)
  choices = pe$personid
  names(choices) = pe$kurzname

  ui = tagList(
    selectInput("bossSelect","Vertreter fuer folgenden Koordinator definieren:", choices)
  )
  ui

}
