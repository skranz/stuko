monatsname = function(monat) {
  na = c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")
  na[as.integer(monat)]
}

lang_datum = function(date=Sys.Date()) {
  mon = format(date, "%m")
  jahr = format(date, "%Y")
  tag = format(date, "%d")

  paste0(tag,". ", monatsname(mon), " ", jahr)
}

semester_name = function(semester, kurz=TRUE) {
  sose = semester %% 10 == 0
  jahr = substring(semester, 1,2)

  if (kurz & sose) {
    sem = "SoSe"
  } else if (kurz) {
    sem = "WiSe"
    jahr = paste0(jahr, as.integer(jahr+1))
  } else if (!kurz & sose) {
    sem = "Sommersemester"
    jahr = paste0("20",jahr)
  } else {
    sem = "Wintersemester"
    jahr = paste0("20",jahr,"/",as.integer(jahr+1))
  }
  paste0(sem," ", jahr)

}
