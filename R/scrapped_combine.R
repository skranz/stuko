scrapped.to.db.csv = function() {
  setwd("D:/libraries/stuko/")
  library(readr)
  library(dbmisc)

  mo = read_csv("modules.csv")
  dbmisc::schema.template(mo)
  vv = read_csv("vvz_wiwi.csv")
  dbmisc::schema.template(vv)

  vv = read_csv("vvz_wiwi.csv")

  zo = unique(mo$zuordnung)
  zo = lapply(zo, function(zo) strsplit(zo,",")) %>%
    unlist %>%  str.trim %>% unique

  sp = zo[str.starts.with(zo,"Schwerpunkt")]

  sg =  lapply(mo$studiengang, function(sg) strsplit(sg,",")) %>% unlist %>%  str.trim %>% unique
  sg


}

scrapped.combine = function() {
  setwd("D:/libraries/stuko/")
  library(readr)

  mo = read_csv("modules.csv")
  vv = read_csv("vvz_wiwi.csv")
  vv = group_by(vv, kurs) %>%
    mutate(
      sem = paste0(unique(semester), collapse=", "),
      dozenten=paste0(unique(dozenten), collapse=", ")
    ) %>%
    ungroup()

  vv = vv[!duplicated(vv$kurs),]
  unique(vv$kurs)

  vk = unique(vv$kurs)
  mk = unique(mo$Titel)

  df = make.amatch.table(vk,mk, names=c("kurs","modul")) %>%
    arrange(desc(dist_lcs))

  df = left_join(df, select(vv,kurs, sem, dozenten), by="kurs" )

  write_csv(df, "kurs_modul_match.csv")
}

make.amatch.table = function(v1, v2, methods=c("lcs","qgram", "osa"), maxDist=Inf,names=c("v1","v2"),...) {
  restore.point("make.amatch.table")

  n = length(methods)
  m.li = lapply(methods, function(method) {
    amatch(v1,v2, method=method, maxDist = maxDist)
  })
  v2.li = lapply(1:n, function(i) {
    v2[m.li[[i]] ]
  })

  d.li = lapply(1:n, function(i) {
    stringdist(v1, v2.li[[i]], method=methods[i], maxDist=maxDist)
  })
  names(v2.li)=paste0(names[2],"_",methods)
  names(d.li)=paste0("dist_", methods)

  df = cbind(data_frame(x=v1),as_data_frame(v2.li), as_data_frame(d.li) )
  colnames(df)[1] = names[1]

  df
}
