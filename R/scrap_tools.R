amatch.with.rel.dist = function(words, keys,  method="qgram") {
  restore.point("amatch.with.rel.dist")
  df = unify.words.table(words, keys, method)
  nc = pmax(nchar(df$word), nchar(df$key))
  df$dist = df$dist / nc
  df

}

unify.words.table = function(words, keys, method="qgram", file=NULL) {
  restore.point("unify.words.table")

  library(stringdist)
  ind = amatch(words, keys, maxDist = Inf, method=method)
  key = keys[ind]
  df = data_frame(word = words,key=key, dist=stringdist(words,key,maxDist=Inf, method=method))

  if (!is.null(file)) {
    readr::write_csv(df, file)
  }
  df
}

match.to.table = function(x, tab, val.col=1, key.col=2) {
  val.ind = match(x, tab[[val.col]])
  tab[[key.col]][val.ind]
}


differing.df.group.rows = function(df, group_var="code", ignore.cols=c()) {
  restore.point("differeing.df.groups")
  # Find Module whose Modulbeschreibung differs
  # for different subjects
  differ = function(x) n_distinct(x) > 1

  ddf = df %>%
    group_by_(group_var) %>%
    summarise_all(differ)

  fields = unique(c(group_var, ignore.cols))

  ddf = ddf[,setdiff(colnames(ddf,fields))]
  differ.mat = as.matrix(ddf)
  differ = rowSums(differ.mat>0)
  which(differ)

}


differing.cols = function(df, collapse=NULL) {
  differs = sapply(df, function(vals) {
    dplyr::n_distinct(vals) != length(vals)
  })
  if (is.null(collapse)) {
    names(df)[differs]
  } else {
    paste0(names(df)[differs], collapse=",")
  }
}
