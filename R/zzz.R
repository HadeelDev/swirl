.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty(s()%N%"اهلا بك، لقد حفظت بعض التغييرات في مكان عملك ",
                  s()%N%"لجعل الامور تسير على مايرام ،اقترح عليك ان تمسحها",
                  s()%N%"swirlقبل ان تبدا ", skip_after=TRUE),
      make_pretty(s()%N%"لتشاهد قائمه من المتغيرات في مكان عملك ls() اكتب",
                  s()%N%"لتسمح كل ماهو موجود على مكان عملك rm(list=ls()) ثم اكتب", skip_after=TRUE),
      make_pretty(s()%N%"عندما تكون جاهزا للبدا swirl() اكتب", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty(s()%N%"عندما تكون جاهزا للبدا swirl() اهلا اكتب",
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  mes
}