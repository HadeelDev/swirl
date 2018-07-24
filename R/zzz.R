.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty(s()%N%"اهلا صديقي  لقد حفظت بعض المتغيرات في مكان عملك",
                  s()%N%" swirl اقترح عليك قبل ان تبدا في ",
                  s()%N%"ان تمسح كل ماهو محفوظ حتي تجعل كل شيء حتى تجعل  كل شيء يجري على مايرام ", skip_after=TRUE),
      make_pretty(s()%N%" ls() حتى ترى جميع المتغيرات في مكان عملك ،اكتب",
                  s()%N%"حتى تمسح جميع ماهو موجود rm(list=ls()) ثم اكتب ", skip_after=TRUE),
      make_pretty(s()%N%"swirl() عندما تكون جاهزا للبدأ اكتب", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty(s()%N%"swirl() اهلا بك عندما تكون جاهزا للبدأ اكتب ",
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