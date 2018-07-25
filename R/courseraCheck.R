#' @importFrom stringr str_detect
courseraCheck <- function(e){
  modtype <- attr(e$les, "type")
  lesson_name <- gsub(" ", "_", attr(e$les, "lesson_name"))
  if(is.null(modtype) || modtype != "Coursera")return()
  
  # allow use of Coursera partner sites (school.coursera.org)
  partner <- attr(e$les, "partner")
  partner <- ifelse(is.null(partner), "class", partner)
  baseurl <- paste0("http://", partner, ".coursera.org/")

  tt <- c(rep(letters, 3), seq(100))
  swirl_out(s()%N%"المرتبطه بهذا الدرس؟ Coursera هل انت حاليا مسجل في دوره")
  yn <- select.list(c("No","Yes"), graphics=FALSE)
  if(yn=="No")return()
  ss <- lapply(1:2, function(i) {
    paste0(sample(tt, sample(seq(400), 1), replace=TRUE), collapse="")
  })
  swirl_out(s()%N%"انك قد اكملت الدرس؟ Coursera هل تريد مني ان ابلغ كورس",
            "اذا نعم، سوف اخذ منك مزيدا من المعلومات ")
  choice <- select.list(c("No","Yes","Maybe later"), graphics=FALSE)
  if(choice=="No") return()
  # Begin submission loop
  ok <- FALSE
  while(!ok) {
    # Get submission credentials
    r <- getCreds(e)
    email <- r["email"]
    passwd <- r["passwd"]
    course_name <- r["courseid"]
    output <- paste0(ss[[1]], substr(e$coursera, 1, 16), ss[[2]],
                     collapse="")
    # Name output file
    output_filename <- paste0(course_name,"_",lesson_name,".txt")
    # Write output to text file
    writeLines(output, output_filename)
    # If going straight to manual submission, then exit loop.
    if(choice=="Maybe later") ok <- TRUE
    # If doing automatic submission, then give it a try.
    if(choice=="Yes"){
      swirl_out(s()%N%"انك قد اكملت الدرس الان Coursera سوف احاول الان ان ابلغ كورس")
      challenge.url <- paste(baseurl, course_name,
                             "assignment/challenge", sep = "/")
      submit.url <- paste(baseurl, course_name,
                          "assignment/submit", sep = "/")
      ch <- try(getChallenge(email, challenge.url), silent=TRUE)
      # Check if url is valid, i.e. challenge received
      ch_ok <- is.list(ch) && exists("ch.key", ch) && !is.na(ch$ch.key)
      if(!is(ch, "try-error") && ch_ok) {
        ch.resp <- challengeResponse(passwd, ch$ch.key)
        # If submit.url is invalid, submitSolution should return a try-error.
        # However, that is not the only way it can fail; see below.
        results <- submitSolution(email, submit.url, ch.resp, 
                                  sid=lesson_name, 
                                  output=output,
                                  signature=ch$state)
        # If incorrect, empty string will be returned
        if(!length(results)) {
          swirl_out(s()%N%"لقد تجاهلت الكثير من الاسئله التي تحتاج الي اكمالها",
                    s()%N%"اذا كنت ترغب بالحصول علي مزيد من النقاط فهذا هو الدرس مره اخرى",
                    s()%N%"لا تتجاهل اكثر من سؤال في المره القادمه")
          return()
        }
        if(!is(results, "try-error")){
          # TODO: It would be best to detect success here, rather than
          # failure, but as of Feb 23 2014, submit.url may not throw
          # an error indicating failure but instead return an HTML
          # notification beginning with the word, "Exception".
          # Here we detect failure by the presence of this word.
          # Server-side behavior could easily change and could easily
          # be course dependent, so some standard handshake will have
          # to be set up eventually.
          swirl_out(results)
          if(!str_detect(results, "[Ee]expected")){
            swirl_out(paste0(s()%N%"بانك قد اكملت الدرس Coursera لقد اخبرت ",
                             course_name, ", ", lesson_name,"."))
            # Remove manual submission text file
            unlink(output_filename)
            # Exit loop since submission successful
            return()
          }
          swirl_out(s()%N%"انا اسف ، يوجد خطأ في ادخال البيانات")
          # Exit loop if user doesn't want to retry auto submission
          ok <- !retry()
        } else {
          swirl_out(s()%N%"انا اسف ، يوجد خطأ في ادخال البيانات")
          # Exit loop if user doesn't want to retry auto submission
          ok <- !retry()
        }
      } else {
        swirl_out(s()%N%"انا اسف ، يوجد خطأ في  انشاء الاتصالات")
        # Exit loop if user doesn't want to retry auto submission
        ok <- !retry()
      }
    } # end of yes branch
  } # end of while loop
  swirl_out(s()%N%"انك قد اكملت الدرس Coursera لاعلام",
            s()%N%"يرجى التحميل", sQuote(output_filename),
            s()%N%"يدويا قد يتوجب عليك زياره البرمجه Coursera لتفعيل  ",
            s()%N%"صفحه الواجبات على موقع الدوره الخاصه بك وايضا رابط تسليم الواجبات",
            s()%N%" swirlرز بجانب درس ",
            s()%N%":قمت بوضع الملف في المكان التالي",
            skip_after=TRUE)
  message(getwd(), "\n")
  readline("...")
}

# Returns TRUE if user would like to retry, FALSE if not
retry <- function() {
  swirl_out(s()%N%"هل تريد اعاده المحاوله التلقائيه او ارسالها يدويا؟")
  ans <- select.list(c("تسليم يدوي", "اعاده المحاوله التلقائيه"), graphics=FALSE)
  # Return TRUE if user would like to retry
  return(ans == "اعاده المحاوله التلقائيه")
}

get_courseid <- function() {
  swirl_out(s()%N%"البند الاول الذي احتاجه هو معرفه الدوره التدريبه الخاصه بك، على سبيل المثال :لو",
            s()%N%"الخاصه بك كانت Coursera صفحه كورس",
            s()%N%"'https://class.coursera.org/rprog-001',",
            s()%N%"اذن سوف يكون رقم الدوره الخاصه بك هو 'rprog-001' (without the quotes).",
            skip_after=TRUE)
  repeat {
    courseid <- readline(":رقم الدوره")
    # Remove quotes if there are any
    courseid <- gsub("\'|\"", "", courseid)
    # Set up test cases
    is_url <- str_detect(courseid, "www[.]|http:|https:")
    is_numbers <- str_detect(courseid, "^[0-9]+$")
    is_example <- str_detect(courseid, fixed("rprog-001"))
    
    # Check if courseid is none of the bad things
    if(!any(is_url, is_numbers, is_example)){
      break
    # courseid is one of the bad things
    } else {
      # Check if courseid is a url
      if(is_url) {
        swirl_out(s()%N%"يبدو انك قمت بإدخال عنوان ويب وليس ما انا عليه",
                  s()%N%"تبحث عن")
      }
      # Check if courseid is all numbers
      if(is_numbers) {
        swirl_out(s()%N%"يبدو انك ادخلت معرفا رقميا وليس ما انا عليه",
                  s()%N%"تبحث عن")
      }
      # Check if the user stole the example courseid
      if(is_example) {
        swirl_out(s()%N%"يبدو انك ادخلت رقم الدوره التعليميه التي انا استخدمها كمثال ",
                  s()%N%"اي انني لاابحث عن هذه الاجابه ")
      }
    }
    swirl_out(s()%N%" Coursera  انا اريد رقم الدوره التي كانت في اخر جزء من عنوان دوره",
              s()%N%" الخاصه بك هي  Coursera  على سبيل المثال اذا كانت الصفحه الرئيسيه لدوره",
              s()%N%"'https://class.coursera.org/rprog-001',",
              s()%N%"اذن سوف يكون رقم الدوره الخاصه بك هي'rprog-001' (without the quotes).",
              skip_after=TRUE)
  }
  courseid
}

getCreds <- function(e) {
  cn <- make_pathname(attr(e$les, "اسم الدوره"))
  credfile <- file.path(e$udat, paste0(cn, ".txt"))
  e$coursera <- digest(paste0("اكمل", paste0(
    rep("_", ifelse(is.null(e$skips), 0, e$skips)), collapse="")),
    algo="sha1", serialize = FALSE)
  
  confirmed <- FALSE 
  need2fix <- FALSE
  while(!confirmed) {
    if(!file.exists(credfile) || need2fix) {
      courseid <- get_courseid()
      email <- readline(":ادخل الايميل ")
      passwd <- readline(":ادخل كلمه المرر ")
      writeLines(c(courseid, email, passwd), credfile)
      r <- c(courseid = courseid, email = email, passwd = passwd)
    } else {
      r <- readLines(credfile, warn=FALSE)
      names(r) <- c("courseid", "email", "passwd")
    }
    swirl_out(s()%N%"هل المعلومات التاليه صحيحه؟", skip_after=TRUE)
    message(":رقم الكورس ", r['courseid'],
            "\n:الايميل المدخل ", r['email'], 
            "\n:كلمه المرور ", r['passwd'])
    yn <- c("ممتاز، اكمل", 
            "لا، اريد ان اغير شيئا ما")
    confirmed <- identical(select.list(yn, graphics=FALSE), yn[1])
    if(!confirmed) need2fix <- TRUE
  }
  return(r)
}

#' @importFrom RCurl getForm
getChallenge <- function(email, challenge.url) {
  params <- list(email_address = email, response_encoding = "delim")
  result <- getForm(challenge.url, .params = params)
  s <- strsplit(result, "|", fixed = TRUE)[[1]]
  list(ch.key = s[5], state = s[7])
}

#' @importFrom digest digest
challengeResponse <- function(password, ch.key) {
  x <- paste(ch.key, password, sep = "")
  digest(x, algo = "sha1", serialize = FALSE)
}

#' @importFrom RCurl postForm base64
submitSolution <- function(email, submit.url, ch.resp, sid, output, 
                           signature, src = "",http.version = NULL) {
  output <- as.character(base64(output))
  src <- as.character(base64(src))
  params <- list(assignment_part_sid = sid,
                 email_address = email,
                 submission = output,
                 submission_aux = src,
                 challenge_response = ch.resp,
                 state = signature)
  params <- lapply(params, URLencode)
  result <- try(postForm(submit.url, .params = params), silent=TRUE)
  if(is(result,"try-error")){
    return(result)
  } else {
    s <- strsplit(result, "\\r\\n")[[1]]
    return(tail(s, 1))
  }
}