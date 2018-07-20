# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c(s()%N%"اجابه صحيحه ممتاز",
                 s()%N%"احسنت",
                 s()%N%"استمر عملك ممتاز",
                 s()%N%"ماشاء الله عليك ",
                 s()%N%"الى الامام يا صديقي",
                 s()%N%"مبدع",
                 s()%N%"احسنت ياذكي",
                 s()%N%"مااروع تميزك",
                 s()%N%"كم انت مبدع",
                 s()%N%"اجابه رائعه",
                 s()%N%"عمل عظيم يااخي",
                 s()%N%"متميز كالعاده",
                 s()%N%"عمل متقن ياطالب العلم",
                 s()%N%"اجابه صحيحه",
                 s()%N%"اجابه موفقه",
                 s()%N%"رزقك الله الجنه",
                 s()%N%"احسنت مع الشكر",
                 s()%N%"ممتاز استمر بهذه الجهود المثمره",
                 s()%N%"ممتاز استمر في العطاء",
                 s()%N%"كم انت مبدع ",
                 s()%N%"اهنئك اجابتك بغايه الروعه ")
  } else {
    phrases <- s()%N%"Correct!"
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c(s()%N%"Almost! Try again.",
                 s()%N%"You almost had it, but not quite. Try again.",
                 s()%N%"Give it another try.",
                 s()%N%"Not quite! Try again.",
                 s()%N%"Not exactly. Give it another go.",
                 s()%N%"That's not exactly what I'm looking for. Try again.",
                 s()%N%"Nice try, but that's not exactly what I was hoping for. Try again.",
                 s()%N%"Keep trying!",
                 s()%N%"That's not the answer I was looking for, but try again.",
                 s()%N%"Not quite right, but keep trying.",
                 s()%N%"You're close...I can feel it! Try it again.",
                 s()%N%"One more time. You can do it!",
                 s()%N%"Not quite, but you're learning! Try again.",
                 s()%N%"Try again. Getting it right on the first try is boring anyway!")
  } else {
    phrases <- s()%N%"Incorrect. Please try again."
  }
  sample(phrases, 1)
}
