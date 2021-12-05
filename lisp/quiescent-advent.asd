(in-package :cl-user)

(defpackage :quiescent-advent.sys
  (:use :asdf :cl))

(in-package :quiescent-advent.sys)

(defsystem :quiescent-advent
  :name "Quiescent-Advent"
  :author "Edward Steere"
  :version "0.0.1"
  :maintainer "edward.steere@gmail.com"
  :license "BSD"
  :description "Solutions to the 2021 advent of code"
  :long-description "The advent of code is a series of programming problems that are fun.  Some poeple race to see who can solve the problems fastest.  This year, as usual, I'm joining those who want to do it quickly."
  :depends-on (:trivia :alexandria :cl-ppcre :trivia.ppcre :arrow-macros :iterate :metabang-bind :arrow-macros :anaphora :cl-heap :ironclad :flexi-streams)
  :components ((:file "day1" :depends-on ("lib"))
               (:file "day2" :depends-on ("lib"))
               (:file "day3" :depends-on ("lib"))
               (:file "day4" :depends-on ("lib"))
               (:file "day5" :depends-on ("lib"))
               (:file "lib")))
