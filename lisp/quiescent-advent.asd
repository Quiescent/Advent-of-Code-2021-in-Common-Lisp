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
  :depends-on (:trivia :alexandria :cl-ppcre :arrow-macros :iterate :metabang-bind :arrow-macros :anaphora :cl-heap)
  :components ((:file "day1" :depends-on ("lib"))
               (:file "lib")))
