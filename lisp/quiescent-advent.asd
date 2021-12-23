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
  :depends-on (:trivia :alexandria :cl-ppcre :trivia.ppcre :arrow-macros :iterate :metabang-bind :arrow-macros :anaphora :cl-heap :ironclad :flexi-streams :queues :queues.simple-queue :queues.priority-queue)
  :components ((:file "day1" :depends-on ("lib"))
               (:file "day2" :depends-on ("lib"))
               (:file "day3" :depends-on ("lib"))
               (:file "day4" :depends-on ("lib"))
               (:file "day5" :depends-on ("lib"))
               (:file "day6" :depends-on ("lib"))
               (:file "day7" :depends-on ("lib"))
               (:file "day8" :depends-on ("lib"))
               (:file "day9" :depends-on ("lib"))
               (:file "day10" :depends-on ("lib"))
               (:file "day11" :depends-on ("lib"))
               (:file "day12" :depends-on ("lib"))
               (:file "day13" :depends-on ("lib"))
               (:file "day14" :depends-on ("lib"))
               (:file "day15" :depends-on ("lib"))
               (:file "day16" :depends-on ("lib"))
               (:file "day17" :depends-on ("lib"))
               (:file "day18" :depends-on ("lib"))
               ;;(:file "day19" :depends-on ("lib"))
               (:file "day20" :depends-on ("lib"))
               (:file "day21" :depends-on ("lib"))
               (:file "day22" :depends-on ("lib"))
               (:file "day23" :depends-on ("lib"))
               (:file "lib")))
