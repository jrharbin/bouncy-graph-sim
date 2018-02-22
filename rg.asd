(defpackage :rg)

(in-package :rg)

(asdf:defsystem rg
  :name "rg"
  :version "0.1"
  :maintainer "JRH"
  :author "JRH"
  :license "GNU Lesser General Public License (LGPL) Version 3"
  :description "Random graph simulator"
  :serial t
  :depends-on ( :alexandria :sdl2 :sdl2-ttf :bordeaux-threads :cl-graph )
  
  :components ((:file "rg")
	       (:file "gui" :depends-on ("rg"))
	       (:file "main" :depends-on ("rg" "gui"))))
