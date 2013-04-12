;;; Copyright (c) 2004-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; Based on Edi's shtml.system,v 1.5 2007/01/01 23:49:16

(in-package :cl-user)

;(load #P"PROJECTS:ylib;defsys")
;(compile-system 'YSTOK-LIBRARY :load t)

(defsystem SHTML (:object-pathname (lw:pathname-location (lw:current-pathname
				  #+(and debug ylib)		"bin/debug/ylib/"
				  #+(and (not debug) ylib)	"bin/ylib/"
				  #+(and debug (not ylib))	"bin/debug/"
				  #-(or debug ylib)		"bin/")))
 :members (#+ylib (YSTOK-LIBRARY :type :system)
           "packages"
           "specials"
           "errors"
           "util"
           "template"
           "api")
 :rules ((:in-order-to :compile :all
          (:requires (:load :previous)))
         (:in-order-to :load :all
          (:caused-by (:compile :previous))
          (:requires (:load :previous))) ))

;(compile-system 'SHTML :load t :force nil)
