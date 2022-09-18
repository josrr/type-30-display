;; -*- coding: utf-8-unix; -*-

;;;; Copyright (c) 2022 José Ronquillo Rivera <josrr@ymail.com>
;;;; This file is part of type-30-display.
;;;;
;;;; type-30-display is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; type-30-display is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with type-30-display.  If not, see <http://www.gnu.org/licenses/>.

;;(require 'sb-rotate-byte)

(asdf:defsystem #:type-30-display
  :defsystem-depends-on (:deploy)
  :description "Minksytron usin McCLIM"
  :author "José Miguel Ronquillo Rivera <jose@rufina.link>"
  :serial t
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :build-operation "deploy-op"
  :build-pathname #P"type-30-display"
  :entry-point "type-30-display:type-30-display-entry-point"
  :depends-on (#:uiop
               #:alexandria
               #:bordeaux-threads
               #:local-time
               #:mcclim
               #:trivial-garbage)
  :components ((:file "package")
               (:file "parameters")
               (:file "animations")
               (:file "type-30-display")))
