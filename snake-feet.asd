(in-package :asdf-user)

(defsystem "snake-feet"
  :description "this package provide some useful iterators. all iterators dont evaluate elements until necessary with exception of some iterators (ireverse, isort and icache). so you may save the consuming memory."
  :version "1.0.0"
  :author "tikubonn <tikubonn@outlook.com>"
  :licence "MIT"
  :depends-on nil 
  :components 
  ((:file "snake-feet")))
