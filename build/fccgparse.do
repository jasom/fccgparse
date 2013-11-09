redo-ifchange ../*.lisp
exec 1>&2
sbcl <<END
(ql:quickload :fccgparse)
(gc :full t)
(sb-ext:save-lisp-and-die "$3" :toplevel #'fccgparse::main :compression t :executable t)
END
