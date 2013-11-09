
redo-ifchange ../*.lisp
exec 1>&2
sbcl <<END
(push #p"$(readlink -f ..)/" asdf:*central-registry*)
(ql:quickload :fccgparse :verbose t)
(gc :full t)
(sb-ext:save-lisp-and-die "$3" :toplevel #'fccgparse::main-cmdline :executable t)
END
