#!/bin/sh

model=$1
filename=$2

sbcl --noinform --noprint --disable-debugger \
--eval "(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (asdf:load-system :pos-tagger))" \
--eval "(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (pos-tagger:load-model \"$model\"))" \
--eval "(pos-tagger:tag-file \"$filename\")" \
--eval "(quit)"


