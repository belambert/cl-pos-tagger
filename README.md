============
pos-tagger
============

This is an trigram HMM-based POS tagger for Lisp.

It can be trained from training data.

Uses Viterbi decoding... using 1-grams, 2-grams, and 3-grams.


Command line usage
------------------

scripts/pos-tag-file-lisp.sh <model> <filename>


API usage
---------

TAGGING:
(asdf:load-system :pos-tagger))
(pos-tagger:load-model model))
(pos-tagger:tag-file filename)


TRAINING:
(defvar *pos-model* (pos-tagger:train-model "/Users/bel/workspace/code/pos-tagger/misc/ptb-swb.pos-train.txt"))
(pos-tagger:save-model *pos-model* "/Users/bel/pos-model.model")


TODO
-----
- Make serialized models text readable
- etc.



