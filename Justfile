tasks:
  just --list

run n='10':
  clj -M -m golden-spiral {{n}}

repl:
  clj -M:nrepl

