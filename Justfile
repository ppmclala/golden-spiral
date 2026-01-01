tasks:
  just --list

fib n='10':
  clj -M -m golden-spiral {{n}}

repl:
  clj -M:nrepl

mandelbrot n='2000':
  clj -M -m mandelbrot {{n}}
