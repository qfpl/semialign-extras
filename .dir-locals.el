(("semialign-diff/src" . ((nil . ((dante-target . "semialign-diff")))))
 ("semialign-diff/doctests" . ((nil . ((dante-target . "doctests")))))
 ("semialign-merge/src" . ((nil . ((dante-target . "semialign-merge")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
(concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
