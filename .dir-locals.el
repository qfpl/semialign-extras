(("src" . ((nil . ((dante-target . "semialign-diff")))))
 ("doctests" . ((nil . ((dante-target . "doctests")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
(concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
