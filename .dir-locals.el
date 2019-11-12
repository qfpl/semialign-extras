(("src" . ((nil . ((dante-target . "lib:semialign-extras")))))
 ("doctests" . ((nil . ((dante-target . "test:doctests")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
(concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
