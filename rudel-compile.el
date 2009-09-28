;; Press M-x eval-buffer to compile and generate autoloads for rudel

(setq rudel-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))

(dolist (dir '("." "jupiter" "obby" "zeroconf"))
  (let ((d (concat rudel-dir "/" dir)))
    (add-to-list 'load-path d)
    (byte-recompile-directory d 0)))
