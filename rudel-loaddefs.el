;;; rudel-loaddefs.el

(autoload 'rudel-join-session "rudel" "Start a collaborative Rudel session" t)
(autoload 'global-rudel-minor-mode "rudel-mode"
  "Bindings for rudel session-level commands" t)

(global-set-key (kbd "C-c c j") 'rudel-join-session)

(setq rudel-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(dolist (dir '("." "jupiter" "obby" "zeroconf"))
  (add-to-list 'load-path (concat rudel-dir "/" dir)))

(eval-after-load 'rudel
  '(progn (global-rudel-minor-mode)
          (require 'rudel-obby)
          (require 'rudel-zeroconf)))

(provide 'rudel-loaddefs)