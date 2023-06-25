
(add-to-list 'load-path "/el/")

(require 'lit)

(global-display-line-numbers-mode)

(add-hook 'find-file-hook
          (lambda ()
            (let ((case-fold-search t))
              (when (string-match-p (file-name-extension buffer-file-name)
                                    "^cpp\\|c\\|h\\|hpp\\|cc\\|cxx\\|hh\\|c++$")
                (lit-mode)))))

(global-set-key (kbd "C-c d") 'lit-delete-spec)
(global-set-key (kbd "C-c i") 'lit-insert-issues)
