;;; undercover-init.el -- Set-up coverage collection -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "lit.el"
              (:report-format 'lcov)
              (:send-report nil)))
