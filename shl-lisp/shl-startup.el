;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary: Ensure startup runs as it should.
;;; Code:

;; Ensure that we are not garbage collecting too often
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)

;; Disable byte-compilation warnings from native-compiled packages.
(setq native-comp-async-report-warnings-errors nil)

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Disabling unwanted ui
(setq default-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(horizontal-scroll-bars)
	(vertical-scroll-bars)))


(provide 'shl-startup)
;; shl-startup.el ends here
