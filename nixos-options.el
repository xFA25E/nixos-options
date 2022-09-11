;;; nixos-options.el --- Browse nixos options  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/nixos-options
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.6"))
;; Keywords: something

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.
;; `nixos-options' is autoloaded, so you can call it right away.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put the following in your init
;; file:

;; (require 'nixos-options)

;;;; Usage

;; Run one of these commands:

;; `nixos-options-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `nixos-options' group.

;; + nix-prettify-mode

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'map)
(require 'markdown-mode)

;;;; Customization

(defgroup nixos-options nil
  "Settings for command `nixos-options'."
  :link '(url-link "https://github.com/xFA25E/nixos-options")
  :group 'nix)

(defcustom nixos-options-buffer-name "*Nixos Options*"
  "Buffer name for `nixos-options-mode'."
  :type 'string
  :group 'nixos-options)

;;;; Variables

(defvar nixos-options-nixpkgs-path
  (car (process-lines "nix" "eval" "--impure" "--expr" "<nixpkgs>"))
  "Path to nixpkgs.")

(defvar nixos-options-json-file
  (with-temp-buffer
    (let ((process-environment
           (cons "NIXPKGS_ALLOW_UNFREE=1" process-environment)))
      (call-process
       "nix" nil t nil "build" "--impure" "--no-link" "--json" "--expr"
       "((import <nixpkgs/nixos/release.nix>) {}).options"))
    (goto-char (point-min))
    (expand-file-name
     "share/doc/nixos/options.json"
     (map-nested-elt (json-parse-buffer :object-type 'alist) [0 outputs out])))
  "Path to json file with nixos options.")

(defvar nixos-options
  (with-temp-buffer
    (insert-file-contents nixos-options-json-file)
    (json-parse-buffer))
  "Parsed nixos options.")

;;;;; Keymaps

(easy-mmode-defmap nixos-options-mode-map
  '(("s" . nixos-options))
  "Keymap for `nixos-options-mode'.")

;;;; Commands

(define-derived-mode nixos-options-mode gfm-view-mode "NixOpts"
  :group 'nixos-options
  :abbrev-table nil
  (buffer-disable-undo))

;;;###autoload
(defun nixos-options (query)
  "Display nixos options that match regexp QUERY."
  (interactive "sRegexp query: ")
  (let ((options (thread-last
                   (map-keys nixos-options)
                   (seq-filter (apply-partially #'string-match-p query))
                   (seq-sort #'string<))))
    (with-current-buffer (get-buffer-create nixos-options-buffer-name)
      (with-silent-modifications
        (erase-buffer)
        (insert "# *" query "*\n\n")
        (seq-do #'nixos-options--draw options))
      (goto-char (point-min))
      (nixos-options-mode)
      (pop-to-buffer (current-buffer)))))

;;;; Functions

;;;;; Public

;;;;; Private

(defun nixos-options--draw (name)
  "Draw option with NAME to screen.
Show detailed information when SHOWP is non-nil."
  (insert "## `" name "`\n\n")
  (when-let ((description (nixos-options--description name)))
    (insert description "\n\n"))
  (let ((type (map-nested-elt nixos-options `[,name "type"])))
    (insert "### Type\n\n" type "\n\n"))
  (when-let ((default (nixos-options--default name)))
    (insert "### Default\n\n```nix\n" default "\n```\n\n"))
  (when-let ((example (nixos-options--example name)))
    (insert "### Example\n\n```nix\n" example "\n```\n\n"))
  (let ((declarations (map-nested-elt nixos-options `[,name "declarations"])))
    (insert "### Declarations\n\n")
    (seq-doseq (d declarations)
      (insert "- " nixos-options-nixpkgs-path "/" d "\n"))
    (insert "\n")))

(defun nixos-options--cache (name)
  "Return cache for option with NAME."
  (let ((option (map-elt nixos-options name)))
    (unless (map-contains-key option "cache")
      (map-put! option "cache" (make-hash-table :test #'equal :size 3)))
    (map-elt option "cache")))

(defun nixos-options--description (name)
  "Get formatted description for option with NAME."
  (let ((cache (nixos-options--cache name)))
    (or (map-elt cache "description")
        (when-let ((description (map-elt (map-elt nixos-options name) "description")))
          (with-temp-buffer
            (call-process-region (concat "<para>" description "</para>") nil
                                 "pandoc" nil t nil "-f" "docbook" "-t" "markdown")
            (goto-char (point-min))
            (while (search-forward-regexp (rx "[???](#opt-" (group (+ (not ")"))) ")") nil t)
              (replace-match "`\\1`"))
            (let ((description (string-trim (buffer-string))))
              (map-put! cache "description" description)
              description))))))

(defun nixos-options--default (name)
  "Get formatted default value for option with NAME."
  (let ((cache (nixos-options--cache name)))
    (or (map-elt cache "default")
        (when-let ((default (map-elt (map-elt nixos-options name) "default")))
          (let ((default (nixos-options--format-nix-value default)))
            (map-put! cache "default" default)
            default)))))

(defun nixos-options--example (name)
  "Get formatted example value for option with NAME."
  (let ((cache (nixos-options--cache name)))
    (or (map-elt cache "example")
        (when-let ((example (map-elt (map-elt nixos-options name) "example")))
          (let ((example (nixos-options--format-nix-value example)))
            (map-put! cache "example" example)
            example)))))

(defun nixos-options--format-nix-value (value)
  "Pretty-print VALUE as nix value."
  (pcase value
    (:null "null")
    (:false "false")
    ('t "true")
    ((and (pred stringp) (rx "\n") (app string-trim value))
     (concat "''\n" (replace-regexp-in-string (rx bol) "  " value) "\n''"))
    ((pred stringp) (format "%S" value))
    ((and (pred vectorp) (guard (zerop (length value)))) "[]")
    ((and (pred hash-table-p) (pred hash-table-empty-p)) "{}")
    ((map ("_type" "literalExpression") ("text" text))
     (string-trim text))
    ((app json-serialize json)
     (let ((expr (concat "builtins.fromJSON ''\n" json "\n''")))
       (with-temp-buffer
         (call-process "nix" nil t nil "eval" "--expr" expr)
         (call-process-region (point-min) (point-max) "alejandra" t '(t nil) nil)
         (string-trim (buffer-string)))))))

;; try with other tools
;; File list of package docbook-utils in sid of architecture all
;; /usr/bin/db2dvi
;; /usr/bin/db2html
;; /usr/bin/db2pdf
;; /usr/bin/db2ps
;; /usr/bin/db2rtf
;; /usr/bin/docbook2dvi
;; /usr/bin/docbook2html
;; /usr/bin/docbook2man
;; /usr/bin/docbook2pdf
;; /usr/bin/docbook2ps
;; /usr/bin/docbook2rtf
;; /usr/bin/docbook2tex
;; /usr/bin/docbook2texi
;; /usr/bin/docbook2txt

;; docbook-utils/jammy,jammy 0.6.14-4 all
;;   Convert DocBook files to other formats (HTML, RTF, PS, man, PDF)

;;;; Footer

(provide 'nixos-options)

;;; nixos-options.el ends here
