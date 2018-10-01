;;; simpson-ruby-version.el --- Function that reads .ruby-version and sets the $PATH correctly for processes inside emacs.

;;; Commentary:
;; This is a function that can be M-x whenever you change projects that require a different version of Ruby.
;; I assume you use projectile to manage projects within Emacs.

;;; Code:

(require 'projectile)
(require 'f)

(defvar simpson-ruby-default-gem-path nil
  "The default path to Ruby.")

(defvar simpson-ruby-rubies-path nil
  "The location of additional Rubies.")

(defvar simpson-ruby-gem-path nil
  "The location of additional gems.")

(defun simpson-ruby-version()
  "Use 'projectile-project-root' to check for .ruby-version file and then set env variables accordingly."
  (interactive)
  (let ((base (file-exists-p (concat (ignore-errors (projectile-project-root)) ".ruby-version")))
        (default-gem simpson-ruby-default-gem-path)
        (path (mapconcat 'identity (seq-uniq (remove-if-not (lambda(x) (null (string-match-p "ruby" x))) (split-string (exec-path-from-shell-getenv "PATH") ":"))) ":")))
    (if base
        (let* ((version (string-trim (f-read-text (concat (projectile-project-root) ".ruby-version"))))
               (base-version (concat (mapconcat 'identity (butlast (split-string version "\\.")) ".") ".0"))
               (ruby-path (concat simpson-ruby-rubies-path "ruby-" version))
               (gem-path (concat simpson-ruby-rubies-path "ruby-" version ":" simpson-ruby-rubies-path "ruby-" version "/lib/ruby/gems/" base-version))
               (gem-home (concat simpson-ruby-gem-path version)))
          (exec-path-from-shell-setenv "PATH" (concat ruby-path "/bin:" gem-path ":" path))
          (setenv "GEM_PATH" gem-path)
          (setenv "GEM_HOME" gem-home))
      (exec-path-from-shell-setenv "PATH" path)
      (setenv "GEM_PATH" "")
      (setenv "GEM_HOME" ""))))

(provide 'simpson-ruby-version)

;;; simpson-ruby-version.el ends here
