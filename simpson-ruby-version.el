;;; simpson-ruby-version.el --- Function that reads .ruby-version and sets the $PATH correctly for processes inside emacs.

;; Adam Simpson <adam@adamsimpson.net>
;; Version: 0.1.1
;; Package-Requires: ((f "0.20.0") (projectile "1.1.0-snapshot"))
;; Keywords: ruby, ruby-version

;;; Commentary:
;; This is a function that can be M-x'd whenever you change projects that require a different version of Ruby.  It's modeled after chruby.
;; Like chruby, if there is a .ruby-version file then the env variables $GEM_HOME and $GEM_PATH are both set accordingly and the gem bin directories are
;; added to the $PATH varaible.
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
  "Check for .ruby-version file and set env variables accordingly."
  (interactive)
  (let ((base (file-exists-p (concat (ignore-errors (projectile-project-root)) ".ruby-version")))
        (default-gem simpson-ruby-default-gem-path)
        (path (mapconcat 'identity (seq-uniq (remove-if-not (lambda(x) (null (string-match-p "ruby" x))) (split-string (exec-path-from-shell-getenv "PATH") ":"))) ":")))
    (if base
        (let* ((version (string-trim (f-read-text (concat (projectile-project-root) ".ruby-version"))))
               (base-version (concat (mapconcat 'identity (butlast (split-string version "\\.")) ".") ".0"))
               (ruby-path (concat simpson-ruby-rubies-path version))
               (gem-bin (concat (file-truename simpson-ruby-gem-path) (nth 1 (split-string version "-"))))
               (gem-path (concat simpson-ruby-rubies-path version "/lib/ruby/gems/" base-version))
               (gem-home (concat (file-truename simpson-ruby-gem-path) version)))
          (exec-path-from-shell-setenv "PATH" (concat gem-bin "/bin:" gem-path "/bin:" ruby-path "/bin:" path))
          (setenv "GEM_PATH" (concat gem-bin ":" gem-path))
          (setenv "GEM_HOME" gem-home))
        (exec-path-from-shell-setenv "PATH" path)
        (setenv "GEM_PATH" "")
        (setenv "GEM_HOME" ""))))

(provide 'simpson-ruby-version)

;;; simpson-ruby-version.el ends here
