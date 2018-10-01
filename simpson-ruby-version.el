;;; simpson-ruby-version.el --- Function that reads .ruby-version and sets the $PATH correctly for processes inside emacs.

;;; Commentary:
;;

;;; Code:

(require 'projectile)
(require 'f)

(defun simpson-ruby-version()
  "Use 'projectile-project-root' to check for .ruby-version file and then set env variables accordingly."
  (interactive)
  (let ((base (file-exists-p (concat (ignore-errors (projectile-project-root)) ".ruby-version")))
        (default-gem "/usr/local/bin/gem")
        (path (mapconcat 'identity (seq-uniq (remove-if-not (lambda(x) (null (string-match-p "ruby" x))) (split-string (exec-path-from-shell-getenv "PATH") ":"))) ":")))
    (if base
        (let* ((version (string-trim (f-read-text (concat (projectile-project-root) ".ruby-version"))))
               (base-version (concat (mapconcat 'identity (butlast (split-string version "\\.")) ".") ".0"))
               (ruby-path (concat "/usr/local/rubies/ruby-" version))
               (gem-path (concat "/usr/local/rubies/ruby-" version ":/usr/local/rubies/ruby-" version "/lib/ruby/gems/" base-version))
               (gem-home (concat "/Users/asimpson/.gem/ruby/" version)))
          (exec-path-from-shell-setenv "PATH" (concat ruby-path "/bin:" gem-path ":" path))
          (setenv "GEM_PATH" gem-path)
          (setenv "GEM_HOME" gem-home))
      (exec-path-from-shell-setenv "PATH" path)
      (setenv "GEM_PATH" "")
      (setenv "GEM_HOME" ""))))

(provide 'simpson-ruby-version)

;;; simpson-ruby-version.el ends here
