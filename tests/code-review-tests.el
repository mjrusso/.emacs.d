;;; code-review-tests.el --- Review-note regression checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'org)

(load (expand-file-name "../conf/code-review.el" (file-name-directory load-file-name))
      nil 'nomessage)

(defun my/review-notes-test--commit (message)
  (my/review-notes-test--git
   "-c" "user.name=Review fixture" "-c" "user.email=fixture@example.invalid"
   "-c" "commit.gpgsign=false" "commit" "--quiet" "-m" message))

(defun my/review-notes-test--git (&rest args)
  (with-temp-buffer
    (should (zerop (apply #'process-file "git" nil t nil args)))
    (string-trim (buffer-string))))

(defun my/review-notes-test--init-repo (directory)
  (make-directory directory t)
  (let ((default-directory directory))
    (my/review-notes-test--git "init" "--quiet")
    (with-temp-file "sample.txt"
      (insert "first\nsecond\nthird\nfourth\n"))
    (my/review-notes-test--git "add" "sample.txt")
    (my/review-notes-test--commit "Initial")))

(defmacro my/review-notes-test--with-repo (&rest body)
  (declare (indent 0))
  `(let* ((temporary-root
           (file-name-as-directory (file-truename (make-temp-file "review-notes-test-" t))))
          (repo (expand-file-name "one/sample/" temporary-root))
          (my/magit-review-notes-directory nil)
          (magit-save-repository-buffers nil)
          (save-abbrevs nil))
     (unwind-protect
         (save-window-excursion
           (my/review-notes-test--init-repo repo)
           (let ((default-directory repo)) ,@body))
       (dolist (buffer (buffer-list))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((path (or my/magit-review-notes--repository
                             buffer-file-name
                             (and (derived-mode-p 'magit-mode) default-directory))))
               (when (and path (string-prefix-p temporary-root path))
                 (set-buffer-modified-p nil)
                 (kill-buffer buffer))))))
       (delete-directory temporary-root t))))

(ert-deftest my/review-notes-save-capture-open-export ()
  (my/review-notes-test--with-repo
    (let ((source (find-file-noselect (expand-file-name "sample.txt" repo)))
          notes)
      (switch-to-buffer source)
      (my/magit-review-notes-capture)
      (setq notes (current-buffer))
      (insert "First comment\n")
      (write-file (expand-file-name "review.org" temporary-root))
      (switch-to-buffer source)
      (my/magit-review-notes-capture)
      (should (eq (current-buffer) notes))
      (insert "Second comment\n")
      (switch-to-buffer source)
      (my/magit-review-notes-open)
      (should (eq (current-buffer) notes))
      (switch-to-buffer source)
      (my/magit-review-notes-export)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "review.md" temporary-root))
        (should (search-forward "First comment" nil t))
        (should (search-forward "Second comment" nil t)))
      (with-current-buffer notes
        (my/magit-review-notes-export)))))

(ert-deftest my/review-notes-revert-preserves-association ()
  (my/review-notes-test--with-repo
    (let ((notes (my/magit-review-notes--get-buffer)))
      (with-current-buffer notes
        (write-file (expand-file-name "review.org" temporary-root))
        (revert-buffer t t))
      (should (eq notes (my/magit-review-notes--get-buffer))))))

(ert-deftest my/review-notes-indirect-and-narrowed-views ()
  (my/review-notes-test--with-repo
    (let* ((notes (my/magit-review-notes--get-buffer))
           (source (find-file-noselect (expand-file-name "sample.txt" repo)))
           (clone (with-current-buffer notes
                    (goto-char (point-max))
                    (insert "* First\nFirst comment\n* Last\nLast comment\n")
                    (write-file (expand-file-name "review.org" temporary-root))
                    (clone-indirect-buffer "*review-subtree*" nil))))
      (dolist (buffer (list notes clone))
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^\\* First$")
          (org-narrow-to-subtree)))
      (switch-to-buffer clone)
      (switch-to-buffer source)
      (my/magit-review-notes-capture)
      (should (eq (current-buffer) notes))
      (should-not (buffer-narrowed-p))
      (insert "Newest comment\n")
      (goto-char (point-min))
      (re-search-forward "^\\* First$")
      (org-narrow-to-subtree)
      (switch-to-buffer clone)
      (switch-to-buffer source)
      (my/magit-review-notes-export)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "review.md" temporary-root))
        (should (search-forward "First comment" nil t))
        (should (search-forward "Last comment" nil t))
        (should (search-forward "Newest comment" nil t)))
      (should (with-current-buffer notes (buffer-narrowed-p)))
      (should (with-current-buffer clone (buffer-narrowed-p)))
      (cl-letf (((symbol-function 'read-buffer)
                 (lambda (&rest _) (buffer-name clone))))
        (my/magit-review-notes-open t)
        (should (eq (current-buffer) notes))
        (my/magit-review-notes-export t)))))

(ert-deftest my/review-notes-fixture-preserves-unrelated-buffers ()
  (let (unrelated source notes)
    (unwind-protect
        (progn
          (my/review-notes-test--with-repo
            (setq source (find-file-noselect (expand-file-name "sample.txt" repo))
                  notes (my/magit-review-notes--get-buffer)
                  unrelated (generate-new-buffer "unrelated background output"))
            (with-current-buffer unrelated (insert "Unsaved background result")))
          (should-not (buffer-live-p source))
          (should-not (buffer-live-p notes))
          (should (buffer-live-p unrelated))
          (with-current-buffer unrelated
            (should (buffer-modified-p))
            (should (equal (buffer-string) "Unsaved background result"))))
      (when (buffer-live-p unrelated)
        (with-current-buffer unrelated (set-buffer-modified-p nil))
        (kill-buffer unrelated)))))

(ert-deftest my/review-notes-separate-repositories-with-same-name ()
  (my/review-notes-test--with-repo
    (let* ((other (expand-file-name "two/sample/" temporary-root))
           (first (my/magit-review-notes--get-buffer)))
      (my/review-notes-test--init-repo other)
      (let ((second (let ((default-directory other))
                      (my/magit-review-notes--get-buffer))))
        (should-not (eq first second))
        (should (eq first (my/magit-review-notes--get-buffer)))
        (should (eq second (let ((default-directory other))
                             (my/magit-review-notes--get-buffer))))))))

(ert-deftest my/review-notes-source-link-survives-custom-directory-and-save ()
  (my/review-notes-test--with-repo
    (let* ((my/magit-review-notes-directory
            (expand-file-name "notes/" temporary-root))
           (source-file (expand-file-name "sample.txt" repo)))
      (make-directory my/magit-review-notes-directory)
      (find-file source-file)
      (my/magit-review-notes-capture)
      (dolist (saved '(nil t))
        (when saved
          (write-file (expand-file-name "review.org" temporary-root)))
        (goto-char (point-min))
        (re-search-forward "^:SOURCE: \\(.*\\)$")
        (let ((link (match-string-no-properties 1))
              (directory default-directory))
          (with-temp-buffer
            (org-mode)
            (insert link)
            (goto-char (point-min))
            (let ((path (org-element-property :path (org-element-context))))
              (should (equal (expand-file-name path directory) source-file)))))))))

(ert-deftest my/review-notes-staged-attribution-uses-index ()
  (my/review-notes-test--with-repo
    (let ((initial (my/review-notes-test--git "rev-parse" "HEAD")))
      (with-temp-file "sample.txt" (insert "first\nSTAGED\nthird\nfourth\n"))
      (my/review-notes-test--git "add" "sample.txt")
      (with-temp-file "sample.txt" (insert "UNSTAGED\nfirst\nSTAGED\nthird\nfourth\n"))
      (dolist (view '(nil "HEAD" status))
        (if (eq view 'status)
            (progn
              (magit-status-setup-buffer repo)
              (magit-section-show-level-4-all))
          (magit-diff-staged view nil '("sample.txt")))
        (goto-char (point-min))
        (re-search-forward "^+STAGED$")
        (beginning-of-line)
        (let ((context (my/magit-review-notes--context)))
          (should-not (plist-get context :hash))
          (should-not (plist-get context :range))
          (should-not (plist-get context :source-file))
          (should (= (plist-get context :line) 2)))
        (re-search-forward "^ third$")
        (beginning-of-line)
        (should (equal (plist-get (my/magit-review-notes--context) :hash) initial))
        (my/magit-review-notes-capture)
        (org-back-to-heading)
        (should (member "staged" (org-get-tags)))
        (should-not (org-entry-get nil "SOURCE"))))))

(ert-deftest my/review-notes-unstaged-source-link ()
  (my/review-notes-test--with-repo
    (with-temp-file "sample.txt" (insert "NEW\nfirst\nsecond\nthird\nfourth\n"))
    (magit-diff-unstaged nil '("sample.txt"))
    (goto-char (point-min))
    (re-search-forward "^+NEW$")
    (beginning-of-line)
    (let ((context (my/magit-review-notes--context)))
      (should-not (plist-get context :hash))
      (should-not (plist-get context :range))
      (should (equal (plist-get context :source-file)
                     (expand-file-name "sample.txt" repo)))
      (should (= (plist-get context :line) 1)))
    (my/magit-review-notes-capture)
    (org-back-to-heading)
    (should (member "source" (org-get-tags)))
    (should (org-entry-get nil "SOURCE"))))

(ert-deftest my/review-notes-working-tree-comparison ()
  (my/review-notes-test--with-repo
    (with-temp-file "sample.txt" (insert "first\nsecond\nthird\nCOMMITTED\n"))
    (my/review-notes-test--git "add" "sample.txt")
    (my/review-notes-test--commit "Next")
    (with-temp-file "sample.txt" (insert "first\nWORKTREE\nthird\nCOMMITTED\n"))
    (dolist (revision '(nil "HEAD" "HEAD~1"))
      (magit-diff-working-tree revision nil '("sample.txt"))
      (goto-char (point-min))
      (re-search-forward "^+WORKTREE$")
      (beginning-of-line)
      (my/magit-review-notes-capture)
      (org-back-to-heading)
      (should-not (org-entry-get nil "COMMIT"))
      (should-not (org-entry-get nil "RANGE"))
      (should (equal (org-entry-get nil "LINE") "2"))
      (should (org-entry-get nil "SOURCE"))
      (should (member "source" (org-get-tags))))))

(ert-deftest my/review-notes-commit-and-range-attribution ()
  (my/review-notes-test--with-repo
    (with-temp-file "sample.txt" (insert "first\nCOMMITTED\nthird\nfourth\n"))
    (my/review-notes-test--git "add" "sample.txt")
    (my/review-notes-test--commit "Change")
    (let ((commit (my/review-notes-test--git "rev-parse" "HEAD")))
      (with-temp-file "sample.txt"
        (insert "UNCOMMITTED\nfirst\nCOMMITTED\nthird\nfourth\n"))
      (dolist (range '(nil "HEAD~1..HEAD" "HEAD~1...HEAD" "HEAD^-1" "HEAD^!"))
        (if range
            (magit-diff-setup-buffer range nil nil '("sample.txt") 'committed)
          (magit-show-commit "HEAD" nil '("sample.txt")))
        (goto-char (point-min))
        (re-search-forward "^+COMMITTED$")
        (beginning-of-line)
        (let ((context (my/magit-review-notes--context)))
          (should (equal (plist-get context :hash) commit))
          (should (equal (plist-get context :range) range))
          (should (= (plist-get context :line) 2)))))))

(ert-deftest my/review-notes-merge-revision-sets ()
  (my/review-notes-test--with-repo
    (let* ((process-environment
            (append '("GIT_AUTHOR_NAME=Review fixture" "GIT_COMMITTER_NAME=Review fixture"
                      "GIT_AUTHOR_EMAIL=fixture@example.invalid"
                      "GIT_COMMITTER_EMAIL=fixture@example.invalid")
                    process-environment))
           (base (my/review-notes-test--git "rev-parse" "HEAD"))
           (parents
            (mapcar
             (lambda (content)
               (with-temp-file "sample.txt" (insert "first\n" content "\nthird\nfourth\n"))
               (my/review-notes-test--git "add" "sample.txt")
               (my/review-notes-test--git
                "-c" "commit.gpgsign=false" "commit-tree"
                (my/review-notes-test--git "write-tree") "-p" base "-m" content))
             '("LEFT" "RIGHT" "THIRD"))))
      (with-temp-file "sample.txt" (insert "first\nMERGED\nthird\nfourth\n"))
      (my/review-notes-test--git "add" "sample.txt")
      (let ((merge (apply #'my/review-notes-test--git
                          "-c" "commit.gpgsign=false" "commit-tree"
                          (my/review-notes-test--git "write-tree") "-m" "Merge"
                          (cl-mapcan (lambda (parent) (list "-p" parent)) parents))))
        (my/review-notes-test--git "update-ref" "HEAD" merge)
        (with-temp-file "sample.txt" (insert "UNCOMMITTED\nfirst\nMERGED\nthird\nfourth\n"))
        (dolist (range '("HEAD^-2" "HEAD^!"))
          (magit-diff-range range nil '("sample.txt"))
          (goto-char (point-min))
          (re-search-forward "^\\++MERGED$")
          (beginning-of-line)
          (let ((context (my/magit-review-notes--context)))
            (should (equal (plist-get context :hash) merge))
            (should (equal (plist-get context :range) range))
            (should-not (plist-get context :source-file))
            (should (= (plist-get context :line) 2))))))))

(ert-deftest my/review-notes-hunk-heading-capture ()
  (my/review-notes-test--with-repo
    (with-temp-file "sample.txt"
      (dotimes (n 40) (insert (format "line %d\n" (1+ n)))))
    (my/review-notes-test--git "add" "sample.txt")
    (my/review-notes-test--commit "Long file")
    (with-temp-file "sample.txt"
      (dotimes (n 40)
        (insert (format "%s %d\n" (if (memq n '(1 35)) "CHANGED" "line") (1+ n)))))
    (magit-diff-unstaged nil '("sample.txt"))
    (let ((diff (current-buffer)))
      (goto-char (point-min))
      (dolist (expected '(2 36))
        (re-search-forward "^@@.*@@")
        (beginning-of-line)
        (let ((next (save-excursion (forward-line 1) (point))))
          (my/magit-review-notes-capture)
          (org-back-to-heading)
          (should (equal (org-entry-get nil "LINE") (number-to-string expected)))
          (should-not (org-entry-get nil "COMMIT"))
          (switch-to-buffer diff)
          (goto-char next))))))

(ert-deftest my/review-notes-orgit-links-roundtrip ()
  (my/review-notes-test--with-repo
    (require 'orgit)
    (require 'ox-gfm)
    (my/review-notes-test--git "remote" "add" "origin"
                               "https://github.com/example/review.git")
    (let ((org-store-link-plist nil)
          (orgit-store-repository-id nil)
          (orgit-log-save-arguments t))
      (switch-to-buffer
       (magit-log-setup-buffer '("HEAD") '("--max-count=10") '("sample.txt")))
      (orgit-log-store)
      (let ((link (plist-get org-store-link-plist :link)))
        (should (string-prefix-p "orgit-log:" link))
        (magit-status repo)
        (switch-to-buffer (orgit-log-open (string-remove-prefix "orgit-log:" link)))
        (should (derived-mode-p 'magit-log-mode))
        (should (equal magit-buffer-log-revisions '("HEAD")))
        (should (member "--max-count=10" magit-buffer-log-args))
        (should (equal magit-buffer-log-files '("sample.txt"))))
      (let ((commit (my/review-notes-test--git "rev-parse" "HEAD")))
        (magit-show-commit "HEAD")
        (orgit-rev-store)
        (let ((link (plist-get org-store-link-plist :link)))
          (should (string-suffix-p (concat "::" commit) link))
          (magit-status repo)
          (orgit-rev-open (string-remove-prefix "orgit-rev:" link))
          (should (derived-mode-p 'magit-revision-mode))
          (should (equal (magit-rev-parse magit-buffer-revision) commit))
          (should (string-match-p
                   (regexp-quote (concat "https://github.com/example/review/commit/" commit))
                   (org-export-string-as (format "[[%s][Review]]" link) 'gfm t))))))))
