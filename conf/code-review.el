;;; code-review.el --- Capture code-review notes in Org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Capture review notes from Magit diffs or source buffers in a per-repository
;; Org buffer. Notes include commit and file links when available and can be
;; exported to Markdown.
;;
;; `C-c a n'   -- capture a note (in a magit or source buffer)
;; `C-c a r o' -- open the repo's notes buffer
;; `C-c a r e' -- export to Markdown

;;; Code:

;; Adds support for linking to Magit buffers from Org documents.
;;
;; https://github.com/magit/orgit
(use-package orgit
  :after (magit org))

;; GitHub-Flavored Markdown exporter.
;;
;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :after org)

(defgroup my/magit-review-notes nil
  "Capture per-commit review notes from magit into Org."
  :group 'magit-extensions)

(defcustom my/magit-review-notes-directory nil
  "Default directory for saving and exporting review notes.
Nil uses the reviewed repository's root."
  :type '(choice (const :tag "Reviewed repository root" nil)
                 (directory :tag "Fixed directory"))
  :group 'my/magit-review-notes)

(defcustom my/magit-review-notes-tags '("question" "todo" "agent" "nit" "bug")
  "Tags offered when capturing with a prefix argument."
  :type '(repeat string)
  :group 'my/magit-review-notes)

(defvar-local my/magit-review-notes--repository nil)
(put 'my/magit-review-notes--repository 'permanent-local t)

(defun my/magit-review-notes--toplevel ()
  "Return the current Git repository root, loading Magit if needed."
  (require 'magit)
  (magit-toplevel))

(defun my/magit-review-notes--repository-root ()
  "Return the notes' repository root, or the current directory outside Git."
  (or my/magit-review-notes--repository
      (file-truename (or (my/magit-review-notes--toplevel) default-directory))))

(defun my/magit-review-notes--find-buffer (&optional name)
  "Return the base notes buffer for NAME or the current repository, if any."
  (let ((buffer
         (if name
             (get-buffer name)
           (let ((root (my/magit-review-notes--repository-root)))
             (seq-find
              (lambda (candidate)
                (and (not (buffer-base-buffer candidate))
                     (equal (buffer-local-value 'my/magit-review-notes--repository candidate)
                            root)))
              (buffer-list))))))
    (and buffer (or (buffer-base-buffer buffer) buffer))))

(defun my/magit-review-notes--get-buffer (&optional name)
  "Return the review-notes buffer NAME (default: the current repo's), creating it."
  (or (my/magit-review-notes--find-buffer name)
      (let* ((root (my/magit-review-notes--repository-root))
             (buf (if name
                      (get-buffer-create name)
                    (generate-new-buffer
                     (format "*%s-review-notes*"
                             (file-name-nondirectory (directory-file-name root)))))))
        (with-current-buffer buf
          (org-mode)
          (setq my/magit-review-notes--repository root
                default-directory (file-name-as-directory
                                   (expand-file-name
                                    (or my/magit-review-notes-directory root))))
          (insert "#+title: Review notes\n#+options: toc:nil num:nil\n"))
        buf)))

(defun my/magit-review-notes--read-buffer (prompt &optional require-match)
  "Read a notes buffer name with PROMPT, defaulting to this repository.
Pass REQUIRE-MATCH to `read-buffer'."
  (read-buffer prompt
               (or (my/magit-review-notes--find-buffer)
                   (format "*%s-review-notes*"
                           (file-name-nondirectory
                            (directory-file-name (my/magit-review-notes--repository-root)))))
               require-match))

(defun my/magit-review-notes--hunk-line (section)
  "Return the new-side line at point within hunk SECTION.
On the heading, use the first changed line."
  (save-excursion
    (let ((target (line-beginning-position)))
      (goto-char (oref section start))
      (when (looking-at "\\(@+\\) .*?\\+\\([0-9]+\\)")
        (let ((prefix-width (1- (length (match-string 1))))
              (line (string-to-number (match-string 2))))
          (forward-line 1)
          (when (< target (point))
            (save-excursion
              (when (re-search-forward
                     (format "^[-+ ]\\{0,%d\\}[-+]" (1- prefix-width))
                     (oref section end) t)
                (setq target (line-beginning-position)))))
          ;; Combined diffs have one prefix column per parent; a minus in any
          ;; column marks a line absent from the result tree.
          (while (< (point) target)
            (unless (string-search
                     "-" (buffer-substring-no-properties
                          (point) (min (+ (point) prefix-width) (line-end-position))))
              (setq line (1+ line)))
            (forward-line 1))
          line)))))

(defun my/magit-review-notes--blame (tip file line)
  "Return the commit hash for FILE:LINE, or nil for an uncommitted line.
TIP selects a revision, `staged' selects the index, and nil selects the
working tree. Git's boundary hashes can omit the final digit; the caller
resolves them to full hashes."
  (let* ((loc (format "%d,%d" line line))
         (out (if (eq tip 'staged)
                  (with-temp-buffer
                    (when (zerop (magit-git-insert "show" (concat ":" file)))
                      (let ((contents (make-nearby-temp-file "review-notes-index-")))
                        (unwind-protect
                            (progn
                              (write-region (point-min) (point-max) contents nil 'silent)
                              (magit-git-string "blame" "-l" "-L" loc
                                                "--contents" (file-local-name contents)
                                                "--" file))
                          (delete-file contents)))))
                (apply #'magit-git-string
                       `("blame" "-l" "-L" ,loc ,@(and tip (list tip))
                         "--" ,file)))))
    (and out
         (string-match "\\`\\^?\\([0-9a-f]\\{7,40\\}\\)" out)
         (let ((hash (match-string 1 out)))
           (unless (string-match-p "\\`0+\\'" hash)
             hash)))))

(defun my/magit-review-notes--range-tip (reference)
  "Return the result revision of committed diff REFERENCE, or nil for one tree."
  (let* ((revisions (magit-git-lines "rev-parse" "--revs-only" "--end-of-options" reference))
         (trees (seq-remove (lambda (revision) (string-prefix-p "^" revision)) revisions)))
    (when (> (length revisions) 1)
      ;; Git prints the destination first for exclusion ranges and combined diffs.
      (if (or (< (length trees) (length revisions)) (> (length trees) 2))
          (car trees)
        (car (last trees))))))

(defun my/magit-hash-range (function range)
  "Leave bookmark commit IDs unset for Git revision-set diffs."
  (if (and (not (string-match-p magit-range-re range))
           (my/magit-review-notes--range-tip range))
      nil
    (funcall function range)))

(defun my/magit-review-notes--snapshot (file)
  "Return (KIND RANGE TARGET) for the diff at point, with FILE when present.
KIND is `commit', `range', `index', or `worktree'.  RANGE preserves the
original revision set.  TARGET is a commit, `staged' for the index, or
nil for the working tree."
  (let ((commit (or magit-buffer-revision (magit-commit-at-point)))
        (range magit-buffer-diff-range)
        tip)
    (cond
     (commit (list 'commit nil commit))
     ((or (equal magit-buffer-diff-typearg "--cached")
          (eq (magit-diff-type) 'staged))
      '(index nil staged))
     ((and range (setq tip (my/magit-review-notes--range-tip range)))
      (list 'range range tip))
     (file '(worktree nil nil))
     (t (user-error "No commit, diff range, or file at point")))))

(defun my/magit-review-notes--context ()
  "Return file, new-side line, diff text, and commit attribution at point.
With an active region, anchor to its first line and include its text."
  (let ((region (and (use-region-p)
                     (cons (region-beginning) (region-end)))))
    (save-excursion
      (when region
        (goto-char (car region)))
      (let* ((section (magit-current-section))
             (in-hunk (and section (magit-section-match 'hunk section)))
             (file (magit-file-at-point))
             (line (and in-hunk (my/magit-review-notes--hunk-line section)))
             (anchor (my/magit-review-notes--snapshot file))
             (snapshot (car anchor))
             (range (cadr anchor))
             (tip (caddr anchor))
             (rev (if (eq snapshot 'commit)
                      tip
                    (and file line
                         (my/magit-review-notes--blame tip file line))))
             (hash (and rev (magit-rev-parse rev)))
             (context (cond (region
                             (buffer-substring-no-properties
                              (car region) (cdr region)))
                            (in-hunk
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))))
        (unless (or hash range file)
          (user-error "No commit, diff range, or file at point"))
        (list :snapshot snapshot
              :hash hash
              :abbrev (and hash (or (magit-rev-abbrev hash) hash))
              :summary (and hash (or (magit-rev-format "%s" hash) ""))
              :range range
              :tip (and range tip)
              :source-file (and (eq snapshot 'worktree)
                                (expand-file-name file (my/magit-review-notes--toplevel)))
              :file file
              :line line
              :lang "diff"
              :context (and context
                            (replace-regexp-in-string "\n+\\'" "" context))
              :repo (abbreviate-file-name
                     (my/magit-review-notes--toplevel)))))))

(defun my/magit-review-notes--file-context ()
  "Return source file, line, text, and commit attribution at point.
With an active region, anchor to its first line and include its text.
Blame the working tree inside Git. Signal a `user-error' unless visiting
a file."
  (let ((path (buffer-file-name)))
    (unless path
      (user-error "Buffer is not visiting a file"))
    (let* ((root (my/magit-review-notes--toplevel))
           (base (or root default-directory))
           (region (and (use-region-p)
                        (cons (region-beginning) (region-end))))
           (file (file-relative-name path base))
           (line (line-number-at-pos (if region (car region) (point)) t))
           ;; Blame with the absolute path: git runs from the source
           ;; buffer's `default-directory' (often a subdirectory), where a
           ;; repository-root-relative path would not resolve.
           (blamed (and root (my/magit-review-notes--blame nil path line)))
           (hash (and blamed (magit-rev-parse blamed)))
           (lang (replace-regexp-in-string "-mode\\'" ""
                                           (symbol-name major-mode)))
           (context (if region
                        (buffer-substring-no-properties (car region) (cdr region))
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
      (list :snapshot 'worktree
            :hash hash
            :abbrev (and hash (or (magit-rev-abbrev hash) hash))
            :summary (and hash (or (magit-rev-format "%s" hash) ""))
            :source-file path
            :file file
            :line line
            :lang lang
            :context (replace-regexp-in-string "\n+\\'" "" context)
            :repo (and root (abbreviate-file-name root))))))

(defun my/magit-review-notes-capture (&optional arg)
  "Append a review note for point or the active region in Magit or a source file.
With prefix ARG, prompt for a tag from `my/magit-review-notes-tags'.
Notes accumulate in a per-repository Org buffer; save with \\[save-buffer]."
  (interactive "P")
  (let* ((ctx (if (derived-mode-p 'magit-mode)
                  (my/magit-review-notes--context)
                (my/magit-review-notes--file-context)))
         (user-tag (and arg
                        (let ((choice (completing-read
                                       "Tag: " my/magit-review-notes-tags)))
                          (unless (equal choice "") choice))))
         (hash (plist-get ctx :hash))
         (range (plist-get ctx :range))
         (file (plist-get ctx :file))
         (line (plist-get ctx :line))
         (link-hash (or hash (plist-get ctx :tip)))
         (tags (append (pcase (plist-get ctx :snapshot)
                         ('index '("staged"))
                         ('worktree '("source"))
                         ('range (list (if hash "blame" "range"))))
                       (and user-tag (list user-tag))))
         (label (or (plist-get ctx :abbrev) range))
         (fileref (and file (format "~%s%s~" file
                                    (if line (format ":%s" line) ""))))
         (heading (mapconcat #'identity (delq nil (list label fileref)) " "))
         (buf (my/magit-review-notes--get-buffer)))
    (pop-to-buffer buf)
    (widen)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (insert (format "* %s%s%s\n"
                    heading
                    (if hash (format " — %s" (plist-get ctx :summary)) "")
                    (if tags (format " :%s:" (mapconcat #'identity tags ":")) "")))
    (insert ":PROPERTIES:\n")
    (when hash (insert (format ":COMMIT: %s\n" hash)))
    (when range (insert (format ":RANGE: %s\n" range)))
    (when file (insert (format ":FILE: %s\n" file)))
    (when line (insert (format ":LINE: %s\n" line)))
    (when link-hash
      (insert (format ":MAGIT: [[orgit-rev:%s::%s][%s in magit]]\n"
                      (plist-get ctx :repo) link-hash
                      (or (magit-rev-abbrev link-hash) link-hash))))
    (when-let* ((source-file (plist-get ctx :source-file)))
      (insert (format ":SOURCE: %s\n"
                      (org-link-make-string
                       (concat "file:" (abbreviate-file-name source-file)
                               (and line (format "::%s" line)))
                       (concat file (and line (format ":%s" line)))))))
    (insert ":END:\n\n")
    (when (plist-get ctx :context)
      (insert (format "#+begin_src %s\n" (or (plist-get ctx :lang) "diff"))
              (plist-get ctx :context)
              "\n#+end_src\n\n"))
    (message "Write your note, save with C-x C-s, jump back with C-x o")))

(defun my/magit-review-notes-open (&optional arg)
  "Switch to the review-notes buffer for the current repository.
With prefix ARG, prompt for the buffer name (defaulting to this repo's),
creating it if needed."
  (interactive "P")
  (pop-to-buffer
   (my/magit-review-notes--get-buffer
    (and arg (my/magit-review-notes--read-buffer "Review notes buffer: ")))))

(defun my/magit-review-notes-export (&optional arg)
  "Export this repository's notes to Markdown using ox-gfm or ox-md.
With prefix ARG, prompt for a notes buffer. Saved notes export beside
the Org file; unsaved notes prompt for a destination."
  (interactive "P")
  (let ((buf (or (my/magit-review-notes--find-buffer
                  (and arg (my/magit-review-notes--read-buffer
                            "Review notes buffer to export: " t)))
                 (user-error "No review notes buffer for this repository"))))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (let* ((backend (if (require 'ox-gfm nil t) 'gfm (progn (require 'ox-md) 'md)))
               (out (if buffer-file-name
                        (concat (file-name-sans-extension buffer-file-name) ".md")
                      (read-file-name "Export to: " default-directory nil nil
                                      "review-notes.md"))))
          (org-export-to-file backend out)
          (message "Exported to %s" out))))))

(with-eval-after-load 'magit
  (advice-add 'magit-hash-range :around #'my/magit-hash-range)
  (define-key magit-mode-map (kbd "C-c a n") #'my/magit-review-notes-capture))

(global-set-key (kbd "C-c a n") #'my/magit-review-notes-capture)
(global-set-key (kbd "C-c a r o") #'my/magit-review-notes-open)
(global-set-key (kbd "C-c a r e") #'my/magit-review-notes-export)
