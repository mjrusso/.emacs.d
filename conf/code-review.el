;;; code-review.el --- Capture code-review notes in Org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Notes are anchored to a commit, and when captured from a diff line, to
;; file:line.  Each note includes an orgit link back to Magit.  In a range diff
;; there is no single commit at point, so the touched line is blamed for
;; attribution.  Notes can also be captured from an ordinary source buffer,
;; anchoring to the file:line at point and blaming that line to attribute a
;; commit (with an Org link back to the source).  Notes accumulate in a
;; project-specific Org buffer and can be exported to Markdown.
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
  "Directory that seeds the save/export prompt for review notes.
Notes accumulate in an unsaved review-notes buffer for each repository.
When saving or exporting, prompts start here.  If nil, prompts start at
the repository root."
  :type '(choice (const :tag "Reviewed repository root" nil)
                 (directory :tag "Fixed directory"))
  :group 'my/magit-review-notes)

(defcustom my/magit-review-notes-tags '("question" "todo" "agent" "nit" "bug")
  "Tags offered when capturing with a prefix argument."
  :type '(repeat string)
  :group 'my/magit-review-notes)

(defun my/magit-review-notes--toplevel ()
  "Return the current Git repository root, loading Magit if needed."
  (require 'magit)
  (magit-toplevel))

(defun my/magit-review-notes--buffer-name ()
  "Return the review-notes buffer name for the current repository.
Repository-specific names keep notes separate across projects and frames.
Falls back to a generic name outside a repository."
  (let ((root (my/magit-review-notes--toplevel)))
    (if root
        (format "*%s-review-notes*"
                (file-name-nondirectory (directory-file-name root)))
      "*review-notes*")))

(defun my/magit-review-notes--get-buffer (&optional name)
  "Return the review-notes buffer NAME (default: the current repo's), creating it.
A new buffer starts in `org-mode' and uses the repository root as
`default-directory', so save/export prompts start in the reviewed project."
  (let* ((name (or name (my/magit-review-notes--buffer-name)))
         (dir (file-name-as-directory
               (expand-file-name (or my/magit-review-notes-directory
                                     (my/magit-review-notes--toplevel)
                                     default-directory))))
         (existed (get-buffer name))
         (buf (get-buffer-create name)))
    (unless existed
      (with-current-buffer buf
        (org-mode)
        (setq default-directory dir)
        (insert "#+title: Review notes\n#+options: toc:nil num:nil\n")))
    buf))

(defun my/magit-review-notes--hunk-line-fallback ()
  "Return the new-side diff line at point by parsing the hunk header.
Count forward from the header to point, skipping removed lines (which
have no new-side number).  Return nil when no hunk header is found.
This is a fallback for `my/magit-review-notes--hunk-line'."
  (save-excursion
    (let ((target (line-beginning-position)))
      ;; New-side range start is the digits after `+' in the hunk header
      ;; (`@@ -a,b +c,d @@', or `@@@ ... +e,f @@@' for combined diffs).
      (when (re-search-backward "^@+ .*?\\+\\([0-9]+\\)" nil t)
        (let ((line (string-to-number (match-string 1))))
          (forward-line 1)              ; first body line is new-side line N
          (while (< (point) target)
            (unless (eq (char-after) ?-)
              (setq line (1+ line)))
            (forward-line 1))
          line)))))

(defun my/magit-review-notes--hunk-line (section)
  "Return the new-side line number for the diff position at point.
Use Magit's own helper when available, falling back to hunk-header
parsing because `magit-diff-hunk-line' is internal. A nil result still
allows the note to be anchored to the commit."
  (or (and (fboundp 'magit-diff-hunk-line)
           (ignore-errors (magit-diff-hunk-line section nil)))
      (my/magit-review-notes--hunk-line-fallback)))

(defun my/magit-review-notes--range-tip (range)
  "Return the tip revision of RANGE (the part after `..'/`...', or RANGE)."
  (if (string-match "\\.\\.\\.?\\(.*\\)\\'" range)
      (let ((tip (match-string 1 range)))
        (if (string-empty-p tip) "HEAD" tip))
    range))

(defun my/magit-review-notes--blame (tip file line)
  "Return the hash of the commit that last touched FILE:LINE.
Blame at revision TIP, or the working tree when TIP is nil. Return nil
when blame cannot resolve the line or the line is not yet committed (the
all-zero hash Git reports for uncommitted lines). A leading `^' marks a
boundary commit, for which Git drops the final hash digit to keep the
column width; that prefix is resolved to a full hash by the caller, so
accept fewer than 40 digits here."
  (let* ((loc (format "%d,%d" line line))
         (out (apply #'magit-git-string
                     `("blame" "-l" "-L" ,loc ,@(and tip (list tip))
                       "--" ,file))))
    (and out
         (string-match "\\`\\^?\\([0-9a-f]\\{7,40\\}\\)" out)
         (let ((hash (match-string 1 out)))
           (unless (string-match-p "\\`0+\\'" hash)
             hash)))))

(defun my/magit-review-notes--context ()
  "Collect the anchor for the commit or diff position at point.
Return a plist containing repository, file, line, surrounding context,
and commit attribution.  Single-commit buffers use that commit directly.
Range diffs blame the touched line to recover a commit when possible.
A working-tree diff (e.g. the unstaged or staged changes in the status
buffer) has neither a commit nor a range, so it is anchored to file:line
and the working-tree line is blamed for attribution.  Line numbers refer
to the new side of the diff."
  (let ((region (and (use-region-p)
                     (cons (region-beginning) (region-end)))))
    (save-excursion
      (when region
        (goto-char (car region)))
      (let* ((section (magit-current-section))
             (in-hunk (and section (magit-section-match 'hunk section)))
             (file (magit-file-at-point))
             (line (and in-hunk (my/magit-review-notes--hunk-line section)))
             (explicit (or magit-buffer-revision (magit-commit-at-point)))
             ;; Revision buffers also set `magit-buffer-range' for the
             ;; commit's own diff.  Only treat it as a range when there is no
             ;; explicit commit to use.
             (range (and (not explicit) magit-buffer-range))
             ;; No commit and no range, but a file at point: a working-tree
             ;; diff (unstaged/staged changes).  Anchor to the file, blaming
             ;; the working-tree line for a commit as `--file-context' does.
             (worktree (and (not explicit) (not range) file))
             (blamed (cond ((and range file line)
                            (my/magit-review-notes--blame
                             (my/magit-review-notes--range-tip range) file line))
                           ((and worktree line)
                            (my/magit-review-notes--blame nil file line))))
             (rev (or explicit blamed))
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
        (list :hash hash
              :abbrev (and hash (or (magit-rev-abbrev hash) hash))
              :summary (and hash (or (magit-rev-format "%s" hash) ""))
              :range range
              :tip (and range (magit-rev-parse
                               (my/magit-review-notes--range-tip range)))
              :blamed (and blamed range t)
              :source (and worktree t)
              :file file
              :line line
              :lang "diff"
              :context (and context
                            (replace-regexp-in-string "\n+\\'" "" context))
              :repo (abbreviate-file-name
                     (my/magit-review-notes--toplevel)))))))

(defun my/magit-review-notes--file-context ()
  "Collect the anchor for the source position at point in a file buffer.
Return a plist shaped like `my/magit-review-notes--context'.  Inside a
Git repository the working-tree line is blamed to attribute a commit;
when the line is not yet committed, or the file is outside any
repository, the note is anchored to file:line alone. Signal a
`user-error' when the buffer is not visiting a file."
  (let ((path (buffer-file-name)))
    (unless path
      (user-error "Buffer is not visiting a file"))
    (let* ((root (my/magit-review-notes--toplevel))
           ;; Anchor paths and the source link to the repository root when
           ;; there is one, otherwise to `default-directory' (which is also
           ;; what the notes buffer adopts), so the link stays resolvable.
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
      (list :hash hash
            :abbrev (and hash (or (magit-rev-abbrev hash) hash))
            :summary (and hash (or (magit-rev-format "%s" hash) ""))
            :range nil
            :tip nil
            :blamed nil
            :source t
            :file file
            :line line
            :lang lang
            :context (replace-regexp-in-string "\n+\\'" "" context)
            :repo (and root (abbreviate-file-name root))))))

(defun my/magit-review-notes-capture (&optional arg)
  "Capture a review note for the position at point.
In a Magit buffer this anchors to the commit or diff position: a single
commit, a commit in a log/status buffer, or a blamed line in a range
diff.  In a file-visiting buffer it anchors to the file:line at point,
blaming the working-tree line to attribute a commit.  With prefix ARG,
also prompt for a tag from `my/magit-review-notes-tags'.  Notes
accumulate in the repository's unsaved review-notes buffer; save it with
\\[save-buffer] to persist."
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
         (tags (delq nil (list (and (plist-get ctx :blamed) "blame")
                               (and (plist-get ctx :source) "source")
                               (and range (not hash) "range")
                               user-tag)))
         (label (or (plist-get ctx :abbrev) range))
         (fileref (and file (format "~%s%s~" file
                                    (if line (format ":%s" line) ""))))
         (heading (mapconcat #'identity (delq nil (list label fileref)) " "))
         (buf (my/magit-review-notes--get-buffer)))
    (pop-to-buffer buf)
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
    (when (and (plist-get ctx :source) file)
      (insert (if line
                  (format ":SOURCE: [[file:%s::%s][%s:%s]]\n" file line file line)
                (format ":SOURCE: [[file:%s][%s]]\n" file file))))
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
  (let ((name (if arg
                  (read-buffer "Review notes buffer: "
                               (my/magit-review-notes--buffer-name))
                (my/magit-review-notes--buffer-name))))
    (pop-to-buffer (my/magit-review-notes--get-buffer name))))

(defun my/magit-review-notes-export (&optional arg)
  "Export the current repository's review notes to Markdown.
With prefix ARG, prompt for the notes buffer.  Use `ox-gfm' when
installed, falling back to `ox-md'.  Org excludes property drawers from
export by default, leaving orgit links and full hashes out of the
Markdown.  Unsaved buffers prompt for the output file."
  (interactive "P")
  (let* ((name (if arg
                   (read-buffer "Review notes buffer to export: "
                                (my/magit-review-notes--buffer-name) t)
                 (my/magit-review-notes--buffer-name)))
         (buf (or (get-buffer name)
                  (user-error "No review notes buffer %s" name))))
    (with-current-buffer buf
      (let* ((backend (if (require 'ox-gfm nil t) 'gfm (progn (require 'ox-md) 'md)))
             (out (if buffer-file-name
                      (concat (file-name-sans-extension buffer-file-name) ".md")
                    (read-file-name "Export to: " default-directory nil nil
                                    "review-notes.md"))))
        (org-export-to-file backend out)
        (message "Exported to %s" out)))))

;; Capture adapts to context (e.g. a commit or diff in Magit, or the file:line
;; at point in a source buffer). Capture is bound both in `magit-mode-map' (in
;; case a global `C-c a …' key is shadowed there) and globally, so it can also
;; be invoked from non-Magit buffers.
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c a n") #'my/magit-review-notes-capture))

(global-set-key (kbd "C-c a n") #'my/magit-review-notes-capture)
(global-set-key (kbd "C-c a r o") #'my/magit-review-notes-open)
(global-set-key (kbd "C-c a r e") #'my/magit-review-notes-export)
