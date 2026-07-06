;;; code-review.el --- Capture Magit review notes in Org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Notes are anchored to a commit, and when captured from a diff line, to
;; file:line.  Each note includes an orgit link back to Magit.  In a range diff
;; there is no single commit at point, so the touched line is blamed for
;; attribution.  Notes accumulate in a project-specific Org buffer and can be
;; exported to Markdown.
;;
;; `C-c r'     -- capture a note (in a magit buffer)
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
parsing because `magit-diff-hunk-line' is internal.  A nil result still
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
  "Return the full hash of the commit that last touched FILE:LINE at TIP.
Return nil when blame cannot resolve the line."
  (let ((out (magit-git-string "blame" "-l" "-L" (format "%d,%d" line line)
                               tip "--" file)))
    (and out
         (string-match "\\`\\^?\\([0-9a-f]\\{40\\}\\)" out)
         (match-string 1 out))))

(defun my/magit-review-notes--context ()
  "Collect the anchor for the commit or diff position at point.
Return a plist containing repository, file, line, surrounding context,
and commit attribution.  Single-commit buffers use that commit directly.
Range diffs blame the touched line to recover a commit when possible.
Line numbers refer to the new side of the diff."
  (let ((region (and (use-region-p)
                     (cons (region-beginning) (region-end)))))
    (save-excursion
      (when region
        (goto-char (car region)))
      (let* ((section (magit-current-section))
             (in-hunk (and section (magit-section-match 'hunk section)))
             (file (and in-hunk (magit-file-at-point)))
             (line (and file (my/magit-review-notes--hunk-line section)))
             (explicit (or magit-buffer-revision (magit-commit-at-point)))
             ;; Revision buffers also set `magit-buffer-range' for the
             ;; commit's own diff.  Only treat it as a range when there is no
             ;; explicit commit to use.
             (range (and (not explicit) magit-buffer-range))
             (blamed (and range file line
                          (my/magit-review-notes--blame
                           (my/magit-review-notes--range-tip range) file line)))
             (rev (or explicit blamed))
             (hash (and rev (magit-rev-parse rev)))
             (context (cond (region
                             (buffer-substring-no-properties
                              (car region) (cdr region)))
                            (in-hunk
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))))
        (unless (or hash range)
          (user-error "No commit or diff range at point"))
        (list :hash hash
              :abbrev (and hash (or (magit-rev-abbrev hash) hash))
              :summary (and hash (or (magit-rev-format "%s" hash) ""))
              :range range
              :tip (and range (magit-rev-parse
                               (my/magit-review-notes--range-tip range)))
              :blamed (and blamed t)
              :file file
              :line line
              :context (and context
                            (replace-regexp-in-string "\n+\\'" "" context))
              :repo (abbreviate-file-name
                     (my/magit-review-notes--toplevel)))))))

(defun my/magit-review-notes-capture (&optional arg)
  "Capture a review note for the commit or diff position at point.
Works on a single commit, on a commit in a log/status buffer, and in a
range diff.  With prefix ARG, also prompt for a tag from
`my/magit-review-notes-tags'.  Notes accumulate in the repository's
unsaved review-notes buffer; save it with \\[save-buffer] to persist."
  (interactive "P")
  (let* ((ctx (my/magit-review-notes--context))
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
                               (and range (not hash) "range")
                               user-tag)))
         (buf (my/magit-review-notes--get-buffer)))
    (pop-to-buffer buf)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (insert (format "* %s%s%s%s\n"
                    (or (plist-get ctx :abbrev) range)
                    (if file
                        (format " ~%s%s~" file
                                (if line (format ":%s" line) ""))
                      "")
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
    (insert ":END:\n\n")
    (when (plist-get ctx :context)
      (insert "#+begin_src diff\n"
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

;; Capture is contextual to Magit buffers.
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c r") #'my/magit-review-notes-capture))

(global-set-key (kbd "C-c a r o") #'my/magit-review-notes-open)
(global-set-key (kbd "C-c a r e") #'my/magit-review-notes-export)
