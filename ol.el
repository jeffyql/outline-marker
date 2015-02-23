(require 'om)
(require 'org-faces)

(defconst ol-self-insert-commands-default
  '(
    ("Outline Navigation")
    ("a" . org-beginning-of-line)
    ("b" . ol-prev-same-level)
    ("c" . ol-create-heading)
    ("d" . ol-move-down)
    ("e" . end-of-line)
    ("f" . ol-next-same-level)
    ("g" . ol-refresh)
    ("h" . ol-backward)
    ("i" . ol-insert-mode)
    ("j" . ol-enter-subtree)
;;    ("j" . next-line)
    ("k" . ol-goto-parent-subtree)
;;    ("k" . previous-line)
    ("l" . ol-forward)
    ("m" . ol-goto-mark)
    ("n" . ol-select-next)
    ("o" . ol-open-at-point)
    ("p" . ol-select-prev)
    ("q" . ol-show-list)
    ("r" . ol-refile)
    ("s" . ol-goto-node-body-beginning)
    ("t" . ol-goto-root)
    ("u" . ol-move-up)
    ("v" . ol-goto-node-end)
    ("w" . ol-edit-node)
    ("x" . ol-set-sticky-mark)
    ("y" . ol-add-link)
    ("z" . next-line)
    ("A" . ol-archive)
    ("D" . ol-control-delete)
    ("H" . my-backward-same-syntax) 
    ("J" . next-line) 
    ("K" . previous-line) 
    ("L" . my-forward-same-syntax)
    ("U" . ol-back-to-parent)
    (">" . org-timestamp-down-day)
    ("<" . org-timestamp-up-day)
    ("." . ol-toggle-timestamp)
    (";" . ol-link-in)
    ("'" . ol-link-out)
    ("0" . ol-bookmark-command)
    ("1" . ol-bookmark-command)
    ("2" . ol-bookmark-command)
    ("3" . ol-bookmark-command)
    ("4" . ol-bookmark-command)
    ("5" . ol-bookmark-command)
    ("6" . ol-bookmark-command)
    ("7" . ol-bookmark-command)
    ("8" . ol-bookmark-command)
    ("9" . ol-bookmark-command)
    ("/" . undo)
    ("?" . ol-occur)
    (" " . self-insert-command)
    ("-" . ol-hypene)
    )
  "The default speed commands.")

(defvar ol-command-mode 'normal)
(defvar ol-subtree-show-state 'fold) ;; fold, outline, and show
(defvar ol-command nil)

(defvar ol-mode-line-cookie nil)
(defvar ol-header-line-cookie nil)

(defvar ol-highlight-overlays nil
  "List of overlays used for something.")
(make-variable-buffer-local 'ol-highlight-overlays)

(defvar ol-subtree-root nil)
(defvar ol-mark nil)  ;; FIXME: change to marker
(defvar ol-mark-sticky nil)
(defvar ol-mark-offset 0)
(defvar ol-mark-display-mode 'parent)
(defvar ol-mark-heading nil)

(defvar ol-mode-line-hl nil)
(make-variable-buffer-local 'ol-mode-line-hl)

(defvar ol-self-insert-command-hook
  '(ol-get-self-insert-command))
(defvar ol-main-file nil)
(defvar ol-numbered-headings-alist nil)
(defvar ol-remember-heading-id "d785d183-0193-43f8-9aaf-dbb7c2bafe7a")
(defvar ol-current-time (current-time))
(defvar ol-today (time-to-days (current-time)))
(defvar ol-default-file nil)
(defvar ol-current-ib nil)
(defvar ol-archive-heading-word "ARCHIVE")
(defvar ol-hidden-heading-word "HIDDEN")
(defvar ol-decoration-mode 'timestamp)
(defvar ol-starting-date (time-to-days (current-time)))
(defvar ol-in-ndays 0)
(defvar ol-ts-bit 4)
(defvar ol-editing-bit 16)
(defvar ol-todo-bit 64)
(defvar ol-editing-nodes-alist nil)
(defvar ol-days 0)

(defvar ol-number-key-alist '(
                                    (?! . ?1)
                                    (?@ . ?2)
                                    (?# . ?3)
                                    (?$ . ?4)
                                    (?% . ?5)
                                    (?^ . ?6)
                                    (?& . ?7)
                                    (?* . ?8)
                                    (?\( . ?9)
                                    (?\) . ?0)))

(defvar ol-bookmark-state 'get)
(defvar ol-save-timer nil)
(defvar ol-sync-timer nil)
(defvar ol-tmp-file "~/tmp.org")

(defvar ol-show-ending-empty-line t)
;(defvar ol-ido nil)

(defconst ol-ts-regexp "[[]\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).+?[]]")

(defconst ol-st-heading "^\\*+[ \t].*?\\([[]\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).+?[]]\\)[ \t]*$")
(defconst ol-heading-regexp-1 "^\\*+ \\(.*?\\)\\([[]\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).+?[]]\\)?\\([ \t]*\\)$")

(defconst ol-heading-regexp-2 "^\\*+ \\(\\(.*?\\)\\([ \t]*\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]\\)*[ \t]*?\\)\\([[]\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).+?[]]\\)?\\([ \t]*\\)$")
  
(defconst numbered-list-option-re "^#\\+numbered-list:[ \t]*\\(.*\\)$")

(defconst numbered-list-elem-re "\\([0-9a-fA-F-]+\\)::\\(\\S-+\\)$")

(defconst todo-list-option-re "^#\\+todo-list:[ \t]*\\(.*?\\)[ \t]*$")

(defconst todo-list-elem-re "[ \t]*\\([0-9a-fA-F-]+\\)::\\(\\S-+\\)[ \t]*$")

(defconst ol-drawer-regexp "^[ \t]*:PROPERTIES:[ \t]*$")

(org-copy-face 'secondary-selection 'ol-active-timestamp-face
  "Face marking an active timestamp."
  :background "NavajoWhite")
;  :background "LightSalmon")
(org-copy-face 'secondary-selection 'ol-inactive-timestamp-face
  "Face marking an active timestamp."
  :background "DarkKhaki")

(org-copy-face 'secondary-selection 'ol-editing-face
  "Face marking an active timestamp."
  :background "Red")

(define-derived-mode ol-mode org-mode "Org-Lite"
  "Orgmode based viewing mode"
  (make-local-variable 'ol-subtree-root)
  (make-local-variable 'mode-line-format)
  (make-local-variable 'ol-mode-line-cookie)
  (make-local-variable 'ol-header-line-cookie)
  (make-local-variable 'ol-command-mode)
  (setq mode-line-format (copy-tree mode-line-format))
  )

;; (define-key ol-mode-map "A" 'ol-save-all-buffers)
;; (define-key ol-mode-map "B" 'ol-prev-same-level)
;; (define-key ol-mode-map "C" 'ol-copy)
;; (define-key ol-mode-map "D" 'ol-refile-to-pool)
;; (define-key ol-mode-map "F" 'ol-next-same-level)
;; (define-key ol-mode-map "H" 'ol-refile-by-scheduling-status)
;; (define-key ol-mode-map "O" '(lambda () (interactive) (org-open-at-point '(4))))
;; (define-key ol-mode-map "S" 'ol-select-prev-active)
;; (define-key ol-mode-map "U" 'ol-bubble-up)
;; (define-key ol-mode-map "Z" 'ol-clone-other-window)

;; (define-key ol-mode-map "X" 'ol-delete-all) 
;; (define-key ol-mode-map "{" 'ol-earlier-fast)
;; (define-key ol-mode-map "}" 'ol-later-fast)
;; (define-key ol-mode-map ">" nil)

;; (define-key ol-mode-map "<" 'ol-earlier)
;; (define-key ol-mode-map ";" 'ol-toggle-timestamp) 
;; (define-key ol-mode-map ">" 'ol-forget) 

(define-key ol-mode-map (kbd "C-k") 'ol-kill-region)
(define-key ol-mode-map [(meta up)] 'ol-move-up)
(define-key ol-mode-map [(meta down)] 'ol-move-down)
;; (define-key ol-mode-map [down] 'org-next-line-or-heading)
;; (define-key ol-mode-map [right] 'ol-forward-char-or-heading)
;; (define-key ol-mode-map [left] 'ol-backward-char-or-heading)

(define-key ol-mode-map [tab] 'ol-toggle-outline)

(defvar ol-mouse-map (make-sparse-keymap))
(define-key org-mouse-map [mouse-2] 'ol-open-at-mouse)

(substitute-key-definition 'self-insert-command 'ol-self-insert-command
                           ol-mode-map global-map)

(defun ol-pre-command ()
  (unless before-change-functions
    (error "before-change-functions lost"))
  (unless after-change-functions
    (error "after-change-functions lost"))
  (unless post-command-hook
    (error "post command hook lost")))

(defun ol-callback (beg end node)
  (when (/= 1 beg)
    (goto-char beg)
    ;; (unless (looking-at ol-heading-regexp-1)
    ;;   (error "incorrect heading syntax at line %d" (line-number-at-pos)))
    (if (re-search-forward ol-ts-regexp end t)
        (om-set-sparse-bit node ol-ts-bit))))

(defun ol-update-callback ()
  (goto-char om-beg)
  (looking-at ol-heading-regexp-1)
  (if (match-beginning 2)
      (unless (om-node-bit-on om-node ol-ts-bit)
        (om-set-sparse-bit om-node ol-ts-bit))
    (if (om-node-bit-on om-node ol-ts-bit)
        (om-clear-sparse-bit om-node ol-ts-bit))))

(defun ol-add-buffer (&optional buf)
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (file (buffer-file-name buf))
        root str tail todos id node)
    (unless (eq buf (current-buffer))
      (switch-to-buffer buf))
;    (unless (eq major-mode 'org-mode)
;      (error "current buffer is not a org-mode buffer"))
    ;(om-check-file-format)
    (setq om-callback 'ol-callback
          om-update-callback 'ol-update-callback)
    (ol-mode)
    ;;(add-to-invisibility-spec 'fold)
    (setq root (om-create-substree 0 1 (point-max)))
    (om-init root)
    (setq tail (memq 'mode-line-buffer-identification mode-line-format))
    (setcdr tail (cons 'ol-mode-line-hl (cdr tail)))
    (setq ol-subtree-root root)
    ;; (when (setq todos (ol-get-todo-list))
    ;;   (while todos
    ;;     (setq id (car todos)
    ;;           todos (cdr todos)
    ;;           node (om-id-lookup id))
    ;;     (om-set-sparse-bit node ol-todo-bit)))
    (om-add-hooks)
    (face-remap-add-relative 'mode-line :background "seashell")
    (face-remap-add-relative 'header-line :background "seashell")
    (ol-show nil (point-min))
    (if (om-first-child root)
        (forward-char (om-offset-from-parent (om-first-child root))))
    ))

(defun ol-get-todo-list (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let ((bound (cdr (om-node-range
                       om-root)))
          (beg 0)
          (end 0)
          current list tl string length id node update)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward todo-list-option-re bound t)
            (setq string (match-string-no-properties 1)
                  length (length string))
            (when (< 0 length)
              (while (< end length)
                (setq end (or (string-match "|" string beg) length)
                      id (substring string beg end)
                      beg (1+ end)
                      list (cons id list)))
              (setq current list)
              (while current
                (setq id (car current)
                      current (cdr current))
                (if (and (setq node (om-id-lookup id))
                         (eq (current-buffer) (om-buf node)))
                    (setq tl (cons id tl))
                  (setq update t)))
              (if update
                  (ol-save-todo-list tl))
              tl)))))))

(defun ol-get-due-list (buf)
  (with-current-buffer buf
    (let* ((list nil)
           (callback
            (lambda (node beg end)
              (let (id)
                (when (ol-timestamp-is-active beg end)
                  (setq id (om-node-id-get-create node)
                        list (cons id list)))))))
      (save-excursion
        (save-restriction
          (widen)
          (om-traverse-sparse-subtree om-root ol-ts-bit callback)
          list)))))
  
(defun ol-quit-ol-mode (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (file (buffer-file-name buf))
         (pair (assoc buf om-root-alist))
         (root (cdr pair)))
    (with-current-buffer buf
      (unless (and root (listp root) (eq root om-root))
        (error "data is out of sync"))
      (if (eq om-last-buffer (current-buffer))
          (setq om-last-buffer nil))
      (mapc 'delete-overlay ol-highlight-overlays)
      (setq ol-highlight-overlays nil)
      ;(om-ring-remove root)
      (setq om-root-alist (delq pair om-root-alist))
      (remove-hook 'post-command-hook 'om-post-command t)
      ;(use-local-map org-mode-map)
      )))

(defun ol-sync-buffer-with-file ()
  (interactive)
  (let (file buffer-A buffer-B)
    (if (verify-visited-file-modtime (current-buffer))
        ;; file is not touched by other parties
        (if (not (buffer-modified-p))
            (message "(buffer and file are already in sync)")
          (save-buffer)
          (message "(file is synced to the modified buffer content)"))
      (setq file (buffer-file-name))
      (if (not (buffer-modified-p))
          (when (y-or-n-p "(file is changed, resync the buffer?)")
            (ol-kill-buffer)
            (switch-to-buffer (find-file-noselect file))
            (ol-add-buffer (current-buffer))
            (message "(buffer is resynced to the updated file)"))
        (when (y-or-n-p "buffer and file are out of sync, do a diff?")
          (ol-quit-ol-mode)
          (widen)
          (show-all)
          (setq buffer-A (current-buffer))
          (write-file ol-tmp-file)
          (setq buffer-B (find-file-noselect file))
          (switch-to-buffer-other-window buffer-B)
          (show-all)
          (ediff-buffers buffer-A buffer-B))))))

(defun ol-kill-buffer (&optional buf)
  (interactive)
  (ol-quit-ol-mode buf)
  (kill-buffer buf))

(defun ol-delete-all ()
  (interactive)
  (let ((current om-root-alist)
        buf)
    (while current
      (setq buf (caar current))
      (if (bufferp buf)
          (with-current-buffer buf
            (setq om-root nil)
            ))
      (setq current (cdr current)))
    (setq om-root-alist nil
          om-last-buffer nil
          ol-mark nil)))

(defun ol-toggle-outline ()
  (interactive)
  (cond
   ((org-at-item-p) (org-cycle) (recenter))
   ((ol-on-heading-p)
    (if (eq om-outline-node ol-subtree-root)
        (setq ol-subtree-show-state
              (if (and (eq this-command last-command)
                       (eq ol-subtree-show-state 'show))
                  'outline 'show))
      (setq ol-subtree-show-state
            (if (eq this-command last-command)
                (if (eq ol-subtree-show-state 'show)
                    'fold
                  (if (and (eq ol-subtree-show-state 'fold)
                           (om-first-child om-node))
                      'outline
                    'show))
              (if (om-first-child om-node)
                  'outline
                'show))))
    (ol-show-outline ol-subtree-show-state))
    (t (call-interactively 'indent-for-tab-command))))

(defun ol-show-outline (state)
  (let ((send (if (and ol-show-ending-empty-line
                       (= (char-before (1- om-subtree-end)) ?\n))
                  (- om-subtree-end 2) (1- om-subtree-end)))
        (end (if (and ol-show-ending-empty-line
                       (= (char-before (1- om-end)) ?\n))
                  (- om-end 2) (1- om-end))))
    (save-excursion
      (goto-char om-beg)
      (cond
       ((eq state 'fold)
        (outline-flag-region (1- om-beg) (point-at-eol) nil)
        (outline-flag-region (point-at-eol) send t))
       ((eq state 'outline)
        (outline-flag-region (1- om-beg) end nil)
        (if (/= om-end om-subtree-end)
            (outline-flag-region om-end send t))
        (if (re-search-forward org-drawer-regexp end t)
            (org-flag-drawer t))
        (om-run-children-forward om-node 'ol-flag-outline))
       (t (outline-flag-region (1- om-beg) send nil))))))

;; Narrow to the subtree with ol-subtree-root as the root.
;; ol-cursor-node must be a direct child of the root.
;; If ol-cursor-node is not set, it will be set to the first
;; child of the the root if exist, otherwise to the root itself.
;; Move the cursor to the heading of ol-cursor-node.
(defun ol-show (&optional bnode pos buf)
  (interactive)
  (let* ((bnode (or bnode ol-subtree-root))
         (range (om-subtree-range bnode))
         (beg (car range))
         (end (cdr range))
         (first (om-first-child bnode))
         (node-end (if first
                       (+ beg (om-offset-from-parent first))
                     end))
                      
         window)
    (if buf
        (unless (eq buf (current-buffer))
          (if (setq window (get-buffer-window buf))
              (select-window window)
            (switch-to-buffer buf))))
    (widen)
    (setq ol-subtree-root bnode) ;; set after switch buffer
    (ol-update-current-time)
    (ol-set-extra)
    ;(mapc 'delete-overlay ol-highlight-overlays)
    ;(setq ol-highlight-overlays nil)
    (outline-flag-region (point-min) (point-max) t)
    (if (om-parent bnode)
      (narrow-to-region beg (1- end)))
    (goto-char beg)
    (outline-flag-region beg (1- node-end) nil)
    (if (re-search-forward ol-drawer-regexp (1- node-end) t)
        (org-flag-drawer t))
    (om-run-children-forward bnode 'ol-flag-outline)
    (if (and (number-or-marker-p pos) (>= pos beg) (and (< pos end))) 
        (goto-char pos)
      (goto-char (point-min)))
    (recenter)
   ))

(defun ol-remove-ts-overlay (pos)
  (let ((olist (overlays-at pos)))
    (mapc 'delete-overlay olist)
    (mapc (lambda (x)
            (setq ol-highlight-overlays
                  (delq x ol-highlight-overlays)))
          olist)))

(defun ol-highlight-ts (beg)
  (let (ov)
    (goto-char beg)
    (when (looking-at ol-st-heading)
      (setq ov (make-overlay (match-beginning 1)
                             (match-end 1)))
      (if (ol-timestamp-is-active beg)
          (overlay-put ov 'face 'ol-active-timestamp-face)
        (overlay-put ov 'face 'ol-inactive-timestamp-face)) 
      (setq ol-highlight-overlays
            (cons ov ol-highlight-overlays)))))

(defun ol-flag-outline (node beg)
  (goto-char beg)
  (outline-flag-region (if (looking-back "\n[ \t]*\n")
                           (match-beginning 0) (1- beg))
                       (point-at-eol) nil))

(defun ol-get-self-insert-command (keys)
  (cdr (assoc keys ol-self-insert-commands-default)))

(defun ol-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (eq ol-command-mode 'normal)
      (progn
        (setq ol-command
              (run-hook-with-args-until-success
               'ol-self-insert-command-hook (this-command-keys)))
        (cond
         ((commandp ol-command)
          (setq this-command ol-command)
          (call-interactively ol-command))
         ((functionp ol-command)
          (funcall ol-command))
         ((and ol-command (listp ol-command))
          (eval ol-command))
         (t (call-interactively 'ol-self-insert-command))))
    (if (and
         (org-table-p)
         (progn
           ;; check if we blank the field, and if that triggers align
           (and (featurep 'org-table) ;;org-table-auto-blank-field  FIXME: un-commentout this
                (member last-command
                        '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c yas/expand))
                (if (or (equal (char-after) ?\ ) (looking-at "[^|\n]*  |"))
                    ;; got extra space, this field does not determine column width
                    (let (org-table-may-need-update) (org-table-blank-field))
                  ;; no extra space, this field may determine column width
                  (org-table-blank-field)))
           t)
         (eq N 1)
         (looking-at "[^|\n]*  |"))
        (let (org-table-may-need-update)
          (goto-char (1- (match-end 0)))
          (delete-char -1)
          (goto-char (match-beginning 0))
          (self-insert-command N))
      (setq org-table-may-need-update t)
      (self-insert-command N)
      (org-fix-tags-on-the-fly)
      (if org-self-insert-cluster-for-undo
          (if (not (eq last-command 'org-self-insert-command))
              (setq org-self-insert-command-undo-counter 1)
            (if (>= org-self-insert-command-undo-counter 20)
                (setq org-self-insert-command-undo-counter 1)
              (and (> org-self-insert-command-undo-counter 0)
                   buffer-undo-list (listp buffer-undo-list)
                   (not (cadr buffer-undo-list)) ; remove nil entry
                   (setcdr buffer-undo-list (cddr buffer-undo-list)))
              (setq org-self-insert-command-undo-counter
                    (1+ org-self-insert-command-undo-counter))))))))

(defun ol-move-down ()
  (interactive)
  (let (pos)
    (cond
     ((ol-on-heading-p)
      (if (not (om-next om-node))
          (message "Cannot move this heading further down")
        (if (eq om-node ol-subtree-root)
            (message "Move not permitted on top node")
          (let ((inhibit-read-only t))
            (setq pos (om-move-down))
            (ol-show)
            (goto-char pos)))))
     ((org-at-item-p) (call-interactively 'org-move-item-down))
     (t (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0)))))

(defun ol-move-up ()
  (interactive)
  (let (pos)
    (cond
     ((ol-on-heading-p)
      (if (not (om-prev om-node))
          (message "Cannot move this heading further up")
        (if (eq om-node ol-subtree-root)
            (message "Move not permitted on top node")
          (let ((inhibit-read-only t))
            (setq pos (om-move-up))
            (ol-show)
            (goto-char pos)))))
     ((org-at-item-p)
      (call-interactively 'org-move-item-up))
     (t (transpose-lines 1) (beginning-of-line -1)))))

;; (defun ol-insert-child-last (txt)
;;   (let* ((first (om-first-child ol-subtree-root)))
;;     (if first
;;         (om-insert-entry-after txt (om-prev first))
;;       (om-insert-entry-under txt ol-subtree-root))
;;     (ol-show)
;;     (goto-char om-heading-position)))

(defun ol-update-date (inc)
  (ol-change-date om-outline-node inc)
    ;(beginning-of-line)
    ;(ol-highlight-ts (point))
  (beginning-of-line))
        
(defun ol-change-date (node inc)
  (let* ((beg (om-beg node))
         (today (time-to-days (current-time)))
         year month day time date dinc tinc end stamp)
    (goto-char beg)
    (if (not (re-search-forward ol-ts-regexp (point-at-eol) t))
        (progn
          (goto-char (point-at-eol))
          (setq stamp (format-time-string "[%Y-%m-%d %a]"))
          (insert (concat stamp " "))
          (om-set-sparse-bit node ol-ts-bit)
          (ol-get-todo-list))
      (setq  year  (string-to-number (match-string 1))
             month (string-to-number (match-string 2))
             day   (string-to-number (match-string 3))
             time (apply 'encode-time (list 0 0 0 day month year))
             date (time-to-days time)
             beg (match-beginning 0)
             end (match-end 0)
             dinc (if (>= today date)
                      (if (> 0 inc)
                          0
                        (1+ (- today date)))
                    inc)
             date (+ date dinc)
             tinc (days-to-time dinc)
             time (time-add time tinc)
             stamp (format-time-string "[%Y-%m-%d %a]" time))
      ;(ol-remove-ts-overlay pos)
      (goto-char beg)
      (delete-region beg end)
      (insert stamp))
      stamp))



(defun ol-later ()
  (interactive)
  (ol-update-date 1))

(defun ol-earlier ()
  (interactive)
  (ol-update-date -1))

;; (defun ol-later-fast ()
;;   (interactive)
;;   (ol-change-date ))

;; (defun ol-earlier-fast ()
;;   (interactive)
;;   (ol-change-date 'down t))

(defun ol-cycle-ts-range ()
  (interactive)
  (cond
   ((= ol-days 0)
    (setq ol-days 7))
   ((= ol-days 7)
    (setq ol-days 30))
   ((= ol-days 30)
    (setq ol-days 0))
   (t
    (setq ol-days 0)))
  (ol-show))

(defun ol-timestamp-is-active (beg end &optional days)
  (let ((today (time-to-days (current-time)))
        year month day time date)
    (goto-char beg)
    (catch 'exit
      (while (re-search-forward ol-ts-regexp end t)
        (setq year  (string-to-number (match-string 1))
              month (string-to-number (match-string 2))
              day   (string-to-number (match-string 3))
              time (apply 'encode-time (list 0 0 0 day month year))
              date (time-to-days time))
        (if (>= today (- date (or days 0)))
            (throw 'exit t))))))
          
(defun ol-later-today (pair)
  (and (time-less-p ol-current-time (car pair))
       (= ol-today (cdr pair))))

(defun ol-in-7-days (pair)
  (let ((date (cdr pair)))
    (and (> date ol-today)
         (< (- date ol-today) 7))))

(defun ol-in-30-days (pair)
  (let ((date (cdr pair)))
    (and (> date ol-today)
         (< (- date ol-today) 30))))

(defun ol-delete-timestamp (node)
  (let ((beg (om-beg node)))
    (goto-char beg)
    (looking-at ol-heading-regexp-1)
    (when (match-beginning 2)
      (delete-region (match-beginning 2) (match-end 2))
      (om-clear-sparse-bit node ol-ts-bit))))

(defun ol-toggle-timestamp ()
  (interactive)
  (let* ((cnode om-outline-node)
         (range (om-node-range cnode))
         (beg (car range))
         (end (cdr range))
         (tsr ol-ts-regexp)
         (ans (or (looking-at tsr)
                  (save-excursion
		    (skip-chars-backward "^[<\n\r\t")
		    (if (> (point) (point-min)) (backward-char 1))
		    (and (looking-at tsr)
			 (> (- (match-end 0) (point)) -1))))))
    (if ans
        (progn
          (delete-region (match-beginning 0) (match-end 0))
          (save-excursion
            (goto-char beg)
            (unless (re-search-forward ol-ts-regexp end t)
                (om-clear-sparse-bit cnode ol-ts-bit))))
      (insert (format-time-string " [%Y-%m-%d %a] "))
      (unless (om-node-bit-on cnode ol-ts-bit)
        (om-set-sparse-bit cnode ol-ts-bit)))))

(defun ol-done ()
  (interactive)
  (let* ((cnode om-outline-node)
         (level (om-level cnode))
         (range (om-node-range cnode))
         (file (buffer-file-name))
         (sl (ol-get-todo-list))
         (beg (car range))
         (pos (point))
         ts pair entry)
    (goto-char beg)
    (looking-at ol-heading-regexp-1)
    (unless (setq beg (match-beginning 2))
      (error "no timestamp on the heading"))
      ;(ol-remove-ts-overlay beg)
    (setq ts (match-string 2))
    (delete-region beg (match-end 6))
    (goto-char (point-at-eol))
    (insert (concat "\n" (make-string (1+ level) ?\s) ts))
    (om-clear-sparse-bit cnode ol-ts-bit)
    ;(ol-remove-subtree-todo cnode)
    (ol-show)
    (goto-char pos)))

(defun ol-menu ()
  (interactive)
  (let (char buf file node window)
    (if (not om-root-alist)
        (if (not ol-main-file)
            (error "org lite buffer is not found")
          (setq file ol-main-file
                buf (find-buffer-visiting file))
          (unless buf
            (setq buf (find-file-noselect file))
            (unless (bufferp buf)
              (error "file: %s not found" file))
            (ol-add-buffer buf))
          (ol-init-sync)
          (ol-show-list)
          )
      (if (eq major-mode 'ol-mode)
          (if (buffer-base-buffer)
              (kill-buffer)
            (unless (eq ol-command-mode 'normal)
              (ol-normal-mode)))
        (setq buf (or om-last-buffer
                      (caar om-root-alist)))
        (if (not (bufferp buf))
            (error "buffer not found"))
        (if (setq window (get-buffer-window buf))
            (select-window window)
          (switch-to-buffer buf))))))

(defun ol-save-all-buffers ()
  (interactive)
  (let ((current om-root-alist)
        file buf buf-saved)
    (while current
      (setq buf (caar current))
      (if (bufferp buf)
          (with-current-buffer buf
            (when (buffer-modified-p)
              (save-buffer)
              (setq buf-saved t))))
      (setq current (cdr current)))
    (unless buf-saved
      (message "(No changes need to be saved)"))))

(defun ol-save-all-buffers-quite ()
  (let ((current om-root-alist)
        file buf)
    (while current
      (setq buf (caar current))
      (if (bufferp buf)
          (with-current-buffer buf
            (if (buffer-modified-p)
                (save-buffer))))
      (setq current (cdr current)))))

(defun ol-back-to-parent ()
  (interactive)
  (unless (eq om-outline-node ol-subtree-root)
    (goto-char (- om-heading-position
                  (om-offset-from-parent om-outline-node)))))

(defun ol-next-visible-node ()
  (let* ((cnode om-outline-node)
         (bnode ol-subtree-root)
         (first (om-first-child cnode))
         (pos (if first (+ om-heading-position
                           (om-offset-from-parent first)))))
    (if (and first (not (outline-invisible-p pos)))
        first
      (om-subtree-next cnode bnode))))

(defun ol-select-prev ()
  (interactive)
  (if (ol-on-heading-p)
      (let ((node om-outline-node)
            prev)
        (if (eq om-outline-node ol-subtree-root)
            (if (not (om-first-child ol-subtree-root))
                (message "no prev visible heading")
              (goto-char (om-prev-visible ol-subtree-root (point-min))))
          (goto-char (point-at-bol))
          (setq prev (om-prev node))
          (if prev
              (goto-char (om-prev-visible prev
                                          (+ (- (point) (om-offset-from-parent node))
                                             (om-offset-from-parent prev))))
            (goto-char  (- (point) (om-offset-from-parent node))))))
    (let (pos)
      (save-excursion
        (beginning-of-line)
        (catch 'exit
          (while (re-search-backward (org-item-beginning-re) om-beg t)
            (when (not (outline-invisible-p (match-beginning 0)))
              (setq pos (match-beginning 0))
              (throw 'exit nil))))
        (unless pos
          (goto-char (1- om-end))
          (if (re-search-backward (org-item-beginning-re) om-beg t)
              (setq pos (match-beginning 0)))))
      (if pos
          (goto-char pos)
        (message "no prev item")))))

(defun ol-select-next ()
  (interactive)
  (let (cnode pos)
    (if (ol-on-heading-p)
        (if (and (eq om-outline-node ol-subtree-root)
                 (null (om-first-child ol-subtree-root)))
            (message "no next visible heading")
          (setq cnode (or (ol-next-visible-node)
                          ol-subtree-root)
                pos (om-beg cnode))
          (goto-char pos))
      (save-excursion
        (end-of-line)
        (catch 'exit
          (while (re-search-forward (org-item-beginning-re) (1- om-end) t)
            (when (not (outline-invisible-p (match-beginning 0)))
              (setq pos (match-beginning 0))
              (throw 'exit nil))))
        (unless pos
          (goto-char om-beg)
          (if (re-search-forward (org-item-beginning-re) (1- om-end) t)
              (setq pos (match-beginning 0)))))
      (if pos
          (goto-char pos)
        (message "no next item")))))

;; (defun ol-find-in-range-node (current root days)
;;   (let* ((pos (om-beg current))
;;          (abit (lsh ol-ts-bit 1))
;;          (callback
;;           (lambda (node beg)
;;             (goto-char beg)
;;             (when (and (looking-at ol-st-heading)
;;                        (ol-timestamp-is-active beg days))
;;               (throw 'exit node))
;;             (om-node-bit-on node abit)))
;;          node)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (setq node
;;               (om-preorder-traverse-plus current callback root))))
;;     (if node
;;         (om-back-up pos node))))

;; (defun ol-next-in-range-timestamp (&optional days)
;;   (let* ((cnode om-outline-node)
;;          (beg om-heading-position)
;;          (first (om-first-child cnode))
;;          root active-node parent)
;;     (ol-update-current-time)
;;     (goto-char beg)
;;     (if (and first
;;              (looking-at ol-st-heading)
;;              (ol-timestamp-is-active beg days)
;;              (not (om-node-bit-on cnode ol-ts-bit)))
;;         (ol-show cnode first)
;;       ;; (if (om-is-ancestor cnode ol-search-root)
;;       ;;     (setq root ol-search-root)
;;       ;;   (setq ol-search-root nil))
;;       (setq active-node
;;             (ol-find-in-range-node cnode root days))
;;       (unless active-node
;;         (setq  active-node
;;                (ol-find-in-range-node root root days))
;;         (if (not active-node)
;;             (message "(no active timestamp entry is found)")
;;           (if (eq active-node cnode)
;;               (message "(no other active timestamp entry is found)")
;;             (message "(wrapped around)"))))
;;       (when active-node
;;         (setq parent (om-parent active-node))
;;         (ol-show parent active-node)))))

;; (defun ol-next-active-timestamp ()
;;     (interactive)
;;     (ol-next-in-range-timestamp))

;; (defun ol-next-in-a-week-timestamp ()
;;     (interactive)
;;     (ol-next-in-range-timestamp 7))

(defun ol-prev-same-level ()
  (interactive)
  (cond
   ((org-at-item-p)
    (let ((col (current-column)))
      (call-interactively 'org-previous-item)
      (forward-char col)))
   ((ol-on-heading-p)
    (let ((prev (om-prev om-outline-node)))
      (unless
          (or (eq om-outline-node ol-subtree-root)
              (not prev))
        (backward-char (om-prev-diff om-outline-node)))))
   (t (message "no action at the location"))))

(defun ol-next-same-level ()
  (interactive)
  (cond
   ((org-at-item-p)
    (let ((col (current-column)))
      (call-interactively 'org-next-item)
      (forward-char col)))
   ((ol-on-heading-p)
    (let ((next (om-next om-outline-node)))
      (unless (or (eq om-outline-node ol-subtree-root)
              (not next))
        (forward-char (om-next-diff om-outline-node)))))
   (t (message "no action at the location"))))

(defun ol-enter-subtree ()
  (interactive)
  (let* ((cnode om-outline-node)
         (first (om-first-child cnode))
         (anchor (or first cnode)))
    (if (eq om-outline-node ol-subtree-root)
        (message "selected view is already the displayed view")
      (ol-show cnode)
      ;; (if first
      ;;     (forward-char (om-offset-from-parent first)))
      )))

(defun ol-goto-parent-subtree ()
  (interactive)
  (let* ((parent (om-parent ol-subtree-root))
         (pos (point-min)))
    (unless parent (error "Top level is already reached."))
    (ol-show parent)
    (goto-char pos)))

(defun ol-forward (&optional prev)
  (interactive)
  (let (node current elem nelem buf window)
    (if (eq ol-subtree-root om-root)
        (if (not (cdr  om-root-alist))
            (message "Org lite file has no sibling")
          (setq elem (rassoc om-root om-root-alist)
                current om-root-alist)
          (catch 'exit
            (while current
              (when (eq (car current) elem)
                (setq nelem 
                      (if (cdr current) 
                          (cadr current)
                        (car om-root-alist)))
                (throw 'exit nil))
              (setq current (cdr current))))
          (setq buf (car nelem)
                node (cdr nelem))
          (ol-show node nil buf))
      (if prev
          (unless (setq node (om-prev ol-subtree-root))
            (message "no previous subtree"))
        (unless (setq node (om-next ol-subtree-root)) 
          (message "no next subtree"))) 
      (if node
          (ol-show node)))))

(defun ol-backward ()
  (interactive)
  (ol-forward t))

(defun ol-goto-node-body-beginning ()
  (interactive)
  (if (/= om-beg om-heading-position)
      (message "selected entry is hidden")
    (goto-char om-beg)
    (if (re-search-forward "^[ \t]*:END:" (1- om-end) t)
        (goto-char (match-end 0))
      (end-of-line))
    (skip-chars-forward " \t\r\n")
    (beginning-of-line)
    (if (>= (point) om-end)
        (goto-char (1- om-end)))))

(defun ol-goto-node-end ()
  (interactive)
  (if (/= om-beg om-heading-position)
      (message "selected entry is hidden")
    (goto-char (1- om-end))))

(defun ol-goto-root ()
  (interactive)
  (if (eq om-root ol-subtree-root)
      (message "already at root level")
    (ol-goto-node om-root)))

(defun ol-list-buffer-name ()
  (concat (buffer-name) "-list"))

(defun ol-show-list ()
  (interactive)
  (if (eq ol-command-mode 'insert)
      (ol-normal-mode)
  (let* ((name (ol-list-buffer-name))
         (buf (get-buffer name))
         window)
    (setq ot-ol-buffer (current-buffer))
    (if (buffer-live-p buf)
        (if (setq window (get-buffer-window buf))
            (select-window window)
          (switch-to-buffer buf))
      (setq buf (get-buffer-create name))
      (switch-to-buffer buf)
      (ot-mode))
    (ot-show)
    )))

(defun ol-escape ()
  (interactive)
  (if (buffer-base-buffer)
      (ol-quit-node-editing))
  (if (eq ol-command-mode 'insert)
      (ol-normal-mode)))

(defun ol-insert-mode ()
  (let ((pos (point)))
    (setq ol-command-mode 'insert
          ol-mode-line-cookie
          (face-remap-add-relative 'mode-line :background "YellowGreen")
          ol-header-line-cookie
          (face-remap-add-relative 'header-line :background "YellowGreen"))
    (recenter)))

(defun ol-edit-node ()
  (interactive)
  (let* ((buf (current-buffer))
         (bname (concat (buffer-name buf) "-write"))
         (hl header-line-format)
         (n 0)
         ibuf elem window)
    (if (not (eq om-node om-outline-node))
        (message "selected entry is hiden")
      (when (setq elem (assoc om-node ol-editing-nodes-alist))
        (setq ibuf (cdr elem))
        (if (buffer-live-p ibuf)
            (if (setq window (get-buffer-window ibuf))
                (select-window window)
              (switch-to-buffer ibuf))
          (setq ol-editing-nodes-alist (delq elem ol-editing-nodes-alist)
                ibuf nil)))
      (unless ibuf
        (while (buffer-live-p (get-buffer bname))
          (setq bname (concat bname
                              (number-to-string (incf n)))))
        (setq ibuf (make-indirect-buffer buf bname))
        (setq ol-editing-nodes-alist (cons (cons om-node ibuf) ol-editing-nodes-alist))
                                        ;(om-set-sparse-bit node ol-editing-bit)
        (if (setq window (get-buffer-window ibuf))
            (select-window window)
          (switch-to-buffer ibuf))
        (ol-mode)
        (face-remap-add-relative 'mode-line :background "YellowGreen")
        (face-remap-add-relative 'header-line :background "YellowGreen")
        (setq
         ol-command-mode 'insert
         header-line-format hl)
        (outline-flag-region om-beg (1- om-end) nil)
        (narrow-to-region om-beg (1- om-end))
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward org-drawer-regexp (1- om-end) t)
              (org-flag-drawer t)))
        ;; (if (and (number-or-marker-p pos) (>= pos beg) (< pos end))
        ;;     (goto-char pos))
        ))))


(defun ol-normal-mode ()
  (let ((pos (point)))
    (setq ol-command-mode 'normal)
    (when ol-mode-line-cookie
      (face-remap-remove-relative ol-mode-line-cookie)
      (setq ol-mode-line-cookie nil))
    (when ol-header-line-cookie
      (face-remap-remove-relative ol-header-line-cookie)
      (setq ol-header-line-cookie nil))
    (recenter)))

(defun ol-quit-node-editing ()
  (interactive)
  (let* ((elem (assoc om-node ol-editing-nodes-alist))
         buf window)
    (if elem
        (setq ol-editing-nodes-alist (delq elem ol-editing-nodes-alist)))
    (kill-buffer)
    (setq buf
          (if (buffer-live-p om-last-buffer)
              om-last-buffer
            (caar om-root-alist)))
    (if (not (bufferp buf))
        (error "buffer not found"))
    (if (setq window (get-buffer-window buf))
        (pop-to-buffer buf)
      (switch-to-buffer buf)
      )))

(defun ol-goto-mark ()
  (interactive)
  (let ((off ol-mark-offset))
    (unless ol-mark
      (error "org lite mark not set"))
    (ol-goto-node ol-mark (eq ol-mark-display-mode 'child))
    (forward-char off)))

(defun ol-goto-node (tnode &optional display-target-as-a-child)
  (let* ((snode om-outline-node)
         (display-mode (if (eq snode ol-subtree-root)
                           'parent 'child))
         (beg (om-beg snode))
         (offset (- (point) beg))
         (buf (om-buf tnode))
         (root (if display-target-as-a-child (om-parent tnode) tnode)))
    (unless (and buf (bufferp buf))
      (error "node not found"))
    (if (and (eq snode tnode)
             (or (and display-target-as-a-child (not (eq snode ol-subtree-root)))
                 (and (not display-target-as-a-child) (eq snode ol-subtree-root))))
        (message "Already on target heading")
      (if (or (eq tnode ol-mark)
              (not ol-mark-sticky))
          (setq ol-mark-sticky nil
                ol-mark snode
                ol-mark-offset offset
                ol-mark-display-mode display-mode))
      (ol-show root nil buf)
      (if display-target-as-a-child
          (forward-char (om-offset-from-parent tnode)))
    )))

(defun ol-set-sticky-mark ()
  (interactive)
  (let* ((cnode om-outline-node)
         (beg (om-beg cnode))
         (offset (- (point) om-heading-position)))
    (if ol-mark-sticky
      (progn
        (setq ol-mark-sticky nil
              ol-mark nil)
        (message "sticky mark removed"))
      (setq ol-mark cnode
            ol-mark-offset offset
            ol-mark-sticky t
            ol-mark-display-mode
            (if (eq ol-mark ol-subtree-root)
                'parent 'child))
      (ol-mark-heading))
    (ol-set-extra)
    (recenter)))

(defun ol-remove-mark (node)
  (if (and ol-mark
           (not (eq ol-mark node))
           (om-is-ancestor
            ol-mark node))
      (setq ol-mark nil)))

(defun ol-open-at-point (&optional in-emacs)
  (interactive "P")
  (let ((org-link-protocols
         (cons (list "id" 'ol-id-open nil)
               org-link-protocols)))
    (org-open-at-point in-emacs)))

(defun ol-id-open (id)
  (let ((node (om-id-lookup id))
        (base (buffer-base-buffer)))
    (unless (and node (listp node))
      (error "Heading with the id \"...%s\" not found, check if the file is opened." (substring id -8)))
    (if base
        (pop-to-buffer base))
    (ol-goto-node node)))

(defun ol-refresh ()
  (interactive)
  (goto-char om-heading-position)
  (setq mark-active nil)
  (if (eq this-command last-command)
      (ol-show)))

(defun ol-subtree-remove-scheduled (node)
  (let* ((callback
          (lambda (n b e)
            (goto-char b)
            (when (looking-at ol-st-heading)
              (om-clear-sparse-bit n ol-ts-bit)
              (replace-match "" nil nil nil 1))
            )))
    (widen)
    (om-traverse-sparse-subtree node ol-ts-bit callback)))

(defun ol-subtree-remove-todo (node)
  (let* ((list (ol-get-todo-list))
         (update nil)
         (callback
          (lambda (n b e)
            (let (id)
            (goto-char b)
            (when (re-search-forward om-id-regexp e t)
              (setq id (match-string 1))
              (when (member id list)
                (om-clear-sparse-bit n ol-todo-bit)
                (setq list (delete id list)
                      update t)))))))
    (om-traverse-sparse-subtree node ol-todo-bit callback)
    (if update
        (ol-save-todo-list list))))

(defun ol-delete-non-local (node)
  (let ((buf (om-buf node)))
    (with-current-buffer buf
      (ol-delete))))

(defun ol-refile-node (&optional node target)
  (interactive)
  (let* ((node (or node om-outline-node))
         (anchor (or (om-next node)
                     (om-prev node)
                     (om-parent node)))
         (target (or target ol-mark)))
    (unless target
      (error "refile target not set"))
    (om-do-copy node target)
    (om-remove-subtree node)
    ))

(defun ol-refile ()
  (let* ((node om-outline-node)
         (anchor (or (om-next node)
                     (om-prev node)
                     (om-parent node)))
         (npos (car (om-subtree-range anchor)))
         (m (make-marker)))
  (set-marker m npos)
  (ol-refile-node node)
  (ol-show)
  (goto-char m)
  (set-marker m nil)))

(defun ol-copy ()
  (interactive)
  (unless ol-mark
    (error "org lite mark not set"))
  (let* ((marked (ol-mark-heading))
         (confirm (concat "Copy under: \"" marked "\" ?")))
    (when (y-or-n-p confirm)
      (om-do-copy om-outline-node ol-mark))
    (ol-show)))

(defun ol-bubble-up ()
  (interactive)
  (let* ((cnode om-outline-node)
         (beg (1- om-beg))
         (end om-subtree-end)
         (parent (om-parent cnode))
         (grand (om-parent parent)))

;    (if (eq ol-subtree-root cnode)
;        (error "bubble up operation cannot operate on buffer root"))
    (unless grand
      (error "the selected subtree is already at top level"))
    (ol-refile-node cnode grand)))

(defun ol-subtree-remove-editing (node)
  (let* ((callback
          (lambda (n b e)
            (let (elem buf)
              (om-clear-sparse-bit n ol-editing-bit)
              (when (setq elem (assq n ol-editing-nodes-alist))
                (setq buf (cdr elem))
                (when (buffer-live-p buf)
                  (with-current-buffer buf 
                    (when (ol-editing-change-invalid)
                      (switch-to-buffer buf)
                      (throw 'quit nil))
                    (kill-buffer)))
                (delq elem ol-editing-nodes-alist))
              ))))
    (catch 'quit
      (om-traverse-sparse-subtree node ol-editing-bit callback))))

(defun ol-archive (&optional node)
  (interactive)
  (let* ((node om-outline-node)
         (anchor (or (om-next node)
                     (om-prev node)
                     (om-parent node)))
         (npos (car (om-subtree-range anchor)))
         (m (make-marker)))
    (set-marker m npos)
    (ol-do-archive node)
    (ol-show)
    (goto-char m)
    (set-marker m nil)))

(defun ol-do-archive (&optional node)
  (let* ((node (or node om-outline-node))
         (buf (if node (om-buf node) (current-buffer)))
         ebiton sbiton tbiton)
    (when (y-or-n-p "archive the subtree under the ARCHIVE sibling ?")
      (with-current-buffer buf
        (widen)
        (catch 'exit
          (if (and (setq ebiton (om-node-bit-or-abit-on node ol-editing-bit))
                   (not (y-or-n-p "subtree editing buffer(s) will be killed, proceed anyway ?")))
              (throw 'exit nil))
          (if (and (setq sbiton (om-node-bit-or-abit-on node ol-ts-bit))
                   (not (y-or-n-p "subtree timestamp(s) will be removed, proceed anyway ?")))
              (throw 'exit nil))
          (ol-refile-to-specific node ol-archive-heading-word))))))
        
(defun ol-refile-to-pool ()
  (interactive)
  (ol-refile-to-specific om-outline-node ol-hidden-heading-word))

(defun ol-refile-to-specific (node name)
  (interactive)
  (let* ((node (or node om-outline-node))
         (parent (om-parent node))
         (target (ol-named-child parent name))
         last)
    (if target
        (setq ol-mark target)
      (setq last (om-last-child parent))
      (om-insert-entry-after (concat name " ") last)
      (setq ol-mark (om-last-child parent)))
    (ol-refile-node node)))

(defun ol-delete ()
  (interactive)
  (let ((end (point)))
    (forward-same-syntax -1)
    (if (<= (point) (1+ (+ om-beg om-level)))
        (message "heading stars modification is not allowed")
      (if (> end om-end)
          (message "editing ending empty line is not allowed")
        (kill-region (point) end)))))
    
(defun ol-control-delete ()
  (interactive)
  (let* ((node om-outline-node)
         (anchor (or (om-next node)
                     (om-prev node)
                     (om-parent node)))
         (npos (car (om-subtree-range anchor)))
         (m (make-marker))
         (count (om-subtree-node-count node))
         ebiton sbiton tbiton)
    (when (y-or-n-p "delete the subtree ?")
      (when (or (< count 5)
                (y-or-n-p
                 (concat
                  "This entry has "
                  (number-to-string count)
                  " sub entries, really want to delete ?")))
        (set-marker m npos)
        (catch 'exit
          (if (and (setq ebiton (om-node-bit-or-abit-on node ol-editing-bit))
                   (not (y-or-n-p "subtree editing buffer(s) will be killed, proceed anyway ?")))
              (throw 'exit nil))
          (if (and (setq sbiton (om-node-bit-or-abit-on node ol-ts-bit))
                   (not (y-or-n-p "subtree timestamp(s) will be removed, proceed anyway ?")))
              (throw 'exit nil))
          (ol-remove-mark node)
          (om-remove-subtree node)
          (ol-show anchor)
          (goto-char m)
          (set-marker m nil)
          (message "")
          )))))

(defun ol-goto-pool ()
  (interactive)
  (let ((mark (ol-named-child
               ol-subtree-root
               ol-hidden-heading-word)))
    (when mark
      (ol-show mark))))


(defun ol-named-child (anchor name)
  (let* ((anchor (om-parent om-outline-node))
         (cb
          (lambda (n b)
            (let (heading)
              (goto-char b)
              (looking-at ol-heading-regexp-2)
              (setq heading (match-string-no-properties 2))
              (if (equal heading name)
                  (throw 'exit n))))))
    (widen)
    (catch 'exit
      (om-run-children-forward anchor cb))))

(defun ol-create-heading ()
  (interactive)
  (let (txt pos)
    (cond
     ((org-at-item-p) (org-insert-item))
     ((ol-on-heading-p)
      (setq txt
            (read-string "Heading: " nil nil nil t))
      (message "")
      (string-match "[ \t]*$" txt)
      (setq txt (replace-match " " nil nil txt))
      (widen)
      (setq pos
            (if (eq om-node ol-subtree-root)
                (om-insert-entry-under txt)
              (om-insert-entry-after txt)))
      (ol-show nil)
      (goto-char pos))
      ;(forward-char (om-offset-from-parent cnode)))
     (t (message "no action at the location")))))


(defun ol-insert-link-string (txt)
    (if (ol-on-heading-p)
        (progn
          (beginning-of-line)
          (when (looking-at ol-heading-regexp-2)
            (goto-char (match-end 2))
            (skip-chars-backward " \t")
            (if (= (following-char) ?\s)
                (progn 
                  (forward-char 1)
                  (unless (= (following-char) ?\s)
                    (setq txt (concat txt " "))))
              (setq txt (concat " " txt " ")))
            (insert txt)
            (unless (= (preceding-char) ?\s)
              (forward-char 1))
            (if (memq (char-after) '(?\s ?\t))
                (delete-region
                 (point)
                 (progn
                   (skip-chars-forward " \t")
                   (point))))
            (outline-flag-region (point-at-bol) (point-at-eol) nil)
            (beginning-of-line)
            ))
      (unless (= (preceding-char) ?\s)
        (setq txt (concat " " txt)))
      (unless (or (eobp) (= (char-after) ?\s))
        (setq txt (concat txt " ")))
      (insert txt)))

(defun ol-add-link ()
  (interactive)
  (let ((pos (point))
        (bnode ol-subtree-root)
        (cnode om-outline-node)
        (link-re "^\\(id\\|file\\|http\\|https\\|Outlook\\):.*")
        (link (car (car org-stored-links)))
        clip desc tmphist all-prefixes txt)
    (if link
        (setq org-stored-links (cdr org-stored-links))
      (setq clip (current-kill 0))
      (if (string-match link-re clip)
          (setq link (match-string 0 clip))
        (setq clip nil)))
    (setq tmphist (append (mapcar 'car org-stored-links)
                          org-insert-link-history)
          all-prefixes (append (mapcar 'car org-link-abbrev-alist-local)
                               (mapcar 'car org-link-abbrev-alist)
                               org-link-types)
          link (completing-read
                "Link: " (append
                          (mapcar (lambda (x) (list (concat x ":")))
                                  all-prefixes))
                nil nil nil tmphist
                (car (car org-stored-links)))
          desc (read-string "Description: "
                            (match-string 1 clip) nil
                            (match-string 1 clip)  t)
          txt (org-make-link-string link desc))
    (ol-insert-link-string txt)
    (ol-show bnode pos)
    ))

(defun ol-link-to-mark ()
  (interactive)
  (let ((bnode ol-subtree-root)
        (cnode om-outline-node))
    (unless ol-mark
      (error "the org-lite mark not set"))
    (let* ((id (om-node-id-get-create ol-mark))
           (txt (org-make-link-string
                 (concat "id:" id) "->")))
      (ol-insert-link-string txt))))

(defun ol-link-out ()
  (interactive)
  (let* ((id (om-node-id-get-create om-outline-node))
         (txt (org-make-link-string (concat "id:" id) "->"))
         (buf (current-buffer))
         (pos om-beg)
         (node ol-mark)
         (offset ol-mark-offset)
         char)
    (unless (ol-on-heading-p)
      (error "cursor not on a heading"))
    (setq ol-mark-sticky nil
          ol-mark-display-mode 'child)
    (ol-goto-node node)
    (forward-char offset)
    (message "including the heading in the link ? [y]es,  [n]o, or [c]ancell")
    (setq char (read-char-exclusive))
    (if (and (/= char ?y) (/= char ?n))
        (message "cancelled")
      (if (= char ?y)
          (setq txt (concat (ol-heading-at pos buf) " " txt)))
      (ol-insert-link-string txt))))

(defun ol-link-in ()
  (interactive)
  (unless ol-mark
    (error "ol mark not set"))
  (let* ((id (om-node-id-get-create ol-mark))
         (heading (ol-node-heading ol-mark))
         (txt (org-make-link-string (concat "id:" id) "->"))
         char)
    (message "including the heading in the link ? [y]es,  [n]o, or [c]ancell")
    (setq char (read-char-exclusive))
    (if (and (/= char ?y) (/= char ?n))
        (message "cancelled")
      (if (= char ?y)
          (setq txt (concat heading " " txt)))
      (ol-insert-link-string txt))))

(defun ol-link-under-mark ()
  (interactive)
  (unless ol-mark
    (error "back link to org lite mark not set"))
  (let* ((id (om-node-id-get-create om-outline-node))
         (heading (concat (ol-heading-at om-heading-position)
                          " "
                          (org-make-link-string
                           (concat "id:" id) "->"))))
  (om-insert-entry-under heading)
  (ol-goto-mark)))

(defun ol-update-current-time ()
  (setq ol-current-time (current-time)
        ol-today (time-to-days (current-time))))

(defun ol-set-extra ()
  (let* ((bnode ol-subtree-root)
         (level (om-level bnode))
         (beg (om-beg bnode))
         (today (time-to-days (current-time)))
         (sdate ol-starting-date)
         (edate (1- (+ ol-starting-date ol-in-ndays)))
         (ml
          (concat
           "TODO: "
           (cond
            ((= ol-days 0)
             "Today ")
            ((= ol-days 7)
             "In 7 days")
            (t
             "In 30 days"))))
         (hl
          (cond
           ((= 0 level)
            "")
           ((= 1 level)
            "OVERVIEW")
           (t
            (concat " " (make-string (1- level) ?\*) " "
                    (ol-heading-at
                     (- beg (om-offset-from-parent bnode))))))))
    
    (setq header-line-format (list "UP: " hl "  |  [Marked Node" (if ol-mark-sticky "(sticky)") ": " (if ol-mark (ol-mark-heading)) "]")
          ol-mode-line-hl ml)
    ))

(defun ol-node-heading (&optional node)
  (let* ((node (or node om-outline-node))
         (buf (om-buf node))
         (pos (om-beg node)))
        (ol-heading-at pos buf)))

(defun ol-heading-at (pos &optional buf)
  (let (heading)
    (save-excursion
      (save-restriction
        (when (bufferp buf)
          (unless (eq buf (current-buffer))
            (set-buffer buf)))
        (widen)
        (goto-char pos)
        (setq heading 
              (if (= pos 0)
                  (file-name-nondirectory (buffer-file-name buf))
                (if (looking-at ol-heading-regexp-2)
                    (match-string-no-properties 2))))))))

(defun ol-get-heading-by-id (id &optional buf)
  (let* ((node (om-id-lookup id))
         (pos (om-beg node)))
    (ol-heading-at pos buf)))

(defun ol-mark-heading ()
  (let ((buf (om-buf ol-mark))
        (beg (om-beg ol-mark)))
    (setq ol-mark-heading
          (ol-heading-at beg buf))
    ))

(defun ol-bookmark-command ()
  (interactive)
  (let* ((key (this-single-command-keys))
         (S (concat key))
         (c (string-to-char S)))
    (ol-jump-to-numbered-location c)))

(defun ol-jump-to-numbered-location (c)
  (let* (elem str file buf id pair root msg node)
    (if (not (setq elem (assoc c ol-numbered-headings-alist)))
        (message "%c is not mapped" c)
      (setq str (cdr elem))
      (unless (string-match numbered-list-elem-re str)
        (error "ill formed entry found in ol-numbered-headings-alist")) 
      (setq id (match-string 1 str)
            file (concat ol-dir (match-string 2 str)))
      (unless (file-exists-p file)
        (error "%s not found" file))
      (if (and (setq buf (find-buffer-visiting file))
               (setq pair (assoc buf om-root-alist)))
          (setq root (cdr pair))
        (setq buf (find-buffer-visiting file))
        (unless buf
          (setq buf (find-file-noselect file)))
        (ol-add-buffer buf)
        (setq root om-root
              msg (concat "File "
                          (file-name-nondirectory file)
                          " is added to the list.")))
      (setq node
            (if (stringp id)
                (om-id-lookup id)
              root))
      (if (not (and node (listp node)))
          (error "entry not found")
        (ol-goto-node node))
      (if msg (message "%s" msg)))))

(defun ol-save-numbered-list ()
  (interactive)
  (let* ((file ol-main-file)
         (length (length ol-dir))
         (func (lambda (x)
                 (concat "(" (char-to-string (car x))
                         ")" (cadr x) "::"
                         (substring (cddr x) length))))
         buf end str pair)
    (when (file-exists-p file)
      (setq buf (or (find-buffer-visiting file)
                    (find-file-noselect file))
            str (mapconcat func ol-numbered-headings-alist "|"))
      (with-current-buffer buf 
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (setq end (cdr (om-subtree-range
                            om-root)))
            (when (re-search-forward
                   numbered-list-option-re end t)
               (replace-match str t t nil 1))))))))

(defun ol-get-bookmark ()
  (let* ((func
          (lambda (x)
            (let* ((num (char-to-string (car x)))
                   (id (cadr x))
                   (file (cddr x)))
              (concat "[" num "]"
                      (if (find-buffer-visiting file)
                          (if id
                              (ol-get-heading-by-id id)
                            (file-name-nondirectory file))))))))
         (mapconcat func ol-numbered-headings-alist " ")))

(defun ol-get-or-set-bookmark ()
  (interactive)
  (if (eq last-command this-command)
      (if (eq ol-bookmark-state 'get)
          (progn (setq ol-bookmark-state 'set)
                 (ol-set-bookmark))
        (setq ol-bookmark-state 'get)
        (ol-get-bookmark))
    (setq ol-bookmark-state 'get)
    (message "Task bookmarks: %s" (ol-get-bookmark))))

(defun ol-set-bookmark (&optional default)
  (interactive)
  (let ((cnode om-outline-node)
        (pos om-heading-position)
        (file (buffer-file-name (buffer-base-buffer)))
        char id)
    (if default
        (setq char ?=)
      (message "Press a number to bookmark the selected heading: ")
      (setq char (read-char-exclusive))
      (message ""))
    (if (and (or (< char ?0) (> char ?9))
             (/= char ?=)
             (/= char ?\s))
        (error "The bookmark must be a number key: [0-9] or '=' or '\s'"))
    (setq ol-numbered-headings-alist
          (assq-delete-all char ol-numbered-headings-alist)
          id
          (if (om-parent cnode)
              (om-node-id-get-create cnode))
          ol-numbered-headings-alist
          (cons (cons char (cons id file)) ol-numbered-headings-alist)
          ol-numbered-headings-alist
          (sort ol-numbered-headings-alist
                (lambda (x y) (< (car x) (car y)))))
    (ol-save-numbered-list)
    (message "bookmark added")))

(defun ol-set-default-bookmark ()
  (interactive)
  (ol-set-bookmark t))

(defun ol-recenter ()
  (interactive)
  (if (and (eq last-command this-command)
           (= (point) (window-start)))
      (recenter '(4))
    (recenter 0)))

(defun ol-occur ()
  (interactive)
  (unless (call-interactively 'occur)
    (show-all)))

(defun ol-clone-other-window ()
  (interactive)
  (let* ((rbuf (generate-new-buffer "org lite read"))
         (buffer-undo-list t)
         (hl header-line-format)
         (txt (buffer-string)))
    (jit-lock-fontify-now (point-min) (point-max))
    (select-window (display-buffer rbuf t))
    (insert txt)
    (goto-char (point-min))
    (face-remap-add-relative 'mode-line :background "LightYellow")
    (face-remap-add-relative 'header-line :background "LightYellow")
    ))

(defun ol-quit-clone ()
  (interactive)
  (let ((base (buffer-base-buffer))
        window)
    (if (not base)
        (message "already a base buffer")
      (kill-buffer)
      (if (setq window (get-buffer-window base))
          (select-window window)
        (switch-to-buffer base)))))


(defun ol-init-sync ()
  (unless ol-save-timer
    (setq ol-save-timer (run-with-idle-timer 5 t 'ol-idle-save-buffer)))
  (unless ol-sync-timer
    (setq ol-sync-timer (run-with-timer 1 3 'ol-update-buffer))))

(defun ol-idle-save-buffer ()
  (let ((current om-root-alist)
        file buf)
    (while current
      (setq buf (caar current))
      (if (and (bufferp buf) (buffer-modified-p buf))
          (with-current-buffer buf (save-buffer))
        (message ""))
      (setq current (cdr current)))))

(defun ol-save-todo-list (sl &optional buf)
  (let* ((buf (or buf (current-buffer)))
         (str (mapconcat 'identity sl "|"))
         end)
    (unless (buffer-live-p buf)
      (error "buffer not found"))
    (with-current-buffer buf 
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (setq end (cdr (om-subtree-range
                          om-root)))
          (when (re-search-forward todo-list-option-re end t)
            (replace-match str t t nil 1)))))))

(defun ol-toggle-todo ()
  (interactive)
  (let* ((cnode om-outline-node)
         (range (om-node-range cnode))
         (beg (car range))
         (end (cdr range))
         (heading (ol-heading-at om-heading-position))
         (list (ol-get-todo-list))
         id)
    (unless (om-parent cnode)
      (error "not on a heading"))
    (catch 'quit
      (when (om-node-bit-on cnode ol-ts-bit)
        (unless (y-or-n-p "the heading is on scheduled list, remove from the list?")
          (throw 'quit nil))
        (goto-char beg)
        (looking-at ol-heading-regexp-1)
        (if (match-beginning 2)
            (progn
              ;(ol-remove-ts-overlay beg)
              (delete-region (match-beginning 2) (match-end 2))
              (om-clear-sparse-bit cnode ol-ts-bit))))
      
      (setq id (om-node-id-get beg (1- end)))
      (unless (stringp id)
        (setq id (om-node-id-get-create cnode))
        (ol-show))
      (if (member id list)
          (if (not (y-or-n-p "heading is already on todo list, remove from the list?"))
              (throw 'quit nil)
            (setq list (delete id list))
            (om-clear-sparse-bit cnode ol-todo-bit))
        (setq list (cons id list))
        (om-set-sparse-bit cnode ol-todo-bit)
        (message "heading is added to todo list"))
      (ol-save-todo-list list))))

(defun ol-on-heading-p ()
  (= om-heading-position (point-at-bol)))

(defun ol-open-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (ol-open-at-point))

(defun ol-kill-region ()
  (interactive)
  (let ((range (if mark-active
                   (cons (region-beginning) (region-end))
                 (cons (1- (line-beginning-position))
                       (1- (line-beginning-position 2))))))
    (kill-region (car range) (cdr range))
    (unless mark-active
      (forward-char))))

(provide 'ol)

