;;; om.el
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2012-2015  Jeff Yuanqian Li
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------
;;
;; Author: Jeff Yuanqian Li 
;; Created: Oct. 29 2012


;; This file contains library routines for automatic updating 
;; the cursor location information in a outline-mode buffer. 
;; The calculation is done each time an editing or cursor movement 
;; is occurred. 

(eval-when-compile
  (require 'cl))
;; Global info
(defvar om-root nil)
(defvar om-buf nil)
(defvar om-node nil)
(defvar om-id nil)
(defvar om-level 0)
(defvar om-beg 0)
(defvar om-end (make-marker))
(defvar om-subtree-end nil)
(defvar om-node-below nil)
(defvar om-redecorate-node-on-leaving nil)
(defvar om-callback nil)
(defvar om-update-callback nil)
(defvar om-outline-node nil)
(defvar om-heading-position nil)
(defvar om-outline-mode 'fold)
(defvar om-last-buffer nil) ;; global
(defvar om-root-alist nil)
(defvar om-saved-id nil)
(defvar om-count 0)
(defvar om-message "outline format protected area")

(defvar om-id-hash-table (make-hash-table :test     'equal
                                          :weakness 'value
                                          :size     1024))

(defvar om-id-regexp "^[ \t]*:ID:[ \t]*\\([^ \t\r\n\f\v]+\\)")

(defun om-new-node ()
  (list nil nil nil nil nil 0))

(defun om-check-file-format ()
  (let (pos)
    (while (re-search-forward "^\\(\\*+\\)[ \t]" nil t)
      (setq pos (1- (match-beginning 0)))
      (unless (= ?\n (char-before pos))
        (error "missing a empty line prior to the heading at %d"
               (line-number-at-pos pos))))
    (unless (= ?\n (char-before (point-max)))
      (error "buffer must end with a empty line"))))
      
(defun om-create-substree (rl beg end)
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (root (om-new-node))
         (current root)
         (cl rl)
         (cb beg)
         pos id pid found parent new node level nl nb offset)
    (if (= 0 rl)
        (setq pos (point-min))
      (setq pos (1+ (+ cb rl)))
      (put-text-property cb pos 'read-only om-message)
      ;(put-text-property cb (1+ cb) 'front-sticky 'read-only) ;; in case it is (point-min)
      (put-text-property (1- pos) pos 'rear-nonsticky 'read-only))
    (goto-char pos)
    (when (re-search-forward om-id-regexp end t)
      (setq pid (match-end 1)
            id (match-string-no-properties 1)
            found t)
      (goto-char pos))
    (while (re-search-forward "^\\(\\*+\\)[ \t]" end t)
      (setq nb (match-beginning 0)
            nl (length (match-string 1))
            pos (match-end 0))
      (put-text-property (1- nb) pos 'read-only om-message)
      ;(put-text-property nb (1+ nb) 'front-sticky 'read-only)
      (put-text-property (1- pos) pos 'rear-nonsticky 'read-only)
      (cond
       ((> nl (1+ cl))       ;; dangling subtree
        (setcar (nthcdr 3 root) nil)
        (error "the heading level at line %d must be %d or less"
               (line-number-at-pos nb) (1+ cl)))
       ((> nl cl) ;; (= nl (1+ cl)) new is the first child
        (setq new (list (- nb cb) nil current nil nil 0)) 
        (setcar (nthcdr 3 current) new))   ;; set first child
       ((= nl cl)
        (when (= nl rl)
          (setcar (nthcdr 3 root) nil)
          (error "the heading level at line %d must be %d or greater"
                 (line-number-at-pos nb) (1+ rl)))
        (setq offset (+ (car current) (- nb cb))
              new (list offset nil (nth 2 current) nil current 0))
        (setcar (nthcdr 1 current) new))               ;; set next
       ((< nl cl) ;; current is the last child
        (when (<= nl rl)
          (setcar (nthcdr 3 root) nil)
          (error "the heading level at line %d must be %d or greater"
                 (line-number-at-pos nb) (1+ rl)))
        (setq node  current
              level cl
              offset (- nb cb))
        (while (< nl level)
          (setq level (1- level)
                offset (+ offset (car node))
                node (nth 2 node)))
        (setq new (list (+ (car node) offset) nil nil nil nil 0))
        (setcar (nthcdr 1 node) new)               ;; set next
        (setcar (nthcdr 4 new) node)               ;; set prev
        (unless (setq parent (nth 2 node))
          (error "not a subtree"))
        (setcar (nthcdr 2 new) parent)))             ;; set parent
      (when om-callback
        (setq pos (point))
        (funcall om-callback cb nb current)
        (goto-char pos))
      (setq pos (point))
      (when (and found (< pid nb))
        (puthash id current om-id-hash-table)
        (if (not (re-search-forward om-id-regexp end t))
            (setq found nil)
          (setq pid (match-end 1)
                id (match-string-no-properties 1))
          (goto-char pos)))
      (setq current new cl nl cb nb))
    (when found
        (puthash id current om-id-hash-table))
    (when om-callback
      (funcall om-callback cb end current))
    (if (= 0 rl)
        (setcar root end)) ;; set offset
    ;(put-text-property (1- end) end 'read-only om-message)
    ;(put-text-property (1- end) end 'front-sticky 'read-only)
    ;(put-text-property (1- end) end 'rear-nonsticky 'read-only)
    root))

(defun om-init (root)
  (setq om-root root
        om-buf (current-buffer)
        om-node root
        om-id   nil
        om-outline-node root
        om-level 0
        om-beg (point-min)
        om-end (if (nth 3 root)
                   (1+ (car (nth 3 root)))
                 (point-max))
        om-subtree-end (point-max)
        om-node-below (nth 2 root)
        om-heading-position (point-min)
        om-root-alist
          (cons (cons (current-buffer) root) om-root-alist))
        (set-buffer-modified-p nil))
        
;; sync the outline-markers with the buffer changes
(defun om-update-outline-markers (node diff &optional skip)
  (let ((first (nth 3 node))
        parent candidate)
    (if (or skip (not first))
        (setq parent (nth 2 node))
      (setcar first (+ (car first) diff))
      (setq parent node node first))
    (while parent
      (setq candidate (nth 1 node))
      (if (not candidate)
          (setq node (nth 2 node)
                parent (nth 2 node))
        (setq node candidate)
        (setcar node (+ (car node) diff))))
    (setcar node (+ (car node) diff))))

(defun om-relocate (anchor)
  (let ((buf (om-buf anchor))
        (off 0)
        (parent (nth 2 anchor))
        (node anchor)
        (first (nth 3 anchor))
        (level 0)
        size next)
    (om-node-redecorate-maybe)
    (while parent
      (setq off (+ off (car node))
            level (1+ level))
      (unless size
        (setq next (nth 1 node))
        (if next
            (setq size (- (car next) off))))
      (setq node parent
            parent (nth 2 node)))
    (setq om-node anchor
          om-root node
          om-buf buf
          om-level level
          om-beg (1+ off)
          om-subtree-end (if size
                             (+ off size 1) 
                          (car node)) 
          om-end (if first
                     (+ off (car first))
                   om-subtree-end)
          om-node-below (or first next)
          om-outline-node anchor
          om-heading-position om-beg)
    (om-cache-node-decoration)
    ))

;; This routine can be called only when outline-markers are in sync with the buffer
(defun om-localization (&optional pos)
  (let* ((buf (or (buffer-base-buffer) (current-buffer)))
         (pos (or pos (point)))
         (root (cdr (assoc buf om-root-alist)))
         (node root)
         (level 0)
         (beg 1)                  ;; node beginning
         (send (car node))         ;; subtree end
         nend ysibling snext first ybeg fbeg pbeg)
    (when om-redecorate-node-on-leaving
      (setq om-redecorate-node-on-leaving nil)
      (om-node-redecorate-maybe))  ;; redecorate node on leaving
    (catch 'exit
      (while t
        (if (and ysibling (>= pos ybeg))
            (setq node ysibling beg ybeg)
          (if ysibling
              (setq send ybeg
                    snext ysibling))
          (setq first (nth 3 node)
                fbeg (if first (+ beg (car first))))
          (when (or (not first) (< pos fbeg))
            (if first
                (setq nend fbeg))
            (throw 'exit nil))
          (setq pbeg beg
                node first
                level (1+ level)
                beg fbeg))
        (if (setq ysibling (nth 1 node))
            (setq ybeg (+ pbeg (car ysibling))))))
    (setq om-root root
          om-buf buf
          om-node node
          om-beg beg
          om-end (or nend send)
          om-subtree-end send
          om-node-below (or first snext)
          om-level level)))

(defun om-localization-plus ()
  (let (beg node parent)
    (om-localization)
    (setq beg om-beg
          node om-node)
    (while (and (outline-invisible-p beg)
                (setq parent (nth 2 node)))
      (setq beg (- beg (car node))
            node parent))
    (setq om-outline-node node
          om-heading-position beg
          om-outline-mode 'fold)
    (unless (buffer-base-buffer)
      (setq om-last-buffer (current-buffer)))
    (om-cache-node-decoration)))

(defun om-delete-subtree (size)
  (let* ((node (om-node-below om-node))
         (parent  (nth 2 node))
         (prev (nth 4 node))
         (next (nth 1 node))
         (anchor (or prev parent)))
    (if prev
        (setcar (nthcdr 1 prev) next)
      (setcar (nthcdr 3 parent) next)
      (setq om-subtree-end (- om-subtree-end size)))
    (if next
        (setcar (nthcdr 4 next) prev))
    (setcar (nthcdr 1 node) nil)
    (setcar (nthcdr 2 node) nil)
    (setcar (nthcdr 4 node) nil)
    (om-clear-sparse-bit node 4 t)
    (om-update-outline-markers anchor (- size) (if prev t))
    (setq om-node-below (om-node-below om-node))))

(defun om-insert-subtree (beg size)
  (let ((parent (nth 2 om-node))
        subtree anchor level alevel offset)
  (goto-char beg)
  (looking-at "^\\(\\*+\\)[ \t]")
  (setq level (length (match-string 1))
        anchor om-node
        subtree (om-create-substree level beg (+ beg size)))
  (if (> level om-level)
      (progn
        (om-update-outline-markers anchor size)
        (setcar subtree (- om-end om-beg))
        (om-insert-node-under anchor subtree)
        (setq om-subtree-end (+ om-subtree-end size)))
    (setq alevel om-level
          offset (+ (car anchor) (- om-end om-beg)))
    (while (> alevel level)
      (setq anchor (nth 2 anchor)
            offset (+ offset (car anchor))
            alevel (1- alevel)))
    (om-update-outline-markers anchor size t)
    (setcar subtree offset)
    (om-insert-node-after anchor subtree))
  (setq om-node-below subtree)))

(defun om-insert-node-under (parent node)
  (let* ((first (nth 3 parent)))
    (when first
      (setcar (nthcdr 4 first) node)
      (setcar (nthcdr 1 node) first))
    (setcar (nthcdr 3 parent) node)
    (setcar (nthcdr 2 node) parent)
    ))

(defun om-insert-node-after (anchor node)
  (let ((next (nth 1 anchor)))
    (when next
      (setcar (nthcdr 4 next) node)
      (setcar (nthcdr 1 node) next))
    (setcar (nthcdr 1 anchor) node)
    (setcar (nthcdr 4 node) anchor)
    (setcar (nthcdr 2 node) (nth 2 anchor))
    ))

(defun om-post-command ()
  (when (or (not (eq om-buf (or (buffer-base-buffer) (current-buffer))))
            (< (point) om-beg)
            (>= (point) om-end))
    (om-localization-plus)))

(defun om-before-change (beg end)
  (if (or (< beg (point-min))
          (> end (point-max)))
      (error "change is out of range"))
  (if (or (not (eq om-buf (or (buffer-base-buffer) (current-buffer))))
          (<= beg om-beg)
          (> beg om-end))
      (om-localization (1- beg)))
  (unless (= beg om-end)  
    (unless om-redecorate-node-on-leaving
      (setq om-redecorate-node-on-leaving t)
      (om-cache-node-decoration))))

(defun om-after-change (beg end length)
  (let (size)
     (if (= beg om-end)
         (if (> length 0) ;; delete subtree
             (om-delete-subtree length)
           ;; add subtree
           (om-insert-subtree beg (- end beg)))
       (setq size (- (- end beg) length))
       (unless (= 0 size)
         (om-update-outline-markers om-node size)
         (setq om-end (+ om-end size)
               om-subtree-end (+ om-subtree-end size))))))

(defun om-add-hooks ()
  (add-hook 'before-change-functions 'om-before-change nil t) 
  (add-hook 'after-change-functions 'om-after-change nil t) 
  (add-hook 'pre-command-hook 'ol-pre-command nil t)
  (add-hook 'post-command-hook 'om-post-command nil t))

;; FIXME: change it io run-hooks for ol hooks
(defun om-cache-node-decoration ()
  (setq om-id (om-node-id-get om-beg om-end)))

;; FIXME: change it io run-hooks for ol hooks
(defun om-node-redecorate-maybe ()
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
         id)
    (when (buffer-live-p om-buf)
      (with-current-buffer om-buf
        (save-excursion
          (save-restriction
            (widen)
            (funcall om-update-callback)
            (goto-char om-beg)
            (if (re-search-forward om-id-regexp om-end t)
                (setq id (match-string-no-properties 1))))))
      (unless (equal id om-id)
        (if om-id
            (remhash om-id om-id-hash-table))
        (if id
            (puthash id om-node om-id-hash-table))))))

(defun om-root (node)
  (let ((root node)
        (up (nth 2 node)))
    (while up
      (setq root up
            up (nth 2 up)))
    root))

(defun om-buf (node)
  (car (rassoc (om-root node) om-root-alist)))

(defun om-level (node)
  (let ((level 0)
        (up (nth 2 node)))
    (while up 
      (setq up (nth 2 up)
            level (1+ level)))
    level))
  
(defun om-beg (node)
  (let ((beg 1)
        (parent (nth 2 node)))
    (while parent
      (setq beg (+ beg (car node))
            node parent
            parent (nth 2 node)))
    beg))

(defun om-parent (node)
  (nth 2 node))

(defun om-first-child (node)
  (nth 3 node))

(defun om-last-child (parent)
  (let ((node (nth 3 parent))
        next)
    (when node
      (setq next (nth 1 node))
      (while next
        (setq node next
              next (nth 1 node))))
    node))

(defun om-next (node)
  (nth 1 node))

(defun om-prev (node)
  (nth 4 node))

(defun om-node-below (node)
  (let (snext)
    (or (nth 3 node)
        (catch 'exit
          (while node
            (if (setq snext (nth 1 node))
                (throw 'exit snext))
            (setq node (nth 2 node)))))))

(defun om-subtree-next (node &optional root)
  (let ((root (or root om-root))
        snext)
    (catch 'exit
      (while (not (eq node root))
        (if (setq snext (nth 1 node))
            (throw 'exit snext))
        (setq node (nth 2 node))))))
    
;; positive value
(defmacro om-offset-from-parent (node)
  (list 'car node))

(defun om-subtree-range (node)
  (let ((off 0)
        (parent (nth 2 node))
        size next beg end)
    (while parent
      (setq off (+ off (car node)))
      (unless size
        (setq next (nth 1 node))
        (if next
            (setq size (- (car next) off))))
      (setq node parent
            parent (nth 2 node)))
    (setq beg (1+ off)
          end (if size (+ beg size) (car node)))
    (cons beg end)))

(defun om-node-range (node)
  (let ((first (nth 3 node))
         beg end)
    (if (not first)
        (om-subtree-range node)
      (setq beg (om-beg node)
            end (+ beg (car first)))
      (cons beg end))))

(defun om-set-node-bit (node bit)
  (let ((cons (nthcdr 5 node)))
    (setcar cons (logior bit (car cons)))))
  
(defun om-set-sparse-bit (node bit)
  (let* ((cons (nthcdr 5 node))
         (abit (lsh bit 1))
         (ancestor (nth 2 node)))
    (setcar cons (logior bit (car cons)))
    (while (and ancestor
                (setq cons (nthcdr 5 ancestor))
                (= 0 (logand abit (car cons))))
      (setcar cons (logior abit (car cons)))
      (setq ancestor (nth 2 ancestor)))))
  
(defun om-clear-sparse-bit (node bit &optional subtree)
  (let* ((parent (nth 2 node))
         (2bits (logior bit (lsh bit 1)))
         (acompl (lognot (lsh bit 1)))
         (cons (nthcdr 5 node))
         child)
    (if subtree
        (setcar cons (logand (lognot 2bits) (car cons)))
      (setcar cons (logand (lognot bit) (car cons))))
    (catch 'exit
      (while parent
        (setq child (nth 3 parent))
        (while child
          (if (/= 0 (logand 2bits (nth 5 child)))
              (throw 'exit nil))
          (setq child (nth 1 child)))
        (setq cons (nthcdr 5 parent)
              parent (nth 2 parent))
        (setcar cons (logand acompl (car cons)))))))

;; subtree is the starting node
(defun om-traverse-sparse-subtree (subtree bit func)
  (let ((abit (lsh bit 1))
        (current subtree)
        (beg (om-beg subtree))
        (first (nth 3 subtree))
        end next send node) ;; send: subtree end
    (catch 'exit  ;; only from funcall
      (while current
        (if first
            (setq end (+ beg (1- (car first)))))
        (if (/= 0 (logand abit (nth 5 current)))
            (setq next first
                  send end)
          (setq next (nth 1 current)
                send (- beg (car current))
                node current)
          (while (and (nth 2 node) (not next))
            (setq node (nth 2 node)
                  send (- send (car node))
                  next (nth 1 node)))
          (setq send (if next (1- (+ send (car next))) (car node))))
        (if (/= 0 (logand bit (nth 5 current)))
            (funcall func current beg (if first end send)))
        (setq current next
              beg (if current (1+ send))
              first (if current (nth 3 current)))))))

(defun om-prev-visible (subtree pos)
  (let ((node subtree)
        (nbeg pos)
        ysibling first ybeg fbeg pbeg)
    (catch 'exit
      (while t
        (if ysibling    
            (setq node ysibling nbeg ybeg)
          (setq first (nth 3 node)
                fbeg (if first (+ nbeg (car first))))
          (if (or (not first) (outline-invisible-p fbeg))
              (throw 'exit nbeg)
            (setq pbeg nbeg
                  node first
                  nbeg fbeg)))
        (if (setq ysibling (nth 1 node))
            (setq ybeg (+ pbeg (car ysibling))))))))
        
(defun om-preorder-traverse (current func &optional subtree)
  (let ((root (or subtree (om-root current)))
        (node current)
        next)
    (setq node   ;; the node below the current node 
          (or (nth 3 current)
              (catch :found
                (while (not (eq node root))
                  (if (setq next (nth 1 node))
                      (throw :found next)
                    (setq node (nth 2 node)))))))
    (catch 'exit
      (while node
        (setq node
              (or (and (funcall func node) (nth 3 node))
                  (catch :found
                    (while (not (eq node root))
                      (if (setq next (nth 1 node))
                          (throw :found next)
                      (setq node (nth 2 node)))))))))))

(defun om-preorder-traverse-plus (current func &optional subtree)
  (let* ((root (or subtree (om-root current)))
         (beg (om-beg current))
         (node (nth 3 current))
         next off)
    (if node
        (setq beg (+ beg (car node)))
      (setq node current
            node (catch :found
                   (while (not (eq node root))
                     (setq off (car node)
                           beg (- beg off)
                           next (nth 1 node))
                     (if next
                       (setq beg (+ beg (car next)))
                       (throw :found next))
                     (setq node (nth 2 node))))))
    (catch 'exit
      (while node
        (if (and (funcall func node beg) (setq node (nth 3 node)))
            (setq beg (+ beg (car node)))
          (setq node
                (catch :found
                  (while (not (eq node root))
                    (setq off (car node)
                          beg (- beg off)
                          next (nth 1 node))
                    (when next
                      (setq beg (+ beg (car next)))
                      (throw :found next))
                    (setq node (nth 2 node))))))))))

(defun om-traverse-beginings (anchor func bit &optional subtree)
  (let ((root (or subtree (om-root anchor)))
        (beg (om-beg anchor))
        (selected anchor)
        (parent anchor)
        candidate)
    (catch 'exit
      (while selected
        (unless (and (funcall func selected beg)
                     (setq candidate (nth 3 selected)))
          (while (and (null candidate) (not (eq parent root)))
            (setq beg (- beg (car parent))
                  candidate (nth 1 parent)
                  parent (nth 2 parent))))
        (setq selected candidate)
        (when selected
          (setq beg (+ beg (car candidate))
                parent selected
                candidate nil))))))

(defun om-traverse-backward (anchor func &optional root)
  (let* ((root (or root (om-root root)))
         (selected anchor)
         (parent anchor)
         candidate)
    (while selected 
      (setq candidate
            (if (and (funcall func selected) (nth 3 selected))
                (nth 3 selected)
              (if (eq selected (nth 3 (nth 2 selected)))
                  (nth 2 selected)
                (nth 4 selected))))
      (setq selected candidate)
      (when selected
        (setq parent selected
              candidate nil)))))


(defun om-clone-subtree (subtree)
  (let* ((root (list 0 nil nil nil nil (nth 5 subtree) 0))
         (ns root)
         (np root)
         (os subtree)
         (op subtree)
         oc nc)
    (while os
      (if (setq oc (nth 3 os))
          (progn
            (setq nc (list (car oc) nil nil nil nil (nth 5 oc) nil))
            (setcar (nthcdr 3 ns) nc)
            (setcar (nthcdr 2 nc) ns))
        (while (and (null oc) (not (eq op subtree)))
          (setq oc (nth 1 op)
                ns np
                op (nth 2 op)
                np (nth 2 np))
          (when (eq oc (nth 3 op))
            (setq oc nil)
            (setcar (nthcdr 1 ns) (nth 3 np))
            (setcar (nthcdr 4 (nth 3 np)) ns)))
        (when oc
          (setq nc (list (car oc) nil nil nil nil (nth 5 oc) nil))
          (setcar (nthcdr 1 ns) nc)
          (setcar (nthcdr 4 nc) ns)
          (setcar (nthcdr 2 nc) (nth 2 ns))))
      (if oc
          (setq os oc
                op os
                ns nc
                np ns)
        (setq os nil)))
    root))

(defun om-move-down ()
  (let* ((inhibit-read-only t)
         (prev om-node)
         (next (om-next prev))
         (beg om-beg)
         (middle om-subtree-end)
         (end (cdr (om-subtree-range next)))
         (txt (buffer-substring middle end)))
    (delete-region middle end)
    (goto-char beg)
    (insert txt)
    (+ beg (- end middle))))

(defun om-move-up ()
  (let* ((inhibit-read-only t)
         (next om-node)
         (prev (om-prev next))
         (middle om-beg)
         (end om-subtree-end)
         (beg (- middle (- (car next) (car prev))))
         (txt (buffer-substring middle end)))
    (delete-region middle end)
    (goto-char beg)
    (insert txt)
    beg))

(defun om-run-children-forward (parent func)
  (let* ((beg (om-beg parent))
         (first (nth 3 parent))
         (child first))
    (while child
      (funcall func child (+ beg (car child)))
      (setq child (nth 1 child)))))

(defun om-insert-entry-under (heading &optional anchor)
  (let* ((inhibit-read-only t)
         (pos (if anchor
                  (cdr (om-node-range anchor))
                om-end))
         (level (1+ (if anchor
                        (om-level anchor)
                      om-level)))
         (heading (concat
                   (make-string level ?\*)
                   " "
                   heading
                   " \n")))
    (goto-char pos)
    (insert heading)
    pos))

(defun om-insert-entry-after (heading &optional anchor)
  (let* ((inhibit-read-only t)
         (pos (if anchor
                  (cdr (om-subtree-range anchor))
                om-subtree-end))
         (level (if anchor
                    (om-level anchor)
                  om-level))
         (heading (concat
                   (make-string level ?\*)
                   " "
                   heading
                   " \n")))
    (goto-char pos)
    (insert heading)
    pos))

(defun om-remove-subtree (node)
  (let* ((inhibit-read-only t)
         (range (om-subtree-range node))
         (beg (car range))
         (end (cdr range)))
    (delete-region beg end)))

(defun om-back-up (pos anchor)
  (let* ((node anchor)
         (parent (nth 2 node))
         (beg (om-beg parent)))
    (while (< pos beg)
      (setq beg (- beg (car parent))
            node parent
            parent (nth 2 parent)))
    node))

(defun om-get-promoted-subtree-string (beg end level)
  (let ((pos beg)
        txt skip)
    (goto-char beg)
    (loop do 
          (setq pos (point)
                skip (if (looking-at "\\*+[ \t]")
                         level
                       (if (looking-at "[ \t]+")
                           (if (<= level (- (match-end 0) pos))
                               level
                             (- (match-end 0) pos))
                         0)))
          (forward-line)
          (setq txt (concat txt
                            (buffer-substring-no-properties
                             (+ pos skip) (point))))
          while (< (point) end))
    txt))

(defun om-get-demoted-subtree-string (beg end level)
  (let ((stars (make-string level ?\*))
        (spaces (make-string level ?\s))
        (pos beg)
        txt prepand)
    (goto-char beg)
    (loop do 
          (setq pos (point)
                prepand (if (looking-at "\\*+[ \t]")
                            stars
                          (if (looking-at "[ \t]")
                              spaces)))
          (forward-line)
          (setq txt
                (concat txt prepand
                        (buffer-substring-no-properties
                         pos (point))))
          while (< (point) end))
    txt))

(defun om-do-copy (subtree anchor)
  (interactive)
  (let* ((inhibit-read-only t)
         (tfirst (nth 3 anchor))
         (trange (om-node-range anchor))
         (nbeg (cdr trange))
         (tbuf (om-buf anchor))
         (olevel (om-level subtree))
         (orange (om-subtree-range subtree))
         (obeg (car orange))
         (oend (cdr orange))
         (nlevel (1+ (om-level anchor)))
         (diff (- nlevel olevel))
         (txt (if (= 0 diff)
                  (buffer-substring-no-properties obeg oend)
                (if (> 0 diff)
                    (om-get-promoted-subtree-string obeg oend (- diff))
                  (om-get-demoted-subtree-string obeg oend diff)))))
    (unless (bufferp tbuf)
      (error "target not found"))
    (if (eq (current-buffer) tbuf)
        (if (and (> nbeg obeg)
                 (<= nbeg oend))
            (error "Cannot refile to position inside the subtree itself")))
    (with-current-buffer tbuf
      (save-excursion ;; in case buffer of org lite mark is current
        (save-restriction
          (widen)
          (goto-char nbeg)
          (insert txt))))))

;; resposible for set bit and indicate need to go into the subtree.
(defun om-mark-bit-enter-function (node bit)
  (let ((flagcons (nthcdr 5 node)))
    (setcar flagcons (logior bit (car flagcons))))) ;; set bit

;; resposible for set abit for the parent.
(defun om-mark-bit-exit-function (node bit)
  (let* ((flagcons (nthcdr 5 node))
         (abit (lsh bit 1))  ;; ancestor bit
         (bits (logior bit abit))
         (neg-bits (lognot bits))
         (pflagcons (nthcdr 5 (nth 2 node))))
    (if (/= 0 (logand bits (car flagcons)))  ;; some bit(s) is on
        (setcar pflagcons (logior abit (car pflagcons)))  ;; set ancestor bit
      )))

(defun om-node-id-get (beg end)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char beg)
      (if (re-search-forward om-id-regexp end t)
          (match-string-no-properties 1)))))

(defun om-subtree-marked-nodes (node bit)
  (let* ((abit (lsh bit 1))
         (list nil)
         (func
          (lambda (n)
            (if (om-node-bit-on n bit)
                (setq list (cons n list)))
            (om-node-bit-on n abit)
            )))
    (if (om-node-bit-on node bit)
       (setq list (cons node list)))
    (if (om-node-bit-on node abit)
        (om-preorder-traverse node func node))
    list))

(defun om-node-id-get-create (node)
  (let* ((range (om-node-range node))
         (beg (car range))
         (end (cdr range))
         (level (om-level node))
         (indent (make-string (1+ level) ?\s))
         buf id)
    (save-excursion
      (save-restriction
        (setq buf (om-buf node))
        (unless (eq (current-buffer) buf)
          (set-buffer buf))
        (widen)
        (goto-char beg)
        (end-of-line)
        (if (re-search-forward om-id-regexp (1- end) t)
            (setq id (match-string-no-properties 1))
          (setq id (org-id-new))
          (if (re-search-forward "^[ \t]*\\(:END:\\)[ \t]*$" (1- end) t)
              (progn
                (goto-char (match-beginning 1))
                (insert (concat ":ID:" "        " id "\n" indent)))
            (insert (concat "\n" indent ":PROPERTIES:\n" indent
                            ":ID:" "        " id "\n" indent ":END: "))))))
    id))

(defun om-id-lookup (id)
  (gethash id om-id-hash-table nil))

(defun om-remove-id (id)
  (remhash id om-id-hash-table))

(defun om-add-id (id node)
  (puthash id node om-id-hash-table))

(defun om-subtree-node-count (node)
  (let* ((count 1)
        (callback
         (lambda (n)
           (incf count))))
    (om-preorder-traverse node callback node)
    count))

;; (defmacro om-parent (node)
;;   (list 'nth 2 node))

 ;; (defmacro om-first (node)
 ;;   `(nth 3 ,node))

;; (defmacro om-first (node)
;;   (list 'nth 3 node))

;; (defmacro om-prev (node)
;;   (list 'nth 4 node))
  
;; (defmacro om-next (node)
;;   (list 'nth 1 node))

;; positive value
(defmacro om-prev-diff (node)
  (list '- (list 'car node) (list 'car (list 'nth 4 node))))

;; positive value
(defmacro om-next-diff (node)
  (list '- (list 'car (list 'nth 1 node)) (list 'car node)))

;; (defmacro om-node-bit-on (node bit)
;;   (list '/= 0 (list 'logand bit (list 'nth 5 node))))

(defun om-node-bit-on (node bit)
  (/= 0 (logand bit (nth 5 node))))

(defun om-node-abit-on (node bit)
  (let ((flags (nth 5 node))
        (abit (lsh bit 1)))
    (/= 0 (logand abit flags))))

(defun om-node-bit-or-abit-on (node bit)
  (let ((flags (nth 5 node))
        (abit (lsh bit 1)))
    (/= 0 (logand (logior bit abit) flags)))) 

(defun om-clear-enter-ancestors (anchor)
  (let* ((node anchor)
         (compl (lognot 1))
         (fcons (nthcdr 5 node)))
    (while node
      (setcar fcons (logand compl (car fcons)))
      (setq node (nth 2 node)
                    fcons (nthcdr 5 node)))))

(defun om-id-to-buffer (id)
  (let ((node (om-id-lookup id)))
    (if node
        (car (rassq (om-root node)
                    om-root-alist)))))

(defun om-get-root-pair (node)
  (let ((second node)
        (root (nth 2 node)))
    (while (nth 2 root)
      (setq second root
            root (nth 2 root)))
    (cons root second)))

(defun om-is-ancestor (node ancestor)
  (let ((up node)
        ia)
    (catch 'exit
      (loop do
            (if (eq up ancestor)
                (throw 'exit t))
            while (setq up (nth 2 up))))))

(defun om-touch ()
  (interactive)
  (widen)
  (goto-char (point-min))
  (while (re-search-forward "^\\*+ " nil t)
    (forward-line -1)
    (if (looking-at "[ \t]+$")
        (delete-region (point) (point-at-eol))
      (unless (= (char-after) ?\n)
        (end-of-line)
        (insert-char ?\n 1)))
    (forward-line)
    (end-of-line)
    (unless (= (char-before) ?\s)
      (insert-char ?\s 1)))
  (goto-char (point-max))
  (unless (= (char-before) ?\n)
    (insert-char ?\n 1)))

(provide 'om)

