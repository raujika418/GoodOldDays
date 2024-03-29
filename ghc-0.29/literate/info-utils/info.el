;; Info package for Emacs  --  Dave Gillespie's version 1.05 of 3/8/92.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; This is based on the version 19 info.el file.
;;
;; Note that Info-directory has been replaced by Info-directory-list,
;; a search path of directories in which to find Info files.
;; Also, Info tries adding ".info" to a file name if the name itself
;; is not found.
;;
;; This file also has several small bug fixes compared to the 18.57 version.


;; LCD Archive Entry:
;; info-dg|Dave Gillespie|daveg@synaptics.com
;; |Info reader with many enhancements; replaces standard info.el.
;; |92-03-08|1.05|~/modes/info.el

;; Also available from anonymous FTP on csvax.cs.caltech.edu.


;; Modified 3/7/1991 by Dave Gillespie:
;; (Author's address: daveg@synaptics.com or daveg@csvax.cs.caltech.edu)
;;
;; Added keys:  i, t, <, >, [, ], {, }, 6, 7, 8, 9, 0.
;; Look at help for info-mode (type ? in Info) for descriptions.
;;
;; If Info-directory-list is undefined and there is no INFOPATH
;; in the environment, use value of Info-directory for compatibility
;; with Emacs 18.57.
;;
;; All files named "localdir" found in the path are appended to "dir",
;; the Info directory.  For this to work, "dir" should contain only
;; one node (Top), and each "localdir" should contain no ^_ or ^L
;; characters.  Generally they will contain only one or several
;; additional lines for the top-level menu.  Note that "dir" is
;; modified in memory each time it is loaded, but not on disk.
;;
;; If "dir" contains a line of the form:  "* Locals:"
;; then the "localdir"s are inserted there instead of at the end.


;; Modified 4/3/1991 by Dave Gillespie:
;;
;; Added Info-mode-hook (suggested by Sebastian Kremer).
;; Also added epoch-info-startup/select-hooks from Simon Spero's info.el.
;;
;; Added automatic decoding of compressed Info files.
;; See documentation for the variable Info-suffix-list.  Default is to
;; run "uncompress" on ".Z" files and "unyabba" on ".Y" files.
;; (See comp.sources.unix v24i073-076 for yabba/unyabba, a free software
;; alternative to compress/uncompress.)
;; Note: "dir" and "localdir" files should not be compressed.
;;
;; Changed variables like Info-enable-edit to be settable by M-x set-variable.
;;
;; Added Info-auto-advance variable.  If t, SPC and DEL will act like
;; } and {, i.e., they advance to the next/previous node if at the end
;; of the buffer.
;;
;; Changed `u' to restore point to most recent location in that node.
;; Added `=' to do this manually at any time.  (Suggested by David Fox).
;;
;; Changed `m' and `0-9' to try interpreting menu name as a file name
;; if not found as a node name.  This allows (dir) menus of the form,
;;     Emacs::		Cool text editor
;; as a shorthand for
;;     Emacs:(emacs).	Cool text editor
;;
;; Enhanced `i' to use line-number information in the index.
;; Added `,' to move among all matches to a previous `i' command.
;;
;; Added `a' (Info-annotate) for adding personal notes to any Info node.
;; Notes are not stored in the actual Info files, but in the user's own
;; ~/.infonotes file.
;;
;; Added Info-footnote-tag, made default be "Ref" instead of "Note".
;;
;; Got mouse-click stuff to work under Emacs version 18.  Check it out!
;; Left and right clicks scroll the Info window.
;; Middle click goes to clicked-on node, e.g., "Next:", a menu, or a note.


;; Modified 6/29/1991 by Dave Gillespie:
;;
;; Renamed epoch-info-startup/select-hooks to Info-startup/select-hook.
;;
;; Made Info-select-node into a command on the `!' key.
;;
;; Added Info-mouse-support user option.
;;
;; Cleaned up the implementation of some routines.
;;
;; Added special treatment of quoted words in annotations:  The `g'
;; command for a nonexistent node name scans for an annotation
;; (in any node of any file) containing that name in quotes:  g foo RET
;; looks for an annotation containing:  "foo"
;; If found, it goes to that file and node.
;;
;; Added a call to set up Info-directory-list in Info-find-node to
;; work around a bug in GNUS where it calls Info-goto-node before info.
;;
;; Added completion for `g' command (inspired by Richard Kim's infox.el).
;; Completion knows all node names for the current file, and all annotation
;; tags (see above).  It does not complete file names or node names in
;; other files.
;;
;; Added `k' (Info-emacs-key) and `*' (Info-elisp-ref) commands.  You may
;; wish to bind these to global keys outside of Info mode.
;;
;; Allowed localdir files to be full dir-like files; only the menu part
;; of each localdir is copied.  Also, redundant menu items are omitted.
;;
;; Changed Info-history to hold only one entry at a time for each node,
;; and to be circular so that multiple `l's come back again to the most
;; recent node.  Note that the format of Info-history entries has changed,
;; which may interfere with external programs that try to operate on it.
;; (Also inspired by Kim's infox.el).
;;
;; Changed `n', `]', `l', etc. to accept prefix arguments to move several
;; steps at once.  Most accept negative arguments to move oppositely.
;;
;; Changed `?' to bury *Help* buffer afterwards to keep it out of the way.
;;
;; Rearranged `?' key's display to be a little better for new users.
;;
;; Changed `a' to save whole window configuration and restore on C-c C-c.
;;
;; Fixed the bug reported by Bill Reynolds on gnu.emacs.bugs.
;;
;; Changed Info-last to restore window-start as well as cursor position.
;;
;; Changed middle mouse button in space after end of node to do Info-last
;; if we got here by following a cross reference, else do Info-global-next.
;;
;; Added some new mouse bindings: shift-left = Info-global-next,
;; shift-right = Info-global-prev, shift-middle = Info-last.
;;
;; Fixed Info-follow-reference not to make assumptions about length
;; of Info-footnote-tag [Linus Tolke].
;;
;; Changed default for Info-auto-advance mode to be press-twice-for-next-node.
;;
;; Modified x-mouse-ignore to preserve last-command variable, so that
;; press-twice Info-auto-advance mode works with the mouse.


;; Modified 3/4/1992 by Dave Gillespie:
;;
;; Added an "autoload" command to help autoload.el.
;;
;; Changed `*' command to look for file `elisp' as well as for `lispref'.
;;
;; Fixed a bug involving footnote names containing regexp special characters.
;;
;; Fixed a bug in completion during `f' (or `r') command.
;;
;; Added TAB (Info-next-reference), M-TAB, and RET keys to Info mode.
;;
;; Added new bindings, `C-h C-k' for Info-emacs-key and `C-h C-f' for
;; Info-elisp-ref.  These bindings are made when info.el is loaded, and
;; only if those key sequences were previously unbound.  These bindings
;; work at any time, not just when Info is already running.


;; Modified 3/8/1992 by Dave Gillespie:
;;
;; Fixed some long lines that were causing trouble with mailers.



(provide 'info)

(defvar Info-mouse-support t
  "*Non-nil means to install Info mouse drivers for Emacs version 18.
Set this to nil if you have your own Info mouse support.")

(defvar Info-history nil
  "List of info nodes user has visited.
Each element of list is a list (\"(FILENAME)NODENAME\" BUFPOS WINSTART).")

(defvar Info-keeping-history t
  "Non-nil if Info-find-node should modify Info-history.
This is for use only by certain internal Info routines.")

(defvar Info-enable-edit nil
  "*Non-nil means the \\[Info-edit] command in Info can edit the current node.")

(defvar Info-enable-active-nodes t
  "*Non-nil allows Info to execute Lisp code associated with nodes.
The Lisp code is executed when the node is selected.")

(defvar Info-restoring-point t
  "*Non-nil means to restore the cursor position when re-entering a node.")

(defvar Info-auto-advance 'twice
  "*Control what SPC and DEL do when they can't scroll any further.
If nil, they beep and remain in the current node.
If t, they move to the next node (like Info-global-next/prev).
If anything else, they must be pressed twice to move to the next node.")

(defvar Info-directory-list (getenv "INFOPATH")
  "List of directories to search for Info documentation files.
Default is to use the environment variable INFOPATH if it exists,
else to use Info-directory if it exists.")

(defvar Info-suffix-list '( (".info" . nil)
			    (".Z" . "uncompress -c %s")
			    (".Y" . "unyabba")
			    (".info.Z" . "uncompress -c %s")
			    (".info.Y" . "unyabba") )
  "List of file name suffixes and associated decoding commands.
Each entry should be (SUFFIX . STRING); if STRING contains %s, that is
changed to name of the file to decode, otherwise the file is given to
the command as standard input.  If STRING is nil, no decoding is done.")

(defvar Info-footnote-tag "Ref"
  "*Symbol that identifies a footnote or cross-reference.
All \"*Note\" references will be changed to use this word instead.")

(defvar Info-current-file nil
  "Info file that Info is now looking at, or nil.")

(defvar Info-current-subfile nil
  "Info subfile that is actually in the *info* buffer now,
or nil if current info file is not split into subfiles.")

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker (make-marker)
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defvar Info-current-file-completions nil
  "Cached completion list for current Info file.")

(defvar Info-index-alternatives nil
  "List of possible matches for last Info-index command.")

(defvar Info-annotations-path '("~/.infonotes" "/usr/lib/info.notes")
  "*Names of files that contain annotations for different Info nodes.
By convention, the first one should reside in your personal directory.
The last should be a world-writable \"public\" annotations file.")

(defvar Info-in-cross-reference nil)
(defvar Info-window-configuration nil)

;;;###autoload
(defun info (&optional file)
  "Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Info file name: " nil nil t))))
  (Info-find-directory-list)
  (Info-setup-x)
  (if file
      (Info-goto-node (concat "(" file ")"))
    (if (get-buffer "*info*")
	(switch-to-buffer "*info*")
      (let ((f Info-annotations-path))
	(while f
	  (if (file-exists-p (car f))
	      (bury-buffer (find-file-noselect (car f))))
	  (setq f (cdr f))))
      (Info-directory))))

(defun Info-find-directory-list ()
  (if (stringp Info-directory-list)
      (let ((path Info-directory-list)
	    list)
	(while (> (length path) 0)
	  (let ((idx (or (string-match ":" path) (length path))))
	    (setq list (cons (substring path 0 idx) list)
		  path (substring path (min (1+ idx) (length path))))))
	(setq Info-directory-list (nreverse list))))
  (or Info-directory-list
      (setq Info-directory-list (list (if (boundp 'Info-directory)
					  Info-directory
					".")))))
(Info-find-directory-list)

;; Go to an info node specified as separate filename and nodename.
;; no-going-back is non-nil if recovering from an error in this function;
;; it says do not attempt further (recursive) error recovery.
(defun Info-find-node (filename nodename &optional no-going-back tryfile line)
  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (if filename
      (let (temp temp-downcase found)
	(setq filename (substitute-in-file-name filename))
	(let ((dirs (if (string-match "^\\./" filename)
			;; If specified name starts with `./'
			;; then just try current directory.
			'("./")
		      Info-directory-list)))
	  ;; Search the directory list for file FILENAME.
	  (while (and dirs (not found))
	    (setq temp (expand-file-name filename (car dirs)))
	    (setq temp-downcase
		  (expand-file-name (downcase filename) (car dirs)))
	    (if (equal temp-downcase temp) (setq temp-downcase nil))
	    ;; Try several variants of specified name.
	    ;; Try downcasing, appending a suffix, or both.
	    (setq found (Info-suffixed-file temp temp-downcase))
	    (setq dirs (cdr dirs))))
	(if found
	    (setq filename found)
	  (error "Info file %s does not exist" filename))))
  ;; Go into info buffer.
  (switch-to-buffer "*info*")
  (run-hooks 'Info-startup-hook)
  (or (eq major-mode 'Info-mode)
      (Info-mode))
  ;; Record the node we are leaving.
  (if (and Info-current-file (not no-going-back))
      (Info-history-add Info-current-file Info-current-node (point)))
  (widen)
  (setq Info-current-node nil
	Info-in-cross-reference nil)
  (unwind-protect
      (progn
	;; Switch files if necessary
	(or (null filename)
	    (equal Info-current-file filename)
	    (let ((buffer-read-only nil))
	      (setq Info-current-file nil
		    Info-current-subfile nil
		    Info-index-alternatives nil
		    Info-current-file-completions nil
		    buffer-file-name nil)
	      (erase-buffer)
	      (Info-insert-file-contents filename t)
	      ;; Add all "localdir" files in search path to "dir" file.
	      (if (string-match "^dir$" (file-name-nondirectory filename))
		  (let ((d Info-directory-list)
			name)
		    (goto-char (point-max))
		    (if (re-search-backward "^ *\\* *Locals *: *\n" nil t)
			(delete-region (match-beginning 0) (match-end 0))
		      (search-backward "\^L" nil t))
		    (while d
		      (setq name (expand-file-name "localdir" (car d)))
		      (if (or (file-exists-p name)
			      (file-exists-p
			       (setq name (concat name ".info"))))
			  ;; Insert menu part of the file
			  (let* ((pt (point))
				 (len (nth 1 (insert-file-contents name))))
			    (if (search-forward "* menu:" (+ pt len) t)
				(progn
				  (forward-line 1)
				  (delete-region pt (point))))))
		      (setq d (cdr d)))
		    ;; Eliminate redundant menu entries.
		    (goto-char (point-min))
		    (while (re-search-forward "\n\\* \\([^:\n]*\\):" nil t)
		      (let ((str (buffer-substring (match-beginning 1)
						   (match-end 1))))
			(save-excursion
			  (if (search-forward (format "\n* %s:" str) nil t)
			      (let ((pt (- (point) 3 (length str))))
				(forward-line 1)
				(delete-region pt (point)))))))))
	      (set-buffer-modified-p nil)
	      (setq default-directory (file-name-directory filename))
	      ;; See whether file has a tag table.  Record the location if yes.
	      (set-marker Info-tag-table-marker nil)
	      (goto-char (point-max))
	      (forward-line -8)
	      (or (equal nodename "*")
		  (not (search-forward "\^_\nEnd tag table\n" nil t))
		  (let (pos)
		    ;; We have a tag table.  Find its beginning.
		    ;; Is this an indirect file?
		    (search-backward "\nTag table:\n")
		    (setq pos (point))
		    (if (save-excursion
			  (forward-line 2)
			  (looking-at "(Indirect)\n"))
			;; It is indirect.  Copy it to another buffer
			;; and record that the tag table is in that buffer.
			(save-excursion
			  (let ((buf (current-buffer)))
			    (set-buffer
			     (get-buffer-create " *info tag table*"))
			    (setq case-fold-search t)
			    (erase-buffer)
			    (insert-buffer-substring buf)
			    (set-marker Info-tag-table-marker
					(match-end 0))))
		     (set-marker Info-tag-table-marker pos))))
	      (setq Info-current-file
		    (file-name-sans-versions buffer-file-name))))
	(if (equal nodename "*")
	    (progn (setq Info-current-node nodename)
		   (Info-set-mode-line)
		   (goto-char (point-min)))
	  ;; Search file for a suitable node.
	  ;; First get advice from tag table if file has one.
	  ;; Also, if this is an indirect info file,
	  ;; read the proper subfile into this buffer.
	  (let ((guesspos (point-min)))
	    (if (marker-position Info-tag-table-marker)
		(save-excursion
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (goto-char Info-tag-table-marker)
		  (if (search-forward (concat "Node: " nodename "\177") nil t)
		      (progn
			(setq guesspos (read (current-buffer)))
			;; If this is an indirect file,
			;; determine which file really holds this node
			;; and read it in.
			(if (not (eq (current-buffer) (get-buffer "*info*")))
			    (setq guesspos
				  (Info-read-subfile guesspos)))))))
	    (goto-char (max (point-min) (- guesspos 1000))))
	  ;; Now search from our advised position (or from beg of buffer)
	  ;; to find the actual node.
	  (let ((regexp (concat "Node: *" (regexp-quote nodename) " *[,\t\n]"))
		(found t))
	    (catch 'foo
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward regexp beg t)
		      (throw 'foo t))))
	      (setq found nil)
	      (let ((bufs (delq nil (mapcar 'get-file-buffer
					    Info-annotations-path)))
		    (pattern (concat "\"" nodename "\""))
		    (pat2 (concat "------ *File: *\\([^ ].*[^ ]\\) *Node: "
				  "*\\([^ ].*[^ ]\\) *Line: *\\([0-9]+\\)"))
		    (afile nil) anode aline)
		(while (and bufs (not anode))
		  (save-excursion
		    (set-buffer (car bufs))
		    (goto-char (point-min))
		    (if (search-forward pattern nil t)
			(if (re-search-backward pat2 nil t)
			    (setq afile
				  (buffer-substring (match-beginning 1)
						    (match-end 1))
				  anode
				  (buffer-substring (match-beginning 2)
						    (match-end 2))
				  aline
				  (string-to-int
				   (buffer-substring (match-beginning 3)
						     (match-end 3)))))))
		  (setq bufs (cdr bufs)))
		(if anode
		    (Info-find-node afile anode t nil aline)
		  (if tryfile
		      (condition-case err
			  (Info-find-node nodename "Top" t)
			(error nil)))))
	      (or Info-current-node
		  (error "No such node: %s" nodename)))
	    (if found
		(progn
		  (Info-select-node)
		  (goto-char (point-min))
		  (if line (forward-line line)))))))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back
	(let ((hist (car Info-history)))
	  ;; The following is no longer safe with new Info-history system
	  ;; (setq Info-history (cdr Info-history))
	  (Info-goto-node (car hist) t)
	  (goto-char (nth 1 hist))))))

(defun Info-history-add (file node point)
  (if Info-keeping-history
      (let* ((name (format "(%s)%s" (Info-file-name-only file) node))
	     (found (assoc name Info-history)))
	(if found
	    (setq Info-history (delq found Info-history)))
	(setq Info-history (cons (list name point
				       (and (eq (window-buffer)
						(current-buffer))
					    (window-start)))
				 Info-history)))))

(defun Info-file-name-only (file)
  (let ((dir (file-name-directory file))
	(p Info-directory-list))
    (while (and p (not (equal (car p) dir)))
      (setq p (cdr p)))
    (if p (file-name-nondirectory file) file)))

(defun Info-read-subfile (nodepos)
  (set-buffer (marker-buffer Info-tag-table-marker))
  (goto-char (point-min))
  (search-forward "\n\^_")
  (let (lastfilepos
	lastfilename)
    (forward-line 2)
    (catch 'foo
      (while (not (looking-at "\^_"))
	(if (not (eolp))
	    (let ((beg (point))
		  thisfilepos thisfilename)
	      (search-forward ": ")
	      (setq thisfilename  (buffer-substring beg (- (point) 2)))
	      (setq thisfilepos (read (current-buffer)))
	      ;; read in version 19 stops at the end of number.
	      ;; Advance to the next line.
	      ;; (forward-line 1)
	      (if (> thisfilepos nodepos)
		  (throw 'foo t))
	      (setq lastfilename thisfilename)
	      (setq lastfilepos thisfilepos))
	  (throw 'foo t))))
    (set-buffer (get-buffer "*info*"))
    (or (equal Info-current-subfile lastfilename)
	(let ((buffer-read-only nil))
	  (setq buffer-file-name nil)
	  (widen)
	  (erase-buffer)
	  (Info-insert-file-contents (Info-suffixed-file lastfilename))
	  (set-buffer-modified-p nil)
	  (setq Info-current-subfile lastfilename)))
    (goto-char (point-min))
    (search-forward "\n\^_")
    (+ (- nodepos lastfilepos) (point))))

(defun Info-suffixed-file (name &optional name2)
  (cond ((file-exists-p name)
	 name)
	((and name2 (file-exists-p name2))
	 name2)
	(t
	 (let ((suff Info-suffix-list)
	       (found nil))
	   (while (and suff (not found))
	     (if (file-exists-p (concat name (car (car suff))))
		 (setq found (concat name (car (car suff))))
	       (if (and name2 (file-exists-p (concat name2 (car (car suff)))))
		   (setq found (concat name2 (car (car suff))))
		 (setq suff (cdr suff)))))
	   found))))

(defun Info-insert-file-contents (file &optional visit)
  (let ((suff Info-suffix-list))
    (while (and suff (or (<= (length file) (length (car (car suff))))
			 (not (equal (substring file
						(- (length (car (car suff)))))
				     (car (car suff))))))
      (setq suff (cdr suff)))
    (if (stringp (cdr (car suff)))
	(let ((command (if (string-match "%s" (cdr (car suff)))
			   (format (cdr (car suff)) file)
			 (concat (cdr (car suff)) " < " file))))
	  (message "%s..." command)
	  (call-process shell-file-name nil t nil "-c" command)
	  (message "")
	  (if visit
	      (progn
		(setq buffer-file-name file)
		(set-buffer-modified-p nil)
		(clear-visited-file-modtime))))
      (insert-file-contents file visit))))

(defun Info-select-node ()
  "Select the node that point is in, after using `g *' to select whole file."
  (interactive)
  (widen)
  (save-excursion
   ;; Find beginning of node.
   (search-backward "\n\^_")
   (forward-line 2)
   ;; Get nodename spelled as it is in the node.
   (re-search-forward "Node:[ \t]*")
   (setq Info-current-node
	 (buffer-substring (point)
			   (progn
			    (skip-chars-forward "^,\t\n")
			    (point))))
   (Info-set-mode-line)
   ;; Find the end of it, and narrow.
   (beginning-of-line)
   (let (active-expression)
     (narrow-to-region (point)
		       (if (re-search-forward "\n[\^_\f]" nil t)
			   (prog1
			    (1- (point))
			    (if (looking-at "[\n\^_\f]*execute: ")
				(progn
				  (goto-char (match-end 0))
				  (setq active-expression
					(read (current-buffer))))))
			 (point-max)))
     (or (equal Info-footnote-tag "Note")
	 (progn
	   (goto-char (point-min))
	   (let ((buffer-read-only nil)
		 (bufmod (buffer-modified-p)))
	     (while (re-search-forward "\\*Note\\([ \n]\\)" nil t)
	       (replace-match (concat "*" Info-footnote-tag "\\1")))
	     (set-buffer-modified-p bufmod))))
     (Info-reannotate-node)
     (run-hooks 'Info-select-hook)
     (if Info-enable-active-nodes (eval active-expression)))))

(defun Info-set-mode-line ()
  (setq mode-line-buffer-identification
	(concat
	 "Info:  ("
	 (if Info-current-file
	     (file-name-nondirectory Info-current-file)
	   "")
	 ")"
	 (or Info-current-node ""))))

;; Go to an info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

(defun Info-goto-node (nodename &optional no-going-back tryfile)
  "Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME.
Actually, the following interpretations of NAME are tried in order:
    (FILENAME)NODENAME
    (FILENAME)     (using Top node)
    NODENAME       (in current file)
    TAGNAME        (see below)
    FILENAME       (using Top node)
where TAGNAME is a string that appears in quotes: \"TAGNAME\", in an
annotation for any node of any file.  (See `a' for annotations.)"
  (interactive (list (Info-read-node-name "Goto node, file or tag: ")
		     nil t))
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (if (= (match-beginning 1) (match-end 1))
		       ""
		     (substring nodename (match-beginning 2) (match-end 2)))
	  nodename (substring nodename (match-beginning 3) (match-end 3)))
    (let ((trim (string-match "\\s *\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s *\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename)
		    no-going-back (and tryfile (equal filename "")))))

(defun Info-restore-point (&optional always)
  "Restore point to same location it had last time we were in this node."
  (interactive "p")
  (if (or Info-restoring-point always)
      (let* ((name (format "(%s)%s"
			   (Info-file-name-only Info-current-file)
			   Info-current-node))
	     (p (assoc name Info-history)))
	(if p (Info-restore-history-entry p)))))

(defun Info-restore-history-entry (entry)
  (goto-char (nth 1 entry))
  (and (nth 2 entry)
       (get-buffer-window (current-buffer))
       (set-window-start (get-buffer-window (current-buffer))
			 (nth 2 entry))))

(defun Info-read-node-name (prompt &optional default)
  (Info-build-node-completions)
  (let* ((completion-ignore-case t)
	 (nodename (completing-read prompt Info-current-file-completions)))
    (if (equal nodename "")
	(or default
	    (Info-read-node-name prompt))
      nodename)))

(defun Info-build-node-completions ()
  (or Info-current-file-completions
      (let ((compl nil))
	(save-excursion
	  (save-restriction
	    (if (marker-buffer Info-tag-table-marker)
		(progn
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (goto-char Info-tag-table-marker)
		  (while (re-search-forward "\nNode: \\(.*\\)\177" nil t)
		    (setq compl
			  (cons (list (buffer-substring (match-beginning 1)
							(match-end 1)))
				compl))))
	      (widen)
	      (goto-char (point-min))
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward "Node: *\\(.*\\) *[,\n\t]" beg t)
		      (setq compl 
			    (cons (list (buffer-substring (match-beginning 1)
							  (match-end 1)))
				  compl)))))))
	  (let ((bufs (delq nil (mapcar 'get-file-buffer
					Info-annotations-path))))
	    (while bufs
	      (set-buffer (car bufs))
	      (goto-char (point-min))
	      (while (re-search-forward "\"\\(.*\\)\"" nil t)
		(setq compl 
		      (cons (list (buffer-substring (match-beginning 1)
						    (match-end 1)))
			    compl)))
	      (setq bufs (cdr bufs)))))
	(setq Info-current-file-completions compl))))

(defvar Info-last-search nil
  "Default regexp for Info S command to search for.")

(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in."
  (interactive "sSearch (regexp): ")
  (if (equal regexp "")
      (setq regexp Info-last-search)
    (setq Info-last-search regexp))
  (let ((found ()) current
	(onode Info-current-node)
	(ofile Info-current-file)
	(opoint (point))
	(osubfile Info-current-subfile))
    (save-excursion
      (save-restriction
	(widen)
	(if (null Info-current-subfile)
	    (progn (re-search-forward regexp) (setq found (point)))
	  (condition-case err
	      (progn (re-search-forward regexp) (setq found (point)))
	    (search-failed nil)))))
    (if (not found) ;can only happen in subfile case -- else would have erred
	(unwind-protect
	    (let ((list ()))
	      (set-buffer (marker-buffer Info-tag-table-marker))
	      (goto-char (point-min))
	      (search-forward "\n\^_\nIndirect:")
	      (save-restriction
		(narrow-to-region (point)
				  (progn (search-forward "\n\^_")
					 (1- (point))))
		(goto-char (point-min))
		(search-forward (concat "\n" osubfile ": "))
		(beginning-of-line)
		(while (not (eobp))
		  (re-search-forward "\\(^.*\\): [0-9]+$")
		  (goto-char (+ (match-end 1) 2))
		  (setq list (cons (cons (read (current-buffer))
					 (buffer-substring (match-beginning 1)
							   (match-end 1)))
				   list))
		  (goto-char (1+ (match-end 0))))
		(setq list (nreverse list)
		      current (car (car list))
		      list (cdr list)))
	      (while list
		(message "Searching subfile %s..." (cdr (car list)))
		(Info-read-subfile (car (car list)))
		(setq list (cdr list))
		(goto-char (point-min))
		(if (re-search-forward regexp nil t)
		    (setq found (point) list ())))
	      (if found
		  (message "")
		(signal 'search-failed (list regexp))))
	  (if (not found)
	      (progn (Info-read-subfile opoint)
		     (goto-char opoint)
		     (Info-select-node)))))
    (widen)
    (goto-char found)
    (Info-select-node)
    (or (and (equal onode Info-current-node)
	     (equal ofile Info-current-file))
	(Info-history-add ofile onode opoint))))

(defun Info-extract-pointer (name &optional errorname)
  (save-excursion
   (goto-char (point-min))
   (forward-line 1)
   (if (re-search-backward (concat name ":") nil t)
       (progn
	 (goto-char (match-end 0))
	 (Info-following-node-name))
     (if (eq errorname t)
	 nil
       (error (concat "Node has no " (capitalize (or errorname name))))))))

(defun Info-following-node-name (&optional allowedchars)
  (skip-chars-forward " \t")
  (buffer-substring
   (point)
   (progn
     (while (looking-at (concat "[" (or allowedchars "^,\t\n") "]"))
       (skip-chars-forward (concat (or allowedchars "^,\t\n") "("))
       (if (looking-at "(")
	   (skip-chars-forward "^)")))
     (skip-chars-backward " ")
     (point))))

(defun Info-next (&optional n)
  "Go to the next node of this node.
A positive or negative prefix argument moves by multiple nodes."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-prev (- n))
    (while (>= (setq n (1- n)) 0)
      (Info-goto-node (Info-extract-pointer "next")))))

(defun Info-prev (&optional n)
  "Go to the previous node of this node.
A positive or negative prefix argument moves by multiple nodes."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-next (- n))
    (while (>= (setq n (1- n)) 0)
      (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous")))))

(defun Info-up (&optional n)
  "Go to the superior node of this node.
A positive prefix argument moves up several times."
  (interactive "p")
  (or n (setq n 1))
  (while (>= (setq n (1- n)) 0)
    (Info-goto-node (Info-extract-pointer "up")))
  (if (interactive-p) (Info-restore-point)))

(defun Info-last (&optional n)
  "Go back to the last node visited.
With a prefix argument, go to Nth most recently visited node.  History is
circular; after oldest node, history comes back around to most recent one.
Argument can be negative to go through the circle in the other direction.
\(In other words, `l' is like \"undo\" and `C-u - l' is like \"redo\".)"
  (interactive "p")
  (or n (setq n 1))
  (or Info-history
      (error "This is the first Info node you looked at"))
  (let ((len (1+ (length Info-history))))
    (setq n (% (+ n (* len 100)) len)))
  (if (> n 0)
      (let ((entry (nth (1- n) Info-history)))
	(Info-history-add Info-current-file Info-current-node (point))
	(while (>= (setq n (1- n)) 0)
	  (setq Info-history (nconc (cdr Info-history)
				    (list (car Info-history)))))
	(setq Info-history (cdr Info-history))
	(let ((Info-keeping-history nil))
	  (Info-goto-node (car entry)))
	(Info-restore-history-entry entry))))

(defun Info-directory ()
  "Go to the Info directory node."
  (interactive)
  (Info-find-node "dir" "top"))

(defun Info-follow-reference (footnotename)
  "Follow cross reference named NAME to the node it refers to.
NAME may be an abbreviation of the reference name."
  (interactive
   (let ((completion-ignore-case t)
	 completions default (start-point (point)) str i)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward (format "\\*%s[ \n\t]*\\([^:]*\\):"
					 Info-footnote-tag)
				 nil t)
	 (setq str (buffer-substring
		    (match-beginning 1)
		    (1- (point))))
	 ;; See if this one should be the default.
	 (and (null default)
	      (< (match-beginning 0) start-point)
	      (<= start-point (point))
	      (setq default t))
	 (setq i 0)
	 (while (setq i (string-match "[ \n\t]+" str i))
	   (setq str (concat (substring str 0 i) " "
			     (substring str (match-end 0))))
	   (setq i (1+ i)))
	 ;; Record as a completion and perhaps as default.
	 (if (eq default t) (setq default str))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     (if completions
	 (let ((item (completing-read (if default
					  (concat "Follow reference named: ("
						  default ") ")
					"Follow reference named: ")
				      completions nil t)))
	   (if (and (string= item "") default)
	       (list default)
	     (list item)))
       (error "No cross-references in this node"))))
  (let (target beg i (str (concat "\\*" Info-footnote-tag " "
				  (regexp-quote footnotename))))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "\\([ \t\n]+\\)"
			(substring str (1+ i))))
      (setq i (+ i 10)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward str nil t)
	  (error "No cross-reference named %s" footnotename))
      (goto-char (match-end 1))
      (setq target
	    (Info-extract-menu-node-name "Bad format cross reference" t)))
    (while (setq i (string-match "[ \t\n]+" target i))
      (setq target (concat (substring target 0 i) " "
			   (substring target (match-end 0))))
      (setq i (+ i 1)))
    (Info-goto-node target)
    (setq Info-in-cross-reference t)))

(defun Info-next-reference (n)
  (interactive "p")
  (let ((pat (format "\\*%s[ \n\t]*\\([^:]*\\):\\|^\\* .*:"
		     Info-footnote-tag))
	(case-fold-search nil)
	(old-pt (point)))
    (while (< n 0)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pat nil t)
	  (setq n (1+ n)))
	(goto-char (point-min))
	(if (re-search-forward "^\\* Menu:" nil t)
	    (setq n (1- n)))))
    (while (>= (setq n (1- n)) 0)
      (or (eobp) (forward-char 1))
      (or (re-search-forward pat nil t)
	  (progn
	    (goto-char (point-min))
	    (or (re-search-forward pat nil t)
		(progn
		  (goto-char old-pt)
		  (error "No cross references in this node")))))
      (goto-char (match-beginning 0))
      (if (looking-at "\\* Menu:")
	  (setq n (1+ n))))))

(defun Info-prev-reference (n)
  (interactive "p")
  (Info-next-reference (- n)))

(defun Info-extract-menu-node-name (&optional errmessage multi-line)
  (skip-chars-forward " \t\n")
  (let ((beg (point))
	str i)
    (skip-chars-forward "^:")
    (forward-char 1)
    (setq str
	  (if (looking-at ":")
	      (buffer-substring beg (1- (point)))
	    (skip-chars-forward " \t\n")
	    (Info-following-node-name (if multi-line "^.,\t" "^.,\t\n"))))
    (while (setq i (string-match "\n" str i))
      (aset str i ?\ ))
    str))

(defun Info-menu (menu-item)
  "Go to node for menu item named (or abbreviated) NAME.
Completion is allowed, and the menu item point is on is the default."
  (interactive
   (let ((completions '())
	 ;; If point is within a menu item, use that item as the default
	 (default nil)
	 (p (point))
	 (last nil))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (while (re-search-forward
		"\n\\* \\([^:\t\n]*\\):" nil t)
	 (if (and (null default)
		  (prog1 (if last (< last p) nil)
		    (setq last (match-beginning 0)))
		  (<= p last))
	     (setq default (car (car completions))))
	 (setq completions (cons (cons (buffer-substring
					 (match-beginning 1)
					 (match-end 1))
				       (match-beginning 1))
				 completions)))
       (if (and (null default) last
		(< last p)
		(<= p (progn (end-of-line) (point))))
	   (setq default (car (car completions)))))
     (let ((item nil))
       (while (null item)
	 (setq item (let ((completion-ignore-case t))
		      (completing-read (if default
					   (format "Menu item (default %s): "
						   default)
					   "Menu item: ")
				       completions nil t)))
	 ;; we rely on the bug (which RMS won't change for his own reasons)
	 ;; that ;; completing-read accepts an input of "" even when the
	 ;; require-match argument is true and "" is not a valid possibility
	 (if (string= item "")
	     (if default
		 (setq item default)
	         ;; ask again
	         (setq item nil))))
       (list item))))
  ;; there is a problem here in that if several menu items have the same
  ;; name you can only go to the node of the first with this command.
  (Info-goto-node (Info-extract-menu-item menu-item) nil t))
  
(defun Info-extract-menu-item (menu-item &optional noerror)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n* menu:" nil t)
	(if (or (search-forward (concat "\n* " menu-item ":") nil t)
		(search-forward (concat "\n* " menu-item) nil t))
	    (progn
	      (beginning-of-line)
	      (forward-char 2)
	      (Info-extract-menu-node-name))
	  (and (not noerror) (error "No such item in menu")))
      (and (not noerror) (error "No menu in this node")))))

(defun Info-extract-menu-counting (count &optional noerror noindex)
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward "\n* menu:" nil t)
	     (or (not noindex)
		 (not (string-match "\\<Index\\>" Info-current-node))))
	(if (search-forward "\n* " nil t count)
	    (progn
	      (or count
		  (while (search-forward "\n* " nil t)))
	      (Info-extract-menu-node-name))
	  (and (not noerror) (error "Too few items in menu")))
      (and (not noerror) (error "No menu in this node")))))

(defun Info-nth-menu-item (n)
  "Go to the node of the Nth menu item."
  (interactive "P")
  (or n (setq n (- last-command-char ?0)))
  (if (< n 1) (error "Index must be at least 1"))
  (Info-goto-node (Info-extract-menu-counting n) nil t))

(defun Info-last-menu-item ()
  "Go to the node of the tenth menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting nil) nil t))

(defun Info-top ()
  "Go to the Top node of this file."
  (interactive)
  (Info-goto-node "Top"))

(defun Info-end ()
  "Go to the final node in this file."
  (interactive)
  (Info-top)
  (let ((Info-keeping-history nil)
	node)
    (Info-last-menu-item)
    (while (setq node (or (Info-extract-pointer "next" t)
			  (Info-extract-menu-counting nil t t)))
      (Info-goto-node node))
    (or (equal (Info-extract-pointer "up" t) "Top")
	(let ((executing-kbd-macro ""))   ; suppress messages
	  (condition-case err
	      (Info-global-next 10000)
	    (error nil))))))

(defun Info-global-next (&optional n)
  "Go to the next node in this file, traversing node structure as necessary.
This works only if the Info file is structured as a hierarchy of nodes.
A positive or negative prefix argument moves by multiple nodes."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-global-prev (- n))
    (while (>= (setq n (1- n)) 0)
      (let (node)
	(cond ((and (string-match "^Top$" Info-current-node)
		    (setq node (Info-extract-pointer "next" t))
		    (Info-extract-menu-item node t))
	       (Info-goto-node node))
	      ((setq node (Info-extract-menu-counting 1 t t))
	       (message "Going down...")
	       (Info-goto-node node))
	      (t
	       (let ((Info-keeping-history Info-keeping-history)
		     (orignode Info-current-node)
		     (ups ""))
		 (while (not (Info-extract-pointer "next" t))
		   (if (and (setq node (Info-extract-pointer "up" t))
			    (not (equal node "Top")))
		       (progn
			 (message "Going%s..." (setq ups (concat ups " up")))
			 (Info-goto-node node)
			 (setq Info-keeping-history nil))
		     (if orignode
			 (let ((Info-keeping-history nil))
			   (Info-goto-node orignode)))
		     (error "Last node in file")))
		 (Info-next))))))))

(defun Info-page-next (&optional n)
  "Scroll forward one screenful, or go to next global node.
A positive or negative prefix argument moves by multiple screenfuls."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-page-prev (- n))
    (while (>= (setq n (1- n)) 0)
      (if (pos-visible-in-window-p (point-max))
	  (progn
	    (Info-global-next)
	    (message "Node: %s" Info-current-node))
	(scroll-up)))))

(defun Info-scroll-next (arg)
  (interactive "P")
  (if Info-auto-advance
      (if (and (pos-visible-in-window-p (point-max))
	       (not (eq Info-auto-advance t))
	       (not (eq last-command this-command)))
	  (message "Hit %s again to go to next node"
		   (if (= last-command-char 0)
		       "mouse button"
		     (key-description (char-to-string last-command-char))))
	(Info-page-next)
	(setq this-command 'Info))
    (scroll-up arg)))

(defun Info-global-prev (&optional n)
  "Go to the previous node in this file, traversing structure as necessary.
This works only if the Info file is structured as a hierarchy of nodes.
A positive or negative prefix argument moves by multiple nodes."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-global-next (- n))
    (while (>= (setq n (1- n)) 0)
      (let ((upnode (Info-extract-pointer "up" t))
	    (prevnode (Info-extract-pointer "prev[ious]*" t)))
	(if (or (not prevnode)
		(equal prevnode upnode))
	    (if (string-match "^Top$" Info-current-node)
		(error "First node in file")
	      (message "Going up...")
	      (Info-up))
	  (Info-goto-node prevnode)
	  (let ((downs "")
		(Info-keeping-history nil)
		node)
	    (while (setq node (Info-extract-menu-counting nil t t))
	      (message "Going%s..." (setq downs (concat downs " down")))
	      (Info-goto-node node))))))))

(defun Info-page-prev (&optional n)
  "Scroll backward one screenful, or go to previous global node.
A positive or negative prefix argument moves by multiple screenfuls."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (Info-page-next (- n))
    (while (>= (setq n (1- n)) 0)
      (if (pos-visible-in-window-p (point-min))
	  (progn
	    (Info-global-prev)
	    (message "Node: %s" Info-current-node)
	    (sit-for 0)
	    ;;(scroll-up 1)   ; work around bug in pos-visible-in-window-p
	    ;;(scroll-down 1)
	    (while (not (pos-visible-in-window-p (point-max)))
	      (scroll-up)))
	(scroll-down)))))

(defun Info-scroll-prev (arg)
  (interactive "P")
  (if Info-auto-advance
      (if (and (pos-visible-in-window-p (point-min))
	       (not (eq Info-auto-advance t))
	       (not (eq last-command this-command)))
	  (message "Hit %s again to go to previous node"
		   (if (= last-command-char 0)
		       "mouse button"
		     (key-description (char-to-string last-command-char))))
	(Info-page-prev)
	(setq this-command 'Info))
    (scroll-down arg)))

(defun Info-index (topic)
  "Look up a string in the index for this file.
The index is defined as the first node in the top-level menu whose
name contains the word \"Index\", plus any immediately following
nodes whose names also contain the word \"Index\".
If there are no exact matches to the specified topic, this chooses
the first match which is a case-insensitive substring of a topic.
Use the `,' command to see the other matches.
Give a blank topic name to go to the Index node itself."
  (interactive "sIndex topic: ")
  (let ((orignode Info-current-node)
	(rnode nil)
	(pattern (format "\n\\* \\([^\n:]*%s[^\n:]*\\):[ \t]*%s"
			 (regexp-quote topic)
			 "\\([^.\n]*\\)\\.[ t]*\\([0-9]*\\)"))
	node)
    (Info-goto-node "Top")
    (or (search-forward "\n* menu:" nil t)
	(error "No index"))
    (or (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t)
	(error "No index"))
    (goto-char (match-beginning 1))
    (let ((Info-keeping-history nil))
      (Info-goto-node (Info-extract-menu-node-name)))
    (or (equal topic "")
	(let ((matches nil)
	      (exact nil)
	      (Info-keeping-history nil)
	      found)
	  (while
	      (progn
		(goto-char (point-min))
		(while (re-search-forward pattern nil t)
		  (setq matches
			(cons (list (buffer-substring (match-beginning 1)
						      (match-end 1))
				    (buffer-substring (match-beginning 2)
						      (match-end 2))
				    Info-current-node
				    (string-to-int (concat "0"
							   (buffer-substring
							    (match-beginning 3)
							    (match-end 3)))))
			      matches)))
		(and (setq node (Info-extract-pointer "next" t))
		     (string-match "\\<Index\\>" node)))
	    (Info-goto-node node))
	  (or matches
	      (progn
		(Info-last)
		(error "No \"%s\" in index" topic)))
	  ;; Here it is a feature that assoc is case-sensitive.
	  (while (setq found (assoc topic matches))
	    (setq exact (cons found exact)
		  matches (delq found matches)))
	  (setq Info-index-alternatives (nconc exact (nreverse matches)))
	  (Info-index-next 0)))))

(defun Info-index-next (num)
  "Go to the next matching index item from the last `i' command."
  (interactive "p")
  (or Info-index-alternatives
      (error "No previous `i' command in this file"))
  (while (< num 0)
    (setq num (+ num (length Info-index-alternatives))))
  (while (> num 0)
    (setq Info-index-alternatives
	  (nconc (cdr Info-index-alternatives)
		 (list (car Info-index-alternatives)))
	  num (1- num)))
  (Info-goto-node (nth 1 (car Info-index-alternatives)))
  (if (> (nth 3 (car Info-index-alternatives)) 0)
      (forward-line (nth 3 (car Info-index-alternatives)))
    (forward-line 3)  ; don't search in headers
    (let ((name (car (car Info-index-alternatives))))
      (if (or (re-search-forward (format
				  "\\(Function\\|Command\\): %s\\( \\|$\\)"
				  (regexp-quote name)) nil t)
	      (search-forward (format "`%s'" name) nil t)
	      (and (string-match "\\`.*\\( (.*)\\)\\'" name)
		   (search-forward
		    (format "`%s'" (substring name 0 (match-beginning 1)))
		    nil t))
	      (search-forward name nil t))
	  (beginning-of-line)
	(goto-char (point-min)))))
  (message "Found \"%s\" in %s.  %s"
	   (car (car Info-index-alternatives))
	   (nth 2 (car Info-index-alternatives))
	   (if (cdr Info-index-alternatives)
	       "(Press `,' for more)"
	     "(Only match)")))

(defun Info-emacs-key (key)
  "Look up an Emacs key sequence in the Emacs manual in the Info system.
This command is designed to be used whether you are already in Info or not."
  (interactive "kLook up key in Emacs manual: ")
  (if (equal key "\C-g")
      (keyboard-quit))
  (info)
  (Info-find-node "emacs" "Top")
  (setq key (key-description key))
  (let (p)
    (if (setq p (string-match "[@{}]" key))
	(setq key (concat (substring key 0 p) "@" (substring key p))))
    (if (string-match "^ESC " key)
	(setq key (concat "M-" (substring key 4))))
    (if (string-match "^M-C-" key)
	(setq key (concat "C-M-" (substring key 4)))))
  (Info-index key))

(defun Info-elisp-ref (func)
  "Look up an Emacs Lisp function in the Elisp manual in the Info system.
This command is designed to be used whether you are already in Info or not."
  (interactive (let ((fn (function-called-at-point))
		     (enable-recursive-minibuffers t)	     
		     val)
		 (setq val (completing-read
			    (format "Look up Emacs Lisp function%s: "
				    (if fn
					(format " (default %s)" fn)
				      ""))
			    obarray 'fboundp t))
		 (list (if (equal val "")
			   fn (intern val)))))
  (info)
  (condition-case err
      (Info-find-node "elisp" "Top")
    (error (Info-find-node "lispref" "Top")))
  (Info-index (symbol-name func)))

(defun Info-reannotate-node ()
  (let ((bufs (delq nil (mapcar 'get-file-buffer Info-annotations-path))))
    (if bufs
	(let ((ibuf (current-buffer))
	      (file (regexp-quote
		     (file-name-nondirectory Info-current-file)))
	      (node (regexp-quote Info-current-node))
	      (savept (point)))
	  (goto-char (point-min))
	  (if (search-forward "\n------ NOTE:\n" nil t)
	      (let ((buffer-read-only nil)
		    (bufmod (buffer-modified-p))
		    top)
		(setq savept (copy-marker savept))
		(goto-char (point-min))
		(while (search-forward "\n------ NOTE:" nil t)
		  (setq top (1+ (match-beginning 0)))
		  (if (search-forward "\n------\n" nil t)
		      (delete-region top (point)))
		  (backward-char 1))
		(set-buffer-modified-p bufmod)))
	  (save-excursion
	    (while bufs
	      (set-buffer (car bufs))
	      (goto-char (point-min))
	      (while (re-search-forward
		      (format
		       "------ *File: *%s *Node: *%s *Line: *\\([0-9]+\\) *\n"
		       file node)
		      nil t)
		(let ((line (string-to-int
			     (buffer-substring (match-beginning 1)
					       (match-end 1))))
		      (top (point))
		      bot)
		  (search-forward "\n------\n" nil t)
		  (setq bot (point))
		  (save-excursion
		    (set-buffer ibuf)
		    (if (integerp savept) (setq savept (copy-marker savept)))
		    (if (= line 0)
			(goto-char (point-max))
		      (goto-char (point-min))
		      (forward-line line))
		    (let ((buffer-read-only nil)
			  (bufmod (buffer-modified-p)))
		      (insert "------ NOTE:\n")
		      (insert-buffer-substring (car bufs) top bot)
		      (set-buffer-modified-p bufmod)))))
	      (setq bufs (cdr bufs))))
	  (goto-char savept)))))

(defvar Info-annotate-map nil
  "Local keymap used within `a' command of Info.")
(if Info-annotate-map
    nil
  ;; (setq Info-annotate-map (nconc (make-sparse-keymap) text-mode-map))
  (setq Info-annotate-map (copy-keymap text-mode-map))
  (define-key Info-annotate-map "\C-c\C-c" 'Info-cease-annotate))

(defun Info-annotate-mode ()
  "Major mode for adding an annotation to an Info node.
Like text mode with the addition of Info-cease-annotate
which returns to Info mode for browsing.
\\{Info-annotate-map}")

(defun Info-annotate (arg)
  "Add a personal annotation to the current Info node.
Only you will be able to see this annotation.
Annotations are stored in the file ~/.infonotes by default.
If point is inside an existing annotation, edit that annotation.
A prefix argument specifies which annotations file (from
Info-annotations-path) is to be edited; default is 1."
  (interactive "p")
  (setq arg (1- arg))
  (if (or (< arg 0) (not (nth arg Info-annotations-path)))
      (if (= arg 0)
	  (setq Info-annotations-path
		(list (read-file-name
		       "Annotations file: " "~/" "~/.infonotes")))
	(error "File number must be in the range from 1 to %d"
	       (length Info-annotations-path))))
  (let ((which nil)
	where pt)
    (if (and (save-excursion
	       (goto-char (min (point-max) (+ (point) 13)))
	       (and (search-backward "------ NOTE:\n" nil t)
		    (setq pt (match-end 0))
		    (search-forward "\n------\n" nil t)))
	     (< (point) (match-end 0)))
	(setq which (format "File: *%s *Node: *%s *Line:.*\n%s"
			    (regexp-quote
			     (file-name-nondirectory Info-current-file))
			    (regexp-quote Info-current-node)
			    (regexp-quote
			     (buffer-substring pt (match-beginning 0))))
	      where (max (- (point) pt) 0)))
    (let ((file (file-name-nondirectory Info-current-file))
	  (node Info-current-node)
	  (line (if (looking-at "[ \n]*\\'") 0
		  (count-lines (point-min) (point)))))
      (or which
	  (let ((buffer-read-only nil)
		(bufmod (buffer-modified-p))
		top)
	    (beginning-of-line)
	    (if (bobp) (goto-char (point-max)))
	    (insert "------ NOTE:\n------\n")
	    (backward-char 20)
	    (set-buffer-modified-p bufmod)))
      ;; (setq Info-window-start (window-start))
      (setq Info-window-configuration (current-window-configuration))
      (pop-to-buffer (find-file-noselect (nth arg Info-annotations-path)))
      (use-local-map Info-annotate-map)
      (setq major-mode 'Info-annotate-mode)
      (setq mode-name "Info Annotate")
      (if which
	  (if (save-excursion
		(goto-char (point-min))
		(re-search-forward which nil t))
	      (progn
		(goto-char (match-beginning 0))
		(forward-line 1)
		(forward-char where)))
	(let ((bufmod (buffer-modified-p)))
	  (goto-char (point-max))
	  (insert (format "\n------ File: %s  Node: %s  Line: %d\n"
			  file node line))
	  (setq pt (point))
	  (insert "\n------\n"
		  "\nPress C-c C-c to save and return to Info.\n")
	  (goto-char pt)
	  (set-buffer-modified-p bufmod))))))

(defun Info-cease-annotate ()
  (interactive)
  (let ((bufmod (buffer-modified-p)))
    (while (save-excursion
	     (goto-char (point-min))
	     (re-search-forward "\n\n?Press .* to save and return to Info.\n"
				nil t))
      (delete-region (1+ (match-beginning 0)) (match-end 0)))
    (while (save-excursion
	     (goto-char (point-min))
	     (re-search-forward "\n------ File:.*Node:.*Line:.*\n+------\n"
				nil t))
      (delete-region (match-beginning 0) (match-end 0)))
    (set-buffer-modified-p bufmod))
  (save-buffer)
  (fundamental-mode)
  (bury-buffer)
  (or (one-window-p) (delete-window))
  (info)
  (setq Info-current-file-completions nil)
  (set-window-configuration Info-window-configuration)
  (Info-reannotate-node))

(defun Info-exit ()
  "Exit Info by selecting some other buffer."
  (interactive)
  (switch-to-buffer (prog1 (other-buffer (current-buffer))
			   (bury-buffer (current-buffer)))))

(defun Info-undefined ()
  "Make command be undefined in Info."
  (interactive)
  (ding))

(defun Info-help ()
  "Enter the Info tutorial."
  (interactive)
  (delete-other-windows)
  (Info-find-node "info"
		  (if (< (window-height) 23)
		      "Help-Small-Screen"
		    "Help")))

(defun Info-summary ()
  "Display a brief summary of all Info commands."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (erase-buffer)
    (insert (documentation 'Info-mode))
    (goto-char (point-min))
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to Info"))
		    (if (/= ?\  (setq ch (read-char)))
			(progn (setq unread-command-char ch) nil)
		      flag))
	(scroll-up)))
    (bury-buffer "*Help*")))

(defun Info-get-token (pos start all &optional errorstring)
  "Return the token around POS,
POS must be somewhere inside the token
START is a regular expression which will match the
    beginning of the tokens delimited string
ALL is a regular expression with a single
    parenthized subpattern which is the token to be
    returned. E.g. '{\(.*\)}' would return any string
    enclosed in braces around POS.
SIG optional fourth argument, controls action on no match
    nil: return nil
    t: beep
    a string: signal an error, using that string."
  (save-excursion
    (goto-char pos)
    (re-search-backward start (max (point-min) (- pos 200)) 'yes)
    (while (and (re-search-forward all (min (point-max) (+ pos 200)) 'yes)
		(not (and (<= (match-beginning 0) pos)
			  (> (match-end 0) pos)))))
    (if (and (<= (match-beginning 0) pos)
	     (> (match-end 0) pos)
	     (match-beginning 1))
	(buffer-substring (match-beginning 1) (match-end 1))
      (cond ((null errorstring)
	     nil)
	    ((eq errorstring t)
	     (beep)
	     nil)
	    (t
	     (error "No %s around position %d" errorstring pos))))))

(defun Info-follow-clicked-node (event)
  "Follow a node reference near clicked point.  Like M, F, N, P or U command.
At end of the node's text, moves to the next node."
  (interactive "@e")
  (let* ((relative-coordinates (coordinates-in-window-p (car event)
							(selected-window)))
	 (rel-x (car relative-coordinates))
	 (rel-y (car (cdr relative-coordinates)))
	 (point (save-excursion
		  (move-to-window-line rel-y)
		  (move-to-column (+ rel-x (current-column)))
		  (point))))
    (Info-follow-nearest-node (max point (1+ (point-min))))))

(defun Info-follow-nearest-node (point)
  "Follow a node reference near point.  Like M, F, N, P or U command.
At end of the node's text, moves to the next node."
  (interactive "d")
  (let (node)
    (cond
     ((= point (point-min)))   ; don't trigger on accidental RET.
     ((setq node (Info-get-token point
				 (format "\\*%s[ \n]" Info-footnote-tag)
				 (format "\\*%s[ \n]\\([^:]*\\):"
					 Info-footnote-tag)))
      (message "Following cross-reference %s..." node)
      (Info-follow-reference node))
     ((setq node (Info-get-token point "\\* " "\\* \\([^:]*\\)::"))
      (message "Selecting menu item %s..." node)
      (Info-goto-node node))
     ((setq node (Info-get-token point "\\* " "\\* \\([^:]*\\):"))
      (message "Selecting menu item %s..." node)
      (Info-menu node))
     ((setq node (Info-get-token point "Up: " "Up: \\([^,\n\t]*\\)"))
      (message "Going up...")
      (Info-goto-node node))
     ((setq node (Info-get-token point "Next: " "Next: \\([^,\n\t]*\\)"))
      (message "Next node...")
      (Info-goto-node node))
     ((setq node (Info-get-token point "File: " "File: \\([^,\n\t]*\\)"))
      (message "Top node...")
      (Info-goto-node "Top"))
     ((setq node (Info-get-token point "Prev[ious]*: "
				 "Prev[ious]*: \\([^,\n\t]*\\)"))
      (message "Previous node...")
      (Info-goto-node node))
     ((save-excursion (goto-char point) (looking-at "[ \n]*\\'"))
      (if Info-in-cross-reference
	  (progn
	    (message "Back to last node...")
	    (Info-last))
	(message "Next node...")
	(Info-global-next))))
    ))

;;; Provide Emacs 18 support for mouse clicks.
(defvar Info-prev-x-right-click nil)
(defvar Info-prev-x-middle-click nil)
(defvar Info-prev-x-left-click nil)
(defvar Info-prev-x-s-right-click nil)
(defvar Info-prev-x-s-middle-click nil)
(defvar Info-prev-x-s-left-click nil)
(defun Info-setup-x ()
  (and (eq window-system 'x)
       (not Info-prev-x-left-click)
       Info-mouse-support
       (progn
	 (setq Info-prev-x-right-click (aref mouse-map 0)
	       Info-prev-x-middle-click (aref mouse-map 1)
	       Info-prev-x-left-click (aref mouse-map 2)
	       Info-prev-x-s-right-click (aref mouse-map 16)
	       Info-prev-x-s-middle-click (aref mouse-map 17)
	       Info-prev-x-s-left-click (aref mouse-map 18))
	 (aset mouse-map 0 'Info-x-right-click)
	 (aset mouse-map 1 'Info-x-middle-click)
	 (aset mouse-map 2 'Info-x-left-click)
	 (aset mouse-map 16 'Info-x-s-right-click)
	 (aset mouse-map 17 'Info-x-s-middle-click)
	 (aset mouse-map 18 'Info-x-s-left-click))))

(defun Info-x-left-click (arg)
  "Handle a left-button mouse click in Info window."
  (Info-handle-click '(condition-case err (Info-scroll-next nil) (error nil))
		     arg Info-prev-x-left-click))

(defun Info-x-right-click (arg)
  "Handle a right-button mouse click in Info window."
  (Info-handle-click '(condition-case err (Info-scroll-prev nil) (error nil))
		     arg Info-prev-x-right-click))

(defun Info-x-middle-click (arg)
  "Handle a middle-button mouse click in Info window."
  (Info-handle-click '(Info-follow-clicked-node (list arg))
		     arg Info-prev-x-middle-click))

(defun Info-x-s-left-click (arg)
  "Handle a shift-left-button mouse click in Info window."
  (Info-handle-click '(Info-global-next) arg Info-prev-x-s-left-click))

(defun Info-x-s-right-click (arg)
  "Handle a shift-right-button mouse click in Info window."
  (Info-handle-click '(Info-global-prev) arg Info-prev-x-s-right-click))

(defun Info-x-s-middle-click (arg)
  "Handle a shift-middle-button mouse click in Info window."
  (Info-handle-click '(Info-last) arg Info-prev-x-s-middle-click))

(defun Info-handle-click (form arg prev)
  (if (and (get-buffer-window "*info*")
	   (coordinates-in-window-p arg (get-buffer-window "*info*")))
      (let ((win (selected-window)))
	(unwind-protect
	    (progn
	      (select-window (get-buffer-window "*info*"))
	      (eval form))
	  (and (window-point win)
	       (select-window win))))
    (funcall prev arg)))

(defun x-mouse-ignore (arg)
  "Don't do anything."
  ;; Added the following line to support Info-auto-advance mode.
  (setq this-command last-command))

(defvar Info-mode-map nil
  "Keymap containing Info commands.")
(if Info-mode-map
    nil
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (define-key Info-mode-map "." 'beginning-of-buffer)
  (define-key Info-mode-map " " 'Info-scroll-next)
  (define-key Info-mode-map "1" 'Info-nth-menu-item)
  (define-key Info-mode-map "2" 'Info-nth-menu-item)
  (define-key Info-mode-map "3" 'Info-nth-menu-item)
  (define-key Info-mode-map "4" 'Info-nth-menu-item)
  (define-key Info-mode-map "5" 'Info-nth-menu-item)
  (define-key Info-mode-map "6" 'Info-nth-menu-item)
  (define-key Info-mode-map "7" 'Info-nth-menu-item)
  (define-key Info-mode-map "8" 'Info-nth-menu-item)
  (define-key Info-mode-map "9" 'Info-nth-menu-item)
  (define-key Info-mode-map "0" 'Info-last-menu-item)
  (define-key Info-mode-map "?" 'Info-summary)
  (define-key Info-mode-map "a" 'Info-annotate)
  (define-key Info-mode-map "b" 'beginning-of-buffer)
  (define-key Info-mode-map "d" 'Info-directory)
  (define-key Info-mode-map "e" 'Info-edit)
  (define-key Info-mode-map "f" 'Info-follow-reference)
  (define-key Info-mode-map "g" 'Info-goto-node)
  (define-key Info-mode-map "h" 'Info-help)
  (define-key Info-mode-map "i" 'Info-index)
  (define-key Info-mode-map "k" 'Info-emacs-key)
  (define-key Info-mode-map "l" 'Info-last)
  (define-key Info-mode-map "m" 'Info-menu)
  (define-key Info-mode-map "n" 'Info-next)
  (define-key Info-mode-map "p" 'Info-prev)
  (define-key Info-mode-map "q" 'Info-exit)
  (define-key Info-mode-map "r" 'Info-follow-reference)
  (define-key Info-mode-map "s" 'Info-search)
  (define-key Info-mode-map "t" 'Info-top)
  (define-key Info-mode-map "u" 'Info-up)
  (define-key Info-mode-map "<" 'Info-top)
  (define-key Info-mode-map ">" 'Info-end)
  (define-key Info-mode-map "[" 'Info-global-prev)
  (define-key Info-mode-map "]" 'Info-global-next)
  (define-key Info-mode-map "{" 'Info-page-prev)
  (define-key Info-mode-map "}" 'Info-page-next)
  (define-key Info-mode-map "=" 'Info-restore-point)
  (define-key Info-mode-map "!" 'Info-select-node)
  (define-key Info-mode-map "@" 'Info-follow-nearest-node)
  (define-key Info-mode-map "," 'Info-index-next)
  (define-key Info-mode-map "*" 'Info-elisp-ref)
  (define-key Info-mode-map "\t" 'Info-next-reference)
  (define-key Info-mode-map "\e\t" 'Info-prev-reference)
  (define-key Info-mode-map "\r" 'Info-follow-nearest-node)
  (define-key Info-mode-map "\177" 'Info-scroll-prev))

(or (lookup-key help-map "\C-k")
    (define-key help-map "\C-k" 'Info-emacs-key))

(or (lookup-key help-map "\C-f")
    (define-key help-map "\C-f" 'Info-elisp-ref))

(let ((help (symbol-function 'help-for-help))
      (doc (documentation 'help-for-help)))
  (and (consp help) (eq (car help) 'lambda)
       (not (string-match "C-k" doc))
       (setq doc (concat
		  doc
		  "\n\nC-k  Info-emacs-key.  Look up a key in Emacs manual."
		  "\nC-f  Info-elisp-ref."
		  "  Look up a function in Emacs Lisp manual."))
       (fset 'help-for-help
	     (cons 'lambda (cons (nth 1 help) (cons doc (nthcdr 2 help)))))))


;;; The following stuff is for Emacs 19 only---it's ignored in Emacs 18.
(defvar Info-mode-mouse-map nil
  "Mouse map for use with Info mode.")

(if Info-mode-mouse-map
    nil
  (if (or (not (boundp 'global-mouse-map))
	  (null (cdr global-mouse-map)))
      nil
    (setq Info-mode-mouse-map (make-sparse-keymap))
    (define-key Info-mode-mouse-map mouse-button-middle
      'Info-follow-clicked-node)
    (define-key Info-mode-mouse-map mouse-button-left 'mouse-scroll-up-full)
    (define-key Info-mode-mouse-map mouse-button-right
      'mouse-scroll-down-full)))

;; Info mode is suitable only for specially formatted data.
(put 'info-mode 'mode-class 'special)

(defun Info-mode ()
  "Info mode is for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes
which discuss related topics.  Info has commands to follow
the references and show you other nodes.

h	Invoke the Info tutorial.
q	Quit Info: return to previously selected file or buffer.

Selecting other nodes:
n	Move to the \"next\" node of this node.
p	Move to the \"previous\" node of this node.
m	Pick menu item specified by name (or abbreviation).
1-9, 0	Pick first..ninth, last item in node's menu.
	Menu items select nodes that are \"subsections\" of this node.
u	Move \"up\" from this node (i.e., from a subsection to a section).
f or r	Follow a cross reference by name (or abbrev).  Type `l' to get back.
RET     Follow cross reference or menu item indicated by cursor.
i	Look up a topic in this file's Index and move to that node.
,	(comma) Move to the next match from a previous `i' command.
l	(letter L) Move back to the last node you were in.

Moving within a node:
Space	Scroll forward a full screen.   DEL       Scroll backward.
b	Go to beginning of node.        Meta->    Go to end of node.
TAB	Go to next cross-reference.     Meta-TAB  Go to previous ref.

Mouse commands:				Shifted mouse commands:
Middle	Follow reference (RET).		S-Middle  Back to last node (`l').
Left	Next page or node (Space).	S-Left    Next node (`]').
Right	Prev page or node (DEL).	S-Right   Prev node (`[').

Advanced commands:
g	Move to node, file, or annotation tag specified by name.
	Examples:  `g Rectangles' `g (Emacs)Rectangles' `g Emacs'.
k	Look up a key sequence in Emacs manual (also C-h C-k at any time).
*	Look up a function name in Emacs Lisp manual (also C-h C-f).
d	Go to the main directory of Info files.
< or t	Go to Top (first) node of this file.
>	Go to last node in this file.
\[	Go to previous node, treating file as one linear document.
\]	Go to next node, treating file as one linear document.
{	Scroll backward, or go to previous node if at top.
}	Scroll forward, or go to next node if at bottom.
=	Restore cursor position from last time in this node.
a	Add a private note (annotation) to the current node.
s	Search this Info file for a node containing the specified regexp.
e	Edit the contents of the current node."
  (kill-all-local-variables)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (use-local-map Info-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (setq buffer-mouse-map Info-mode-mouse-map)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (make-local-variable 'Info-current-file-completions)
  (make-local-variable 'Info-index-alternatives)
  (make-local-variable 'Info-history)
  (run-hooks 'Info-mode-hook)
  (Info-set-mode-line))

(defvar Info-edit-map nil
  "Local keymap used within `e' command of Info.")
(if Info-edit-map
    nil
  ;; (setq Info-edit-map (nconc (make-sparse-keymap) text-mode-map))
  (setq Info-edit-map (copy-keymap text-mode-map))
  (define-key Info-edit-map "\C-c\C-c" 'Info-cease-edit))

;; Info-edit mode is suitable only for specially formatted data.
(put 'info-edit-mode 'mode-class 'special)

(defun Info-edit-mode ()
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of Info-cease-edit
which returns to Info mode for browsing.
\\{Info-edit-map}"
  )

(defun Info-edit ()
  "Edit the contents of this Info node.
Allowed only if variable Info-enable-edit is non-nil."
  (interactive)
  (or Info-enable-edit
      (error "Editing info nodes is not enabled"))
  (use-local-map Info-edit-map)
  (setq major-mode 'Info-edit-mode)
  (setq mode-name "Info Edit")
  (kill-local-variable 'mode-line-buffer-identification)
  (setq buffer-read-only nil)
  ;; Make mode line update.
  (set-buffer-modified-p (buffer-modified-p))
  (message (substitute-command-keys
	     "Editing: Type \\[Info-cease-edit] to return to info")))

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (use-local-map Info-mode-map)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (Info-set-mode-line)
  (setq buffer-read-only t)
  ;; Make mode line update.
  (set-buffer-modified-p (buffer-modified-p))
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))

(run-hooks 'Info-load-hook)

;;; End.
