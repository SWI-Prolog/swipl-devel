;;;  SCCS: @(#)90/11/15 qptokens.el    2.3
;;;		    Quintus Prolog - GNU Emacs Interface
;;;                         Support Functions
;;;
;;;	            Consolidated by Sitaram Muralidhar
;;;
;;;		           sitaram@quintus.com
;;;		      Quintus Computer Systems, Inc.
;;;			      2 May 1989	   
;;;
;;; This file defines functions that support the Quintus Prolog - GNU Emacs
;;; interface.
;;;
;;;			       Acknowledgements
;;;
;;
;;; This interface was made possible by contributions from Fernando
;;; Pereira and various customers of Quintus Computer Systems, Inc.,
;;; based on code for Quintus's Unipress Emacs interface.
;;; 

;;; Functions defined in this file are:
;;;   - region-to-string()
;;;   - BadToken(message)
;;;   - next-token()
;;;   - pre-preceding-char()


(defun region-to-string ()
  (buffer-substring (min (point) (mark)) (max (point) (mark))))

(defmacro pattern-at-point (x)
  (list 'and (list 'looking-at x) '(goto-char (match-end 0)) t))

(defvar *digits*
  "0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")

(defvar *character-escapes* nil)

(defun BadToken (message)
  (error message))

(defun pre-preceding-char ()
  (let (char)
    (if (bobp)
        (setq char -1)
      (progn
        (backward-char)
        (setq char (preceding-char))
        (forward-char)))
    char
    ))

(defun next-token ()
  (let ((char (following-char)) ttype)
    (set-mark (point))
    (cond 
     ((eobp) (setq ttype "eof"))
     ((and (>= char ?a) (<= char ?z))
      (forward-char)
      (pattern-at-point "[A-Za-z_0-9]*")
      (if (= (following-char) ?\()
          (setq ttype "functor")
        (setq ttype "atom")))
     ((or (and (>= char ?\001) (<= char ? )) (= char ?\177))
      (forward-char)
      (pattern-at-point "[\001- \177]*")
      (setq ttype (next-token)))
     ((or 
       (= char ?,) (= char ?\) ) (= char ?\( ) 
       (= char ?\| ) (= char ?]) (= char ?}))
      (forward-char)
      (setq ttype (char-to-string char)))
     ((and (>= char ?A) (<= char ?Z))
      (forward-char)
      (pattern-at-point  "[A-Za-z_0-9]*")
      (setq ttype "variable"))
     ((or (= char ?#) (= char ?$) (= char ?&) (= char ?*) (= char ?+)
          (= char ?-) (= char ?:) (= char ?<) (= char ?=) (= char ?>)
          (= char ?\?) (= char ?@) (= char ?\\) (= char ?^) (= char ?`)
          (= char ?~))
      (forward-char)
      (pattern-at-point "[-+*/\\^<>=`~:.?@#$&]*") 
      (if (= (following-char) ?\( )
          (setq ttype "functor")
        (setq ttype "atom")
        ))
     ((= char ?_ )
      (forward-char)
      (pattern-at-point "[A-Za-z_0-9]*") 
      (setq ttype "variable"))
     ((and (>= char ?0) (<= char ?9))
      (forward-char)
      (pattern-at-point  "[0-9]*")
      (if (= (following-char) ?\')
          (let  ((base (string-to-int (region-to-string))) x)
            (cond 
             ((= base 0) (setq ttype "integer"))
             ((< base 10)
              (forward-char)
              (setq x (substring *digits* 0 base))
              (if  (pattern-at-point (concat "[" x "][" x "]*"))
                  (setq ttype "integer")
                (progn 
                  (backward-char)
                  (setq ttype "integer"))

                )
              )
             ((< base 37)
              (forward-char)
              (setq x (substring *digits* 0 (+ 10 (* 2 (- base 10)))))
              (if (pattern-at-point (concat "[" x "][" x "]*"))
                  (setq ttype "integer")             
                (progn 
                  (backward-char)
                  (setq ttype "integer"))
                )
              )
             (t (setq ttype "integer"))
             )
            )
        (if (= (following-char) ?.)
            (progn 
              (forward-char)
              (if (or (eobp) (looking-at "[?\001- ?\177]"))
                  (progn 
                    (backward-char)
                    (setq ttype "integer"))
                (if (not (pattern-at-point "[0-9][0-9]*"))
                    (BadToken "Floating point number has no decimal digits")
                  (if (not (pattern-at-point "[eE]"))
                      (setq ttype "float")
                    (if (and (not (pattern-at-point "[0-9][0-9]*"))
                             (not (pattern-at-point "[-+][0-9][0-9]*")))
                        (progn 
                          (backward-char)
                          (setq ttype "float")
                          )
                      (setq ttype "float")
                      )
                    )
                  )
                )
              )
          (setq ttype "integer")
          )
        )
      )
     ((= char ?.)
      (forward-char)
      (if (and (not (eobp))
               (not (pattern-at-point "[\001- \177]")))
          (progn 
            (pattern-at-point "[-+*/\\^<>=`~:.?@#$&]*")
            (if (= (following-char) ?\( )
                (setq ttype "functor")
              (setq ttype "atom")))
        (setq ttype "stop")))
     ((= char ?\')
      (let ((continue t))
        (forward-char)
        (while continue
          (if (not (search-forward "'" nil t))
              (BadToken "Quoted atom not closed")
            (if *character-escapes*
                (if (not (= (pre-preceding-char) ?\\))
                    (if (not (pattern-at-point "'"))
                        (setq continue nil)))
              (if (not (pattern-at-point "'"))
                  (setq continue nil)))))
        (if (= (following-char) ?\( )
            (setq ttype "functor")
          (setq ttype "atom")
          )))
     ((= char ?% )
      (forward-char)
      (pattern-at-point ".*\n")
      (setq ttype (next-token)))
     ((= char ?/)
      (forward-char)
      (if (not (pattern-at-point "\\*"))
          (progn 
            (pattern-at-point "[-+*/\\^<>=`~:.?@#$&]*")
            (if (= (following-char) ?\( )
                (setq ttype "functor")
              (setq ttype "atom")
              )
            )
        (if (not (search-forward "*/" nil t))
            (BadToken "Delimited comment not closed")
          (setq ttype (next-token))
          )
        )
      )
     ((or (= char ?!) (= char ?\; ))
      (forward-char)
      (if (= (following-char) ?\( )
          (setq ttype "functor")
        (setq ttype "atom")
        )
      )
     ((= char ?[ )
      (forward-char)
      (if (= (following-char) ?])
          (progn 
            (forward-char)
            (if (= (following-char) ?\( )
                (setq ttype "functor")
              (setq ttype "atom")
              )
            )
        (setq ttype "[")
        )
      )
     ((= char ?\")
      (let  ((continue t))
        (forward-char)
        (while continue
          (if (not (search-forward """" nil t))
              (BadToken "Quoted string not closed")
            (if *character-escapes*
                (if (not (= (pre-preceding-char) ?\\))
                    (if (not (pattern-at-point "\""))
                        (setq continue nil)
                      )
                  )
              (if (not (pattern-at-point "\""))
                  (setq continue nil)
                )
              )
            )
          )
        (setq ttype "string")
        ))
     ((= char ?{)
      (forward-char)
      (if (= (following-char) ?})
          (progn 
            (forward-char)
            (if (= (following-char) ?\()
                   (setq ttype "functor")
                   (setq ttype "atom")
                   )
                )
            (setq ttype "{")
            )
        )
     ((= char ?\000)
      (forward-char)
      (BadToken (concat "Cannot handle "
                        (char-to-string char)
                        " as a token"))
      )
     )
    ttype
    ))


