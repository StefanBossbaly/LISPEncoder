(DEFUN ENCODED (l)
	(AND (LISTP l) (EQ (FIRST l) 'E) (LISTP (REST l)))
)

;Encoded the list
(DEFUN ENCODE (l)
	(COND 		
		;Make sure we have a list
		((NOT (LISTP l)) 'Not-List-Exception)

		;Call the helper function
		(T (CONS 'E (ENCODE_REC l)))
	)
)

;Helper function for our encode function
(DEFUN ENCODE_REC (l)
	(COND
		;We are at the end of the list so just return nil
		((EQUAL (FIRST l) NIL) NIL)

		;If there are no consecutive items just keep going on
		((EQUAL (NUMOFCONITEMS l (FIRST l)) 1) (APPEND (CONS (FIRST l) ()) (ENCODE_REC (REST l))))

		;We have consecutive items so lets process them
		(T
			;Create the tag and append it to the front of the list and recursively call the ecode function on the rest of the list
			(APPEND (GENTAG (FIRST l) (NUMOFCONITEMS l (FIRST l))) (ENCODE_REC (SKIP l (NUMOFCONITEMS l (FIRST l)))))
		)
	)
)

;Returns the tag for the list
(DEFUN GENTAG (item num)
	(CONS (CONS 'TAG (CONS item (CONS num ()))) ())
)

;Skips ahead in the list
(DEFUN SKIP (l items)
	(COND
		;Keep going cause the party doesn't stop until we hit zero
		((NOT (EQUAL items 0)) (SKIP (REST l) (- items 1)))
		
		;Base case return the list
		(T l)
	)
)

;Counts the number of consecutive items in the list
(DEFUN NUMOFCONITEMS (l item)
	(COND
		;If the first of the list equals the item
		((EQUAL (FIRST l) item) (+ 1 (NUMOFCONITEMS (REST l) item)))
		
		;Base case return 0
		(T 0)
	)
)

(DEFUN DECODE (l)
	(COND 
		((NOT (LISTP l)) 'Not-List-Exception)
		((NOT (ENCODED l)) 'Not-Encoded-List-Exception)
		(T (DECODE_REC (REST l)))
	)
)

(DEFUN DECODE_REC (l)
	(COND
		;We are at the end of the list so just return nil
		((EQUAL (FIRST l) NIL) NIL)

		;SE
		((ISTAG (FIRST l)) (APPEND (EXPLODETAG (FIRST l)) (DECODE_REC (REST l))))

		(T (APPEND (CONS (FIRST l) ()) (DECODE_REC (REST l))))
	)

)

(DEFUN ISTAG (l)
	(COND
		((NOT (LISTP l)) NIL)
		(T (EQUAL 'TAG (FIRST l)))
	)
)

(DEFUN EXPLODETAG (l)
	(EXPLODETAG_REC (FIRST (REST l)) (FIRST (REST (REST l))) 0)
)

(DEFUN EXPLODETAG_REC (item times counter)
	(COND 
		((EQUAL times counter) NIL)
		(T (CONS item (EXPLODETAG_REC item times (+ 1 counter))))
	)
)
