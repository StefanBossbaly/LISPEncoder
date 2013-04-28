(DEFUN ENCODED (l)
	(AND (LISTP l) (EQ (FIRST l) 'E) (LISTP (REST l)))
)

;Encoded the list
(DEFUN ENCODE (l)
	(COND 
		((NULL l) 'NullListException)
		((NOT (ENCODED l)) 'NotEncodedListException)
		(T (ENCODE_REC (REST l)))
	)
)

;Helper function for our encode function
(DEFUN ENCODE_REC (l)
	(l)
)

;Counts the number of consecutive items in the list
(DEFUN NUMOFCONITEMS (l item)
	(COND
		((EQUAL (FIRST l) item) (+ 1 (NUMOFCONITEMS (REST l) item)))
		(T 0)
	)
)

(DEFUN DECODE (l)
	(l)
)
