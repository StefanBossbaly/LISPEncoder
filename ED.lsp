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
	(COND
		((EQUAL (FIRST l) NIL) l)
		((EQUAL (NUMOFCONITEMS l (FIRST l)) 0) (ENCODE_REC (REST l)))
		(T
			;Remove them from the list
			(SET 'CLEAN-LIST (REMOVEITEMS l (NUMOFCONITEMS l (FIRST l))))

			(PRINT CLEAN-LIST)

			(PRINT (FIRST l))
			(PRINT (NUMOFCONITEMS l (FIRST l)))
			;(APPEND CLEAN-LIST (ENCODE_REC (REST CLEAN-LIST)))
		)
	)
)

;(DEFUN INSERTTAG (l item num)
;	('TAG item num)
;)

;Counts the number of consecutive items in the list
(DEFUN NUMOFCONITEMS (l item)
	(COND
		((EQUAL (FIRST l) item) (+ 1 (NUMOFCONITEMS (REST l) item)))
		(T 0)
	)
)

;Removes the next set of items from the list
;It seems that LISP does not have a built in function
;for this so I am going to write my own recusive and compeletely
;inefficent function to do this
(DEFUN REMOVEITEMS (l num)
	(COND
		((EQUAL 0 num) l)
		(T (REMOVEITEMS (REST l) (- num 1)))
	)
)

(DEFUN DECODE (l)
	(COND 
		((NULL l) 'NullListException)
		((NOT (ENCODED l)) 'NotEncodedListException)
		(T (ENCODE_REC (REST l)))
	)
)
