(DEFUN ENCODED (l)
	(AND (LISTP l) (EQ (FIRST l) 'E) (LISTP (REST l)))
)

;Encoded the list
(DEFUN ENCODE (l)
	(COND 
		;List can not be nil
		((NULL l) 'NullListException)
		
		;Make sure we have a list
		((NOT (LISTP l)) 'NotListException)

		;Call the helper function
		(T (ENCODE_REC l))
	)
)

;Helper function for our encode function
(DEFUN ENCODE_REC (l)
	(COND
		;We are at the end of the list so just return nil
		((EQUAL (FIRST l) NIL) NIL)

		;If there are no consecutive items just keep going on
		((EQUAL (NUMOFCONITEMS l (FIRST l)) 0) (ENCODE_REC (REST l)))

		;We have consecutive items so lets process them
		(T
			
			;Create the tag and append it to the end of the list
			;Remove them from the list
			(SET 'CLEAN-LIST (REMOVEITEMS l (NUMOFCONITEMS l (FIRST l))))

			(APPEND 'TEST (ENCODE_REC (REST l))
		)
	)
)

;(DEFUN INSERTTAG (l item num)
;	('TAG item num)
;)

;Counts the number of consecutive items in the list
(DEFUN NUMOFCONITEMS (l item)
	(COND
		;If the first of the list equals the item
		((EQUAL (FIRST l) item) (+ 1 (NUMOFCONITEMS (REST l) item)))
		
		;Base case return 0
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
