;Sees if the list is encoded
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
	
	;Check to see if we have a nested list
	(IF (LISTP (FIRST l))
		;We do have a nested list
		(COND
			;Make sure that it isn't just the NIL value
			((EQUAL (FIRST l) NIL) NIL)
			
			;Get the encoding of the nested list and continue
			(T (APPEND (CONS (ENCODE_REC (FIRST l)) NIL) (ENCODE_REC (REST l))))
		)

		;We don't have a nested list
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

;Decodes a list that was encoded using ENCODE
(DEFUN DECODE (l)
	(COND 
		;If is not a list then return an error
		((NOT (LISTP l)) 'Not-List-Exception)

		;If the list is not encoded then return an error
		((NOT (ENCODED l)) 'Not-Encoded-List-Exception)

		;Call the helper function
		(T (DECODE_REC (REST l)))
	)
)

;Decode helper function
(DEFUN DECODE_REC (l)
	(COND
		;We are at the end of the list so just return nil
		((EQUAL (FIRST l) NIL) NIL)

		;If the element is a tag then explode the tag and recurse
		((ISTAG (FIRST l)) (APPEND (EXPLODETAG (FIRST l)) (DECODE_REC (REST l))))

		;Just explore the rest of the list
		(T (APPEND (CONS (FIRST l) ()) (DECODE_REC (REST l))))
	)

)

;Check to see if the list is a tag element
(DEFUN ISTAG (l)
	(COND
		;If the element is not a list than it can not be a tag element
		((NOT (LISTP l)) NIL)

		;Make sure the first element of the list is  "TAG"
		(T (EQUAL 'TAG (FIRST l)))
	)
)

;Explodes the tag to a decoded list representation
(DEFUN EXPLODETAG (l)
	;Call our helper function
	(EXPLODETAG_REC (FIRST (REST l)) (FIRST (REST (REST l))) 0)
)

;Recursive helper function
(DEFUN EXPLODETAG_REC (item times counter)
	(COND 
		;If the counter is equal to times then end the recursion
		((EQUAL times counter) NIL)

		;Add the item to the list, increment the counter and continue recursion
		(T (CONS item (EXPLODETAG_REC item times (+ 1 counter))))
	)
)
