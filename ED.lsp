(DEFUN ENCODED (l)
	(AND (LISTP l) (EQ (FIRST l) 'E) (LISTP (REST l)))
)

(DEFUN ENCODE (l)
	(COND 
		((NULL l) 'NullListException)
		((NOT (ENCODED l)) 'NotEncodedListException)
		(T (ENCODE_REC (REST l)))
	)
)

(DEFUN ENCODE_REC (l)
	(l)
)

(DEFUN ENCODER (l)
	(l)
)


(DEFUN DECODE (l)
	(l)
)
