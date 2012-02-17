'(r-expressions (<- Sys.time (lambda ()
			      (let () (r-block (r-call structure (r-call
								  .Internal (r-call
  Sys.time))
						       (*named* class (r-call
  c "POSIXt" "POSIXct")))))))
	       (<- Sys.timezone (lambda ()
				  (let ()
				       (r-block (r-call as.vector (r-call
								   Sys.getenv
								   "TZ"))))))
	       (<- as.POSIXlt (lambda (x tz)
				(let ((x ())
				      (tzone ())
				      (fromchar ())
				      (tz ()))
				     (r-block (when (missing tz)
						    (<- tz ""))
					      (<- fromchar (lambda (x)
							     (let ((res ())
								   (f ())
								   (j ())
								   (xx ()))
								  (r-block (<-
  xx (r-call r-index x 1))
  (if (r-call is.na xx) (r-block (<- j 1)
				 (while (&& (r-call is.na xx)
					    (r-call <= (<- j (r-call + j 1))
						    (r-call length x)))
					(<- xx (r-call r-index x j)))
				 (if (r-call is.na xx)
				     (<- f "%Y-%m-%d"))))
  (if (|\|\|| (r-call is.na xx) (r-call ! (r-call is.na (r-call strptime xx
								(<- f "%Y-%m-%d %H:%M:%OS"))))
	      (r-call ! (r-call is.na (r-call strptime xx
					      (<- f "%Y/%m/%d %H:%M:%OS"))))
	      (r-call ! (r-call is.na (r-call strptime xx
					      (<- f "%Y-%m-%d %H:%M"))))
	      (r-call ! (r-call is.na (r-call strptime xx
					      (<- f "%Y/%m/%d %H:%M"))))
	      (r-call ! (r-call is.na (r-call strptime xx
					      (<- f "%Y-%m-%d"))))
	      (r-call ! (r-call is.na (r-call strptime xx
					      (<- f "%Y/%m/%d")))))
      (r-block (<- res (r-call strptime x f))
	       (if (r-call nchar tz) (r-block (<- res (r-call attr<- res "tzone"
							      tz))
					      tz))
	       (return res)))
  (r-call stop "character string is not in a standard unambiguous format")))))
					      (if (r-call inherits x "POSIXlt")
						  (return x))
					      (if (r-call inherits x "Date")
						  (return (r-call .Internal (r-call
  Date2POSIXlt x))))
					      (<- tzone (r-call attr x "tzone"))
					      (if (|\|\|| (r-call inherits x "date")
							  (r-call inherits x "dates"))
						  (<- x (r-call as.POSIXct x)))
					      (if (r-call is.character x)
						  (return (r-call fromchar (r-call
  unclass x))))
					      (if (r-call is.factor x)
						  (return (r-call fromchar (r-call
  as.character x))))
					      (if (&& (r-call is.logical x)
						      (r-call all (r-call is.na
  x)))
						  (<- x (r-call
							 as.POSIXct.default x)))
					      (if (r-call ! (r-call inherits x
								    "POSIXct"))
						  (r-call stop (r-call gettextf
  "do not know how to convert '%s' to class \"POSIXlt\""
  (r-call deparse (substitute x)))))
					      (if (&& (missing tz)
						      (r-call ! (r-call is.null
  tzone)))
						  (<- tz (r-call r-index tzone
								 1)))
					      (r-call .Internal (r-call
								 as.POSIXlt x
								 tz))))))
	       (<- as.POSIXct (lambda (x tz)
				(let ((tz ()))
				     (r-block (when (missing tz)
						    (<- tz ""))
					      (r-call UseMethod "as.POSIXct")))))
	       (<- as.POSIXct.Date (lambda (x ...)
				     (let ()
					  (r-block (r-call structure (r-call *
  (r-call unclass x) 86400)
							   (*named* class (r-call
  c "POSIXt" "POSIXct")))))))
	       (<- as.POSIXct.date (lambda (x ...)
				     (let ((x ()))
					  (r-block (if (r-call inherits x "date")
						       (r-block (<- x (r-call
  * (r-call - x 3653) 86400))
								(return (r-call
  structure x (*named* class (r-call c "POSIXt" "POSIXct")))))
						       (r-call stop (r-call
  gettextf "'%s' is not a \"date\" object"
  (r-call deparse (substitute x)))))))))
	       (<- as.POSIXct.dates (lambda (x ...)
				      (let ((x ())
					    (z ()))
					   (r-block (if (r-call inherits x "dates")
							(r-block (<- z (r-call
  attr x "origin"))
								 (<- x (r-call
  * (r-call as.numeric x) 86400))
								 (if (&& (r-call
  == (r-call length z) 3)
  (r-call is.numeric z))
  (<- x (r-call + x
		(r-call as.numeric (r-call ISOdate (r-call r-index z 3)
					   (r-call r-index z 1)
					   (r-call r-index z 2) 0)))))
								 (return (r-call
  structure x (*named* class (r-call c "POSIXt" "POSIXct")))))
							(r-call stop (r-call
  gettextf "'%s' is not a \"dates\" object"
  (r-call deparse (substitute x)))))))))
	       (<- as.POSIXct.POSIXlt (lambda (x tz)
					(let ((tzone ())
					      (tz ()))
					     (r-block (when (missing tz)
							    (<- tz ""))
						      (<- tzone (r-call attr x
  "tzone"))
						      (if (&& (missing tz)
							      (r-call ! (r-call
  is.null tzone)))
							  (<- tz (r-call
								  r-index tzone
								  1)))
						      (r-call structure (r-call
  .Internal (r-call as.POSIXct x tz))
							      (*named* class (r-call
  c "POSIXt" "POSIXct"))
							      (*named* tzone tz))))))
	       (<- as.POSIXct.default (lambda (x tz)
					(let ((tz ()))
					     (r-block (when (missing tz)
							    (<- tz ""))
						      (if (r-call inherits x "POSIXct")
							  (return x))
						      (if (|\|\|| (r-call
								   is.character
								   x)
								  (r-call
								   is.factor x))
							  (return (r-call
								   as.POSIXct
								   (r-call
								    as.POSIXlt
								    x)
								   tz)))
						      (if (&& (r-call
							       is.logical x)
							      (r-call all (r-call
  is.na x)))
							  (return (r-call
								   structure (r-call
  as.numeric x)
								   (*named*
								    class (r-call
  c "POSIXt" "POSIXct")))))
						      (r-call stop (r-call
								    gettextf "do not know how to convert '%s' to class \"POSIXlt\""
								    (r-call
  deparse (substitute x))))))))
	       (<- as.numeric.POSIXlt (lambda (x)
					(let ()
					     (r-block (r-call as.POSIXct x)))))
	       (<- format.POSIXlt (lambda (x format usetz ...)
				    (let ((np ())
					  (secs ())
					  (times ())
					  (usetz ())
					  (format ()))
					 (r-block (when (missing format)
							(<- format ""))
						  (when (missing usetz)
							(<- usetz *r-false*))
						  (if (r-call ! (r-call
								 inherits x "POSIXlt"))
						      (r-call stop "wrong class"))
						  (if (r-call == format "")
						      (r-block (<- times (r-call
  unlist (r-call r-index (r-call unclass x)
		 (r-call : 1 3))))
							       (<- secs (r-call
  r-aref x (index-in-strlist sec (r-call attr x #0="names"))))
							       (<- secs (r-call
  r-index secs (r-call ! (r-call is.na secs))))
							       (<- np (r-call
  getOption "digits.secs"))
							       (if (r-call
								    is.null np)
								   (<- np 0)
								   (<- np (r-call
  min 6 np)))
							       (if (r-call >=
  np 1)
								   (r-block (for
  i (r-call - (r-call : 1 np) 1)
  (if (r-call all (r-call < (r-call abs (r-call - secs
						(r-call round secs i)))
			  9.9999999999999995e-07))
      (r-block (<- np i) (break))))))
							       (<- format (if
  (r-call all (r-call == (r-call r-index times
				 (r-call ! (r-call is.na times)))
		      0))
  "%Y-%m-%d" (if (r-call == np 0) "%Y-%m-%d %H:%M:%S"
		 (r-call paste "%Y-%m-%d %H:%M:%OS" np
			 (*named* sep "")))))))
						  (r-call .Internal (r-call
  format.POSIXlt x format usetz))))))
	       (<- strftime format.POSIXlt)
	       (<- strptime (lambda (x format tz)
			      (let ((tz ()))
				   (r-block (when (missing tz)
						  (<- tz ""))
					    (r-call .Internal (r-call strptime
  (r-call as.character x) format tz))))))
	       (<- format.POSIXct (lambda (x format tz usetz ...)
				    (let ((tzone ())
					  (usetz ())
					  (tz ())
					  (format ()))
					 (r-block (when (missing format)
							(<- format ""))
						  (when (missing tz)
							(<- tz ""))
						  (when (missing usetz)
							(<- usetz *r-false*))
						  (if (r-call ! (r-call
								 inherits x "POSIXct"))
						      (r-call stop "wrong class"))
						  (if (&& (missing tz)
							  (r-call ! (r-call
  is.null (<- tzone (r-call attr x "tzone")))))
						      (<- tz tzone))
						  (r-call structure (r-call
  format.POSIXlt (r-call as.POSIXlt x tz) format usetz r-dotdotdot)
							  (*named* names (r-call
  names x)))))))
	       (<- print.POSIXct (lambda (x ...)
				   (let ()
					(r-block (r-call print (r-call format
  x (*named* usetz *r-true*) r-dotdotdot)
							 r-dotdotdot)
						 (r-call invisible x)))))
	       (<- print.POSIXlt (lambda (x ...)
				   (let ()
					(r-block (r-call print (r-call format
  x (*named* usetz *r-true*))
							 r-dotdotdot)
						 (r-call invisible x)))))
	       (<- summary.POSIXct (lambda (object digits ...)
				     (let ((x ())
					   (digits ()))
					  (r-block (when (missing digits)
							 (<- digits 15))
						   (<- x (r-call r-index (r-call
  summary.default (r-call unclass object)
  (*named* digits digits) r-dotdotdot)
								 (r-call : 1 6)))
						   (r-block (ref= %r:1 (r-call
  oldClass object))
							    (<- x (r-call
								   class<- x
								   %r:1))
							    %r:1)
						   (r-block (ref= %r:2 (r-call
  attr object "tzone"))
							    (<- x (r-call
								   attr<- x "tzone"
								   %r:2))
							    %r:2)
						   x))))
	       (<- summary.POSIXlt (lambda (object digits ...)
				     (let ((digits ()))
					  (r-block (when (missing digits)
							 (<- digits 15))
						   (r-call summary (r-call
								    as.POSIXct
								    object)
							   (*named* digits
								    digits)
							   r-dotdotdot)))))
	       (<- "+.POSIXt" (lambda (e1 e2)
				(let ((e2 ())
				      (e1 ())
				      (coerceTimeUnit ()))
				     (r-block (<- coerceTimeUnit (lambda (x)
								   (let ()
  (r-block (switch (r-call attr x "units")
		   (*named* secs x) (*named* mins (r-call * 60 x))
		   (*named* hours (r-call * (r-call * 60 60) x))
		   (*named* days (r-call * (r-call * (r-call * 60 60) 24) x))
		   (*named* weeks (r-call * (r-call * (r-call * (r-call * 60 60)
							      24)
						    7)
					  x)))))))
					      (if (r-call == (r-call nargs) 1)
						  (return e1))
					      (if (&& (r-call inherits e1 "POSIXt")
						      (r-call inherits e2 "POSIXt"))
						  (r-call stop "binary + is not defined for \"POSIXt\" objects"))
					      (if (r-call inherits e1 "POSIXlt")
						  (<- e1 (r-call as.POSIXct e1)))
					      (if (r-call inherits e2 "POSIXlt")
						  (<- e2 (r-call as.POSIXct e2)))
					      (if (r-call inherits e1 "difftime")
						  (<- e1 (r-call coerceTimeUnit
								 e1)))
					      (if (r-call inherits e2 "difftime")
						  (<- e2 (r-call coerceTimeUnit
								 e2)))
					      (r-call structure (r-call + (r-call
  unclass e1)
  (r-call unclass e2))
						      (*named* class (r-call c
  "POSIXt" "POSIXct"))
						      (*named* tzone (r-call
  check_tzones e1 e2)))))))
	       (<- "-.POSIXt" (lambda (e1 e2)
				(let ((e2 ())
				      (coerceTimeUnit ()))
				     (r-block (<- coerceTimeUnit (lambda (x)
								   (let ()
  (r-block (switch (r-call attr x "units")
		   (*named* secs x) (*named* mins (r-call * 60 x))
		   (*named* hours (r-call * (r-call * 60 60) x))
		   (*named* days (r-call * (r-call * (r-call * 60 60) 24) x))
		   (*named* weeks (r-call * (r-call * (r-call * (r-call * 60 60)
							      24)
						    7)
					  x)))))))
					      (if (r-call ! (r-call inherits e1
								    "POSIXt"))
						  (r-call stop "Can only subtract from POSIXt objects"))
					      (if (r-call == (r-call nargs) 1)
						  (r-call stop "unary - is not defined for \"POSIXt\" objects"))
					      (if (r-call inherits e2 "POSIXt")
						  (return (r-call difftime e1
								  e2)))
					      (if (r-call inherits e2 "difftime")
						  (<- e2 (r-call unclass (r-call
  coerceTimeUnit e2))))
					      (if (r-call ! (r-call is.null (r-call
  attr e2 "class")))
						  (r-call stop "can only subtract numbers from POSIXt objects"))
					      (r-call structure (r-call - (r-call
  unclass (r-call as.POSIXct e1))
  e2)
						      (*named* class (r-call c
  "POSIXt" "POSIXct")))))))
	       (<- Ops.POSIXt (lambda (e1 e2)
				(let ((e2 ())
				      (e1 ())
				      (boolean ()))
				     (r-block (if (r-call == (r-call nargs) 1)
						  (r-call stop "unary" .Generic
							  " not defined for \"POSIXt\" objects"))
					      (<- boolean (switch .Generic (*named*
  < *r-missing*)
								  (*named* >
  *r-missing*)
								  (*named* ==
  *r-missing*)
								  (*named* !=
  *r-missing*)
								  (*named* <=
  *r-missing*)
								  (*named* >=
  *r-true*)
								  *r-false*))
					      (if (r-call ! boolean)
						  (r-call stop .Generic
							  " not defined for \"POSIXt\" objects"))
					      (if (|\|\|| (r-call inherits e1
								  "POSIXlt")
							  (r-call is.character
								  e1))
						  (<- e1 (r-call as.POSIXct e1)))
					      (if (|\|\|| (r-call inherits e2
								  "POSIXlt")
							  (r-call is.character
								  e1))
						  (<- e2 (r-call as.POSIXct e2)))
					      (r-call check_tzones e1 e2)
					      (r-call NextMethod .Generic)))))
	       (<- Math.POSIXt (lambda (x ...)
				 (let () (r-block (r-call stop .Generic
							  " not defined for POSIXt objects")))))
	       (<- check_tzones (lambda (...)
				  (let ((tzs ()))
				       (r-block (<- tzs (r-call unique (r-call
  sapply (r-call list r-dotdotdot) (lambda (x)
				     (let ((y ()))
					  (r-block (<- y (r-call attr x "tzone"))
						   (if (r-call is.null y) "" y)))))))
						(<- tzs (r-call r-index tzs
								(r-call != tzs
  "")))
						(if (r-call > (r-call length
  tzs)
							    1)
						    (r-call warning "'tzone' attributes are inconsistent"))
						(if (r-call length tzs)
						    (r-call r-index tzs 1)
						    ())))))
	       (<- Summary.POSIXct (lambda (... na.rm)
				     (let ((val ())
					   (tz ())
					   (args ())
					   (ok ()))
					  (r-block (<- ok (switch .Generic (*named*
  max *r-missing*)
								  (*named* min
  *r-missing*)
								  (*named*
								   range
								   *r-true*)
								  *r-false*))
						   (if (r-call ! ok)
						       (r-call stop .Generic
							       " not defined for \"POSIXct\" objects"))
						   (<- args (r-call list
								    r-dotdotdot))
						   (<- tz (r-call do.call "check_tzones"
								  args))
						   (<- val (r-call NextMethod
								   .Generic))
						   (r-block (ref= %r:3 (r-call
  oldClass (r-call r-aref args 1)))
							    (<- val (r-call
  class<- val %r:3))
							    %r:3)
						   (r-block (<- val (r-call
  attr<- val "tzone" tz))
							    tz)
						   val))))
	       (<- Summary.POSIXlt (lambda (... na.rm)
				     (let ((val ())
					   (tz ())
					   (args ())
					   (ok ()))
					  (r-block (<- ok (switch .Generic (*named*
  max *r-missing*)
								  (*named* min
  *r-missing*)
								  (*named*
								   range
								   *r-true*)
								  *r-false*))
						   (if (r-call ! ok)
						       (r-call stop .Generic
							       " not defined for \"POSIXlt\" objects"))
						   (<- args (r-call list
								    r-dotdotdot))
						   (<- tz (r-call do.call "check_tzones"
								  args))
						   (<- args (r-call lapply args
								    as.POSIXct))
						   (<- val (r-call do.call
								   .Generic (r-call
  c args (*named* na.rm na.rm))))
						   (r-call as.POSIXlt (r-call
  structure val (*named* class (r-call c "POSIXt" "POSIXct"))
  (*named* tzone tz)))))))
	       (<- "[.POSIXct" (lambda (x ... drop)
				 (let ((val ())
				       (x ())
				       (cl ())
				       (drop ()))
				      (r-block (when (missing drop)
						     (<- drop *r-true*))
					       (<- cl (r-call oldClass x))
					       (r-block (<- x (r-call class<-
  x ()))
							())
					       (<- val (r-call NextMethod "["))
					       (r-block (<- val (r-call class<-
  val cl))
							cl)
					       (r-block (ref= %r:4 (r-call attr
  x "tzone"))
							(<- val (r-call attr<-
  val "tzone" %r:4))
							%r:4)
					       val))))
	       (<- "[[.POSIXct" (lambda (x ... drop)
				  (let ((val ())
					(x ())
					(cl ())
					(drop ()))
				       (r-block (when (missing drop)
						      (<- drop *r-true*))
						(<- cl (r-call oldClass x))
						(r-block (<- x (r-call class<-
  x ()))
							 ())
						(<- val (r-call NextMethod "[["))
						(r-block (<- val (r-call
								  class<- val
								  cl))
							 cl)
						(r-block (ref= %r:5 (r-call
  attr x "tzone"))
							 (<- val (r-call attr<-
  val "tzone" %r:5))
							 %r:5)
						val))))
	       (<- "[<-.POSIXct" (lambda (x ... value)
				   (let ((x ())
					 (tz ())
					 (cl ())
					 (value ()))
					(r-block (if (r-call ! (r-call
								as.logical (r-call
  length value)))
						     (return x))
						 (<- value (r-call as.POSIXct
								   value))
						 (<- cl (r-call oldClass x))
						 (<- tz (r-call attr x "tzone"))
						 (r-block (ref= %r:6 (r-block
  (<- value (r-call class<- value
		    ()))
  ()))
							  (<- x (r-call class<-
  x %r:6))
							  %r:6)
						 (<- x (r-call NextMethod
							       .Generic))
						 (r-block (<- x (r-call class<-
  x cl))
							  cl)
						 (r-block (<- x (r-call attr<-
  x "tzone" tz))
							  tz)
						 x))))
	       (<- as.character.POSIXt (lambda (x ...)
					 (let ()
					      (r-block (r-call format x
							       r-dotdotdot)))))
	       (<- as.data.frame.POSIXct as.data.frame.vector)
	       (<- is.na.POSIXlt (lambda (x)
				   (let ()
					(r-block (r-call is.na (r-call
								as.POSIXct x))))))
	       (<- c.POSIXct (lambda (... recursive)
			       (let ((recursive ()))
				    (r-block (when (missing recursive)
						   (<- recursive *r-false*))
					     (r-call structure (r-call c (r-call
  unlist (r-call lapply (r-call list r-dotdotdot) unclass)))
						     (*named* class (r-call c
  "POSIXt" "POSIXct")))))))
	       (<- c.POSIXlt (lambda (... recursive)
			       (let ((recursive ()))
				    (r-block (when (missing recursive)
						   (<- recursive *r-false*))
					     (r-call as.POSIXlt (r-call do.call
  "c" (r-call lapply (r-call list r-dotdotdot) as.POSIXct)))))))
	       (<- all.equal.POSIXct (lambda (target current ... scale)
				       (let ((scale ()))
					    (r-block (when (missing scale)
							   (<- scale 1))
						     (r-call check_tzones
							     target current)
						     (r-call NextMethod "all.equal")))))
	       (<- ISOdatetime (lambda (year month day hour min sec tz)
				 (let ((x ())
				       (tz ()))
				      (r-block (when (missing tz)
						     (<- tz ""))
					       (<- x (r-call paste year month
							     day hour min sec
							     (*named* sep "-")))
					       (r-call as.POSIXct (r-call
								   strptime x
								   "%Y-%m-%d-%H-%M-%OS"
								   (*named* tz
  tz))
						       (*named* tz tz))))))
	       (<- ISOdate (lambda (year month day hour min sec tz)
			     (let ((tz ())
				   (sec ())
				   (min ())
				   (hour ()))
				  (r-block (when (missing hour)
						 (<- hour 12))
					   (when (missing min)
						 (<- min 0))
					   (when (missing sec)
						 (<- sec 0))
					   (when (missing tz)
						 (<- tz "GMT"))
					   (r-call ISOdatetime year month day
					    hour min sec tz)))))
	       (<- as.matrix.POSIXlt (lambda (x ...)
				       (let ()
					    (r-block (r-call as.matrix (r-call
  as.data.frame (r-call unclass x))
							     r-dotdotdot)))))
	       (<- mean.POSIXct (lambda (x ...)
				  (let ()
				       (r-block (r-call structure (r-call mean
  (r-call unclass x) r-dotdotdot)
							(*named* class (r-call
  c "POSIXt" "POSIXct"))
							(*named* tzone (r-call
  attr x "tzone")))))))
	       (<- mean.POSIXlt (lambda (x ...)
				  (let ()
				       (r-block (r-call as.POSIXlt (r-call mean
  (r-call as.POSIXct x) r-dotdotdot))))))
	       (<- difftime (lambda (time1 time2 tz units)
			      (let ((zz ())
				    (z ())
				    (time2 ())
				    (time1 ())
				    (units ())
				    (tz ()))
				   (r-block (when (missing tz)
						  (<- tz ""))
					    (when (missing units)
						  (<- units (r-call c "auto" "secs"
								    "mins" "hours"
								    "days" "weeks")))
					    (<- time1 (r-call as.POSIXct time1
							      (*named* tz tz)))
					    (<- time2 (r-call as.POSIXct time2
							      (*named* tz tz)))
					    (<- z (r-call - (r-call unclass
								    time1)
							  (r-call unclass time2)))
					    (<- units (r-call match.arg units))
					    (if (r-call == units "auto")
						(r-block (if (r-call all (r-call
  is.na z))
							     (<- units "secs")
							     (r-block (<- zz (r-call
  min (r-call abs z) (*named* na.rm *r-true*)))
  (if (|\|\|| (r-call is.na zz) (r-call < zz 60))
      (<- units "secs") (if (r-call < zz 3600)
			    (<- units "mins")
			    (if (r-call < zz 86400)
				(<- units "hours")
				(<- units "days"))))))))
					    (switch units (*named* secs (r-call
  structure z (*named* units "secs")
  (*named* class "difftime")))
						    (*named* mins (r-call
								   structure (r-call
  / z 60)
								   (*named*
								    units "mins")
								   (*named*
								    class "difftime")))
						    (*named* hours (r-call
								    structure
								    (r-call /
  z 3600)
								    (*named*
  units "hours")
								    (*named*
  class "difftime")))
						    (*named* days (r-call
								   structure (r-call
  / z 86400)
								   (*named*
								    units "days")
								   (*named*
								    class "difftime")))
						    (*named* weeks (r-call
								    structure
								    (r-call /
  z (r-call * 7 86400))
								    (*named*
  units "weeks")
								    (*named*
  class "difftime"))))))))
	       (<- as.difftime (lambda (tim format units)
				 (let ((units ())
				       (format ()))
				      (r-block (when (missing format)
						     (<- format "%X"))
					       (when (missing units)
						     (<- units "auto"))
					       (if (r-call inherits tim "difftime")
						   (return tim))
					       (if (r-call is.character tim)
						   (r-block (r-call difftime (r-call
  strptime tim (*named* format format))
								    (r-call
  strptime "0:0:0" (*named* format "%X"))
								    (*named*
  units units)))
						   (r-block (if (r-call ! (r-call
  is.numeric tim))
								(r-call stop "'tim' is not character or numeric"))
							    (if (r-call ==
  units "auto")
								(r-call stop "need explicit units for numeric conversion"))
							    (if (r-call ! (r-call
  %in% units (r-call c "secs" "mins" "hours" "days" "weeks")))
								(r-call stop "invalid units specified"))
							    (r-call structure
								    tim (*named*
  units units)
								    (*named*
  class "difftime"))))))))
	       (<- units (lambda (x)
			   (let () (r-block (r-call UseMethod "units")))))
	       (<- "units<-" (lambda (x value)
			       (let () (r-block (r-call UseMethod "units<-")))))
	       (<- units.difftime (lambda (x)
				    (let ()
					 (r-block (r-call attr x "units")))))
	       (<- "units<-.difftime" (lambda (x value)
					(let ((newx ())
					      (sc ())
					      (from ()))
					     (r-block (<- from (r-call units x))
						      (if (r-call == from value)
							  (return x))
						      (if (r-call ! (r-call
  %in% value (r-call c "secs" "mins" "hours" "days" "weeks")))
							  (r-call stop "invalid units specified"))
						      (<- sc (r-call cumprod (r-call
  c (*named* secs 1) (*named* mins 60)
  (*named* hours 60) (*named* days 24) (*named* weeks 7))))
						      (<- newx (r-call / (r-call
  * (r-call as.vector x) (r-call r-index sc from))
  (r-call r-index sc value)))
						      (r-call structure newx
							      (*named* units
  value)
							      (*named* class "difftime"))))))
	       (<- as.double.difftime (lambda (x units ...)
					(let ((x ())
					      (units ()))
					     (r-block (when (missing units)
							    (<- units "auto"))
						      (if (r-call != units "auto")
							  (r-block (<- x (r-call
  units<- x units))
								   units))
						      (r-call as.double (r-call
  as.vector x))))))
	       (<- as.data.frame.difftime
		   as.data.frame.vector)
	       (<- format.difftime (lambda (x ...)
				     (let ()
					  (r-block (r-call paste (r-call format
  (r-call unclass x) r-dotdotdot)
							   (r-call units x))))))
	       (<- print.difftime (lambda (x digits ...)
				    (let ((y ())
					  (digits ()))
					 (r-block (when (missing digits)
							(<- digits (r-call
								    getOption
								    "digits")))
						  (if (|\|\|| (r-call is.array
  x)
							      (r-call > (r-call
  length x)
  1))
						      (r-block (r-call cat "Time differences in "
  (r-call attr x "units") "\n" (*named* sep ""))
							       (<- y (r-call
  unclass x))
							       (r-block (<- y
  (r-call attr<- y "units"
	  ()))
  ())
							       (r-call print y))
						      (r-call cat "Time difference of "
							      (r-call format (r-call
  unclass x)
  (*named* digits digits))
							      " " (r-call attr
  x "units")
							      "\n" (*named* sep
  "")))
						  (r-call invisible x)))))
	       (<- round.difftime (lambda (x digits ...)
				    (let ((units ())
					  (digits ()))
					 (r-block (when (missing digits)
							(<- digits 0))
						  (<- units (r-call attr x "units"))
						  (r-call structure (r-call
  NextMethod)
							  (*named* units units)
							  (*named* class "difftime"))))))
	       (<- "[.difftime" (lambda (x ... drop)
				  (let ((val ())
					(x ())
					(cl ())
					(drop ()))
				       (r-block (when (missing drop)
						      (<- drop *r-true*))
						(<- cl (r-call oldClass x))
						(r-block (<- x (r-call class<-
  x ()))
							 ())
						(<- val (r-call NextMethod "["))
						(r-block (<- val (r-call
								  class<- val
								  cl))
							 cl)
						(r-block (ref= %r:7 (r-call
  attr x "units"))
							 (<- val (r-call attr<-
  val "units" %r:7))
							 %r:7)
						val))))
	       (<- Ops.difftime (lambda (e1 e2)
				  (let ((u1 ())
					(e2 ())
					(boolean ())
					(e1 ())
					(coerceTimeUnit ()))
				       (r-block (<- coerceTimeUnit (lambda (x)
  (let () (r-block (switch (r-call attr x "units")
			   (*named* secs x)
			   (*named* mins (r-call * 60 x))
			   (*named* hours (r-call * (r-call * 60 60) x))
			   (*named* days (r-call * (r-call * (r-call * 60 60)
							   24)
						 x))
			   (*named* weeks (r-call * (r-call * (r-call * (r-call
  * 60 60)
  24)
							    7)
						  x)))))))
						(if (r-call == (r-call nargs)
							    1)
						    (r-block (switch .Generic
  (*named* + (r-block)) (*named* - (r-block (r-block (ref= %r:8 (r-call - (r-call
  unclass e1)))
						     (<- e1 (r-call r-index<-
								    e1
								    *r-missing*
								    %r:8))
						     %r:8)))
  (r-call stop "unary" .Generic
	  " not defined for \"difftime\" objects"))
							     (return e1)))
						(<- boolean (switch .Generic (*named*
  < *r-missing*)
								    (*named* >
  *r-missing*)
								    (*named* ==
  *r-missing*)
								    (*named* !=
  *r-missing*)
								    (*named* <=
  *r-missing*)
								    (*named* >=
  *r-true*)
								    *r-false*))
						(if boolean (r-block (if (&& (r-call
  inherits e1 "difftime")
  (r-call inherits e2 "difftime"))
  (r-block (<- e1 (r-call coerceTimeUnit e1))
	   (<- e2 (r-call coerceTimeUnit e2))))
  (r-call NextMethod .Generic))
						    (if (|\|\|| (r-call ==
  .Generic "+")
								(r-call ==
  .Generic "-"))
							(r-block (if (&& (r-call
  inherits e1 "difftime")
  (r-call ! (r-call inherits e2 "difftime")))
  (return (r-call structure (r-call NextMethod .Generic)
		  (*named* units (r-call attr e1 "units"))
		  (*named* class "difftime"))))
								 (if (&& (r-call
  ! (r-call inherits e1 "difftime"))
  (r-call inherits e2 "difftime"))
  (return (r-call structure (r-call NextMethod .Generic)
		  (*named* units (r-call attr e2 "units"))
		  (*named* class "difftime"))))
								 (<- u1 (r-call
  attr e1 "units"))
								 (if (r-call ==
  (r-call attr e2 "units") u1)
  (r-block (r-call structure (r-call NextMethod .Generic)
		   (*named* units u1) (*named* class "difftime")))
  (r-block (<- e1 (r-call coerceTimeUnit e1))
	   (<- e2 (r-call coerceTimeUnit e2))
	   (r-call structure (r-call NextMethod .Generic)
		   (*named* units "secs")
		   (*named* class "difftime")))))
							(r-block (r-call stop
  .Generic "not defined for \"difftime\" objects"))))))))
	       (<- "*.difftime" (lambda (e1 e2)
				  (let ((e2 ())
					(e1 ())
					(tmp ()))
				       (r-block (if (&& (r-call inherits e1 "difftime")
							(r-call inherits e2 "difftime"))
						    (r-call stop "both arguments of * cannot be \"difftime\" objects"))
						(if (r-call inherits e2 "difftime")
						    (r-block (<- tmp e1)
							     (<- e1 e2)
							     (<- e2 tmp)))
						(r-call structure (r-call * e2
  (r-call unclass e1))
							(*named* units (r-call
  attr e1 "units"))
							(*named* class "difftime"))))))
	       (<- "/.difftime" (lambda (e1 e2)
				  (let ()
				       (r-block (if (r-call inherits e2 "difftime")
						    (r-call stop "second argument of / cannot be a \"difftime\" object"))
						(r-call structure (r-call / (r-call
  unclass e1)
  e2)
							(*named* units (r-call
  attr e1 "units"))
							(*named* class "difftime"))))))
	       (<- Math.difftime (lambda (x ...)
				   (let ()
					(r-block (r-call stop .Generic
							 "not defined for \"difftime\" objects")))))
	       (<- mean.difftime (lambda (x ... na.rm)
				   (let ((args ())
					 (coerceTimeUnit ())
					 (na.rm ()))
					(r-block (when (missing na.rm)
						       (<- na.rm *r-false*))
						 (<- coerceTimeUnit (lambda (x)
  (let () (r-block (r-call as.vector (switch (r-call attr x "units")
					     (*named* secs x)
					     (*named* mins (r-call * 60 x))
					     (*named* hours (r-call * (r-call
  * 60 60)
								    x))
					     (*named* days (r-call * (r-call *
  (r-call * 60 60) 24)
								   x))
					     (*named* weeks (r-call * (r-call
  * (r-call * (r-call * 60 60) 24) 7)
								    x))))))))
						 (if (r-call length (r-call
  list r-dotdotdot))
						     (r-block (<- args (r-call
  c (r-call lapply (r-call list x r-dotdotdot) coerceTimeUnit)
  (*named* na.rm na.rm)))
							      (r-call structure
  (r-call do.call "mean" args) (*named* units "secs")
  (*named* class "difftime")))
						     (r-block (r-call structure
  (r-call mean (r-call as.vector x)
	  (*named* na.rm na.rm))
  (*named* units (r-call attr x "units"))
  (*named* class "difftime"))))))))
	       (<- Summary.difftime (lambda (... na.rm)
				      (let ((args ())
					    (ok ())
					    (coerceTimeUnit ()))
					   (r-block (<- coerceTimeUnit (lambda (x)
  (let () (r-block (r-call as.vector (switch (r-call attr x "units")
					     (*named* secs x)
					     (*named* mins (r-call * 60 x))
					     (*named* hours (r-call * (r-call
  * 60 60)
								    x))
					     (*named* days (r-call * (r-call *
  (r-call * 60 60) 24)
								   x))
					     (*named* weeks (r-call * (r-call
  * (r-call * (r-call * 60 60) 24) 7)
								    x))))))))
						    (<- ok (switch .Generic (*named*
  max *r-missing*)
								   (*named* min
  *r-missing*)
								   (*named*
								    range
								    *r-true*)
								   *r-false*))
						    (if (r-call ! ok)
							(r-call stop .Generic
								" not defined for \"difftime\" objects"))
						    (<- args (r-call c (r-call
  lapply (r-call list r-dotdotdot) coerceTimeUnit)
  (*named* na.rm na.rm)))
						    (r-call structure (r-call
  do.call .Generic args)
							    (*named* units "secs")
							    (*named* class "difftime"))))))
	       (<- seq.POSIXt (lambda (from to by length.out along.with ...)
				(let ((mon ())
				      (yr ())
				      (r1 ())
				      (by2 ())
				      (by ())
				      (valid ())
				      (res ())
				      (to ())
				      (from ())
				      (status ())
				      (tz ())
				      (cfrom ())
				      (along.with ())
				      (length.out ()))
				     (r-block (when (missing length.out)
						    (<- length.out ()))
					      (when (missing along.with)
						    (<- along.with ()))
					      (if (missing from)
						  (r-call stop "'from' must be specified"))
					      (if (r-call ! (r-call inherits
								    from "POSIXt"))
						  (r-call stop "'from' must be a POSIXt object"))
					      (<- cfrom (r-call as.POSIXct from))
					      (if (r-call != (r-call length
  cfrom)
							  1)
						  (r-call stop "'from' must be of length 1"))
					      (<- tz (r-call attr cfrom "tzone"))
					      (if (r-call ! (missing to))
						  (r-block (if (r-call ! (r-call
  inherits to "POSIXt"))
							       (r-call stop "'to' must be a POSIXt object"))
							   (if (r-call != (r-call
  length (r-call as.POSIXct to))
  1)
							       (r-call stop "'to' must be of length 1"))))
					      (if (r-call ! (missing along.with))
						  (r-block (<- length.out (r-call
  length along.with)))
						  (if (r-call ! (r-call is.null
  length.out))
						      (r-block (if (r-call !=
  (r-call length length.out) 1)
								   (r-call stop
  "'length.out' must be of length 1"))
							       (<- length.out
								   (r-call
								    ceiling
								    length.out)))))
					      (<- status (r-call c (r-call ! (missing
  to))
								 (r-call ! (missing
  by))
								 (r-call ! (r-call
  is.null length.out))))
					      (if (r-call != (r-call sum status)
							  2)
						  (r-call stop "exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified"))
					      (if (missing by)
						  (r-block (<- from (r-call
  unclass cfrom))
							   (<- to (r-call
								   unclass (r-call
  as.POSIXct to)))
							   (<- res (r-call
								    seq.int
								    from to (*named*
  length.out length.out)))
							   (return (r-call
								    structure
								    res (*named*
  class (r-call c "POSIXt" "POSIXct"))
								    (*named*
  tzone tz)))))
					      (if (r-call != (r-call length by)
							  1)
						  (r-call stop "'by' must be of length 1"))
					      (<- valid 0)
					      (if (r-call inherits by "difftime")
						  (r-block (<- by (r-call * (switch
  (r-call attr by "units") (*named* secs 1)
  (*named* mins 60) (*named* hours 3600) (*named* days 86400)
  (*named* weeks (r-call * 7 86400)))
  (r-call unclass by))))
						  (if (r-call is.character by)
						      (r-block (<- by2 (r-call
  r-aref (r-call strsplit by " "
		 (*named* fixed *r-true*))
  1))
							       (if (|\|\|| (r-call
  > (r-call length by2) 2)
  (r-call < (r-call length by2) 1))
								   (r-call stop
  "invalid 'by' string"))
							       (<- valid (r-call
  pmatch (r-call r-index by2
		 (r-call length by2))
  (r-call c "secs" "mins" "hours" "days" "weeks" "months" "years" "DSTdays")))
							       (if (r-call
								    is.na valid)
								   (r-call stop
  "invalid string for 'by'"))
							       (if (r-call <=
  valid 5)
								   (r-block (<-
  by (r-call r-index (r-call c 1 60 3600 86400
			     (r-call * 7 86400))
	     valid))
  (if (r-call == (r-call length by2) 2) (<- by (r-call * by
						       (r-call as.integer (r-call
  r-index by2 1))))))
								   (<- by (if
  (r-call == (r-call length by2) 2) (r-call as.integer (r-call r-index by2 1))
  1))))
						      (if (r-call ! (r-call
  is.numeric by))
							  (r-call stop "invalid mode for 'by'"))))
					      (if (r-call is.na by)
						  (r-call stop "'by' is NA"))
					      (if (r-call <= valid 5)
						  (r-block (<- from (r-call
  unclass (r-call as.POSIXct from)))
							   (if (r-call ! (r-call
  is.null length.out))
							       (<- res (r-call
  seq.int from (*named* by by)
  (*named* length.out length.out)))
							       (r-block (<- to
  (r-call unclass (r-call as.POSIXct to)))
  (<- res (r-call + (r-call seq.int 0
			    (r-call - to from) by)
		  from))))
							   (return (r-call
								    structure
								    res (*named*
  class (r-call c "POSIXt" "POSIXct"))
								    (*named*
  tzone tz))))
						  (r-block (<- r1 (r-call
								   as.POSIXlt
								   from))
							   (if (r-call == valid
  7)
							       (r-block (if (missing
  to)
  (r-block (<- yr (r-call seq.int (r-call r-aref r1
					  (index-in-strlist year (r-call attr
  r1 #0#)))
			  (*named* by by)
			  (*named* length length.out))))
  (r-block (<- to (r-call as.POSIXlt to))
	   (<- yr (r-call seq.int (r-call r-aref r1
					  (index-in-strlist year (r-call attr
  r1 #0#)))
			  (r-call r-aref to
				  (index-in-strlist year (r-call attr to #0#)))
			  by))))
  (r-block (<- r1 (r-call r-aref<- r1
			  (index-in-strlist year (r-call attr r1 #0#)) yr))
	   yr)
  (r-block (ref= %r:9 (r-call - 1)) (<- r1 (r-call r-aref<- r1
						   (index-in-strlist isdst (r-call
  attr r1 #0#))
						   %r:9))
	   %r:9)
  (<- res (r-call as.POSIXct r1)))
							       (if (r-call ==
  valid 6)
								   (r-block (if
  (missing to) (r-block (<- mon (r-call seq.int (r-call r-aref r1
							(index-in-strlist mon
  (r-call attr r1 #0#)))
					(*named* by by)
					(*named* length length.out))))
  (r-block (<- to (r-call as.POSIXlt to))
	   (<- mon (r-call seq.int (r-call r-aref r1
					   (index-in-strlist mon (r-call attr
  r1 #0#)))
			   (r-call + (r-call * 12
					     (r-call - (r-call r-aref to
							       (index-in-strlist
								year (r-call
  attr to #0#)))
						     (r-call r-aref r1
							     (index-in-strlist
							      year (r-call attr
  r1 #0#)))))
				   (r-call r-aref to
					   (index-in-strlist mon (r-call attr
  to #0#))))
			   by))))
  (r-block (<- r1 (r-call r-aref<- r1
			  (index-in-strlist mon (r-call attr r1 #0#)) mon))
	   mon)
  (r-block (ref= %r:10 (r-call - 1)) (<- r1 (r-call r-aref<- r1
						    (index-in-strlist isdst (r-call
  attr r1 #0#))
						    %r:10))
	   %r:10)
  (<- res (r-call as.POSIXct r1)))
								   (if (r-call
  == valid 8)
  (r-block (if (r-call ! (missing to)) (r-block (<- length.out (r-call + 2
  (r-call floor (r-call / (r-call - (r-call unclass (r-call as.POSIXct to))
				  (r-call unclass (r-call as.POSIXct from)))
			86400))))))
	   (r-block (ref= %r:11 (r-call seq.int (r-call r-aref r1
							(index-in-strlist mday
  (r-call attr r1 #0#)))
					(*named* by by)
					(*named* length length.out)))
		    (<- r1 (r-call r-aref<- r1
				   (index-in-strlist mday (r-call attr r1 #0#))
				   %r:11))
		    %r:11)
	   (r-block (ref= %r:12 (r-call - 1))
		    (<- r1 (r-call r-aref<- r1
				   (index-in-strlist isdst (r-call attr r1 #0#))
				   %r:12))
		    %r:12)
	   (<- res (r-call as.POSIXct r1))
	   (if (r-call ! (missing to)) (<- res (r-call r-index res
						       (r-call <= res
							       (r-call
								as.POSIXct to)))))))))
							   (return res)))))))
	       (<- cut.POSIXt (lambda (x breaks labels start.on.monday right
					 ...)
				(let ((res ())
				      (maxx ())
				      (incr ())
				      (start ())
				      (valid ())
				      (by2 ())
				      (breaks ())
				      (x ())
				      (right ())
				      (start.on.monday ())
				      (labels ()))
				     (r-block (when (missing labels)
						    (<- labels ()))
					      (when (missing start.on.monday)
						    (<- start.on.monday
							*r-true*))
					      (when (missing right)
						    (<- right *r-false*))
					      (if (r-call ! (r-call inherits x
								    "POSIXt"))
						  (r-call stop "'x' must be a date-time object"))
					      (<- x (r-call as.POSIXct x))
					      (if (r-call inherits breaks "POSIXt")
						  (r-block (<- breaks (r-call
  as.POSIXct breaks)))
						  (if (&& (r-call is.numeric
								  breaks)
							  (r-call == (r-call
  length breaks)
								  1))
						      (r-block)
						      (if (&& (r-call
							       is.character
							       breaks)
							      (r-call == (r-call
  length breaks)
  1))
							  (r-block (<- by2 (r-call
  r-aref (r-call strsplit breaks " "
		 (*named* fixed *r-true*))
  1))
								   (if (|\|\||
  (r-call > (r-call length by2) 2) (r-call < (r-call length by2) 1))
  (r-call stop "invalid specification of 'breaks'"))
								   (<- valid (r-call
  pmatch (r-call r-index by2
		 (r-call length by2))
  (r-call c "secs" "mins" "hours" "days" "weeks" "months" "years" "DSTdays")))
								   (if (r-call
  is.na valid)
  (r-call stop "invalid specification of 'breaks'"))
								   (<- start (r-call
  as.POSIXlt (r-call min x
		     (*named* na.rm *r-true*))))
								   (<- incr 1)
								   (if (r-call
  > valid 1)
  (r-block (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist sec (r-call attr start
								    #0#))
				      0))
		    0)
	   (<- incr 59.990000000000002)))
								   (if (r-call
  > valid 2)
  (r-block (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist min (r-call attr start
								    #0#))
				      0))
		    0)
	   (<- incr (r-call - 3600 1))))
								   (if (r-call
  > valid 3)
  (r-block (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist hour (r-call attr start
  #0#))
				      0))
		    0)
	   (<- incr (r-call - 86400 1))))
								   (if (r-call
  == valid 5)
  (r-block (r-block (ref= %r:13 (r-call - (r-call r-aref start
						  (index-in-strlist mday (r-call
  attr start #0#)))
					(r-call r-aref start
						(index-in-strlist wday (r-call
  attr start #0#)))))
		    (<- start (r-call r-aref<- start
				      (index-in-strlist mday (r-call attr start
  #0#))
				      %r:13))
		    %r:13)
	   (if start.on.monday (r-block (ref= %r:14 (r-call + (r-call r-aref
  start (index-in-strlist mday (r-call attr start #0#)))
							    (r-call ifelse (r-call
  > (r-call r-aref start
	    (index-in-strlist wday (r-call attr start #0#)))
  0)
								    1 (r-call
  - 6))))
					(<- start (r-call r-aref<- start
							  (index-in-strlist
							   mday (r-call attr
  start #0#))
							  %r:14))
					%r:14))
	   (<- incr (r-call * 7 86400))))
								   (if (r-call
  == valid 6)
  (r-block (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist mday (r-call attr start
  #0#))
				      1))
		    1)
	   (<- incr (r-call * 31 86400))))
								   (if (r-call
  == valid 7)
  (r-block (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist mon (r-call attr start
								    #0#))
				      0))
		    0)
	   (r-block (<- start (r-call r-aref<- start
				      (index-in-strlist mday (r-call attr start
  #0#))
				      1))
		    1)
	   (<- incr (r-call * 366 86400))))
								   (if (r-call
  == valid 8)
  (<- incr (r-call * 25 3600)))
								   (if (r-call
  == (r-call length by2) 2)
  (<- incr (r-call * incr
		   (r-call as.integer (r-call r-index by2 1)))))
								   (<- maxx (r-call
  max x (*named* na.rm *r-true*)))
								   (<- breaks
  (r-call seq.int start
	  (r-call + maxx incr) breaks))
								   (<- breaks
  (r-call r-index breaks
	  (r-call : 1
		  (r-call + 1
			  (r-call max (r-call which (r-call < breaks maxx))))))))
							  (r-call stop "invalid specification of 'breaks'"))))
					      (<- res (r-call cut (r-call
								   unclass x)
							      (r-call unclass
  breaks)
							      (*named* labels
  labels)
							      (*named* right
  right)
							      r-dotdotdot))
					      (if (r-call is.null labels)
						  (r-block (ref= %r:15 (r-call
  as.character (r-call r-index breaks
		       (r-call - (r-call length breaks)))))
							   (<- res (r-call
								    levels<-
								    res %r:15))
							   %r:15))
					      res))))
	       (<- julian (lambda (x ...)
			    (let () (r-block (r-call UseMethod "julian")))))
	       (<- julian.POSIXt (lambda (x origin ...)
				   (let ((res ())
					 (origin ()))
					(r-block (when (missing origin)
						       (<- origin (r-call
								   as.POSIXct
								   "1970-01-01"
								   (*named* tz
  "GMT"))))
						 (if (r-call != (r-call length
  origin)
							     1)
						     (r-call stop "'origin' must be of length one"))
						 (<- res (r-call difftime (r-call
  as.POSIXct x)
								 origin (*named*
  units "days")))
						 (r-call structure res
							 (*named* origin origin))))))
	       (<- weekdays (lambda (x abbreviate)
			      (let () (r-block (r-call UseMethod "weekdays")))))
	       (<- weekdays.POSIXt (lambda (x abbreviate)
				     (let ((abbreviate ()))
					  (r-block (when (missing abbreviate)
							 (<- abbreviate
							     *r-false*))
						   (r-call format x
							   (r-call ifelse
								   abbreviate
								   "%a" "%A"))))))
	       (<- months (lambda (x abbreviate)
			    (let () (r-block (r-call UseMethod "months")))))
	       (<- months.POSIXt (lambda (x abbreviate)
				   (let ((abbreviate ()))
					(r-block (when (missing abbreviate)
						       (<- abbreviate *r-false*))
						 (r-call format x
							 (r-call ifelse
								 abbreviate "%b"
								 "%B"))))))
	       (<- quarters (lambda (x abbreviate)
			      (let () (r-block (r-call UseMethod "quarters")))))
	       (<- quarters.POSIXt (lambda (x ...)
				     (let ((x ()))
					  (r-block (<- x (r-call %/% (r-block
  (ref= %r:0 (r-call as.POSIXlt x)) (r-call r-aref %r:0
					    (index-in-strlist mon (r-call attr
  %r:0 #0#))))
								 3))
						   (r-call paste "Q"
							   (r-call + x 1)
							   (*named* sep ""))))))
	       (<- trunc.POSIXt (lambda (x units)
				  (let ((x ())
					(units ()))
				       (r-block (when (missing units)
						      (<- units (r-call c "secs"
  "mins" "hours" "days")))
						(<- units (r-call match.arg
								  units))
						(<- x (r-call as.POSIXlt x))
						(if (r-call > (r-call length (r-call
  r-aref x (index-in-strlist sec (r-call attr x #0#))))
							    0)
						    (switch units (*named* secs
  (r-block (r-block (ref= %r:16 (r-call trunc (r-call r-aref x
						      (index-in-strlist sec (r-call
  attr x #0#)))))
		    (<- x (r-call r-aref<- x
				  (index-in-strlist sec (r-call attr x #0#))
				  %r:16))
		    %r:16)))
							    (*named* mins (r-block
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist sec (r-call attr x #0#)) 0))
	   0)))
							    (*named* hours (r-block
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist sec (r-call attr x #0#)) 0))
	   0)
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist min (r-call attr x #0#)) 0))
	   0)))
							    (*named* days (r-block
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist sec (r-call attr x #0#)) 0))
	   0)
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist min (r-call attr x #0#)) 0))
	   0)
  (r-block (<- x (r-call r-aref<- x
			 (index-in-strlist hour (r-call attr x #0#)) 0))
	   0)
  (r-block (ref= %r:17 (r-call - 1)) (<- x (r-call r-aref<- x
						   (index-in-strlist isdst (r-call
  attr x #0#))
						   %r:17))
	   %r:17)))))
						x))))
	       (<- round.POSIXt (lambda (x units)
				  (let ((x ())
					(units ()))
				       (r-block (when (missing units)
						      (<- units (r-call c "secs"
  "mins" "hours" "days")))
						(if (&& (r-call is.numeric
								units)
							(r-call == units 0))
						    (<- units "secs"))
						(<- units (r-call match.arg
								  units))
						(<- x (r-call as.POSIXct x))
						(<- x (r-call + x
							      (switch units (*named*
  secs 0.5)
  (*named* mins 30) (*named* hours 1800) (*named* days 43200))))
						(r-call trunc.POSIXt x
							(*named* units units))))))
	       (<- "[.POSIXlt" (lambda (x ... drop)
				 (let ((val ())
				       (drop ()))
				      (r-block (when (missing drop)
						     (<- drop *r-true*))
					       (<- val (r-call lapply x "["
							       r-dotdotdot (*named*
  drop drop)))
					       (r-block (ref= %r:18 (r-call
  attributes x))
							(<- val (r-call
								 attributes<-
								 val %r:18))
							%r:18)
					       val))))
	       (<- "[<-.POSIXlt" (lambda (x i value)
				   (let ((x ())
					 (cl ())
					 (value ()))
					(r-block (if (r-call ! (r-call
								as.logical (r-call
  length value)))
						     (return x))
						 (<- value (r-call as.POSIXlt
								   value))
						 (<- cl (r-call oldClass x))
						 (r-block (ref= %r:19 (r-block
  (<- value (r-call class<- value
		    ()))
  ()))
							  (<- x (r-call class<-
  x %r:19))
							  %r:19)
						 (for n (r-call names x)
						   (r-block (ref= %r:20 (r-call
  r-aref value n))
							    (r-block (ref=
  %r:21 (r-call r-index<- (r-call r-aref x n) i %r:20))
  (<- x (r-call r-aref<- x n %r:21)) %r:21)
							    %r:20))
						 (r-block (<- x (r-call class<-
  x cl))
							  cl)
						 x))))
	       (<- as.data.frame.POSIXlt (lambda (x row.names optional ...)
					   (let ((value ())
						 (optional ())
						 (row.names ()))
						(r-block (when (missing
								row.names)
							       (<- row.names ()))
							 (when (missing
								optional)
							       (<- optional
								   *r-false*))
							 (<- value (r-call
								    as.data.frame.POSIXct
								    (r-call
  as.POSIXct x)
								    row.names
								    optional
								    r-dotdotdot))
							 (if (r-call ! optional)
							     (r-block (ref=
  %r:22 (r-call r-aref (r-call deparse (substitute x)) 1))
  (<- value (r-call names<- value %r:22)) %r:22))
							 value))))
	       (<- rep.POSIXct (lambda (x ...)
				 (let ((y ()))
				      (r-block (<- y (r-call NextMethod))
					       (r-call structure y
						       (*named* class (r-call
  c "POSIXt" "POSIXct"))
						       (*named* tzone (r-call
  attr x "tzone")))))))
	       (<- rep.POSIXlt (lambda (x ...)
				 (let ((y ()))
				      (r-block (<- y (r-call lapply x rep
							     r-dotdotdot))
					       (r-block (ref= %r:23 (r-call
  attributes x))
							(<- y (r-call
							       attributes<- y
							       %r:23))
							%r:23)
					       y))))
	       (<- diff.POSIXt (lambda (x lag differences ...)
				 (let ((i1 ())
				       (xlen ())
				       (r ())
				       (ismat ())
				       (differences ())
				       (lag ()))
				      (r-block (when (missing lag)
						     (<- lag 1))
					       (when (missing differences)
						     (<- differences 1))
					       (<- ismat (r-call is.matrix x))
					       (<- r (if (r-call inherits x "POSIXlt")
							 (r-call as.POSIXct x)
							 x))
					       (<- xlen (if ismat (r-call
								   r-index (r-call
  dim x)
								   1)
							    (r-call length r)))
					       (if (|\|\|| (r-call > (r-call
  length lag)
								   1)
							   (r-call > (r-call
  length differences)
								   1)
							   (r-call < lag 1)
							   (r-call <
								   differences
								   1))
						   (r-call stop "'lag' and 'differences' must be integers >= 1"))
					       (if (r-call >= (r-call * lag
  differences)
							   xlen)
						   (return (r-call structure (r-call
  numeric 0)
								   (*named*
								    class "difftime")
								   (*named*
								    units "secs"))))
					       (<- i1 (r-call : (r-call - 1)
							      (r-call - lag)))
					       (if ismat (for i (r-call : 1
  differences)
							   (<- r (r-call - (r-call
  r-index r i1 *r-missing*
  (*named* drop *r-false*))
  (r-call r-index r
	  (r-call : (r-call - (r-call nrow r))
		  (r-call - (r-call + (r-call - (r-call nrow r) lag) 1)))
	  *r-missing* (*named* drop *r-false*)))))
						   (for i (r-call : 1
								  differences)
						     (<- r (r-call - (r-call
  r-index r i1)
								   (r-call
								    r-index r
								    (r-call :
  (r-call - (r-call length r)) (r-call - (r-call + (r-call - (r-call length r)
							   lag)
						 1))))))))
					       r))))
	       (<- duplicated.POSIXlt (lambda (x incomparables ...)
					(let ((x ())
					      (incomparables ()))
					     (r-block (when (missing
							     incomparables)
							    (<- incomparables
								*r-false*))
						      (<- x (r-call as.POSIXct
								    x))
						      (r-call NextMethod "duplicated"
							      x)))))
	       (<- unique.POSIXlt (lambda (x incomparables ...)
				    (let ((incomparables ()))
					 (r-block (when (missing incomparables)
							(<- incomparables
							    *r-false*))
						  (r-call r-index x
							  (r-call ! (r-call
  duplicated x incomparables r-dotdotdot)))))))
	       (<- sort.POSIXlt (lambda (x decreasing na.last ...)
				  (let ((na.last ())
					(decreasing ()))
				       (r-block (when (missing decreasing)
						      (<- decreasing *r-false*))
						(when (missing na.last)
						      (<- na.last NA))
						(r-call r-index x
							(r-call order (r-call
  as.POSIXct x)
								(*named*
								 na.last
								 na.last)
								(*named*
								 decreasing
								 decreasing))))))))
