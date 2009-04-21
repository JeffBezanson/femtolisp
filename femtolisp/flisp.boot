zero?
#function(#array(uint8 62 1 53 0 47 38 11) [])
vector.map
#function(#array(uint8 62 2 50 0 52 1 53 1 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 47 54 0 0 48 35 2 50 0 60 66 2 53 0 11) [#function(#array(uint8 62 1 54 0 0 53 0 54 2 0 54 2 1 53 0 42 3 1 43 11) [])])
  vector.alloc]) length])
vector->list
#function(#array(uint8 62 1 50 0 52 1 53 0 3 1 46 64 4 3 11) [#function(#array(uint8 65 3 48 53 0 50 0 60 66 2 53 1 11) [#function(#array(uint8 62 1 54 1 0 54 0 0 53 0 35 2 42 54 0 1 27 58 0 1 11) [])])
							      length])
untrace
#function(#array(uint8 62 1 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 53 0 29 50 0 13 6 37 0 52 1 54 0 0 52 2 52 3 52 4 52 5 53 0 3 1 3 1 3 1 3 1 4 2 11 45 11) [trace-lambda
  set-top-level-value! cadr caar last-pair caddr]) top-level-value])
transpose
#function(#array(uint8 62 1 52 0 52 1 53 0 27 12 11) [mapcar list])
trace
#function(#array(uint8 62 1 50 0 52 1 53 0 3 1 64 3 2 2 50 2 11) [#function(#array(uint8 65 2 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 54 1 0 29 50 0 13 17 6 99 0 52 1 54 2 0 50 0 54 0 0 52 2 50 3 28 1 50 4 50 5 28 2 28 1 50 6 50 7 54 2 0 28 2 28 2 28 1 52 8 52 9 50 10 60 53 0 3 2 3 1 50 4 50 11 28 2 28 1 52 2 50 7 54 1 0 28 2 28 1 52 8 53 0 3 1 3 2 28 1 3 6 28 3 4 2 11 45 11) [trace-lambda
  set-top-level-value! nconc begin princ "(" print quote copy-list map #function(#array(uint8 62 1 50 0 50 1 50 2 28 2 50 3 53 0 28 2 28 3 11) [begin
  princ " " print]) ")\n"]) to-proper]) cadr]) top-level-value ok])
to-proper
#function(#array(uint8 62 1 53 0 18 6 11 0 53 0 11 53 0 16 6 22 0 53 0 28 1 11 53 0 29 52 0 53 0 30 3 1 27 11) [to-proper])
table.values
#function(#array(uint8 62 1 52 0 50 1 60 46 53 0 4 3 11) [table.foldl #function(#array(uint8 62 3 53 1 53 2 27 11) [])])
table.foreach
#function(#array(uint8 62 2 52 0 50 1 60 46 53 1 4 3 11) [table.foldl #function(#array(uint8 62 3 54 0 0 53 0 53 1 3 2 2 44 11) [])])
table.invert
#function(#array(uint8 62 1 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 52 0 50 1 60 46 54 0 0 3 3 2 53 0 11) [table.foldl
  #function(#array(uint8 62 3 52 0 54 0 0 53 1 53 0 4 3 11) [put!])]) table])
table.keys
#function(#array(uint8 62 1 52 0 50 1 60 46 53 0 4 3 11) [table.foldl #function(#array(uint8 62 3 53 0 53 2 27 11) [])])
table.pairs
#function(#array(uint8 62 1 52 0 50 1 60 46 53 0 4 3 11) [table.foldl #function(#array(uint8 62 3 53 0 53 1 27 53 2 27 11) [])])
table.clone
#function(#array(uint8 62 1 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 52 0 50 1 60 46 54 0 0 3 3 2 53 0 11) [table.foldl
  #function(#array(uint8 62 3 52 0 54 0 0 53 0 53 1 4 3 11) [put!])]) table])
symbol-syntax
#function(#array(uint8 62 1 52 0 52 1 53 0 45 4 3 11) [get *syntax-environment*])
string.trim
#function(#array(uint8 62 3 50 0 45 45 64 4 3 11) [#function(#array(uint8 65 3 50 0 60 57 0 2 50 1 60 57 1 2 50 2 52 3 54 0 0 3 1 64 4 2 11) [#function(#array(uint8 62 4 53 2 53 3 39 1 6 26 0 2 52 0 53 1 52 1 53 0 53 2 3 2 3 2 6 49 0 54 0 0 53 0 53 1 52 2 53 0 53 2 3 2 53 3 4 4 11 53 2 11) [string.find
  string.char string.inc]) #function(#array(uint8 62 3 52 0 53 2 47 3 2 1 6 34 0 2 52 1 53 1 52 2 53 0 52 3 53 0 53 2 3 2 3 2 3 2 6 55 0 54 0 1 53 0 53 1 52 3 53 0 53 2 3 2 4 3 11 53 2 11) [>
  string.find string.char string.dec]) #function(#array(uint8 65 2 52 0 54 1 0 54 0 0 54 1 0 54 1 1 47 53 0 3 4 54 0 1 54 1 0 54 1 2 53 0 3 3 4 3 11) [string.sub])
  length])])
string.tail
#function(#array(uint8 62 2 52 0 53 0 52 1 53 0 47 53 1 3 3 52 2 53 0 3 1 4 3 11) [string.sub
  string.inc sizeof])
string.rep
#function(#array(uint8 62 2 53 1 49 4 39 6 65 0 52 0 53 1 47 3 2 6 23 0 50 1 11 53 1 48 38 6 37 0 52 2 53 0 4 1 11 53 1 49 2 38 6 54 0 52 2 53 0 53 0 4 2 11 52 2 53 0 53 0 53 0 4 3 11 52 3 53 1 3 1 6 92 0 52 2 53 0 52 4 53 0 53 1 48 35 2 3 2 4 2 11 52 4 52 2 53 0 53 0 3 2 53 1 49 2 37 2 4 2 11) [<=
  "" string odd? string.rep])
string.join
#function(#array(uint8 62 2 53 0 18 6 11 0 50 0 11 50 1 52 2 3 0 64 4 2 11) [""
  #function(#array(uint8 65 2 52 0 53 0 54 0 0 29 3 2 2 52 1 50 2 60 54 0 0 30 3 2 2 52 3 53 0 4 1 11) [io.write
  for-each #function(#array(uint8 62 1 52 0 54 0 0 54 1 1 3 2 2 52 0 54 0 0 53 0 4 2 11) [io.write])
  io.tostring!]) buffer])
string.map
#function(#array(uint8 62 2 50 0 52 1 3 0 52 2 53 1 3 1 64 4 3 11) [#function(#array(uint8 65 3 50 0 47 64 3 2 2 52 1 53 0 4 1 11) [#function(#array(uint8 65 2 45 53 0 54 0 1 39 6 49 0 2 52 0 54 0 0 54 1 0 52 1 54 1 1 53 0 3 2 3 1 3 2 2 52 2 54 1 1 53 0 3 2 57 0 5 3 0 11) [io.putc
  string.char string.inc]) io.tostring!]) buffer length])
splice-form?
#function(#array(uint8 62 1 53 0 23 1 6 27 0 2 53 0 29 50 0 13 1 7 27 0 2 53 0 29 50 1 13 1 7 37 0 2 53 0 50 2 13 11) [*comma-at*
  *comma-dot* *comma*])
set-syntax!
#function(#array(uint8 62 2 52 0 52 1 53 0 53 1 4 3 11) [put!
							 *syntax-environment*])
self-evaluating?
#function(#array(uint8 62 1 53 0 16 1 6 14 0 2 53 0 20 17 1 7 47 0 2 52 0 53 0 3 1 1 6 47 0 2 53 0 20 1 6 47 0 2 53 0 52 1 53 0 3 1 13 11) [constant?
  top-level-value])
repl
#function(#array(uint8 62 0 50 0 45 45 64 4 3 11) [#function(#array(uint8 65 3 50 0 60 57 0 2 50 1 60 57 1 2 53 1 3 0 2 52 2 4 0 11) [#function(#array(uint8 62 0 52 0 50 1 3 1 2 52 2 52 3 3 1 2 50 4 50 5 60 50 6 60 61 64 4 2 11) [princ
  "> " io.flush *output-stream* #function(#array(uint8 65 2 52 0 52 1 3 1 17 1 6 25 0 2 50 2 52 3 53 0 3 1 64 4 2 11) [io.eof?
  *input-stream* #function(#array(uint8 65 2 52 0 53 0 3 1 2 53 0 56 1 2 44 11) [print
  that]) load-process]) #function(#array(uint8 62 0 52 0 4 0 11) [read])
  #function(#array(uint8 62 1 52 0 52 1 3 1 2 52 2 53 0 4 1 11) [io.discardbuffer
								 *input-stream*
								 raise])])
  #function(#array(uint8 62 0 50 0 60 50 1 60 61 6 23 0 52 2 3 0 2 54 0 1 4 0 11 45 11) [#function(#array(uint8 62 0 54 0 0 3 0 1 6 16 0 2 52 0 4 0 11) [newline])
  #function(#array(uint8 62 1 52 0 53 0 4 1 11) [print-exception]) newline])
  newline])])
ref-uint16-LE
#function(#array(uint8 62 2 52 0 53 0 53 1 47 34 2 42 47 3 2 52 0 53 0 53 1 48 34 2 42 49 8 3 2 34 2 11) [ash])
ref-uint32-LE
#function(#array(uint8 62 2 52 0 53 0 53 1 47 34 2 42 47 3 2 52 0 53 0 53 1 48 34 2 42 49 8 3 2 52 0 53 0 53 1 49 2 34 2 42 49 16 3 2 52 0 53 0 53 1 49 3 34 2 42 49 24 3 2 34 4 11) [ash])
remainder
#function(#array(uint8 62 2 53 0 53 0 53 1 37 2 53 1 36 2 35 2 11) [])
revappend
#function(#array(uint8 62 2 52 0 52 1 53 0 3 1 53 1 4 2 11) [nconc reverse])
reverse
#function(#array(uint8 62 1 52 0 52 1 46 53 0 4 3 11) [foldl cons])
separate
#function(#array(uint8 62 2 54 0 0 53 0 53 1 46 46 4 4 11) [] #0=[#function(#array(uint8 62 4 53 1 18 6 14 0 53 2 53 3 27 11 53 0 53 1 29 3 1 6 43 0 54 0 0 53 0 53 1 30 53 1 29 53 2 27 53 3 4 4 11 44 6 66 0 54 0 0 53 0 53 1 30 53 2 53 1 29 53 3 27 4 4 11 45 11) [] #0#)
								  ()])
quote-value
#function(#array(uint8 62 1 52 0 53 0 3 1 6 14 0 53 0 11 50 1 53 0 28 2 11) [self-evaluating?
  quote])
print-to-string
#function(#array(uint8 62 1 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 52 0 53 0 54 0 0 3 2 2 52 1 53 0 4 1 11) [io.print
  io.tostring!]) buffer])
println
#function(#array(uint8 63 0 52 0 53 0 33 52 1 3 0 2 11) [print newline])
procedure?
#function(#array(uint8 62 1 53 0 24 1 7 38 0 2 52 0 53 0 3 1 50 1 13 1 7 38 0 2 53 0 23 1 6 38 0 2 53 0 29 50 2 13 11) [typeof
  function lambda])
print-exception
#function(#array(uint8 62 1 53 0 23 1 6 29 0 2 53 0 29 50 0 13 1 6 29 0 2 52 1 53 0 49 4 3 2 6 72 0 52 2 52 3 50 4 52 5 53 0 3 1 50 6 52 7 53 0 3 1 50 8 3 6 2 52 9 52 3 52 10 53 0 3 1 3 2 5 9 1 53 0 23 1 6 95 0 2 53 0 29 50 11 13 1 6 95 0 2 53 0 30 23 6 117 0 52 2 52 3 50 12 52 5 53 0 3 1 50 13 3 4 5 9 1 53 0 23 1 6 131 0 2 53 0 29 50 14 13 6 155 0 52 2 52 3 50 15 3 2 2 52 2 52 3 53 0 30 27 33 5 9 1 53 0 23 1 6 169 0 2 53 0 29 50 16 13 6 200 0 52 17 52 7 53 0 3 1 3 1 2 52 2 52 3 50 18 52 5 53 0 3 1 3 3 5 9 1 52 19 53 0 3 1 1 6 219 0 2 52 1 53 0 49 2 3 2 6 248 0 52 2 52 3 53 0 29 50 20 3 3 2 50 21 52 5 53 0 3 1 64 3 2 5 9 1 52 2 52 3 50 22 3 2 2 52 9 52 3 53 0 3 2 2 52 2 52 3 52 23 3 2 2 44 11) [type-error
  length= io.princ *stderr* "type-error: " cadr ": expected " caddr ", got "
  io.print cadddr unbound-error "unbound-error: eval: variable " " has no value"
  error "error: " load-error print-exception "in file " list? ": " #function(#array(uint8 65 2 52 0 53 0 3 1 1 7 16 0 2 53 0 20 6 24 0 52 1 5 26 0 52 2 52 3 53 0 4 2 11) [string?
  io.princ io.print *stderr*]) "*** Unhandled exception: " *linefeed*])
print
#function(#array(uint8 63 0 52 0 52 1 53 0 27 12 11) [io.print *output-stream*])
positive?
#function(#array(uint8 62 1 52 0 53 0 47 4 2 11) [>])
princ
#function(#array(uint8 63 0 52 0 52 1 53 0 27 12 11) [io.princ *output-stream*])
peephole
#function(#array(uint8 62 1 53 0 11) [])
pad-r
#function(#array(uint8 62 3 52 0 53 0 52 1 53 2 53 1 52 2 53 0 3 1 35 2 3 2 4 2 11) [string
  string.rep length])
pad-l
#function(#array(uint8 62 3 52 0 52 1 53 2 53 1 52 2 53 0 3 1 35 2 3 2 53 0 4 2 11) [string
  string.rep length])
odd?
#function(#array(uint8 62 1 52 0 53 0 3 1 17 11) [even?])
nreconc
#function(#array(uint8 62 2 52 0 52 1 53 0 3 1 53 1 4 2 11) [nconc nreverse])
nreverse
#function(#array(uint8 62 1 50 0 46 64 4 2 11) [#function(#array(uint8 65 2 45 54 0 0 23 6 34 0 2 54 0 0 30 54 0 0 53 0 54 0 0 57 0 2 32 2 58 0 0 5 3 0 2 53 0 11) [])])
nlist*
#function(#array(uint8 63 0 53 0 30 16 6 13 0 53 0 29 11 53 0 52 0 53 0 30 33 32 11) [nlist*])
nestlist
#function(#array(uint8 62 3 52 0 53 2 47 3 2 6 14 0 46 11 53 1 52 1 53 0 53 0 53 1 3 1 53 2 48 35 2 3 3 27 11) [<=
  nestlist])
negative?
#function(#array(uint8 62 1 53 0 47 39 11) [])
newline
#function(#array(uint8 62 0 52 0 52 1 3 1 2 44 11) [princ *linefeed*])
mod
#function(#array(uint8 62 2 53 0 53 0 53 1 37 2 53 1 36 2 35 2 11) [])
memv
#function(#array(uint8 62 2 53 1 16 6 10 0 45 11 53 1 29 53 0 14 6 22 0 53 1 11 44 6 36 0 52 0 53 0 53 1 30 4 2 11 45 11) [memv])
mark-label
#function(#array(uint8 62 2 52 0 53 0 52 1 53 1 4 3 11) [emit :label])
member
#function(#array(uint8 62 2 53 1 16 6 10 0 45 11 53 1 29 53 0 15 6 22 0 53 1 11 44 6 36 0 52 0 53 0 53 1 30 4 2 11 45 11) [member])
map-int
#function(#array(uint8 62 2 52 0 53 1 47 3 2 6 14 0 46 11 50 1 53 0 47 3 1 46 27 46 64 4 3 11) [<=
  #function(#array(uint8 65 3 53 0 57 1 2 48 54 0 1 48 35 2 50 0 60 66 2 53 0 11) [#function(#array(uint8 62 1 54 0 1 54 1 0 53 0 3 1 46 27 32 2 54 0 1 30 58 0 1 11) [])])])
map!
#function(#array(uint8 62 2 53 1 45 53 1 23 6 31 0 2 53 1 53 0 53 1 29 3 1 31 2 53 1 30 57 1 5 5 0 2 11) [])
make-label
#function(#array(uint8 62 1 52 0 4 0 11) [gensym])
make-code-emitter
#function(#array(uint8 62 0 46 52 0 3 0 47 41 3 11) [table])
macroexpand-in
#function(#array(uint8 62 2 53 0 16 6 11 0 53 0 11 50 0 52 1 53 0 29 53 1 3 2 64 4 2 11) [#function(#array(uint8 65 2 53 0 6 29 0 52 0 52 1 53 0 3 1 54 0 0 30 33 52 2 53 0 3 1 4 2 11 50 3 52 4 54 0 0 3 1 64 4 2 11) [macroexpand-in
  cadr caddr #function(#array(uint8 65 2 53 0 6 22 0 52 0 53 0 54 1 0 30 33 54 1 1 4 2 11 54 1 0 29 50 1 13 6 36 0 54 1 0 11 54 1 0 29 50 2 13 6 81 0 52 3 50 2 52 4 54 1 0 3 1 52 0 52 5 54 1 0 3 1 54 1 1 3 2 52 6 54 1 0 3 1 4 4 11 54 1 0 29 50 7 13 6 115 0 50 8 52 4 54 1 0 3 1 52 9 52 10 54 1 0 3 1 3 1 64 4 3 11 52 11 50 12 60 54 1 0 4 2 11) [macroexpand-in
  quote lambda nlist* cadr caddr cdddr let-syntax #function(#array(uint8 65 3 52 0 53 1 52 1 52 2 50 3 60 53 0 3 2 54 2 1 3 2 4 2 11) [macroexpand-in
  nconc map #function(#array(uint8 62 1 53 0 29 52 0 52 1 53 0 3 1 54 3 1 3 2 54 3 1 28 3 11) [macroexpand-in
  cadr])]) f-body cddr map #function(#array(uint8 62 1 52 0 53 0 54 2 1 4 2 11) [macroexpand-in])])
  macrocall?]) assq])
macroexpand
#function(#array(uint8 62 1 52 0 53 0 46 4 2 11) [macroexpand-in])
lookup-sym
#function(#array(uint8 62 4 53 1 18 6 11 0 50 0 11 50 1 53 1 29 64 4 2 11) [(global)
  #function(#array(uint8 65 2 50 0 52 1 54 0 0 53 0 47 3 3 64 4 2 11) [#function(#array(uint8 65 2 53 0 6 30 0 54 1 3 6 20 0 50 0 53 0 28 2 11 50 1 54 1 2 53 0 28 3 11 52 2 54 1 0 54 1 1 30 54 1 3 1 7 51 0 2 54 0 0 18 6 60 0 54 1 2 5 66 0 54 1 2 48 34 2 45 4 4 11) [arg
  closed lookup-sym]) index-of])])
macrocall?
#function(#array(uint8 62 1 53 0 29 20 1 6 21 0 2 52 0 52 1 53 0 29 45 4 3 11) [get
  *syntax-environment*])
macroexpand-1
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 53 0 6 15 0 53 0 54 0 0 30 12 11 54 0 0 11) [])
  macrocall?])
make-enum-table
#function(#array(uint8 62 2 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 47 52 0 52 1 54 0 1 3 1 3 1 50 2 60 66 11) [1-
  length #function(#array(uint8 62 1 52 0 54 0 0 54 1 1 53 0 42 54 1 0 53 0 34 2 4 3 11) [put!])])
						      table])
make-system-image
#function(#array(uint8 62 1 50 0 52 1 53 0 52 2 52 3 52 4 3 4 50 5 64 4 3 11) [#function(#array(uint8 65 3 52 0 50 1 60 52 2 3 0 3 2 2 52 3 53 0 4 1 11) [for-each
  #function(#array(uint8 62 1 53 0 22 1 6 61 0 2 52 0 53 0 3 1 17 1 6 61 0 2 52 1 53 0 3 1 24 17 1 6 61 0 2 52 2 53 0 54 0 1 3 2 17 1 6 61 0 2 52 3 52 1 53 0 3 1 3 1 17 6 108 0 52 4 54 0 0 53 0 3 2 2 52 5 54 0 0 50 6 3 2 2 52 4 54 0 0 52 1 53 0 3 1 3 2 2 52 5 54 0 0 50 6 4 2 11 45 11) [constant?
  top-level-value memq iostream? io.print io.write "\n"]) environment io.close])
  file :write :create :truncate (*linefeed* *directory-separator* *argv* that)])
map
#function(#array(uint8 62 2 53 1 16 6 11 0 53 1 11 53 0 53 1 29 3 1 52 0 53 0 53 1 30 3 2 27 11) [map])
mapcar
#function(#array(uint8 63 1 54 0 0 53 0 53 1 4 2 11) [] #0=[#function(#array(uint8 62 2 53 1 18 6 13 0 53 0 4 0 11 53 1 29 16 6 24 0 53 1 29 11 44 6 56 0 53 0 52 0 52 1 53 1 3 2 33 54 0 0 53 0 52 0 52 2 53 1 3 2 3 2 27 11 45 11) [map
  car cdr] #0#) ()])
load
#function(#array(uint8 62 1 50 0 52 1 53 0 52 2 3 2 64 4 2 11) [#function(#array(uint8 65 2 50 0 60 50 1 60 61 11) [#function(#array(uint8 62 0 50 0 45 64 3 2 45 45 45 4 3 11) [#function(#array(uint8 65 2 50 0 60 57 0 11) [#function(#array(uint8 62 3 52 0 54 1 0 3 1 17 6 34 0 54 0 0 52 1 54 1 0 3 1 53 0 52 2 53 1 3 1 4 3 11 52 3 54 1 0 3 1 2 52 2 53 1 4 1 11) [io.eof?
  read load-process io.close])])]) #function(#array(uint8 62 1 52 0 54 0 0 3 1 2 52 1 50 2 54 1 0 53 0 28 3 4 1 11) [io.close
  raise load-error])]) file :read])
listp
#function(#array(uint8 62 1 53 0 18 1 7 13 0 2 53 0 23 11) [])
list-partition
#function(#array(uint8 62 2 52 0 53 1 47 3 2 6 19 0 52 1 50 2 4 1 11 52 3 52 4 53 0 53 1 47 46 46 3 5 4 1 11) [<=
  error "list-partition: invalid count" nreverse list-part-])
list-part-
#function(#array(uint8 62 5 53 0 16 6 31 0 52 0 53 2 47 3 2 6 28 0 52 1 53 3 3 1 53 4 27 11 53 4 11 52 2 53 2 53 1 3 2 6 62 0 52 3 53 0 53 1 47 46 52 1 53 3 3 1 53 4 27 4 5 11 52 3 53 0 30 53 1 48 53 2 34 2 53 0 29 53 3 27 53 4 4 5 11) [>
  nreverse >= list-part-])
list->vector
#function(#array(uint8 62 1 52 0 53 0 12 11) [vector])
list*
#function(#array(uint8 63 0 53 0 30 16 6 13 0 53 0 29 11 53 0 29 52 0 53 0 30 33 27 11) [list*])
list-head
#function(#array(uint8 62 2 52 0 53 1 47 3 2 6 14 0 46 11 53 0 29 52 1 53 0 30 53 1 48 35 2 3 2 27 11) [<=
  list-head])
list-ref
#function(#array(uint8 62 2 52 0 53 0 53 1 3 2 29 11) [list-tail])
list-tail
#function(#array(uint8 62 2 52 0 53 1 47 3 2 6 15 0 53 0 11 52 1 53 0 30 53 1 48 35 2 4 2 11) [<=
  list-tail])
list?
#function(#array(uint8 62 1 53 0 18 1 7 25 0 2 53 0 23 1 6 25 0 2 52 0 53 0 30 4 1 11) [list?])
load-process
#function(#array(uint8 62 1 52 0 53 0 4 1 11) [eval])
length>
#function(#array(uint8 62 2 53 1 47 39 6 12 0 53 0 11 53 1 47 38 6 30 0 53 0 23 1 6 29 0 2 53 0 11 53 0 18 6 41 0 53 1 47 39 11 52 0 53 0 30 53 1 48 35 2 4 2 11) [length>])
length=
#function(#array(uint8 62 2 53 1 47 39 6 11 0 45 11 53 1 47 38 6 22 0 53 0 18 11 53 0 18 6 33 0 53 1 47 38 11 52 0 53 0 30 53 1 48 35 2 4 2 11) [length=])
lastcdr
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 52 0 53 0 30 4 1 11) [lastcdr])
last-pair
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 53 0 30 16 6 21 0 53 0 11 44 6 33 0 52 0 53 0 30 4 1 11 45 11) [last-pair])
just-compile-args
#function(#array(uint8 62 3 52 0 50 1 60 53 1 4 2 11) [for-each #function(#array(uint8 62 1 52 0 54 0 0 54 0 2 45 53 0 4 4 11) [compile-in])])
iota
#function(#array(uint8 62 1 52 0 52 1 53 0 4 2 11) [map-int identity])
io.readline
#function(#array(uint8 62 1 52 0 53 0 50 1 4 2 11) [io.readuntil #\x000a])
in-env?
#function(#array(uint8 62 2 53 1 23 1 6 33 0 2 52 0 53 0 53 1 29 3 2 1 7 33 0 2 52 1 53 0 53 1 30 4 2 11) [memq
  in-env?])
hex5
#function(#array(uint8 62 1 52 0 52 1 53 0 49 16 3 2 49 5 50 2 4 3 11) [pad-l
  number->string #\0])
identity
#function(#array(uint8 62 1 53 0 11) [])
index-of
#function(#array(uint8 62 3 53 1 18 6 10 0 45 11 53 0 53 1 29 13 6 22 0 53 2 11 44 6 41 0 52 0 53 0 53 1 30 53 2 48 34 2 4 3 11 45 11) [index-of])
get-defined-vars
#function(#array(uint8 62 1 52 0 54 0 0 53 0 3 1 4 1 11) [delete-duplicates] #0=[#function(#array(uint8 62 1 53 0 16 6 10 0 46 11 53 0 29 50 0 13 1 6 25 0 2 53 0 30 23 6 92 0 52 1 53 0 3 1 20 1 6 48 0 2 52 1 53 0 3 1 28 1 1 7 91 0 2 52 1 53 0 3 1 23 1 6 85 0 2 52 2 53 0 3 1 20 1 6 85 0 2 52 2 53 0 3 1 28 1 1 7 91 0 2 46 11 53 0 29 50 3 13 6 115 0 52 4 52 5 54 0 0 53 0 30 3 2 12 11 46 11) [define
  cadr caadr begin append map] #0#) ()])
function?
#function(#array(uint8 62 1 53 0 24 1 7 38 0 2 52 0 53 0 3 1 50 1 13 1 7 38 0 2 53 0 23 1 6 38 0 2 53 0 29 50 2 13 11) [typeof
  function lambda])
for-each
#function(#array(uint8 62 2 53 1 23 6 26 0 53 0 53 1 29 3 1 2 52 0 53 0 53 1 30 4 2 11 44 11) [for-each])
foldr
#function(#array(uint8 62 3 53 2 18 6 11 0 53 1 11 53 0 53 2 29 52 0 53 0 53 1 53 2 30 3 3 4 2 11) [foldr])
foldl
#function(#array(uint8 62 3 53 2 18 6 11 0 53 1 11 52 0 53 0 53 0 53 2 29 53 1 3 2 53 2 30 4 3 11) [foldl])
filter
#function(#array(uint8 62 2 54 0 0 53 0 53 1 46 4 3 11) [] #0=[#function(#array(uint8 62 3 53 1 18 6 11 0 53 2 11 53 0 53 1 29 3 1 6 38 0 54 0 0 53 0 53 1 30 53 1 29 53 2 27 4 3 11 44 6 55 0 54 0 0 53 0 53 1 30 53 2 4 3 11 45 11) [] #0#)
							       ()])
f-body
#function(#array(uint8 62 1 50 0 54 0 0 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 53 0 18 6 12 0 54 0 0 11 50 0 53 0 54 0 0 28 3 52 1 50 2 60 53 0 3 2 27 11) [lambda
  map #function(#array(uint8 62 1 45 11) [])]) get-defined-vars])] [#function(#array(uint8 62 1 53 0 16 6 10 0 45 11 53 0 30 46 13 6 22 0 53 0 29 11 44 6 32 0 50 0 53 0 27 11 45 11) [begin])
								    ()])
expand
#function(#array(uint8 62 1 52 0 53 0 4 1 11) [macroexpand])
every
#function(#array(uint8 62 2 53 1 16 1 7 31 0 2 53 0 53 1 29 3 1 1 6 31 0 2 52 0 53 0 53 1 30 4 2 11) [every])
even?
#function(#array(uint8 62 1 52 0 53 0 48 3 2 47 38 11) [logand])
eval
#function(#array(uint8 62 1 52 0 52 1 53 0 3 1 3 1 4 0 11) [compile-thunk
							    expand])
error
#function(#array(uint8 63 0 52 0 50 1 53 0 27 4 1 11) [raise error])
emit-nothing
#function(#array(uint8 62 1 53 0 11) [])
encode-byte-code
#function(#array(uint8 62 1 50 0 52 1 52 2 53 0 3 1 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 52 2 53 0 3 1 49 3 52 3 50 4 60 53 0 3 2 36 2 34 2 50 5 3 2 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 54 0 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 53 0 3 1 47 52 2 3 0 52 2 3 0 52 3 3 0 45 64 4 7 11) [#function(#array(uint8 65 7 45 53 1 53 0 39 6 144 0 2 54 0 0 53 1 42 57 5 2 53 5 52 0 13 6 62 0 52 1 53 2 54 0 0 53 1 48 34 2 42 52 2 53 4 3 1 3 3 2 53 1 49 2 34 2 57 1 5 141 0 52 3 53 4 52 4 52 5 52 6 54 1 0 1 6 88 0 2 52 7 53 5 50 8 3 2 6 101 0 50 9 53 5 64 3 2 5 103 0 53 5 3 2 3 1 3 2 2 53 1 48 34 2 57 1 2 53 1 53 0 39 6 140 0 50 10 54 0 0 53 1 42 64 3 2 5 141 0 45 5 3 0 2 52 11 50 12 60 53 3 3 2 2 52 13 53 4 4 1 11) [:label
  put! sizeof io.write byte get Instructions memq (:jmp :brt :brf)
  #function(#array(uint8 65 2 53 0 52 0 14 6 13 0 52 1 11 53 0 52 2 14 6 24 0 52 3 11 53 0 52 4 14 6 35 0 52 5 11 45 11) [:jmp
  :jmp.l :brt :brt.l :brf :brf.l]) #function(#array(uint8 65 2 50 0 54 0 5 64 4 2 11) [#function(#array(uint8 65 2 52 0 53 0 50 1 3 2 6 38 0 52 2 54 1 4 52 3 54 0 0 3 1 3 2 2 54 1 1 48 34 2 58 1 1 11 52 0 53 0 50 4 3 2 6 74 0 52 2 54 1 4 52 5 54 0 0 3 1 3 2 2 54 1 1 48 34 2 58 1 1 11 52 0 53 0 50 6 3 2 6 139 0 52 2 54 1 4 52 5 54 0 0 3 1 3 2 2 54 1 1 48 34 2 58 1 1 2 52 2 54 1 4 52 5 54 2 0 54 1 1 42 3 1 3 2 2 54 1 1 48 34 2 58 1 1 11 52 0 53 0 50 7 3 2 6 202 0 52 8 54 1 3 52 9 54 1 4 3 1 54 0 0 3 3 2 52 2 54 1 4 54 3 0 6 184 0 52 3 5 186 0 52 10 47 3 1 3 2 2 54 1 1 48 34 2 58 1 1 11 45 11) [memv
  (:loadv.l :loadg.l :setg.l) io.write uint32 (:loada :seta :call :tcall :loadv
					       :loadg :setg :list :+ :- :* :/
					       :vector :argc :vargc :loadi8
					       :let) uint8 (:loadc :setc)
  (:jmp :brf :brt) put! sizeof uint16])]) table.foreach #function(#array(uint8 62 2 52 0 54 0 4 53 0 3 2 2 52 1 54 0 4 54 2 0 6 28 0 52 2 5 30 0 52 3 52 4 54 0 2 53 1 3 2 3 1 4 2 11) [io.seek
  io.write uint32 uint16 get]) io.tostring!]) length table buffer])
  list->vector]) >= length count #function(#array(uint8 62 1 52 0 53 0 50 1 4 2 11) [memq
  (:loadv :loadg :setg :jmp :brt :brf)]) 65536]) peephole nreverse])
emit
#function(#array(uint8 63 2 52 0 53 1 50 1 3 2 6 25 0 50 2 53 0 48 42 64 3 2 5 26 0 45 2 53 0 47 52 3 53 1 53 2 27 53 0 47 42 3 2 43 2 53 0 11) [memq
  (:loadv :loadg :setg) #function(#array(uint8 65 2 50 0 54 0 0 49 2 42 64 4 2 11) [#function(#array(uint8 65 2 50 0 54 1 2 29 64 4 2 11) [#function(#array(uint8 65 2 50 0 52 1 54 1 0 53 0 3 2 6 28 0 52 2 54 1 0 53 0 3 2 5 57 0 52 3 54 1 0 53 0 54 0 0 3 3 2 54 0 0 48 34 2 58 0 0 2 54 0 0 48 35 2 64 4 2 11) [#function(#array(uint8 65 2 54 3 0 49 2 54 1 0 43 2 53 0 28 1 58 3 2 2 52 0 53 0 50 1 3 2 6 43 0 50 2 54 3 1 64 3 2 58 3 1 11 45 11) [>=
  256 #function(#array(uint8 65 2 53 0 52 0 14 6 13 0 52 1 11 53 0 52 2 14 6 24 0 52 3 11 53 0 52 4 14 6 35 0 52 5 11 45 11) [:loadv
  :loadv.l :loadg :loadg.l :setg :setg.l])]) has? get put!])])]) nreconc])
disassemble
#function(#array(uint8 62 1 52 0 53 0 47 3 2 2 52 1 4 0 11) [disassemble-
							     newline])
disassemble-
#function(#array(uint8 62 2 50 0 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 50 0 53 0 47 42 53 0 48 42 64 4 3 11) [#function(#array(uint8 65 3 50 0 45 64 4 2 11) [#function(#array(uint8 65 2 50 0 60 57 0 2 50 1 47 52 2 54 0 0 3 1 64 4 3 11) [#function(#array(uint8 62 1 53 0 23 1 6 16 0 2 53 0 29 50 0 13 6 39 0 52 1 50 2 3 1 2 52 3 53 0 54 3 1 48 34 2 4 2 11 52 4 53 0 4 1 11) [compiled-lambda
  princ "\n" disassemble- print]) #function(#array(uint8 65 3 45 53 0 53 1 39 6 32 0 2 50 0 52 1 52 2 54 1 0 53 0 42 3 2 64 3 2 5 3 0 11) [#function(#array(uint8 65 2 52 0 54 0 0 47 3 2 6 20 0 52 1 3 0 5 21 0 45 2 47 54 4 1 48 35 2 50 2 60 66 2 52 3 52 4 54 0 0 3 1 50 5 52 6 52 7 53 0 3 1 48 3 2 50 8 3 4 2 54 0 0 48 34 2 58 0 0 2 50 9 53 0 64 4 2 11) [>
  newline #function(#array(uint8 62 1 52 0 50 1 4 1 11) [princ "\t"]) princ
  hex5 ":  " string.tail string "\t" #function(#array(uint8 65 2 52 0 53 0 50 1 3 2 6 44 0 54 2 0 54 3 1 52 2 54 3 0 54 1 0 3 2 42 3 1 2 54 1 0 49 4 34 2 58 1 0 11 52 0 53 0 50 3 3 2 6 82 0 54 2 0 54 3 1 54 3 0 54 1 0 42 42 3 1 2 54 1 0 48 34 2 58 1 0 11 52 0 53 0 50 4 3 2 6 119 0 52 5 52 6 54 3 0 54 1 0 42 3 1 3 1 2 54 1 0 48 34 2 58 1 0 11 52 0 53 0 50 7 3 2 6 184 0 52 5 52 6 54 3 0 54 1 0 42 3 1 50 8 3 2 2 54 1 0 48 34 2 58 1 0 2 52 5 52 6 54 3 0 54 1 0 42 3 1 3 1 2 54 1 0 48 34 2 58 1 0 11 52 0 53 0 50 9 3 2 6 227 0 52 5 50 10 52 11 52 12 54 3 0 54 1 0 3 2 3 1 3 2 2 54 1 0 49 2 34 2 58 1 0 11 52 0 53 0 50 13 3 2 6 14 1 52 5 50 10 52 11 52 2 54 3 0 54 1 0 3 2 3 1 3 2 2 54 1 0 49 4 34 2 58 1 0 11 45 11) [memv
  (:loadv.l :loadg.l :setg.l) ref-uint32-LE (:loadv :loadg :setg)
  (:loada :seta :call :tcall :list :+ :- :* :/ :vector :argc :vargc :loadi8
   :let) princ number->string (:loadc :setc) " " (:jmp :brf :brt) "@" hex5
  ref-uint16-LE (:jmp.l :brf.l :brt.l)])]) get 1/Instructions]) length])])])
							   function->vector])
display
#function(#array(uint8 62 1 52 0 53 0 3 1 2 44 11) [princ])
delete-duplicates
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 50 0 53 0 29 53 0 30 64 4 3 11) [#function(#array(uint8 65 3 52 0 53 0 53 1 3 2 6 20 0 52 1 53 1 4 1 11 53 0 52 1 53 1 3 1 27 11) [member
  delete-duplicates])])
count-
#function(#array(uint8 62 3 53 1 18 6 11 0 53 2 11 52 0 53 0 53 1 30 53 0 53 1 29 3 1 6 36 0 53 2 48 34 2 5 38 0 53 2 4 3 11) [count-])
copy-tree
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 52 0 53 0 29 3 1 52 0 53 0 30 3 1 27 11) [copy-tree])
count
#function(#array(uint8 62 2 52 0 53 0 53 1 47 4 3 11) [count-])
copy-list
#function(#array(uint8 62 1 53 0 16 6 11 0 53 0 11 53 0 29 52 0 53 0 30 3 1 27 11) [copy-list])
const-to-idx-vec
#function(#array(uint8 62 1 50 0 53 0 48 42 53 0 49 2 42 64 4 3 11) [#function(#array(uint8 65 3 50 0 52 1 53 1 3 1 64 4 2 11) [#function(#array(uint8 65 2 52 0 50 1 60 54 0 0 3 2 2 53 0 11) [table.foreach
  #function(#array(uint8 62 2 54 0 0 53 1 53 0 43 11) [])]) vector.alloc])])
cond->if
#function(#array(uint8 62 1 52 0 53 0 30 4 1 11) [cond-clauses->if])
cond-clauses->if
#function(#array(uint8 62 1 53 0 16 6 10 0 45 11 50 0 53 0 29 64 4 2 11) [#function(#array(uint8 65 2 53 0 29 50 0 13 6 18 0 50 1 53 0 30 27 11 50 2 53 0 29 50 1 53 0 30 27 52 3 54 0 0 30 3 1 28 4 11) [else
  begin if cond-clauses->if])])
compile-while
#function(#array(uint8 62 4 50 0 52 1 53 0 3 1 52 1 53 0 3 1 64 4 3 11) [#function(#array(uint8 65 3 52 0 54 0 0 54 0 1 45 45 3 4 2 52 1 54 0 0 53 0 3 2 2 52 0 54 0 0 54 0 1 45 54 0 2 3 4 2 52 2 54 0 0 52 3 53 1 3 3 2 52 2 54 0 0 52 4 3 2 2 52 0 54 0 0 54 0 1 45 54 0 3 3 4 2 52 2 54 0 0 52 5 53 0 3 3 2 52 1 54 0 0 53 1 4 2 11) [compile-in
  mark-label emit :brf :pop :jmp]) make-label])
compile-short-circuit
#function(#array(uint8 62 6 53 3 16 6 21 0 52 0 53 0 53 1 53 2 53 4 4 4 11 53 3 30 16 6 42 0 52 0 53 0 53 1 53 2 53 3 29 4 4 11 50 1 52 2 53 0 3 1 64 4 2 11) [compile-in
  #function(#array(uint8 65 2 52 0 54 0 0 54 0 1 45 54 0 3 29 3 4 2 52 1 54 0 0 52 2 3 2 2 52 1 54 0 0 54 0 5 53 0 3 3 2 52 1 54 0 0 52 3 3 2 2 52 4 54 0 0 54 0 1 54 0 2 54 0 3 30 54 0 4 54 0 5 3 6 2 52 5 54 0 0 53 0 4 2 11) [compile-in
  emit :dup :pop compile-short-circuit mark-label]) make-label])
compile-let
#function(#array(uint8 62 4 50 0 53 3 29 53 3 30 64 4 3 11) [#function(#array(uint8 65 3 52 0 53 1 52 1 52 2 53 0 3 1 3 1 3 2 6 25 0 45 5 37 0 52 3 52 4 50 5 53 0 3 2 3 1 2 52 6 54 0 0 52 7 52 8 54 0 1 53 0 44 3 3 3 3 2 50 9 52 10 54 0 0 54 0 1 53 1 3 3 64 4 2 11) [length=
  length cadr error string "apply: incorrect number of arguments to " emit
  :loadv compile-f #function(#array(uint8 65 2 52 0 54 1 0 52 1 3 2 2 52 0 54 1 0 54 1 2 6 28 0 52 2 5 30 0 52 3 48 53 0 34 2 4 3 11) [emit
  :close :tcall :call]) compile-arglist])])
compile-or
#function(#array(uint8 62 4 52 0 53 0 53 1 53 2 53 3 45 52 1 4 6 11) [compile-short-circuit
  :brt])
compile-prog1
#function(#array(uint8 62 3 52 0 53 0 53 1 45 52 1 53 2 3 1 3 4 2 52 2 53 2 3 1 23 6 53 0 52 3 53 0 53 1 45 52 2 53 2 3 1 3 4 2 52 4 53 0 52 5 4 2 11 45 11) [compile-in
  cadr cddr compile-begin emit :pop])
compile-sym
#function(#array(uint8 62 4 50 0 52 1 53 2 53 1 47 44 3 4 64 4 2 11) [#function(#array(uint8 65 2 50 0 53 0 29 64 4 2 11) [#function(#array(uint8 65 2 53 0 50 0 14 6 30 0 52 1 54 1 0 54 1 3 47 42 52 2 54 0 0 3 1 4 3 11 53 0 50 3 14 6 65 0 52 1 54 1 0 54 1 3 48 42 52 2 54 0 0 3 1 52 4 54 0 0 3 1 4 4 11 52 1 54 1 0 54 1 3 49 2 42 54 1 2 4 3 11) [arg
  emit cadr closed caddr])]) lookup-sym])
compile-thunk
#function(#array(uint8 62 1 52 0 50 1 46 53 0 28 3 4 1 11) [compile lambda])
compile-for
#function(#array(uint8 62 5 52 0 53 4 3 1 6 56 0 52 1 53 0 53 1 45 53 2 3 4 2 52 1 53 0 53 1 45 53 3 3 4 2 52 1 53 0 53 1 45 53 4 3 4 2 52 2 53 0 52 3 4 2 11 52 4 50 5 4 1 11) [1arg-lambda?
  compile-in emit :for error "for: third form must be a 1-argument lambda"])
compile-if
#function(#array(uint8 62 4 50 0 52 1 53 0 3 1 52 1 53 0 3 1 64 4 3 11) [#function(#array(uint8 65 3 52 0 54 0 0 54 0 1 45 52 1 54 0 3 3 1 3 4 2 52 2 54 0 0 52 3 53 0 3 3 2 52 0 54 0 0 54 0 1 54 0 2 52 4 54 0 3 3 1 3 4 2 54 0 2 6 72 0 52 2 54 0 0 52 5 3 2 5 83 0 52 2 54 0 0 52 6 53 1 3 3 2 52 7 54 0 0 53 0 3 2 2 52 0 54 0 0 54 0 1 54 0 2 52 8 54 0 3 3 1 23 6 126 0 52 9 54 0 3 3 1 5 127 0 45 3 4 2 52 7 54 0 0 53 1 4 2 11) [compile-in
  cadr emit :brf caddr :ret :jmp mark-label cdddr cadddr]) make-label])
compile-call
#function(#array(uint8 62 4 50 0 53 3 29 64 4 2 11) [#function(#array(uint8 65 2 50 0 53 0 20 1 6 53 0 2 52 1 53 0 54 0 1 3 2 17 1 6 53 0 2 53 0 22 1 6 53 0 2 52 2 53 0 3 1 1 6 53 0 2 52 3 53 0 3 1 24 6 65 0 52 3 53 0 3 1 5 67 0 53 0 64 4 2 11) [#function(#array(uint8 65 2 50 0 53 0 24 1 6 18 0 2 52 1 53 0 3 1 64 4 2 11) [#function(#array(uint8 65 2 53 0 17 6 25 0 52 0 54 2 0 54 2 1 45 54 0 0 3 4 5 26 0 45 2 50 1 52 2 54 2 0 54 2 1 54 2 3 30 3 3 64 4 2 11) [compile-in
  #function(#array(uint8 65 2 54 0 0 6 24 0 50 0 52 1 52 2 54 0 0 45 3 3 64 4 2 11 52 3 54 3 0 54 3 2 6 40 0 52 4 5 42 0 52 5 53 0 4 3 11) [#function(#array(uint8 65 2 53 0 1 6 20 0 2 52 0 54 4 3 30 53 0 3 2 17 6 35 0 52 1 54 2 0 53 0 3 2 5 36 0 45 2 50 2 54 1 0 64 4 2 11) [length=
  argc-error #function(#array(uint8 65 2 53 0 52 0 14 6 42 0 54 1 0 47 38 6 28 0 52 1 54 5 0 52 2 4 2 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 53 0 52 3 14 6 82 0 54 1 0 47 38 6 68 0 52 1 54 5 0 52 4 4 2 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 53 0 52 5 14 6 121 0 54 1 0 47 38 6 107 0 52 6 54 3 0 48 4 2 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 53 0 52 7 14 6 161 0 54 1 0 47 38 6 147 0 52 1 54 5 0 52 8 4 2 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 53 0 52 9 14 6 200 0 54 1 0 47 38 6 186 0 52 6 54 3 0 48 4 2 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 53 0 52 10 14 6 242 0 54 1 0 47 38 6 228 0 52 1 54 5 0 52 11 50 12 4 3 11 52 1 54 5 0 54 2 0 54 1 0 4 3 11 52 1 54 5 0 54 5 2 1 6 5 1 2 54 2 0 52 13 13 6 13 1 52 14 5 16 1 54 2 0 4 2 11) [:list
  emit :loadnil :+ :load0 :- argc-error :* :load1 :/ :vector :loadv [] :apply
  :tapply])]) get arg-counts emit :tcall :call]) compile-arglist])
  builtin->instruction]) in-env? constant? top-level-value])])
compile-f
#function(#array(uint8 63 2 50 0 52 1 3 0 52 2 53 1 3 1 64 4 3 11) [#function(#array(uint8 65 3 54 0 2 18 17 6 31 0 52 0 53 0 52 1 52 2 52 3 53 1 3 1 3 1 3 3 5 82 0 52 4 53 1 3 1 18 6 58 0 52 0 53 0 52 5 52 3 53 1 3 1 3 3 5 82 0 52 0 53 0 52 6 53 1 16 6 74 0 47 5 80 0 52 3 53 1 3 1 3 3 2 52 7 53 0 52 8 53 1 3 1 54 0 0 27 44 52 9 54 0 1 3 1 3 4 2 52 0 53 0 52 10 3 2 2 52 11 52 12 53 0 47 42 3 1 52 13 53 0 3 1 4 2 11) [emit
  :let 1+ length lastcdr :argc :vargc compile-in to-proper caddr :ret function
  encode-byte-code const-to-idx-vec]) make-code-emitter cadr])
compile-app
#function(#array(uint8 62 4 50 0 53 3 29 64 4 2 11) [#function(#array(uint8 65 2 53 0 23 1 6 31 0 2 53 0 29 50 0 13 1 6 31 0 2 52 1 52 2 53 0 3 1 3 1 6 51 0 52 3 54 0 0 54 0 1 54 0 2 54 0 3 4 4 11 52 4 54 0 0 54 0 1 54 0 2 54 0 3 4 4 11) [lambda
  list? cadr compile-let compile-call])])
compile-and
#function(#array(uint8 62 4 52 0 53 0 53 1 53 2 53 3 44 52 1 4 6 11) [compile-short-circuit
  :brf])
compile
#function(#array(uint8 62 1 52 0 46 53 0 4 2 11) [compile-f])
compile-arglist
#function(#array(uint8 62 3 50 0 52 1 53 2 52 2 3 2 64 4 2 11) [#function(#array(uint8 65 2 53 0 6 57 0 52 0 54 0 0 52 1 54 0 2 52 2 3 2 54 0 1 3 3 2 50 3 52 4 52 5 50 6 60 52 7 53 0 52 2 3 2 3 2 27 64 3 2 2 52 2 48 34 2 11 52 0 54 0 0 54 0 2 54 0 1 3 3 2 52 8 54 0 2 4 1 11) [just-compile-args
  list-head MAX_ARGS #function(#array(uint8 65 2 52 0 54 1 0 54 1 1 45 53 0 4 4 11) [compile-in])
  nconc map #function(#array(uint8 62 1 52 0 53 0 27 11) [list]) list-partition
  length]) length> MAX_ARGS])
compile-begin
#function(#array(uint8 62 4 53 3 16 6 20 0 52 0 53 0 53 1 53 2 45 4 4 11 53 3 30 16 6 41 0 52 0 53 0 53 1 53 2 53 3 29 4 4 11 52 0 53 0 53 1 45 53 3 29 3 4 2 52 1 53 0 52 2 3 2 2 52 3 53 0 53 1 53 2 53 3 30 4 4 11) [compile-in
  emit :pop compile-begin])
compile-in
#function(#array(uint8 62 4 53 3 20 6 21 0 52 0 53 0 53 1 53 3 50 1 4 4 11 53 3 16 6 161 0 53 3 47 13 6 43 0 52 2 53 0 52 3 4 2 11 53 3 48 13 6 59 0 52 2 53 0 52 4 4 2 11 53 3 44 13 6 75 0 52 2 53 0 52 5 4 2 11 53 3 45 13 6 91 0 52 2 53 0 52 6 4 2 11 53 3 46 13 6 107 0 52 2 53 0 52 7 4 2 11 53 3 26 1 6 136 0 2 52 8 53 3 49 128 3 2 1 6 136 0 2 52 9 53 3 49 127 3 2 6 150 0 52 2 53 0 52 10 53 3 4 3 11 52 2 53 0 52 11 53 3 4 3 11 50 12 53 3 29 64 4 2 11) [compile-sym
  [:loada :loadc :loadg] emit :load0 :load1 :loadt :loadf :loadnil >= <=
  :loadi8 :loadv #function(#array(uint8 65 2 53 0 50 0 14 6 27 0 52 1 54 0 0 52 2 52 3 54 0 3 3 1 4 3 11 53 0 50 4 14 6 56 0 52 5 54 0 0 54 0 1 54 0 2 52 6 54 0 3 3 1 4 4 11 53 0 50 7 14 6 81 0 52 8 54 0 0 54 0 1 54 0 2 54 0 3 4 4 11 53 0 50 9 14 6 107 0 52 10 54 0 0 54 0 1 54 0 2 54 0 3 30 4 4 11 53 0 50 11 14 6 129 0 52 12 54 0 0 54 0 1 54 0 3 4 3 11 53 0 50 13 14 6 167 0 52 1 54 0 0 52 2 52 14 54 0 1 54 0 3 3 2 3 3 2 52 1 54 0 0 52 15 4 2 11 53 0 50 16 14 6 193 0 52 17 54 0 0 54 0 1 54 0 2 54 0 3 30 4 4 11 53 0 50 18 14 6 219 0 52 19 54 0 0 54 0 1 54 0 2 54 0 3 30 4 4 11 53 0 50 20 14 6 255 0 52 21 54 0 0 54 0 1 52 3 54 0 3 3 1 50 9 52 22 54 0 3 3 1 27 4 4 11 53 0 50 23 14 6 39 1 52 24 54 0 0 54 0 1 52 3 54 0 3 3 1 52 25 54 0 3 3 1 52 26 54 0 3 3 1 4 5 11 53 0 50 27 14 6 86 1 52 5 54 0 0 54 0 1 45 52 25 54 0 3 3 1 3 4 2 52 28 54 0 0 54 0 1 52 3 54 0 3 3 1 50 29 4 4 11 53 0 50 30 14 6 172 1 52 5 54 0 0 54 0 1 45 50 13 46 52 3 54 0 3 3 1 28 3 3 4 2 52 31 52 25 54 0 3 3 1 3 1 6 136 1 45 5 142 1 52 32 50 33 3 1 2 52 5 54 0 0 54 0 1 45 52 25 54 0 3 3 1 3 4 2 52 1 54 0 0 52 34 4 2 11 52 35 54 0 0 54 0 1 54 0 2 54 0 3 4 4 11) [quote
  emit :loadv cadr cond compile-in cond->if if compile-if begin compile-begin
  prog1 compile-prog1 lambda compile-f :closure and compile-and or compile-or
  while compile-while cddr for compile-for caddr cadddr set! compile-sym [:seta
  :setc :setg] trycatch 1arg-lambda? error "trycatch: second form must be a 1-argument lambda"
  :trycatch compile-app])])
char?
#function(#array(uint8 62 1 52 0 53 0 3 1 50 1 13 11) [typeof wchar])
cddr
#function(#array(uint8 62 1 53 0 30 30 11) [])
cddar
#function(#array(uint8 62 1 53 0 29 30 30 11) [])
cdadr
#function(#array(uint8 62 1 53 0 30 29 30 11) [])
cdaar
#function(#array(uint8 62 1 53 0 29 29 30 11) [])
cdar
#function(#array(uint8 62 1 53 0 29 30 11) [])
cdddr
#function(#array(uint8 62 1 53 0 30 30 30 11) [])
cadar
#function(#array(uint8 62 1 53 0 29 30 29 11) [])
cadddr
#function(#array(uint8 62 1 53 0 30 30 30 29 11) [])
caddr
#function(#array(uint8 62 1 53 0 30 30 29 11) [])
caaar
#function(#array(uint8 62 1 53 0 29 29 29 11) [])
caadr
#function(#array(uint8 62 1 53 0 30 29 29 11) [])
caar
#function(#array(uint8 62 1 53 0 29 29 11) [])
cadr
#function(#array(uint8 62 1 53 0 30 29 11) [])
builtin->instruction
#function(#array(uint8 62 1 50 0 52 1 52 2 50 3 53 0 3 2 3 1 64 4 2 11) [#function(#array(uint8 65 2 52 0 52 1 53 0 3 2 1 6 17 0 2 53 0 11) [has?
  Instructions]) intern string #\:])
bq-process
#function(#array(uint8 62 1 52 0 53 0 3 1 6 36 0 53 0 25 6 33 0 50 1 52 2 52 3 53 0 3 1 3 1 64 4 2 11 53 0 11 53 0 16 6 49 0 50 4 53 0 28 2 11 53 0 29 50 5 13 6 73 0 52 2 52 2 52 6 53 0 3 1 3 1 4 1 11 53 0 29 50 7 13 6 89 0 52 6 53 0 4 1 11 52 8 52 9 53 0 3 2 17 6 121 0 50 10 52 11 53 0 3 1 52 12 52 13 53 0 3 2 64 4 3 11 44 6 134 0 50 14 53 0 46 64 4 3 11 45 11) [self-evaluating?
  #function(#array(uint8 65 2 53 0 29 50 0 13 6 18 0 52 1 53 0 30 27 11 52 2 52 1 53 0 28 3 11) [list
  vector apply]) bq-process vector->list quote backquote cadr *comma* any
  splice-form? #function(#array(uint8 65 3 53 0 18 6 14 0 50 0 53 1 27 11 52 1 50 2 53 1 27 52 3 53 0 3 1 28 1 4 2 11) [list
  nconc nlist* bq-process]) lastcdr map bq-bracket1 #function(#array(uint8 65 3 45 53 0 23 1 6 18 0 2 53 0 29 50 0 13 17 6 43 0 2 52 1 53 0 29 3 1 53 1 27 57 1 2 53 0 30 57 0 5 3 0 2 50 2 53 0 23 6 69 0 52 3 53 1 52 4 53 0 3 1 28 1 3 2 5 106 0 53 0 18 6 84 0 52 5 53 1 3 1 5 106 0 44 6 105 0 52 3 53 1 52 6 53 0 3 1 28 1 3 2 5 106 0 45 64 4 2 11) [*comma*
  bq-bracket #function(#array(uint8 65 2 53 0 30 18 6 13 0 53 0 29 11 50 0 53 0 27 11) [nconc])
  nreconc cadr nreverse bq-process])])
bq-bracket
#function(#array(uint8 62 1 53 0 16 6 19 0 52 0 52 1 53 0 3 1 28 2 11 53 0 29 50 2 13 6 39 0 52 0 52 3 53 0 3 1 28 2 11 53 0 29 50 4 13 6 59 0 50 5 52 3 53 0 3 1 28 2 11 53 0 29 50 6 13 6 75 0 52 3 53 0 4 1 11 44 6 90 0 52 0 52 1 53 0 3 1 28 2 11 45 11) [list
  bq-process *comma* cadr *comma-at* copy-list *comma-dot*])
bq-bracket1
#function(#array(uint8 62 1 53 0 23 1 6 16 0 2 53 0 29 50 0 13 6 26 0 52 1 53 0 4 1 11 52 2 53 0 4 1 11) [*comma*
  cadr bq-process])
assv
#function(#array(uint8 62 2 53 1 16 6 10 0 45 11 52 0 53 1 3 1 53 0 14 6 26 0 53 1 29 11 44 6 40 0 52 1 53 0 53 1 30 4 2 11 45 11) [caar
  assv])
assoc
#function(#array(uint8 62 2 53 1 16 6 10 0 45 11 52 0 53 1 3 1 53 0 15 6 26 0 53 1 29 11 44 6 40 0 52 1 53 0 53 1 30 4 2 11 45 11) [caar
  assoc])
argc-error
#function(#array(uint8 62 2 52 0 52 1 50 2 53 0 50 3 53 1 53 1 48 38 6 26 0 50 4 5 28 0 50 5 3 5 4 1 11) [error
  string "compile error: " " expects " " argument." " arguments."])
arg-counts
#table(:not 1  :set-cdr! 2  :cons 2  :number? 1  :equal? 2  :cdr 1  :vector? 1  :eqv? 2  :apply 2  := 2  :atom? 1  :aref 2  :compare 2  :< 2  :null? 1  :eq? 2  :car 1  :set-car! 2  :builtin? 1  :aset! 3  :bound? 1  :boolean? 1  :pair? 1  :symbol? 1  :fixnum? 1)
append2
#function(#array(uint8 62 2 53 0 18 6 11 0 53 1 11 53 0 29 52 0 53 0 30 53 1 3 2 27 11) [append2])
any
#function(#array(uint8 62 2 53 1 23 1 6 31 0 2 53 0 53 1 29 3 1 1 7 31 0 2 52 0 53 0 53 1 30 4 2 11) [any])
__start
#function(#array(uint8 62 1 52 0 3 0 2 53 0 30 23 6 33 0 53 0 30 56 1 2 52 2 52 3 53 0 3 1 3 1 5 49 0 53 0 56 1 2 52 4 52 5 3 1 2 52 6 3 0 2 52 7 47 4 1 11) [__init_globals
  *argv* __script cadr princ *banner* repl exit])
__init_globals
#function(#array(uint8 62 0 52 0 50 1 13 1 7 27 0 2 52 0 50 2 13 1 7 27 0 2 52 0 50 3 13 6 42 0 50 4 56 5 2 50 6 56 7 5 51 0 50 8 56 5 2 50 9 56 7 2 52 10 56 11 2 52 12 56 13 11) [*os-name*
  win32 win64 windows "\\" *directory-separator* "\r\n" *linefeed* "/" "\n"
  *stdout* *output-stream* *stdin* *input-stream*])
__script
#function(#array(uint8 62 1 50 0 60 50 1 60 61 11) [#function(#array(uint8 62 0 52 0 54 0 0 4 1 11) [load])
						    #function(#array(uint8 62 1 52 0 53 0 3 1 2 52 1 48 4 1 11) [print-exception
  exit])])
abs
#function(#array(uint8 62 1 53 0 47 39 6 14 0 53 0 35 1 11 53 0 11) [])
append
#function(#array(uint8 63 0 53 0 18 6 10 0 46 11 53 0 30 18 6 21 0 53 0 29 11 44 6 39 0 52 0 53 0 29 52 1 53 0 30 33 4 2 11 45 11) [append2
  append])
MAX_ARGS
127
Instructions
#table(:set-cdr! 32  :/ 37  :call 3  := 38  :aref 42  :let 65  :argc 62  :loadg 52  :car 29  :brt.l 10  :vargc 63  :loada 53  :aset! 43  :pair? 23  :fixnum? 26  :brf 6  :closure 60  :number? 21  :loadv.l 51  :seta 57  :brf.l 9  :for 66  :dup 1  :compare 40  :eq? 13  :+ 34  :jmp 5  :loadt 44  :brt 7  :builtin? 24  :close 64  :tcall 4  :ret 11  :loadf 45  :jmp.l 8  :nop 0  :tapply 12  :setc 58  :cons 27  :equal? 15  :cdr 30  :setg.l 59  :eqv? 14  :list 28  :atom? 16  :load0 47  :< 39  :null? 18  :load1 48  :set-car! 31  :setg 56  :bound? 22  :symbol? 20  :loadi8 49  :not 17  :* 36  :pop 2  :loadnil 46  :loadv 50  :vector 41  :- 35  :trycatch 61  :vector? 25  :apply 33  :loadc 54  :loadg.l 55  :boolean? 19)
>=
#function(#array(uint8 62 2 53 1 53 0 39 1 7 17 0 2 53 0 53 1 38 11) [])
>
#function(#array(uint8 62 2 53 1 53 0 39 11) [])
<=
#function(#array(uint8 62 2 53 0 53 1 39 1 7 17 0 2 53 0 53 1 38 11) [])
1arg-lambda?
#function(#array(uint8 62 1 53 0 23 1 6 53 0 2 53 0 29 50 0 13 1 6 53 0 2 53 0 30 23 1 6 53 0 2 52 1 53 0 3 1 23 1 6 53 0 2 52 2 52 1 53 0 3 1 48 4 2 11) [lambda
  cadr length=])
1/Instructions
#table(2 :pop  15 :equal?  38 :=  14 :eqv?  40 :compare  22 :bound?  36 :*  60 :closure  56 :setg  23 :pair?  3 :call  58 :setc  21 :number?  8 :jmp.l  51 :loadv.l  66 :for  65 :let  55 :loadg.l  5 :jmp  27 :cons  46 :loadnil  42 :aref  25 :vector?  13 :eq?  35 :-  12 :tapply  32 :set-cdr!  62 :argc  20 :symbol?  7 :brt  49 :loadi8  18 :null?  52 :loadg  1 :dup  45 :loadf  59 :setg.l  50 :loadv  61 :trycatch  11 :ret  30 :cdr  28 :list  48 :load1  41 :vector  0 :nop  29 :car  17 :not  4 :tcall  43 :aset!  39 :<  63 :vargc  53 :loada  44 :loadt  34 :+  6 :brf  16 :atom?  10 :brt.l  31 :set-car!  54 :loadc  19 :boolean?  47 :load0  9 :brf.l  26 :fixnum?  37 :/  24 :builtin?  64 :close  33 :apply  57 :seta)
/=
#function(#array(uint8 62 2 53 0 53 1 38 17 11) [])
1+
#function(#array(uint8 62 1 53 0 48 34 2 11) [])
1-
#function(#array(uint8 62 1 53 0 48 35 2 11) [])
*whitespace*
"\t\n\v\f\r \u0085  ᠎           \u2028\u2029  　"
*syntax-environment*
#table(letrec #function(#array(uint8 63 1 50 0 52 1 52 2 53 0 3 2 52 3 52 4 52 1 50 5 60 53 0 3 2 53 1 3 2 3 1 28 3 52 1 50 6 60 53 0 3 2 27 11) [lambda
  map car f-body nconc #function(#array(uint8 62 1 50 0 53 0 27 11) [set!])
  #function(#array(uint8 62 1 45 11) [])])  backquote #function(#array(uint8 62 1 52 0 53 0 4 1 11) [bq-process])  when #function(#array(uint8 63 1 50 0 53 0 52 1 53 1 3 1 45 28 4 11) [if
  f-body])  dotimes #function(#array(uint8 63 1 50 0 53 0 29 52 1 53 0 3 1 64 4 3 11) [#function(#array(uint8 65 3 50 0 47 50 1 53 1 48 28 3 50 2 53 0 28 1 52 3 54 0 1 3 1 28 3 28 4 11) [for
  - lambda f-body]) cadr])  unwind-protect #function(#array(uint8 62 2 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 50 0 50 1 54 0 0 50 2 53 0 28 1 50 3 54 0 1 50 4 53 0 28 2 28 3 28 3 28 3 54 0 1 28 3 11) [prog1
  trycatch lambda begin raise]) gensym])  define-macro #function(#array(uint8 63 1 50 0 50 1 53 0 29 28 2 50 2 53 0 30 52 3 53 1 3 1 28 3 28 3 11) [set-syntax!
  quote lambda f-body])  unless #function(#array(uint8 63 1 50 0 53 0 45 52 1 53 1 3 1 28 4 11) [if
  f-body])  let* #function(#array(uint8 63 1 53 0 16 6 15 0 52 0 53 1 4 1 11 50 1 52 2 53 0 3 1 28 1 52 3 50 4 28 1 53 0 30 28 1 52 5 53 1 3 1 3 3 28 3 52 6 53 0 3 1 28 2 11) [f-body
  lambda caar nconc let* copy-list cadar])  case #function(#array(uint8 63 1 50 0 45 64 4 2 11) [#function(#array(uint8 65 2 50 0 60 57 0 2 50 1 52 2 3 0 64 4 2 11) [#function(#array(uint8 62 2 53 1 50 0 13 6 13 0 50 0 11 53 1 18 6 21 0 45 11 53 1 16 6 40 0 50 1 53 0 52 2 53 1 3 1 28 3 11 53 1 30 18 6 61 0 50 1 53 0 52 2 53 1 29 3 1 28 3 11 50 3 53 0 50 4 53 1 28 2 28 3 11) [else
  eqv? quote-value memv quote]) #function(#array(uint8 65 2 50 0 53 0 54 1 0 28 2 28 1 52 1 50 2 28 1 52 3 52 4 50 5 60 54 1 1 3 2 3 1 3 2 28 3 11) [let
  nconc cond copy-list map #function(#array(uint8 62 1 54 1 0 54 0 0 53 0 29 3 2 53 0 30 27 11) [])])
  gensym])])  define #function(#array(uint8 63 1 53 0 20 6 18 0 50 0 53 0 53 1 29 28 3 11 50 0 53 0 29 50 1 53 0 30 52 2 53 1 3 1 28 3 28 3 11) [set!
  lambda f-body])  assert #function(#array(uint8 62 1 50 0 53 0 44 50 1 50 2 50 3 53 0 28 2 28 2 28 2 28 4 11) [if
  raise quote assert-failed])  catch #function(#array(uint8 62 2 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 50 0 54 0 1 50 1 53 0 28 1 50 2 50 3 50 4 53 0 28 2 50 5 50 6 53 0 28 2 50 7 50 8 28 2 28 3 50 5 50 9 53 0 28 2 54 0 0 28 3 28 4 50 10 53 0 28 2 50 11 53 0 28 2 28 4 28 3 28 3 11) [trycatch
  lambda if and pair? eq car quote thrown-value cadr caddr raise]) gensym])  label #function(#array(uint8 62 2 50 0 53 0 28 1 50 1 53 0 53 1 28 3 28 3 45 28 2 11) [lambda
  set!])  do #function(#array(uint8 63 2 50 0 52 1 3 0 53 1 29 52 2 52 3 53 0 3 2 52 2 52 4 53 0 3 2 52 2 50 5 60 53 0 3 2 64 4 6 11) [#function(#array(uint8 65 6 50 0 53 0 50 1 53 2 50 2 53 1 52 3 50 4 28 1 52 5 54 0 1 30 3 1 3 2 52 3 50 4 28 1 52 5 54 0 2 3 1 52 3 53 0 28 1 52 5 53 4 3 1 3 2 28 1 3 3 28 4 28 3 28 2 28 1 52 3 53 0 28 1 52 5 53 3 3 1 3 2 28 3 11) [letrec
  lambda if nconc begin copy-list]) gensym map car cadr #function(#array(uint8 62 1 52 0 53 0 3 1 23 6 19 0 52 1 53 0 4 1 11 53 0 29 11) [cddr
  caddr])])  let #function(#array(uint8 63 1 50 0 45 64 4 2 11) [#function(#array(uint8 65 2 54 0 0 20 6 33 0 54 0 0 57 0 2 54 0 1 29 58 0 0 2 54 0 1 30 58 0 1 5 34 0 45 2 50 0 50 1 52 2 50 3 60 54 0 0 3 2 52 4 54 0 1 3 1 28 3 52 2 50 5 60 54 0 0 3 2 64 4 3 11) [#function(#array(uint8 65 3 54 0 0 6 20 0 50 0 54 0 0 53 0 28 3 5 22 0 53 0 53 1 27 11) [label])
  lambda map #function(#array(uint8 62 1 53 0 23 6 12 0 53 0 29 11 53 0 11) [])
  f-body #function(#array(uint8 62 1 53 0 23 6 15 0 52 0 53 0 4 1 11 45 11) [cadr])])])  throw #function(#array(uint8 62 2 50 0 50 1 50 2 50 3 28 2 53 0 53 1 28 4 28 2 11) [raise
  list quote thrown-value])  time #function(#array(uint8 62 1 50 0 52 1 3 0 64 4 2 11) [#function(#array(uint8 65 2 50 0 53 0 50 1 28 1 28 2 28 1 50 2 54 0 0 50 3 50 4 50 5 50 1 28 1 53 0 28 3 50 6 28 4 28 3 28 3 11) [let
  time.now prog1 princ "Elapsed time: " - " seconds\n"]) gensym]))
*print-width*
80
*print-pretty*
#t
*banner*
";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
