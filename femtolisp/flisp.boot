zero?
#function("n1e0_V;" [])
vector.map
#function("n2b0d1e131p42;" [#function("q2b0d1e031p42;" [#function("q2_f00`S2b0lr2e0;" [#function("n1f00e0f20f21e0Z31[;" [])])
							vector.alloc]) length])
vector->list
#function("n1b0d1e031^p43;" [#function("q3`e0b0lr2e1;" [#function("n1f10f00e0S2Zf01Kj01;" [])])
			     length])
untrace
#function("n1b0d1e031p42;" [#function("q2e0Mb0=6U0d1f00d2d3d4d5e03131313142;];" [trace-lambda
  set-top-level-value! cadr caar last-pair caddr]) top-level-value])
transpose
#function("n1d0d1e0K<;" [mapcar list])
trace
#function("n1b0d1e031p322b2;" [#function("q2b0d1e031p42;" [#function("q2b0d1e031p42;" [#function("q2f10Mb0=A6\x930d1f20b0f00d2b3L1b4b5L2L1b6b7f20L2L2L1d8d9b:le03231b4b;L2L1d2b7f10L2L1d8e03132L136L342;];" [trace-lambda
  set-top-level-value! nconc begin princ "(" print quote copy-list map #function("n1b0b1b2L2b3e0L2L3;" [begin
  princ " " print]) ")\n"]) to-proper]) cadr]) top-level-value ok])
to-proper
#function("n1e0B6;0e0;e0@6F0e0L1;e0Md0e0N31K;" [to-proper])
table.pairs
#function("n1d0b1l^e043;" [table.foldl #function("n3e0e1Ke2K;" [])])
table.keys
#function("n1d0b1l^e043;" [table.foldl #function("n3e0e2K;" [])])
table.invert
#function("n1b0d130p42;" [#function("q2d0b1l^f00332e0;" [table.foldl #function("n3d0f00e1e043;" [put!])])
			  table])
table.foreach
#function("n2d0b1l^e143;" [table.foldl #function("n3f00e0e1322\\;" [])])
table.values
#function("n1d0b1l^e043;" [table.foldl #function("n3e1e2K;" [])])
table.clone
#function("n1b0d130p42;" [#function("q2d0b1l^f00332e0;" [table.foldl #function("n3d0f00e0e143;" [put!])])
			  table])
symbol-syntax
#function("n1d0d1e0]43;" [get *syntax-environment*])
string.tail
#function("n2d0e0d1e0_e133d2e03143;" [string.sub string.inc sizeof])
string.trim
#function("n3b0]]p43;" [#function("q3b0li02b1li12b2d3f0031p42;" [#function("n4e2e3W16J02d0e1d1e0e232326a0f00e0e1d2e0e232e344;e2;" [string.find
  string.char string.inc]) #function("n3d0e2_3216R02d1e1d2e0d3e0e23232326g0f01e0e1d3e0e23243;e2;" [>
  string.find string.char string.dec]) #function("q2d0f10f00f10f11_e034f01f10f12e03343;" [string.sub])
								 length])])
string.map
#function("n2b0d130d2e131p43;" [#function("q3b0_p322d1e041;" [#function("q2]e0f01W6a02d0f00f10d1f11e03231322d2f11e032i0530;" [io.putc
  string.char string.inc]) io.tostring!]) buffer length])
string.join
#function("n2e0B6;0b0;b1d230p42;" ["" #function("q2d0e0f00M322d1b2lf00N322d3e041;" [io.write
  for-each #function("n1d0f00f11322d0f00e042;" [io.write]) io.tostring!])
				   buffer])
string.rep
#function("n2e1a4W6q0d0e1_326G0b1;e1`V6U0d2e041;e1a2V6f0d2e0e042;d2e0e0e043;d3e1316\x8c0d2e0d4e0e1`S23242;d4d2e0e032e1a2U242;" [<=
  "" string odd? string.rep])
splice-form?
#function("n1e0G16K02e0Mb0=17K02e0Mb1=17U02e0b2=;" [*comma-at* *comma-dot*
						    *comma*])
set-syntax!
#function("n2d0d1e0e143;" [put! *syntax-environment*])
separate
#function("n2f00e0e1^^44;" [] #0=[#function("n4e1B6>0e2e3K;e0e1M316[0f00e0e1Ne1Me2Ke344;\\6r0f00e0e1Ne2e1Me3K44;];" [] #0#)
				  ()])
reverse
#function("n1d0d1^e043;" [foldl cons])
revappend
#function("n2d0d1e031e142;" [nconc reverse])
remainder
#function("n2e0e0e1U2e1T2S2;" [])
ref-uint32-LE
#function("n2d0e0e1_R2Z_32d0e0e1`R2Za832d0e0e1a2R2Za@32d0e0e1a3R2ZaH32R4;" [ash])
ref-uint16-LE
#function("n2d0e0e1_R2Z_32d0e0e1`R2Za832R2;" [ash])
repl
#function("n0b0]]p43;" [#function("q3b0li02b1li12e1302d240;" [#function("n0d0b1312d2d3312b4b5lb6lmp42;" [princ
  "> " io.flush *output-stream* #function("q2d0d131A16I02b2d3e031p42;" [io.eof?
  *input-stream* #function("q2d0e0312e0h12\\;" [print that]) load-process])
  #function("n0d040;" [read]) #function("n1d0d1312d2e041;" [io.discardbuffer
							    *input-stream*
							    raise])])
							      #function("n0b0lb1lm6G0d2302f0140;];" [#function("n0f003016@02d040;" [newline])
  #function("n1d0e041;" [print-exception]) newline]) newline])])
self-evaluating?
#function("n1e0@16>02e0DA17_02d0e03116_02e0D16_02e0d1e031=;" [constant?
							      top-level-value])
quote-value
#function("n1d0e0316>0e0;b1e0L2;" [self-evaluating? quote])
procedure?
#function("n1e0H17C02d0e031b1=;" [typeof function])
println
#function("o0d0e0Qd1302;" [print newline])
print-to-string
#function("n1b0d130p42;" [#function("q2d0e0f00322d1e041;" [io.print
							   io.tostring!])
			  buffer])
print-exception
#function("n1e0G16M02e0Mb0=16M02d1e0a4326x0d2d3b4d5e031b6d7e031b8362d9d3d:e03132591e0G16\x8f02e0Mb;=16\x8f02e0NG6\xa50d2d3b<d5e031b=34591e0G16\xb302e0Mb>=6\xcb0d2d3b?322d2d3e0NKQ591e0G16\xd902e0Mb@=6\xf80dAd7e031312d2d3bBd5e03133591dCe03116\x0b02d1e0a2326(0d2d3e0MbD332bEd5e031p32591d2d3bF322d9d3e0322d2d3dG322\\;" [type-error
  length= io.princ *stderr* "type-error: " cadr ": expected " caddr ", got "
  io.print cadddr unbound-error "unbound-error: eval: variable " " has no value"
  error "error: " load-error print-exception "in file " list? ": " #function("q2d0e03117@02e0D6H0d15J0d2d3e042;" [string?
  io.princ io.print *stderr*]) "*** Unhandled exception: " *linefeed*])
print
#function("o0d0d1e0K<;" [io.print *output-stream*])
positive?
#function("n1d0e0_42;" [>])
princ
#function("o0d0d1e0K<;" [io.princ *output-stream*])
peephole
#function("n1e0;" [])
pad-l
#function("n3d0d1e2e1d2e031S232e042;" [string string.rep length])
pad-r
#function("n3d0e0d1e2e1d2e031S23242;" [string string.rep length])
odd?
#function("n1d0e031A;" [even?])
nreverse
#function("n1b0^p42;" [#function("q2]f00G6R02f00Nf00e0f00i02P2j005302e0;" [])])
nreconc
#function("n2d0d1e031e142;" [nconc nreverse])
nlist*
#function("o0e0N@6=0e0M;e0d0e0NQP;" [nlist*])
negative?
#function("n1e0_W;" [])
nestlist
#function("n3d0e2_326>0^;e1d1e0e0e131e2`S233K;" [<= nestlist])
newline
#function("n0d0d1312\\;" [princ *linefeed*])
memv
#function("n2e1@6:0];e1Me0>6F0e1;\\6T0d0e0e1N42;];" [memv])
mod
#function("n2e0e0e1U2e1T2S2;" [])
member
#function("n2e1@6:0];e1Me0?6F0e1;\\6T0d0e0e1N42;];" [member])
mark-label
#function("n2d0e0d1e143;" [emit :label])
map!
#function("n2e1]e1G6O02e1e0e1M31O2e1Ni15502;" [])
map-int
#function("n2d0e1_326>0^;b1e0_31^K^p43;" [<= #function("q3e0i12`f01`S2b0lr2e0;" [#function("n1f01f10e031^KP2f01Nj01;" [])])])
make-system-image
#function("n1b0d1e0d2d3d434b5p43;" [#function("q3d0b1ld230322d3e041;" [for-each
  #function("n1e0F16m02d0e031A16m02d1e031HA16m02d2e0f0132A16m02d3d1e03131A6\x9c0d4f00e0322d5f00b6322d4f00d1e031322d5f00b642;];" [constant?
  top-level-value memq iostream? io.print io.write "\n"]) environment io.close])
				    file :write :create :truncate (*linefeed*
								   *directory-separator*
								   *argv* that)])
make-enum-table
#function("n2b0d130p42;" [#function("q2_d0d1f013131b2lr;" [1- length #function("n1d0f00f11e0Zf10e0R243;" [put!])])
			  table])
macroexpand-1
#function("n1e0@6;0e0;b0d1e031p42;" [#function("q2e06?0e0f00N<;f00;" [])
				     macrocall?])
macroexpand
#function("n1d0e0^42;" [macroexpand-in])
lookup-sym
#function("n4e1B6;0b0;b1e1Mp42;" [(global)
				  #function("q2b0d1f00e0_33p42;" [#function("q2e06N0f136D0b0e0L2;b1f12e0L3;d2f10f11Nf1317c02f00B6l0f125r0f12`R2]44;" [arg
  closed lookup-sym]) index-of])])
macrocall?
#function("n1e0MD16E02d0d1e0M]43;" [get *syntax-environment*])
macroexpand-in
#function("n2e0@6;0e0;b0d1e0Me132p42;" [#function("q2e06M0d0d1e031f00NQd2e03142;b3d4f0031p42;" [macroexpand-in
  cadr caddr #function("q2e06F0d0e0f10NQf1142;f10Mb1=6T0f10;f10Mb2=6\x810d3b2d4f1031d0d5f1031f1132d6f103144;f10Mb7=6\xa30b8d4f1031d9d:f103131p43;d;b<lf1042;" [macroexpand-in
  quote lambda nlist* cadr caddr cdddr let-syntax #function("q3d0e1d1d2b3le032f213242;" [macroexpand-in
  nconc map #function("n1e0Md0d1e031f3132f31L3;" [macroexpand-in cadr])])
  f-body cddr map #function("n1d0e0f2142;" [macroexpand-in])]) macrocall?])
					assq])
make-code-emitter
#function("n0^d030_Y3;" [table])
make-label
#function("n1d040;" [gensym])
map
#function("n2e1@6;0e1;e0e1M31d0e0e1N32K;" [map])
mapcar
#function("o1f00e0e142;" [] #0=[#function("n2e1B6=0e040;e1M@6H0e1M;\\6h0e0d0d1e132Qf00e0d0d2e13232K;];" [map
  car cdr] #0#) ()])
listp
#function("n1e0B17=02e0G;" [])
load
#function("n1b0d1e0d232p42;" [#function("q2b0lb1lm;" [#function("n0b0]p32]]]43;" [#function("q2b0li0;" [#function("n3d0f1031A6R0f00d1f1031e0d2e13143;d3f10312d2e141;" [io.eof?
  read load-process io.close])])]) #function("n1d0f00312d1b2f10e0L341;" [io.close
  raise load-error])]) file :read])
list-tail
#function("n2d0e1_326?0e0;d1e0Ne1`S242;" [<= list-tail])
list-ref
#function("n2d0e0e132M;" [list-tail])
list-head
#function("n2d0e1_326>0^;e0Md1e0Ne1`S232K;" [<= list-head])
list*
#function("o0e0N@6=0e0M;e0Md0e0NQK;" [list*])
list->vector
#function("n1d0e0<;" [vector])
list-part-
#function("n5e0@6O0d0e2_326L0d1e331e4K;e4;d2e2e1326n0d3e0e1_^d1e331e4K45;d3e0Ne1`e2R2e0Me3Ke445;" [>
  nreverse >= list-part-])
list-partition
#function("n2d0e1_326C0d1b241;d3d4e0e1_^^3541;" [<= error "list-partition: invalid count"
						 nreverse list-part-])
list?
#function("n1e0B17I02e0G16I02d0e0N41;" [list?])
load-process
#function("n1d0e041;" [eval])
length>
#function("n2e1_W6<0e0;e1_V6N0e0G16M02e0;e0B6Y0e1_W;d0e0Ne1`S242;" [length>])
length=
#function("n2e1_W6;0];e1_V6F0e0B;e0B6Q0e1_V;d0e0Ne1`S242;" [length=])
lastcdr
#function("n1e0@6;0e0;d0e0N41;" [lastcdr])
last-pair
#function("n1e0@6;0e0;e0N@6E0e0;\\6Q0d0e0N41;];" [last-pair])
iota
#function("n1d0d1e042;" [map-int identity])
just-compile-args
#function("n3d0b1le142;" [for-each #function("n1d0f00f02]e044;" [compile-in])])
io.readline
#function("n1d0e0b142;" [io.readuntil #\x000a])
in-env?
#function("n2e1G16Q02d0e0e1M3217Q02d1e0e1N42;" [memq in-env?])
hex5
#function("n1d0d1e0a@32a5b243;" [pad-l number->string #\0])
identity
#function("n1e0;" [])
index-of
#function("n3e1B6:0];e0e1M=6F0e2;\\6Y0d0e0e1Ne2`R243;];" [index-of])
get-defined-vars
#function("n1d0f00e03141;" [delete-duplicates] #0=[#function("n1e0@6:0^;e0Mb0=16I02e0NG6\x8c0d1e031D16`02d1e031L117\x8b02d1e031G16\x8502d2e031D16\x8502d2e031L117\x8b02^;e0Mb3=6\xa30d4d5f00e0N32<;^;" [define
  cadr caadr begin append map] #0#) ()])
function?
#function("n1e0H17C02d0e031b1=;" [typeof function])
for-each
#function("n2e1G6J0e0e1M312d0e0e1N42;\\;" [for-each])
foldr
#function("n3e2B6;0e1;e0e2Md0e0e1e2N3342;" [foldr])
foldl
#function("n3e2B6;0e1;d0e0e0e2Me132e2N43;" [foldl])
filter
#function("n2f00e0e1^43;" [] #0=[#function("n3e1B6;0e2;e0e1M316V0f00e0e1Ne1Me2K43;\\6g0f00e0e1Ne243;];" [] #0#)
				 ()])
f-body
#function("n1b0f00e031p42;" [#function("q2b0d1e031p42;" [#function("q2e0B6<0f00;b0e0f00L3d1b2le032K;" [lambda
  map #function("n1];" [])]) get-defined-vars])] [#function("n1e0@6:0];e0N^=6F0e0M;\\6P0b0e0K;];" [begin])
						  ()])
expand
#function("n1d0e041;" [macroexpand])
every
#function("n2e1@17O02e0e1M3116O02d0e0e1N42;" [every])
even?
#function("n1d0e0`32_V;" [logand])
eval
#function("n1d0d1e0313140;" [compile-thunk expand])
error
#function("o0d0b1e0K41;" [raise error])
encode-byte-code
#function("n1b0d1d2e03131p42;" [#function("q2b0d1d2e031a3d3b4le032T2R2b532p42;" [#function("q2b0d1f0031p42;" [#function("q2b0d1e031_d230d230d330]p47;" [#function("q7]e1e0W6\xc002f00e1Zi52e5d0=6n0d1e2f00e1`R2Zd2e431332e1a2R2i15\xbd0d3e4d4d5d6f1016\x8802d7e5b8326\x950b9e5p325\x970e53231322e1`R2i12e1e0W6\xbc0b:f00e1Zp325\xbd0]5302d;b<le3322d=e441;" [:label
  put! sizeof io.write byte get Instructions memq (:jmp :brt :brf)
  #function("q2e0d0>6=0d1;e0d2>6H0d3;e0d4>6S0d5;];" [:jmp :jmp.l :brt :brt.l
						     :brf :brf.l])
  #function("q2b0f05p42;" [#function("q2d0e0b1326V0d2f14d3f0031322f11`R2j11;d0e0b4326z0d2f14d5f0031322f11`R2j11;d0e0b6326\xbb0d2f14d5f0031322f11`R2j112d2f14d5f20f11Z31322f11`R2j11;d0e0b7326\xfa0d8f13d9f1431f00332d2f14f306\xe80d35\xea0d:_31322f11`R2j11;];" [memv
  (:loadv.l :loadg.l :setg.l) io.write uint32 (:loada :seta :call :tcall :loadv
					       :loadg :setg :list :+ :- :* :/
					       :vector :argc :vargc :loadi8
					       :let) uint8 (:loadc :setc)
  (:jmp :brf :brt) put! sizeof uint16])]) table.foreach #function("n2d0f04e0322d1f04f206L0d25N0d3d4f02e1323142;" [io.seek
  io.write uint32 uint16 get]) io.tostring!]) length table buffer])
  list->vector]) >= length count #function("n1d0e0b142;" [memq (:loadv :loadg
  :setg :jmp :brt :brf)]) 65536]) peephole nreverse])
emit-nothing
#function("n1e0;" [])
emit
#function("o2d0e1b1326I0b2e0`Zp325J0]2e0_d3e1e2Ke0_Z32[2e0;" [memq (:loadv
								    :loadg
								    :setg)
							      #function("q2b0f00a2Zp42;" [#function("q2b0f12Mp42;" [#function("q2b0d1f10e0326L0d2f10e0325i0d3f10e0f00332f00`R2j002f00`S2p42;" [#function("q2f30a2f10[2e0L1j322d0e0b1326[0b2f31p32j31;];" [>=
  256 #function("q2e0d0>6=0d1;e0d2>6H0d3;e0d4>6S0d5;];" [:loadv :loadv.l :loadg
							 :loadg.l :setg :setg.l])])
  has? get put!])])]) nreconc])
display
#function("n1d0e0312\\;" [princ])
disassemble-
#function("n2b0d1e031p42;" [#function("q2b0e0_Ze0`Zp43;" [#function("q3b0]p42;" [#function("q2b0li02b1_d2f0031p43;" [#function("n1d0e03116A02e0HA6X0d1b2312d3e0f31`R242;d4e041;" [function?
  princ "\n" disassemble- print]) #function("q3]e0e1W6P02b0d1d2f10e0Z32p32530;" [#function("q2d0f00_326D0d1305E0]2_f41`S2b2lr2d3d4f0031b5d6d7e031`32b8342f00`R2j002b9e0p42;" [>
  newline #function("n1d0b141;" [princ "\t"]) princ hex5 ":  " string.tail
  string "\t" #function("q2d0e0b1326\\0f20f31d2f30f1032Z312f10a4R2j10;d0e0b3326\x820f20f31f30f10ZZ312f10`R2j10;d0e0b4326\xa70d5d6f30f10Z31312f10`R2j10;d0e0b7326\xe80d5d6f30f10Z31b8322f10`R2j102d5d6f30f10Z31312f10`R2j10;d0e0b9326\x130d5b:d;d<f30f103231322f10a2R2j10;d0e0b=326>1d5b:d;d2f30f103231322f10a4R2j10;];" [memv
  (:loadv.l :loadg.l :setg.l) ref-uint32-LE (:loadv :loadg :setg)
  (:loada :seta :call :tcall :list :+ :- :* :/ :vector :argc :vargc :loadi8
   :let) princ number->string (:loadc :setc) " " (:jmp :brf :brt) "@" hex5
  ref-uint16-LE (:jmp.l :brf.l :brt.l)])]) get 1/Instructions]) length])])])
			    function->vector])
disassemble
#function("n1d0e0_322d140;" [disassemble- newline])
delete-duplicates
#function("n1e0@6;0e0;b0e0Me0Np43;" [#function("q3d0e0e1326D0d1e141;e0d1e131K;" [member
  delete-duplicates])])
count-
#function("n3e1B6;0e2;d0e0e1Ne0e1M316T0e2`R25V0e243;" [count-])
copy-tree
#function("n1e0@6;0e0;d0e0M31d0e0N31K;" [copy-tree])
count
#function("n2d0e0e1_43;" [count-])
copy-list
#function("n1e0@6;0e0;e0Md0e0N31K;" [copy-list])
const-to-idx-vec
#function("n1b0e0`Ze0a2Zp43;" [#function("q3b0d1e131p42;" [#function("q2d0b1lf00322e0;" [table.foreach
  #function("n2f00e1e0[;" [])]) vector.alloc])])
cond-clauses->if
#function("n1e0@6:0];b0e0Mp42;" [#function("q2e0Mb0=6B0b1e0NK;b2e0Mb1e0NKd3f00N31L4;" [else
  begin if cond-clauses->if])])
cond->if
#function("n1d0e0N41;" [cond-clauses->if])
compile-while
#function("n4b0d1e031d1e031p43;" [#function("q3d0f00f01]]342d1f00e0322d0f00f01]f02342d2f00d3e1332d2f00d4322d0f00f01]f03342d2f00d5e0332d1f00e142;" [compile-in
  mark-label emit :brf :pop :jmp]) make-label])
compile-sym
#function("n4b0d1e2e1_\\34p42;" [#function("q2b0e0Mp42;" [#function("q2e0b0>6N0d1f10f13_Zd2f003143;e0b3>6q0d1f10f13`Zd2f0031d4f003144;d1f10f13a2Zf1243;" [arg
  emit cadr closed caddr])]) lookup-sym])
compile-prog1
#function("n3d0e0e1]d1e231342d2e231G6e0d3e0e1]d2e231342d4e0d542;];" [compile-in
  cadr cddr compile-begin emit :pop])
compile-or
#function("n4d0e0e1e2e3]d146;" [compile-short-circuit :brt])
compile-let
#function("n4b0e3Me3Np43;" [#function("q3d0e1d1d2e03131326I0]5U0d3d4b5e032312d6f00d7d8f01e0\\33332b9d:f00f01e133p42;" [length=
  length cadr error string "apply: incorrect number of arguments to " emit
  :loadv compile-f #function("q2d0f10d1322d0f10f126L0d25N0d3`e0R243;" [emit
  :close :tcall :call]) compile-arglist])])
compile-short-circuit
#function("n6e3@6E0d0e0e1e2e444;e3N@6Z0d0e0e1e2e3M44;b1d2e031p42;" [compile-in
								    #function("q2d0f00f01]f03M342d1f00d2322d1f00f05e0332d1f00d3322d4f00f01f02f03Nf04f05362d5f00e042;" [compile-in
  emit :dup :pop compile-short-circuit mark-label]) make-label])
compile-thunk
#function("n1d0b1^e0L341;" [compile lambda])
compile-if
#function("n4b0d1e031d1e031p43;" [#function("q3d0f00f01]d1f0331342d2f00d3e0332d0f00f01f02d4f0331342f026x0d2f00d5325\x830d2f00d6e1332d7f00e0322d0f00f01f02d8f0331G6\xae0d9f03315\xaf0]342d7f00e142;" [compile-in
  cadr emit :brf caddr :ret :jmp mark-label cdddr cadddr]) make-label])
compile-for
#function("n5d0e4316h0d1e0e1]e2342d1e0e1]e3342d1e0e1]e4342d2e0d342;d4b541;" [1arg-lambda?
  compile-in emit :for error "for: third form must be a 1-argument lambda"])
compile-call
#function("n4b0e3Mp42;" [#function("q2b0e0D16e02d1e0f0132A16e02e0F16e02d2e03116e02d3e031H6q0d3e0315s0e0p42;" [#function("q2b0e0H16B02d1e031p42;" [#function("q2e0A6I0d0f20f21]f00345J0]2b1d2f20f21f23N33p42;" [compile-in
  #function("q2f006H0b0d1d2f00]33p42;d3f30f326X0d45Z0d5e043;" [#function("q2e016D02d0f43Ne032A6S0d1f20e0325T0]2b2f10p42;" [length=
  argc-error #function("q2e0d0>6Z0f10_V6L0d1f50d242;d1f50f20f1043;e0d3>6\x820f10_V6t0d1f50d442;d1f50f20f1043;e0d5>6\xa90f10_V6\x9b0d6f30`42;d1f50f20f1043;e0d7>6\xd10f10_V6\xc30d1f50d842;d1f50f20f1043;e0d9>6\xf80f10_V6\xea0d6f30`42;d1f50f20f1043;e0d:>6\"0f10_V6\x140d1f50d;b<43;d1f50f20f1043;d1f50f5216512f20d==6=1d>5@1f2042;" [:list
  emit :loadnil :+ :load0 :- argc-error :* :load1 :/ :vector :loadv [] :apply
  :tapply])]) get arg-counts emit :tcall :call]) compile-arglist])
  builtin->instruction]) in-env? constant? top-level-value])])
compile-begin
#function("n4e3@6D0d0e0e1e2]44;e3N@6Y0d0e0e1e2e3M44;d0e0e1]e3M342d1e0d2322d3e0e1e2e3N44;" [compile-in
  emit :pop compile-begin])
compile-and
#function("n4d0e0e1e2e3\\d146;" [compile-short-circuit :brf])
compile-app
#function("n4b0e3Mp42;" [#function("q2e0G16O02e0Mb0=16O02d1d2e031316c0d3f00f01f02f0344;d4f00f01f02f0344;" [lambda
  list? cadr compile-let compile-call])])
compile
#function("n1d0^e042;" [compile-f])
compile-arglist
#function("n3b0d1e2d232p42;" [#function("q2e06i0d0f00d1f02d232f01332b3d4d5b6ld7e0d23232Kp322d2`R2;d0f00f02f01332d8f0241;" [just-compile-args
  list-head MAX_ARGS #function("q2d0f10f11]e044;" [compile-in]) nconc map #function("n1d0e0K;" [list])
  list-partition length]) length> MAX_ARGS])
compile-f
#function("o2b0d130d2e131p43;" [#function("q3f02BA6O0d0e0d1d2d3e13131335\x820d4e131B6j0d0e0d5d3e131335\x820d0e0d6e1@6z0_5\x800d3e131332d7e0d8e131f00K\\d9f0131342d0e0d:322d;d<e0_Z31d=e03142;" [emit
  :let 1+ length lastcdr :argc :vargc compile-in to-proper caddr :ret function
  encode-byte-code const-to-idx-vec]) make-code-emitter cadr])
compile-in
#function("n4e3D6E0d0e0e1e3b144;e3@6\xd10e3_=6[0d2e0d342;e3`=6k0d2e0d442;e3\\=6{0d2e0d542;e3]=6\x8b0d2e0d642;e3^=6\x9b0d2e0d742;e3J16\xb802d8e3a\xb03216\xb802d9e3a\xaf326\xc60d2e0d:e343;d2e0d;e343;b<e3Mp42;" [compile-sym
  [:loada :loadc :loadg] emit :load0 :load1 :loadt :loadf :loadnil >= <=
  :loadi8 :loadv #function("q2e0b0>6K0d1f00d2d3f033143;e0b4>6h0d5f00f01f02d6f033144;e0b7>6\x810d8f00f01f02f0344;e0b9>6\x9b0d:f00f01f02f03N44;e0b;>6\xb10d<f00f01f0343;e0b=>6\xd70d1f00d2d>f01f0332332d1f00d?42;e0b@>6\xf10dAf00f01f02f03N44;e0bB>6\x0b0dCf00f01f02f03N44;e0bD>6/0dEf00f01d3f0331b9dFf0331K44;e0bG>6W1dHf00f01d3f0331dIf0331dJf033145;e0bK>6\x861d5f00f01]dIf0331342dLf00f01d3f0331bM44;e0bN>6\xdc1d5f00f01]b=^d3f0331L3342dOdIf0331316\xb81]5\xbe1dPbQ312d5f00f01]dIf0331342d1f00dR42;dSf00f01f02f0344;" [quote
  emit :loadv cadr cond compile-in cond->if if compile-if begin compile-begin
  prog1 compile-prog1 lambda compile-f :closure and compile-and or compile-or
  while compile-while cddr for compile-for caddr cadddr set! compile-sym [:seta
  :setc :setg] trycatch 1arg-lambda? error "trycatch: second form must be a 1-argument lambda"
  :trycatch compile-app])])
char?
#function("n1d0e031b1=;" [typeof wchar])
cddr
#function("n1e0NN;" [])
cdar
#function("n1e0MN;" [])
cdaar
#function("n1e0MMN;" [])
cdadr
#function("n1e0NMN;" [])
cddar
#function("n1e0MNN;" [])
cdddr
#function("n1e0NNN;" [])
cadar
#function("n1e0MNM;" [])
cadddr
#function("n1e0NNNM;" [])
caddr
#function("n1e0NNM;" [])
caaar
#function("n1e0MMM;" [])
caadr
#function("n1e0NMM;" [])
caar
#function("n1e0MM;" [])
cadr
#function("n1e0NM;" [])
builtin->instruction
#function("n1b0d1d2b3e03231p42;" [#function("q2d0d1e03216A02e0;" [has?
								  Instructions])
				  intern string #\:])
bq-process
#function("n1d0e0316T0e0I6Q0b1d2d3e03131p42;e0;e0@6a0b4e0L2;e0Mb5=6y0d2d2d6e0313141;e0Mb7=6\x890d6e041;d8d9e032A6\xa90b:d;e031d<d=e032p43;\\6\xb60b>e0^p43;];" [self-evaluating?
  #function("q2e0Mb0=6B0d1e0NK;d2d1e0L3;" [list vector apply]) bq-process
  vector->list quote backquote cadr *comma* any splice-form? #function("q3e0B6>0b0e1K;d1b2e1Kd3e031L142;" [list
  nconc nlist* bq-process]) lastcdr map bq-bracket1 #function("q3]e0G16B02e0Mb0=A6[02d1e0M31e1Ki12e0Ni05302b2e0G6u0d3e1d4e031L1325\x9a0e0B6\x840d5e1315\x9a0\\6\x990d3e1d6e031L1325\x9a0]p42;" [*comma*
  bq-bracket #function("q2e0NB6=0e0M;b0e0K;" [nconc]) nreconc cadr nreverse
  bq-process])])
bq-bracket
#function("n1e0@6C0d0d1e031L2;e0Mb2=6W0d0d3e031L2;e0Mb4=6k0b5d3e031L2;e0Mb6=6{0d3e041;\\6\x8a0d0d1e031L2;];" [list
  bq-process *comma* cadr *comma-at* copy-list *comma-dot*])
bq-bracket1
#function("n1e0G16@02e0Mb0=6J0d1e041;d2e041;" [*comma* cadr bq-process])
assv
#function("n2e1@6:0];d0e131e0>6J0e1M;\\6X0d1e0e1N42;];" [caar assv])
assoc
#function("n2e1@6:0];d0e131e0?6J0e1M;\\6X0d1e0e1N42;];" [caar assoc])
argc-error
#function("n2d0d1b2e0b3e1e1`V6J0b45L0b53541;" [error string "compile error: "
					       " expects " " argument." " arguments."])
arg-counts
#table(:not 1  :set-cdr! 2  :cons 2  :number? 1  :equal? 2  :cdr 1  :vector? 1  :eqv? 2  :apply 2  := 2  :atom? 1  :aref 2  :compare 2  :< 2  :null? 1  :eq? 2  :car 1  :set-car! 2  :builtin? 1  :aset! 3  :bound? 1  :boolean? 1  :pair? 1  :symbol? 1  :fixnum? 1)
append2
#function("n2e0B6;0e1;e0Md0e0Ne132K;" [append2])
any
#function("n2e1G16O02e0e1M3117O02d0e0e1N42;" [any])
abs
#function("n1e0_W6>0e0S1;e0;" [])
__script
#function("n1b0lb1lm;" [#function("n0d0f0041;" [load])
			#function("n1d0e0312d1`41;" [print-exception exit])])
__init_globals
#function("n0d0b1=17K02d0b2=17K02d0b3=6Z0b4h52b6h75c0b8h52b9h72d:h;2d<h=;" [*os-name*
  win32 win64 windows "\\" *directory-separator* "\r\n" *linefeed* "/" "\n"
  *stdout* *output-stream* *stdin* *input-stream*])
__start
#function("n1d0302e0NG6Q0e0Nh12d2d3e031315a0e0h12d4d5312d6302d7_41;" [__init_globals
  *argv* __script cadr princ *banner* repl exit])
append
#function("o0e0B6:0^;e0NB6E0e0M;\\6W0d0e0Md1e0NQ42;];" [append2 append])
MAX_ARGS
127
Instructions
#table(:nop 0  :tapply 12  :set-cdr! 32  :/ 37  :setc 58  :cons 27  :equal? 15  :cdr 30  :call 3  :eqv? 14  := 38  :setg.l 59  :list 28  :atom? 16  :aref 42  :load0 47  :let 65  :argc 62  :< 39  :null? 18  :loadg 52  :load1 48  :car 29  :brt.l 10  :vargc 63  :loada 53  :set-car! 31  :setg 56  :aset! 43  :bound? 22  :pair? 23  :symbol? 20  :fixnum? 26  :loadi8 49  :not 17  :* 36  :pop 2  :loadnil 46  :brf 6  :vector 41  :- 35  :loadv 50  :closure 60  :number? 21  :trycatch 61  :loadv.l 51  :vector? 25  :brf.l 9  :seta 57  :apply 33  :dup 1  :for 66  :loadc 54  :compare 40  :eq? 13  :+ 34  :jmp 5  :loadt 44  :brt 7  :builtin? 24  :loadg.l 55  :close 64  :tcall 4  :ret 11  :boolean? 19  :loadf 45  :jmp.l 8)
>=
#function("n2e1e0W17A02e0e1V;" [])
>
#function("n2e1e0W;" [])
<=
#function("n2e0e1W17A02e0e1V;" [])
1arg-lambda?
#function("n1e0G16e02e0Mb0=16e02e0NG16e02d1e031G16e02d2d1e031`42;" [lambda cadr
								    length=])
1/Instructions
#table(2 :pop  45 :loadf  59 :setg.l  15 :equal?  38 :=  50 :loadv  61 :trycatch  14 :eqv?  30 :cdr  40 :compare  11 :ret  28 :list  48 :load1  22 :bound?  36 :*  60 :closure  41 :vector  0 :nop  29 :car  56 :setg  23 :pair?  17 :not  4 :tcall  43 :aset!  3 :call  58 :setc  21 :number?  8 :jmp.l  39 :<  63 :vargc  51 :loadv.l  53 :loada  66 :for  44 :loadt  65 :let  55 :loadg.l  5 :jmp  27 :cons  46 :loadnil  34 :+  6 :brf  16 :atom?  42 :aref  10 :brt.l  31 :set-car!  25 :vector?  54 :loadc  13 :eq?  19 :boolean?  47 :load0  12 :tapply  32 :set-cdr!  62 :argc  20 :symbol?  26 :fixnum?  35 :-  9 :brf.l  7 :brt  37 :/  18 :null?  52 :loadg  49 :loadi8  1 :dup  24 :builtin?  64 :close  33 :apply  57 :seta)
/=
#function("n2e0e1VA;" [])
1+
#function("n1e0`R2;" [])
1-
#function("n1e0`S2;" [])
*whitespace*
"\t\n\v\f\r \u0085  ᠎           \u2028\u2029  　"
*syntax-environment*
#table(define #function("o1e0D6B0b0e0e1ML3;b0e0Mb1e0Nd2e131L3L3;" [set! lambda
								   f-body])  letrec #function("o1b0d1d2e032d3d4d1b5le032e13231L3d1b6le032K;" [lambda
  map car f-body nconc #function("n1b0e0K;" [set!])
  #function("n1];" [])])  backquote #function("n1d0e041;" [bq-process])  assert #function("n1b0e0\\b1b2b3e0L2L2L2L4;" [if
  raise quote assert-failed])  label #function("n2b0e0L1b1e0e1L3L3]L2;" [lambda
  set!])  do #function("o2b0d130e1Md2d3e032d2d4e032d2b5le032p46;" [#function("q6b0e0b1e2b2e1d3b4L1d5f01N3132d3b4L1d5f0231d3e0L1d5e43132L133L4L3L2L1d3e0L1d5e33132L3;" [letrec
  lambda if nconc begin copy-list]) gensym map car cadr #function("n1d0e031G6C0d1e041;e0M;" [cddr
  caddr])])  when #function("o1b0e0d1e131]L4;" [if f-body])  dotimes #function("o1b0e0Md1e031p43;" [#function("q3b0_b1e1`L3b2e0L1d3f0131L3L4;" [for
  - lambda f-body]) cadr])  unwind-protect #function("n2b0d130p42;" [#function("q2b0b1f00b2e0L1b3f01b4e0L2L3L3L3f01L3;" [prog1
  trycatch lambda begin raise]) gensym])  define-macro #function("o1b0b1e0ML2b2e0Nd3e131L3L3;" [set-syntax!
  quote lambda f-body])  unless #function("o1b0e0]d1e131L4;" [if f-body])  let #function("o1b0]p42;" [#function("q2f00D6Q0f00i02f01Mj002f01Nj015R0]2b0b1d2b3lf0032d4f0131L3d2b5lf0032p43;" [#function("q3f006D0b0f00e0L35F0e0e1K;" [label])
  lambda map #function("n1e0G6<0e0M;e0;" []) f-body #function("n1e0G6?0d0e041;];" [cadr])])])  throw #function("n2b0b1b2b3L2e0e1L4L2;" [raise
  list quote thrown-value])  time #function("n1b0d130p42;" [#function("q2b0e0b1L1L2L1b2f00b3b4b5b1L1e0L3b6L4L3L3;" [let
  time.now prog1 princ "Elapsed time: " - " seconds\n"]) gensym])  let* #function("o1e0@6?0d0e141;b1d2e031L1d3b4L1e0NL1d5e13133L3d6e031L2;" [f-body
  lambda caar nconc let* copy-list cadar])  case #function("o1b0]p42;" [#function("q2b0li02b1d230p42;" [#function("n2e1b0=6=0b0;e1B6E0];e1@6X0b1e0d2e131L3;e1NB6m0b1e0d2e1M31L3;b3e0b4e1L2L3;" [else
  eqv? quote-value memv quote]) #function("q2b0e0f10L2L1d1b2L1d3d4b5lf11323132L3;" [let
  nconc cond copy-list map #function("n1f10f00e0M32e0NK;" [])]) gensym])])  catch #function("n2b0d130p42;" [#function("q2b0f01b1e0L1b2b3b4e0L2b5b6e0L2b7b8L2L3b5b9e0L2f00L3L4b:e0L2b;e0L2L4L3L3;" [trycatch
  lambda if and pair? eq car quote thrown-value cadr caddr raise]) gensym]))
*print-width*
80
*print-pretty*
#t
*banner*
";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
