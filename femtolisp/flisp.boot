zero?
#function("n1f0`W;" [])
vector.map
#function("n2c0e1f131q42;" [#function("rc0e1f031q42;" [#function("r`g00avc0ms2f0;" [#function("n1g00f0g20g21f0[31\\;" [])]) vector.alloc]) length])
vector->list
#function("n1c0e1f031_q43;" [#function("raf0c0ms2f1;" [#function("n1g10g00f0v[g01Kk01;" [])]) length])
untrace
#function("n1c0e1f031q42;" [#function("re0f0316K0e1g00e2f031b2[42;^;" [traced? set-top-level-value! function:vals]) top-level-value])
transpose
#function("n1e0e1f0t3;" [mapcar list])
traced?
#function("n1e0f031e0g0031>;" [function:code] #0=[#function("o0e0c1f0K312c2f0t2;" [println x #.apply] #0#) ()])
trace
#function("n1c0e1f031q322c2;" [#function("rc0e130q42;" [#function("re0g0031@6p0e1g10e2c3f0c4c5c6c7g10L2f0L3L2c8c7g00L2f0L3L3L33142;^;" [traced? set-top-level-value! eval lambda begin println cons quote apply]) gensym]) top-level-value ok])
to-proper
#function("n1f0A6;0f0;f0?6F0f0L1;f0Me0f0N31K;" [to-proper])
table.values
#function("n1e0c1m_f043;" [table.foldl #function("n3f1f2K;" [])])
table.pairs
#function("n1e0c1m_f043;" [table.foldl #function("n3f0f1Kf2K;" [])])
table.keys
#function("n1e0c1m_f043;" [table.foldl #function("n3f0f2K;" [])])
table.invert
#function("n1c0e130q42;" [#function("re0c1m_g00332f0;" [table.foldl #function("n3e0g00f1f043;" [put!])]) table])
table.foreach
#function("n2e0c1m_f143;" [table.foldl #function("n3g00f0f1322];" [])])
table.clone
#function("n1c0e130q42;" [#function("re0c1m_g00332f0;" [table.foldl #function("n3e0g00f0f143;" [put!])]) table])
symbol-syntax
#function("n1e0e1f0^43;" [get *syntax-environment*])
string.trim
#function("n3c0^^q43;" [#function("rc0mj02c1mj12c2e3g0031q42;" [#function("n4f2f3X16J02e0f1e1f0f232326a0g00f0f1e2f0f232f344;f2;" [string.find string.char string.inc]) #function("n3e0f2`3216R02e1f1e2f0e3f0f23232326g0g01f0f1e3f0f23243;f2;" [> string.find string.char string.dec]) #function("re0g10g00g10g11`f034g01g10g12f03343;" [string.sub]) length])])
string.tail
#function("n2e0f0e1f0`f13342;" [string.sub string.inc])
string.rpad
#function("n3e0f0e1f2f1e2f031v3242;" [string string.rep length])
string.rep
#function("n2f1b4X6q0e0f1`326G0c1;f1aW6U0e2f041;f1b2W6f0e2f0f042;e2f0f0f043;e3f1316\x8b0e2f0e4f0f1av3242;e4e2f0f032f1b2U242;" [<= "" string odd? string.rep])
string.map
#function("n2c0e130e2f131q43;" [#function("rc0`q322e1f041;" [#function("r^f0g01X6`02e0g00g10e1g11f03231322e2g11f032j0520;" [io.putc string.char string.inc]) io.tostring!]) buffer length])
string.lpad
#function("n3e0e1f2f1e2f031v32f042;" [string string.rep length])
string.join
#function("n2f0A6;0c0;c1e230q42;" ["" #function("re0f0g00M322e1c2mg00N322e3f041;" [io.write for-each #function("n1e0g00g11322e0g00f042;" [io.write]) io.tostring!]) buffer])
splice-form?
#function("n1f0F16K02f0Mc0<17K02f0Mc1<17U02f0c2<;" [*comma-at* *comma-dot* *comma*])
simple-sort
#function("n1f0A17>02f0NA6D0f0;c0f0Mq42;" [#function("rc0e1c2mg00N32q42;" [#function("re0e1f0M31g00L1e1f0N3143;" [nconc simple-sort]) separate #function("n1f0g00X;" [])])])
set-syntax!
#function("n2e0e1f0f143;" [put! *syntax-environment*])
separate
#function("n2g00f0f1__44;" [] #0=[#function("n4f1A6>0f2f3K;f0f1M316[0g00f0f1Nf1Mf2Kf344;g00f0f1Nf2f1Mf3K44;" [] #0#) ()])
self-evaluating?
#function("n1f0?16>02f0C@17_02e0f03116_02f0C16_02f0e1f031<;" [constant? top-level-value])
reverse
#function("n1e0e1_f043;" [foldl cons])
revappend
#function("n2e0e1f031f142;" [nconc reverse])
repl
#function("n0c0^^q43;" [#function("rc0mj02c1mj12f1302e240;" [#function("n0e0c1312e2e3312c4c5mc6mpq42;" [princ "> " io.flush *output-stream* #function("re0e131@16H02c2e3f031q42;" [io.eof? *input-stream* #function("re0f0312f0i12];" [print that]) load-process]) #function("n0e040;" [read]) #function("n1e0e1312e2f041;" [io.discardbuffer *input-stream* raise])]) #function("n0c0mc1mp6G0e2302g0140;^;" [#function("n0g003016@02e040;" [newline]) #function("n1e0f041;" [print-exception]) newline]) newline])])
remainder
#function("n2f0f0f1Vf1T2v;" [])
ref-uint32-LE
#function("n2e0f0f1`u[`32e0f0f1au[b832e0f0f1b2u[b@32e0f0f1b3u[bH32R4;" [ash])
ref-uint16-LE
#function("n2e0f0f1`u[`32e0f0f1au[b832u;" [ash])
random
#function("n1e0f0316F0e1e230f042;e330f0T2;" [integer? mod rand rand.double])
quote-value
#function("n1e0f0316>0f0;c1f0L2;" [self-evaluating? quote])
println
#function("o0e0f0Q2e1302;" [print newline])
print-to-string
#function("n1c0e130q42;" [#function("re0f0g00322e1f041;" [io.print io.tostring!]) buffer])
print-exception
#function("n1c0^^q43;" [#function("rc0mj02c1mj12g00F16[02g00Mc2<16[02e3g00b4326\x850f0c4e5g0031c6e7g0031c8352f1e9g0031315H1g00F16\x9f02g00Mc:<16\x9f02g00NF6\xb40f0c;e5g0031c<335H1g00F16\xc402g00Mc=<6\xd90f0c>312f0g00NQ25H1g00F16\xe902g00Mc?<6\x080e@e7g0031312f0cAe5g0031325H1eBg003116\x1d02e3g00b2326:1f0g00McC322cDe5g0031q325H1f0cE312f1g00312f0eF312];" [#function("o0e0e1f0t3;" [io.princ *error-stream*]) #function("o0e0e1f0t3;" [io.print *error-stream*]) type-error length= "type-error: " cadr ": expected " caddr ", got " cadddr unbound-error "unbound-error: eval: variable " " has no value" error "error: " load-error print-exception "in file " list? ": " #function("re0f03117?02f0C6H0g005K0g01f041;" [string?]) "*** Unhandled exception: " *linefeed*])])
print
#function("o0e0e1f0t3;" [io.print *output-stream*])
princ
#function("o0e0e1f0t3;" [io.princ *output-stream*])
positive?
#function("n1e0f0`42;" [>])
peephole
#function("n1f0;" [])
odd?
#function("n1e0f031@;" [even?])
nreverse
#function("n1c0_q42;" [#function("r^g00F6Q02g00Ng00f0g00j02P2k005202f0;" [])])
nreconc
#function("n2e0e1f031f142;" [nconc nreverse])
nlist*
#function("o0f0N?6=0f0M;f0e0f0NQ2P;" [nlist*])
newline
#function("n0e0e1312];" [princ *linefeed*])
nestlist
#function("n3e0f2`326>0_;f1e1f0f0f131f2av33K;" [<= nestlist])
negative?
#function("n1f0`X;" [])
mod0
#function("n2f0f0f1Vf1T2v;" [])
mod
#function("n2f0e0f0f132f1T2v;" [div])
memv
#function("n2f1?6:0^;f1Mf0=6F0f1;e0f0f1N42;" [memv])
member
#function("n2f1?6:0^;f1Mf0>6F0f1;e0f0f1N42;" [member])
mark-label
#function("n2e0f0e1f143;" [emit :label])
mapcar
#function("o1g00f0f142;" [] #0=[#function("n2f1A6=0f040;f1M?6H0f1M;f0e0e1f132Q2g00f0e0e2f13232K;" [map car cdr] #0#) ()])
map-int
#function("n2e0f1`326>0_;c1f0`31_K_q43;" [<= #function("rf0j12ag01avc0ms2f0;" [#function("n1g01g10f031_KP2g01Nk01;" [])])])
map!
#function("n2f1^f1F6O02f1f0f1M31O2f1Nj15502;" [])
map
#function("n2f1?6;0f1;f0f1M31e0f0f1N32K;" [map])
make-system-image
#function("n1c0e1f0e2e3e434c5e6q44;" [#function("r^i02c1c2mq42;" [*print-pretty* #function("rc0mc1mpf0302;" [#function("n0e0c1me2e3e430313142;" [for-each #function("n1f0E16m02e0f031@16m02e1f031G@16m02e2f0g1132@16m02e3e1f03131@6\x9c0e4g10f0322e5g10c6322e4g10e1f031322e5g10c642;^;" [constant? top-level-value memq iostream? io.print io.write "\n"]) nreverse simple-sort environment]) #function("n1g00302e0f041;" [raise])]) #function("n0e0g00312g02i1;" [io.close *print-pretty*])]) file :write :create :truncate (*linefeed* *directory-separator* *argv* that *print-pretty* *print-width* *print-readably*) *print-pretty*])
make-label
#function("n1e040;" [gensym])
make-enum-table
#function("n2c0e130q42;" [#function("r`e0e1g013131c2ms;" [1- length #function("n1e0g00g11f0[g10f0u43;" [put!])]) table])
make-code-emitter
#function("n0_e030`Z3;" [table])
macroexpand-in
#function("n2f0?6;0f0;c0e1f0Mf132q42;" [#function("rf06M0e0e1f031g00NQ2e2f03142;c3e4g0031q42;" [macroexpand-in cadr caddr #function("rf06F0e0f0g10NQ2g1142;g10Mc1<6T0g10;g10Mc2<6\x920c3e4g1031F6\x8d0e5g1031F6\x830c6e4g1031K5\x8a0e7g10315\x8e0^q42;g10Mc8<6\xc10c9e:g1031e;c2L1_L1e<e4g10313133L1q43;e=c>mg1042;" [macroexpand-in quote lambda #function("rc0e1f031e2f0g2132q43;" [#function("re0c1e2g3031f0A6G0f15Y0c1f0f1L3e3c4mf032Ke5g303144;" [nlist* lambda cadr map #function("n1^;" []) cdddr]) get-defined-vars macroexpand-in]) cddr cdddr begin caddr let-syntax #function("re0f1e1e2c3mf032g213242;" [macroexpand-in nconc map #function("n1f0Me0e1f031g3132g31L3;" [macroexpand-in cadr])]) cadr nconc copy-list map #function("n1e0f0g2142;" [macroexpand-in])]) macrocall?]) assq])
macroexpand-1
#function("n1f0?6;0f0;c0e1f031q42;" [#function("rf06?0f0g00Nt2;g00;" []) macrocall?])
macroexpand
#function("n1e0f0_42;" [macroexpand-in])
macrocall?
#function("n1f0MC16E02e0e1f0M^43;" [get *syntax-environment*])
lookup-sym
#function("n4f1A6;0c0;c1f1Mq42;" [(global) #function("rc0e1g00f0`33q42;" [#function("rf06M0g136C0c0f0L2;c1g12f0L3;e2g10g11Ng1317b02g00A6k0g125p0g12au^44;" [arg closed lookup-sym]) index-of])])
load-process
#function("n1e0f041;" [eval])
load
#function("n1c0e1f0e232q42;" [#function("rc0mc1mp;" [#function("n0c0^q32^^^43;" [#function("rc0mj0;" [#function("n3e0g1031@6R0g00e1g1031f0e2f13143;e3g10312e2f141;" [io.eof? read load-process io.close])])]) #function("n1e0g00312e1c2g10f0L341;" [io.close raise load-error])]) file :read])
list?
#function("n1f0A17I02f0F16I02e0f0N41;" [list?])
list-tail
#function("n2e0f1`326?0f0;e1f0Nf1av42;" [<= list-tail])
list-ref
#function("n2e0f0f132M;" [list-tail])
list-partition
#function("n2c0^q42;" [#function("rc0mj02e1g01`326I0e2c341;e4f0g00g01`__3541;" [#function("n5f0?6O0e0f2`326L0e1f331f4K;f4;e2f2f1326o0g00f0f1`_e1f331f4K45;g00f0Nf1af2uf0Mf3Kf445;" [> nreverse >=]) <= error "list-partition: invalid count" nreverse])])
list-head
#function("n2e0f1`326>0_;f0Me1f0Nf1av32K;" [<= list-head])
list->vector
#function("n1e0f0t2;" [vector])
list*
#function("o0f0N?6=0f0M;f0Me0f0NQ2K;" [list*])
length>
#function("n2f1`X6<0f0;f1`W6N0f0F16M02f0;f0A6Y0f1`X;e0f0Nf1av42;" [length>])
length=
#function("n2f1`X6;0^;f1`W6F0f0A;f0A6Q0f1`W;e0f0Nf1av42;" [length=])
lastcdr
#function("n1f0?6;0f0;e0f0N41;" [lastcdr])
last-pair
#function("n1f0?6;0f0;f0N?6E0f0;e0f0N41;" [last-pair])
just-compile-args
#function("n3e0c1mf142;" [for-each #function("n1e0g00g02^f044;" [compile-in])])
iota
#function("n1e0e1f042;" [map-int identity])
io.readline
#function("n1e0f0c142;" [io.readuntil #\x000a])
index-of
#function("n3f1A6:0^;f0f1M<6F0f2;e0f0f1Nf2au43;" [index-of])
in-env?
#function("n2e0c1mf142;" [any #function("n1e0g00f042;" [memq])])
identity
#function("n1f0;" [])
hex5
#function("n1e0e1f0b@32b5c243;" [string.lpad number->string #\0])
get-defined-vars
#function("n1e0g00f03141;" [delete-duplicates] #0=[#function("n1f0?6:0_;f0Mc0<16I02f0NF6\x8c0e1f031C16`02e1f031L117\x8b02e1f031F16\x8502e2f031C16\x8502e2f031L117\x8b02_;f0Mc3<6\xa40e4e5g00f0N32t2;_;" [define cadr caadr begin append map] #0#) ()])
for-each
#function("n2f1F6J0f0f1M312e0f0f1N42;];" [for-each])
foldr
#function("n3f2A6;0f1;f0f2Me0f0f1f2N3342;" [foldr])
foldl
#function("n3f2A6;0f1;e0f0f0f2Mf132f2N43;" [foldl])
fits-i8
#function("n1f0I16O02e0f0b\xb03216O02e1f0b\xaf42;" [>= <=])
filter
#function("n2g00f0f1_43;" [] #0=[#function("n3f1A6;0f2;f0f1M316V0g00f0f1Nf1Mf2K43;g00f0f1Nf243;" [] #0#) ()])
expand
#function("n1e0f041;" [macroexpand])
every
#function("n2f1?17O02f0f1M3116O02e0f0f1N42;" [every])
even?
#function("n1e0f0a32`W;" [logand])
eval
#function("n1e0e1f0313140;" [compile-thunk expand])
error
#function("o0e0c1f0K41;" [raise error])
encode-byte-code
#function("n1c0e1e2f03131q42;" [#function("rc0e1e2f031b3e3c4mf032T2uc532q42;" [#function("rc0e1g0031q42;" [#function("rc0e1f031`e230e230e330^q47;" [#function("r^f1f0X6\xbc02g00f1[j52f5e0<6k0e1f2g00f1au[e2f431332f1b2uj15\xb90e3f4e4e5e6g1016\x8502e7f5c8326\x920c9f5q325\x940f53231322f1auj12f1f0X6\xb80c:g00f1[q325\xb90^5202e;c<mf3322e=f441;" [:label put! sizeof io.write byte get Instructions memq (:jmp :brt :brf) #function("rf0e0=6<0e1;f0e2=6G0e3;f0e4=6R0e5;^;" [:jmp :jmp.l :brt :brt.l :brf :brf.l]) #function("rc0g05q42;" [#function("re0f0c1326T0e2g14e3g0031322g11auk11;e0f0c4326w0e2g14e5g0031322g11auk11;e0f0c6326\xb60e2g14e5g0031322g11auk112e2g14e5g20g11[31322g11auk11;e0f0c7326\xf40e8g13e9g1431g00332e2g14g306\xe30e35\xe50e:`31322g11auk11;^;" [memv (:loadv.l :loadg.l :setg.l) io.write uint32 (:loada :seta :call :tcall :loadv :loadg :setg :list :+ :- :* :/ :vector :argc :vargc :loadi8 :apply :tapply) uint8 (:loadc :setc) (:jmp :brf :brt) put! sizeof uint16])]) table.foreach #function("n2e0g04f0322e1g04g206L0e25N0e3e4g02f1323142;" [io.seek io.write uint32 uint16 get]) io.tostring!]) length table buffer]) list->vector]) >= length count #function("n1e0f0c142;" [memq (:loadv :loadg :setg :jmp :brt :brf)]) 65536]) peephole nreverse])
emit-nothing
#function("n1f0;" [])
emit
#function("o2e0f1c1326I0c2f0a[q325J0^2f0`e3f1f2Kf0`[32\\2f0;" [memq (:loadv :loadg :setg) #function("rc0g00b2[q42;" [#function("rc0g12Mq42;" [#function("rc0e1g10f0326K0e2g10f0325f0e3g10f0g00332g00auk002g00avq42;" [#function("rg30b2g10\\2f0L1k322e0f0c1326Z0c2g31q32k31;^;" [>= 256 #function("rf0e0=6<0e1;f0e2=6G0e3;f0e4=6R0e5;^;" [:loadv :loadv.l :loadg :loadg.l :setg :setg.l])]) has? get put!])])]) nreconc])
div
#function("n2f0f1Vf0`X16Q02f1`X16J02a17Q02b/17W02`u;" [])
display
#function("n1e0f0312];" [princ])
disassemble
#function("o1f1A6J0e0f0`322e1302];5K0^2c2f1Me3f031e4f031q44;" [disassemble newline #function("rc0^q42;" [#function("rc0mj02c1`e2g0131q43;" [#function("n1f0J16>02f0G@6T0e0c1312e2f0g10au42;e3f041;" [princ "\n" disassemble print]) #function("r^f0f1X6M02c0e1c2m^e333q32520;" [#function("re0g00`326C0e1305D0^2`g20avc2ms2e3e4g0031c5e6e7f031a32c8342g00auk002c9f0q42;" [> newline #function("n1e0c141;" [princ "\t"]) princ hex5 ":  " string.tail string "\t" #function("re0f0c1326Z0g20g32e2g31g1032[312g10b4uk10;e0f0c3326\x7f0g20g32g31g10[[312g10auk10;e0f0c4326\xa30e5e6g31g10[31312g10auk10;e0f0c7326\xe20e5e6g31g10[31c8322g10auk102e5e6g31g10[31312g10auk10;e0f0c9326\x0c0e5c:e;e<g31g103231322g10b2uk10;e0f0c=32661e5c:e;e2g31g103231322g10b4uk10;^;" [memv (:loadv.l :loadg.l :setg.l) ref-uint32-LE (:loadv :loadg :setg) (:loada :seta :call :tcall :list :+ :- :* :/ :vector :argc :vargc :loadi8 :apply :tapply) princ number->string (:loadc :setc) " " (:jmp :brf :brt) "@" hex5 ref-uint16-LE (:jmp.l :brf.l :brt.l)])]) table.foldl #function("n3f217J02f1g21g00[<16J02f0;" []) Instructions]) length])]) function:code function:vals])
delete-duplicates
#function("n1f0?6;0f0;c0f0Mf0Nq43;" [#function("re0f0f1326C0e1f141;f0e1f131K;" [member delete-duplicates])])
count
#function("n2c0^q42;" [#function("rc0mj02f0g00g01`43;" [#function("n3f1A6;0f2;g00f0f1Nf0f1M316T0f2au5V0f243;" [])])])
copy-tree
#function("n1f0?6;0f0;e0f0M31e0f0N31K;" [copy-tree])
copy-list
#function("n1f0?6;0f0;f0Me0f0N31K;" [copy-list])
const-to-idx-vec
#function("n1c0e1f0b2[31q42;" [#function("re0c1mg00a[322f0;" [table.foreach #function("n2g00f1f0\\;" [])]) vector.alloc])
compile-while
#function("n4c0e1f031e1f031q43;" [#function("re0g00g01^^342e1g00f0322e0g00g01^g02342e2g00e3f1332e2g00e4322e0g00g01^g03342e2g00e5f0332e1g00f142;" [compile-in mark-label emit :brf :pop :jmp]) make-label])
compile-thunk
#function("n1e0c1_f0L341;" [compile lambda])
compile-sym
#function("n4c0e1f2f1`]34q42;" [#function("rc0f0Mq42;" [#function("rf0c0=6M0e1g10g13`[e2g003143;f0c3=6p0e1g10g13a[e2g0031e4g003144;e1g10g13b2[g1243;" [arg emit cadr closed caddr])]) lookup-sym])
compile-short-circuit
#function("n6f3?6E0e0f0f1f2f444;f3N?6Z0e0f0f1f2f3M44;c1e2f031q42;" [compile-in #function("re0g00g01^g03M342e1g00e2322e1g00g05f0332e1g00e3322e4g00g01g02g03Ng04g05362e5g00f042;" [compile-in emit :dup :pop compile-short-circuit mark-label]) make-label])
compile-prog1
#function("n3e0f0f1^e1f231342e2f231F6e0e3f0f1^e2f231342e4f0e542;^;" [compile-in cadr cddr compile-begin emit :pop])
compile-or
#function("n4e0f0f1f2f3^e146;" [compile-short-circuit :brt])
compile-let
#function("n4c0f3Mf3Nq43;" [#function("re0f1e1e2f03131326H0^5T0e3e4c5f032312e6g00e7e8g01f0]33332c9e:g00g01f133q42;" [length= length cadr error string "apply: incorrect number of arguments to " emit :loadv compile-f #function("re0g10e1322e0g10g126K0e25M0e3af0u43;" [emit :copyenv :tcall :call]) compile-arglist])])
compile-in
#function("n4f3C6E0e0f0f1f3c144;f3?6\xba0f3`<6[0e2f0e342;f3a<6k0e2f0e442;f3]<6{0e2f0e542;f3^<6\x8b0e2f0e642;f3_<6\x9b0e2f0e742;e8f3316\xaf0e2f0e9f343;e2f0e:f343;c;f3Mq42;" [compile-sym [:loada :loadc :loadg] emit :load0 :load1 :loadt :loadf :loadnil fits-i8 :loadi8 :loadv #function("rf0c0=6J0e1g00e2e3g033143;f0c4=6c0e5g00g01g02g0344;f0c6=6}0e7g00g01g02g03N44;f0c8=6\x930e9g00g01g0343;f0c:=6\xb90e1g00e2e;g01g0332332e1g00e<42;f0c==6\xd30e>g00g01g02g03N44;f0c?=6\xed0e@g00g01g02g03N44;f0cA=6\x110eBg00g01e3g0331c6eCg0331K44;f0cD=691eEg00g01e3g0331eFg0331eGg033145;f0cH=6^1eIg00g01]e3g0331342e1g00eJ42;f0cK=6\x8d1eIg00g01^eFg0331342eLg00g01e3g0331cM44;f0cN=6\xe31eIg00g01^c:_e3g0331L3342eOeFg0331316\xbf1^5\xc51ePcQ312eIg00g01^eFg0331342e1g00eR42;eSg00g01g02g0344;" [quote emit :loadv cadr if compile-if begin compile-begin prog1 compile-prog1 lambda compile-f :closure and compile-and or compile-or while compile-while cddr for compile-for caddr cadddr return compile-in :ret set! compile-sym [:seta :setc :setg] trycatch 1arg-lambda? error "trycatch: second form must be a 1-argument lambda" :trycatch compile-app])])
compile-if
#function("n4c0e1f031e1f031q43;" [#function("re0g00g01^e1g0331342e2g00e3f0332e0g00g01g02e4g0331342g026w0e2g00e5325\x820e2g00e6f1332e7g00f0322e0g00g01g02e8g0331F6\xad0e9g03315\xae0^342e7g00f142;" [compile-in cadr emit :brf caddr :ret :jmp mark-label cdddr cadddr]) make-label])
compile-for
#function("n5e0f4316h0e1f0f1^f2342e1f0f1^f3342e1f0f1^f4342e2f0e342;e4c541;" [1arg-lambda? compile-in emit :for error "for: third form must be a 1-argument lambda"])
compile-f
#function("o2c0e130e2f131q43;" [#function("rg02A@6D0e0f0e1325w0e2f131A6_0e0f0e3e4f131335w0e0f0e5f1?6o0`5u0e4f131332e6f0e7f131g00K]e8g0131342e0f0e9322e:e;f0`[31e<f03142;" [emit :let lastcdr :argc length :vargc compile-in to-proper caddr :ret function encode-byte-code const-to-idx-vec]) make-code-emitter cadr])
compile-call
#function("n4c0f3Mq42;" [#function("rc0f0C16d02e1f0g0132@16d02f0E16d02e2f03116d02e3f031G6p0e3f0315r0f0q42;" [#function("rc0f0G16A02e1f031q42;" [#function("rf0@6H0e0g20g21^g00345I0^2c1e2g20g21g23N33q42;" [compile-in #function("rg006G0c0e1e2g00^33q42;e3g30g326W0e45Y0e5f043;" [#function("rf016C02e0g43Nf032@6R0e1g20f0325S0^2c2g10q42;" [length= argc-error #function("rf0e0=6Y0g10`W6K0e1g50e242;e1g50g20g1043;f0e3=6\x940g10`W6s0e1g50e442;g10b2W6\x860e1g50e542;e1g50g20g1043;f0e6=6\xe00g10`W6\xad0e7g30a42;g10aW6\xbf0e1g50e842;g10b2W6\xd20e1g50e942;e1g50g20g1043;f0e:=6\x080g10`W6\xfa0e1g50e;42;e1g50g20g1043;f0e<=6/0g10`W6!0e7g30a42;e1g50g20g1043;f0e==6Y1g10`W6K1e1g50e>c?43;e1g50g20g1043;f0e@=6\x8c1g10b2X6t1e7g30b242;e1g50g526\x841eA5\x861e@g1043;e1g50g2042;" [:list emit :loadnil :+ :load0 :add2 :- argc-error :neg :sub2 :* :load1 :/ :vector :loadv [] :apply :tapply])]) get arg-counts emit :tcall :call]) compile-arglist]) builtin->instruction]) in-env? constant? top-level-value])])
compile-begin
#function("n4f3?6D0e0f0f1f2^44;f3N?6Y0e0f0f1f2f3M44;e0f0f1^f3M342e1f0e2322e3f0f1f2f3N44;" [compile-in emit :pop compile-begin])
compile-arglist
#function("n3c0e1f2e232q42;" [#function("rf06g0e0g00e1g02e232g01332c3e4e5c6me7f0e23232Kq322e2au;e0g00g02g01332e8g0241;" [just-compile-args list-head MAX_ARGS #function("re0g10g11^f044;" [compile-in]) nconc map #function("n1e0f0K;" [list]) list-partition length]) length> MAX_ARGS])
compile-app
#function("n4c0f3Mq42;" [#function("rf0F16N02f0Mc0<16N02e1e2f031316b0e3g00g01g02g0344;e4g00g01g02g0344;" [lambda list? cadr compile-let compile-call])])
compile-and
#function("n4e0f0f1f2f3]e146;" [compile-short-circuit :brf])
compile
#function("n1e0_f042;" [compile-f])
char?
#function("n1e0f031c1<;" [typeof wchar])
cddr
#function("n1f0NN;" [])
cdddr
#function("n1f0NNN;" [])
cddar
#function("n1f0MNN;" [])
cdar
#function("n1f0MN;" [])
cdadr
#function("n1f0NMN;" [])
cdaar
#function("n1f0MMN;" [])
cadr
#function("n1f0NM;" [])
caddr
#function("n1f0NNM;" [])
cadddr
#function("n1f0NNNM;" [])
cadar
#function("n1f0MNM;" [])
caar
#function("n1f0MM;" [])
caadr
#function("n1f0NMM;" [])
caaar
#function("n1f0MMM;" [])
builtin->instruction
#function("n1c0e1e2c3f03231q42;" [#function("re0e1f03216@02f0;" [has? Instructions]) intern string #\:])
bq-process
#function("n1e0f0316T0f0H6Q0c1e2e3f03131q42;f0;f0?6a0c4f0L2;f0Mc5<6y0e2e2e6f0313141;f0Mc7<6\x890e6f041;e8e9f032@6\xa90c:e;f031e<e=f032q43;c>f0_q43;" [self-evaluating? #function("rf0Mc0<6A0e1f0NK;e2e1f0L3;" [list vector apply]) bq-process vector->list quote backquote cadr *comma* any splice-form? #function("rf0A6=0c0f1K;e1c2f1Ke3f031L142;" [list nconc nlist* bq-process]) lastcdr map bq-bracket1 #function("r^f0F16A02f0Mc0<@6Z02e1f0M31f1Kj12f0Nj05202c2f0F6t0e3f1e4f031L1325\x910f0A6\x830e5f1315\x910e3f1e6f031L132q42;" [*comma* bq-bracket #function("rf0NA6<0f0M;c0f0K;" [nconc]) nreconc cadr nreverse bq-process])])
bq-bracket1
#function("n1f0F16@02f0Mc0<6J0e1f041;e2f041;" [*comma* cadr bq-process])
bq-bracket
#function("n1f0?6C0e0e1f031L2;f0Mc2<6W0e0e3f031L2;f0Mc4<6k0c5e3f031L2;f0Mc6<6{0e3f041;e0e1f031L2;" [list bq-process *comma* cadr *comma-at* copy-list *comma-dot*])
assv
#function("n2f1?6:0^;e0f131f0=6J0f1M;e1f0f1N42;" [caar assv])
assoc
#function("n2f1?6:0^;e0f131f0>6J0f1M;e1f0f1N42;" [caar assoc])
array?
#function("n1f0H17E02c0e1f031q42;" [#function("rf0F16?02f0Mc0<;" [array]) typeof])
argc-error
#function("n2e0e1c2f0c3f1f1aW6J0c45L0c53541;" [error string "compile error: " " expects " " argument." " arguments."])
arg-counts
#table(:not 1  :set-cdr! 2  :cons 2  :number? 1  :equal? 2  :cdr 1  :vector? 1  :eqv? 2  := 2  :div0 2  :atom? 1  :aref 2  :compare 2  :< 2  :null? 1  :eq? 2  :car 1  :set-car! 2  :builtin? 1  :aset! 3  :bound? 1  :boolean? 1  :pair? 1  :symbol? 1  :fixnum? 1)
append2
#function("n2f0A6;0f1;f0Me0f0Nf132K;" [append2])
append
#function("o0f0A6:0_;f0NA6E0f0M;e0f0Me1f0NQ242;" [append2 append])
any
#function("n2f1F16O02f0f1M3117O02e0f0f1N42;" [any])
abs
#function("n1f0`X6=0f0w;f0;" [])
__start
#function("n1e0302f0NF6Q0f0Ni12e2e3f031315a0f0i12e4e5312e6302e7`41;" [__init_globals *argv* __script cadr princ *banner* repl exit])
__script
#function("n1c0mc1mp;" [#function("n0e0g0041;" [load]) #function("n1e0f0312e1a41;" [print-exception exit])])
__init_globals
#function("n0e0c1<17K02e0c2<17K02e0c3<6Z0c4i52c6i75c0c8i52c9i72e:i;2e<i=2e>i?;" [*os-name* win32 win64 windows "\\" *directory-separator* "\r\n" *linefeed* "/" "\n" *stdout* *output-stream* *stdin* *input-stream* *stderr* *error-stream*])
MAX_ARGS
127
Instructions
#table(:sub2 70  :nop 0  :set-cdr! 32  :/ 37  :setc 59  :tapply 68  :cons 27  dummy_nil 74  :equal? 14  :cdr 30  :call 3  :eqv? 13  := 39  :setg.l 60  :list 28  :atom? 15  :aref 43  :load0 48  :let 66  dummy_t 72  :argc 62  :< 40  :null? 17  :loadg 53  :load1 49  :car 29  :brt.l 10  :vargc 63  :loada 54  :set-car! 31  :setg 57  :aset! 44  :bound? 21  :pair? 22  :symbol? 19  :fixnum? 25  :loadi8 50  :not 16  :* 36  :neg 71  :pop 2  :loadnil 47  :brf 6  :vector 42  :- 35  :loadv 51  :closure 61  dummy_f 73  :number? 20  :trycatch 64  :add2 69  :loadv.l 52  :vector? 24  :brf.l 9  :seta 58  :apply 33  :dup 1  :div0 38  :copyenv 65  :for 67  :loadc 55  :compare 41  :eq? 12  :function? 26  :+ 34  :jmp 5  :loadt 45  :brt 7  :builtin? 23  :loadg.l 56  :tcall 4  :ret 11  :boolean? 18  :loadf 46  :jmp.l 8)
>=
#function("n2f1f0X17A02f0f1W;" [])
>
#function("n2f1f0X;" [])
<=
#function("n2f0f1X17A02f0f1W;" [])
1arg-lambda?
#function("n1f0F16e02f0Mc0<16e02f0NF16e02e1f031F16e02e2e1f031a42;" [lambda cadr length=])
1-
#function("n1f0av;" [])
1+
#function("n1f0au;" [])
/=
#function("n2f0f1W@;" [])
*whitespace*
"\t\n\v\f\r \u0085  ᠎           \u2028\u2029  　"
*syntax-environment*
#table(define #function("o1f0C6B0c0f0f1ML3;c0f0Mc1f0Nf1KKL3;" [set! lambda])  letrec #function("o1c0e1e2f032e3e1c4mf032f132KKe1c5mf032K;" [lambda map car nconc #function("n1c0f0K;" [set!]) #function("n1^;" [])])  backquote #function("n1e0f041;" [bq-process])  assert #function("n1c0f0]c1c2c3f0L2L2L2L4;" [if raise quote assert-failed])  label #function("n2c0f0L1c1f0f1L3L3^L2;" [lambda set!])  do #function("o2c0e130f1Me2e3f032e2e4f032e2c5mf032q46;" [#function("rc0f0c1f2c2f1e3c4L1e5g01N3132e3c4L1e5g0231e3f0L1e5f43132L133L4L3L2L1e3f0L1e5f33132L3;" [letrec lambda if nconc begin copy-list]) gensym map car cadr #function("n1e0f031F6C0e1f041;f0M;" [cddr caddr])])  when #function("o1c0f0c1f1K^L4;" [if begin])  unwind-protect #function("n2c0e130e130q43;" [#function("rc0f1c1_g01L3L2L1c2c3g00c1f0L1c4f1L1c5f0L2L3L3L3f1L1L3L3;" [let lambda prog1 trycatch begin raise]) gensym])  dotimes #function("o1c0f0Me1f031q43;" [#function("rc0`c1f1aL3e2c3L1f0L1L1e4g013133L4;" [for - nconc lambda copy-list]) cadr])  define-macro #function("o1c0c1f0ML2c2f0Nf1KKL3;" [set-syntax! quote lambda])  unless #function("o1c0f0^c1f1KL4;" [if begin])  let #function("o1c0^q42;" [#function("rg00C6P0g00j02g01Mk002g01Nk015Q0^2c0c1e2c3mg0032g01KKe2c4mg0032q43;" [#function("rg006C0c0g00f0L35E0f0f1K;" [label]) lambda map #function("n1f0F6<0f0M;f0;" []) #function("n1f0F6?0e0f041;^;" [cadr])])])  cond #function("o0c0^q42;" [#function("rc0mj02f0g0041;" [#function("n1f0?6:0^;c0f0Mq42;" [#function("rf0Mc0<17A02f0M]<6K0c1f0NK;c2f0Mc1f0NKg10g00N31L4;" [else begin if])])])])  throw #function("n2c0c1c2c3L2f0f1L4L2;" [raise list quote thrown-value])  time #function("n1c0e130q42;" [#function("rc0f0c1L1L2L1c2g00c3c4c5c1L1f0L3c6L4L3L3;" [let time.now prog1 princ "Elapsed time: " - " seconds\n"]) gensym])  let* #function("o1f0?6L0e0c1L1_L1e2f13133L1;e0c1L1e3f031L1L1e2f0NF6}0e0c4L1f0NL1e2f13133L15\x7F0f13133e5f031L2;" [nconc lambda copy-list caar let* cadar])  case #function("o1c0^q42;" [#function("rc0mj02c1e230q42;" [#function("n2f1c0<6=0c0;f1A6E0^;f1?6X0c1f0e2f131L3;f1NA6m0c1f0e2f1M31L3;c3f0c4f1L2L3;" [else eqv? quote-value memv quote]) #function("rc0f0g10L2L1e1c2L1e3e4c5mg11323132L3;" [let nconc cond copy-list map #function("n1g10g00f0M32f0NK;" [])]) gensym])])  catch #function("n2c0e130q42;" [#function("rc0g01c1f0L1c2c3c4f0L2c5c6f0L2c7c8L2L3c5c9f0L2g00L3L4c:f0L2c;f0L2L4L3L3;" [trycatch lambda if and pair? eq car quote thrown-value cadr caddr raise]) gensym]))
*banner*
";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
