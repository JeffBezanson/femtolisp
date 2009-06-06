zero?
#function("r1g0`W;" [])
vector.map
#function("r2c0e1g131u42;" [#function("vc0e1g031u42;" [#function("v`i00azc0qw2g0;" [#function("r1i00g0i20i21g0[31\\;" [])]) vector.alloc]) length])
vector->list
#function("r1c0e1g031_u43;" [#function("vag0c0qw2g1;" [#function("r1i10i00g0z[i01Ko01;" [])]) length])
untrace
#function("r1c0e1g031u42;" [#function("ve0g0316K0e1i00e2g031b2[42;^;" [traced? set-top-level-value! function:vals]) top-level-value])
traced?
#function("r1e0g031e0i0031>;" [function:code] #0=[#function("s0e0c1g0K312c2g0x2;" [println x #.apply] #0#) ()])
trace
#function("r1c0e1g031u322c2;" [#function("vc0e130u42;" [#function("ve0i0031@6p0e1i10e2c3g0c4c5c6c7i10L2g0L3L2c8c7i00L2g0L3L3L33142;^;" [traced? set-top-level-value! eval lambda begin println cons quote apply]) gensym]) top-level-value ok])
to-proper
#function("r1g0A6;0g0;g0?6F0g0L1;g0Me0g0N31K;" [to-proper])
table.values
#function("r1e0c1q_g043;" [table.foldl #function("r3g1g2K;" [])])
table.pairs
#function("r1e0c1q_g043;" [table.foldl #function("r3g0g1Kg2K;" [])])
table.keys
#function("r1e0c1q_g043;" [table.foldl #function("r3g0g2K;" [])])
table.invert
#function("r1c0e130u42;" [#function("ve0c1q_i00332g0;" [table.foldl #function("r3e0i00g1g043;" [put!])]) table])
table.foreach
#function("r2e0c1q_g143;" [table.foldl #function("r3i00g0g1322];" [])])
table.clone
#function("r1c0e130u42;" [#function("ve0c1q_i00332g0;" [table.foldl #function("r3e0i00g0g143;" [put!])]) table])
symbol-syntax
#function("r1e0e1g0^43;" [get *syntax-environment*])
string.trim
#function("r3c0^^u43;" [#function("vc0qm02c1qm12c2e3i0031u42;" [#function("r4g2g3X16J02e0g1e1g0g232326a0i00g0g1e2g0g232g344;g2;" [string.find string.char string.inc]) #function("r3e0g2`3216R02e1g1e2g0e3g0g23232326g0i01g0g1e3g0g23243;g2;" [> string.find string.char string.dec]) #function("ve0i10i00i10i11`g034i01i10i12g03343;" [string.sub]) length])])
string.tail
#function("r2e0g0e1g0`g13342;" [string.sub string.inc])
string.rpad
#function("r3e0g0e1g2g1e2g031z3242;" [string string.rep string.count])
string.rep
#function("r2g1b4X6q0e0g1`326G0c1;g1aW6U0e2g041;g1b2W6f0e2g0g042;e2g0g0g043;e3g1316\x8b0e2g0e4g0g1az3242;e4e2g0g032g1b2U242;" [<= "" string odd? string.rep])
string.map
#function("r2c0e130e2g131u43;" [#function("vc0`u322e1g041;" [#function("v^g0i01X6`02e0i00i10e1i11g03231322e2i11g032m0520;" [io.putc string.char string.inc]) io.tostring!]) buffer length])
string.lpad
#function("r3e0e1g2g1e2g031z32g042;" [string string.rep string.count])
string.join
#function("r2g0A6;0c0;c1e230u42;" ["" #function("ve0g0i00M322e1c2qi00N322e3g041;" [io.write for-each #function("r1e0i00i11322e0i00g042;" [io.write]) io.tostring!]) buffer])
simple-sort
#function("r1g0A17>02g0NA6D0g0;c0g0Mu42;" [#function("vc0e1c2qi00N32u42;" [#function("ve0e1g0M31i00L1e1g0N3143;" [nconc simple-sort]) separate #function("r1g0i00X;" [])])])
set-syntax!
#function("r2e0e1g0g143;" [put! *syntax-environment*])
separate
#function("r2i00g0g1__44;" [] #0=[#function("r4g1A6>0g2g3K;g0g1M316[0i00g0g1Ng1Mg2Kg344;i00g0g1Ng2g1Mg3K44;" [] #0#) ()])
self-evaluating?
#function("r1g0?16>02g0C@17_02e0g03116_02g0C16_02g0e1g031<;" [constant? top-level-value])
reverse!
#function("r1c0_u42;" [#function("v^i00F6Q02i00Ni00g0i00m02P2o005202g0;" [])])
reverse
#function("r1e0e1_g043;" [foldl cons])
revappend
#function("r2e0e1g031g142;" [nconc reverse])
repl
#function("r0c0^^u43;" [#function("vc0qm02c1qm12g1302e240;" [#function("r0e0c1312e2e3312c4c5qc6qtu42;" [princ "> " io.flush *output-stream* #function("ve0e131@16H02c2e3g031u42;" [io.eof? *input-stream* #function("ve0g0312g0k12];" [print that]) load-process]) #function("r0e040;" [read]) #function("r1e0e1312e2g041;" [io.discardbuffer *input-stream* raise])]) #function("r0c0qc1qt6G0e2302i0140;^;" [#function("r0i003016@02e040;" [newline]) #function("r1e0g041;" [print-exception]) newline]) newline])])
remainder
#function("r2g0g0g1Vg1T2z;" [])
ref-uint32-LE
#function("r2e0g0g1`y[`32e0g0g1ay[b832e0g0g1b2y[b@32e0g0g1b3y[bH32R4;" [ash])
ref-uint16-LE
#function("r2e0g0g1`y[`32e0g0g1ay[b832y;" [ash])
random
#function("r1e0g0316F0e1e230g042;e330g0T2;" [integer? mod rand rand.double])
quote-value
#function("r1e0g0316>0g0;c1g0L2;" [self-evaluating? quote])
println
#function("s0e0g0Q2e1302;" [print newline])
print-to-string
#function("r1c0e130u42;" [#function("ve0g0i00322e1g041;" [io.print io.tostring!]) buffer])
print-exception
#function("r1c0^^u43;" [#function("vc0qm02c1qm12i00F16[02i00Mc2<16[02e3i00b4326\x850g0c4e5i0031c6e7i0031c8352g1e9i0031315M1i00F16\x9f02i00Mc:<16\x9f02i00NF6\xb40g0c;e5i0031c<335M1i00F16\xc402i00Mc=<6\xd90g0c>312g0i00NQ25M1i00F16\xe902i00Mc?<6\x080e@e7i0031312g0cAe5i0031325M1eBi003116\x1d02e3i00b2326?1g1i00M312g0cC312cDe5i0031u325M1g0cE312g1i00312g0eF312];" [#function("s0e0e1g0x3;" [io.princ *error-stream*]) #function("s0e0e1g0x3;" [io.print *error-stream*]) type-error length= "type-error: " cadr ": expected " caddr ", got " cadddr unbound-error "unbound-error: eval: variable " " has no value" error "error: " load-error print-exception "in file " list? ": " #function("ve0g03117?02g0C6H0i005K0i01g041;" [string?]) "*** Unhandled exception: " *linefeed*])])
print
#function("s0e0e1g0x3;" [io.print *output-stream*])
princ
#function("s0e0e1g0x3;" [io.princ *output-stream*])
positive?
#function("r1e0g0`42;" [>])
odd?
#function("r1e0g031@;" [even?])
nreconc
#function("r2e0e1g031g142;" [nconc reverse!])
newline
#function("r0e0e1312];" [princ *linefeed*])
nestlist
#function("r3e0g2`326>0_;g1e1g0g0g131g2az33K;" [<= nestlist])
negative?
#function("r1g0`X;" [])
mod0
#function("r2g0g0g1Vg1T2z;" [])
mod
#function("r2g0e0g0g132g1T2z;" [div])
memv
#function("r2g1?6:0^;g1Mg0=6F0g1;e0g0g1N42;" [memv])
member
#function("r2g1?6:0^;g1Mg0>6F0g1;e0g0g1N42;" [member])
mark-label
#function("r2e0g0e1g143;" [emit :label])
mapcar
#function("s1i00g0g142;" [] #0=[#function("r2g1A6=0g040;g1M?6H0g1M;g0e0e1g132Q2i00g0e0e2g13232K;" [map car cdr] #0#) ()])
map-int
#function("r2e0g1`326>0_;c1g0`31_K_u43;" [<= #function("vg0m12ai01azc0qw2g0;" [#function("r1i01i10g031_KP2i01No01;" [])])])
map!
#function("r2g1^g1F6O02g1g0g1M31O2g1Nm15502;" [])
map
#function("r2g1?6;0g1;g0g1M31e0g0g1N32K;" [map])
make-system-image
#function("r1c0e1g0e2e3e434c5e6u44;" [#function("v^k02c1c2qu42;" [*print-pretty* #function("vc0qc1qtg0302;" [#function("r0e0c1qe2e3e430313142;" [for-each #function("r1g0E16m02e0g031@16m02e1g031G@16m02e2g0i1132@16m02e3e1g03131@6\x9c0e4i10g0322e5i10c6322e4i10e1g031322e5i10c642;^;" [constant? top-level-value memq iostream? io.print io.write "\n"]) reverse! simple-sort environment]) #function("r1i00302e0g041;" [raise])]) #function("r0e0i00312i02k1;" [io.close *print-pretty*])]) file :write :create :truncate (*linefeed* *directory-separator* *argv* that *print-pretty* *print-width* *print-readably*) *print-pretty*])
make-label
#function("r1e040;" [gensym])
make-code-emitter
#function("r0_e030`Z3;" [table])
macroexpand-1
#function("r1g0?6;0g0;c0e1g031u42;" [#function("vg06?0g0i00Nx2;i00;" []) macrocall?])
macroexpand
#function("r1c0^^u43;" [#function("vc0qm02c1qm12g1i00_42;" [#function("r2c0e1g031F6]0e2g031F6T0c3e1g031K5Z0e4g0315^0^u42;" [#function("vc0e1g031i11g0i0132u43;" [#function("ve0c1e2i1031g0A6G0g15Y0c1g0g1L3e3c4qg032Ke5i103144;" [list* lambda cadr map #function("r1^;" []) lastcdr]) get-defined-vars]) cddr cdddr begin caddr]) #function("r2g0?6;0g0;c0e1g0Mg132u42;" [#function("vg06N0i11e0g031i00NQ2e1g03142;c2e3i0031u42;" [cadr caddr #function("vg06G0i21g0i10NQ2i1142;i10Mc0<6U0i10;i10Mc1<6k0i20i10i1142;i10Mc2<6\x9a0c3e4i1031e5c1L1_L1e6e7i10313133L1u43;e8c9qi1042;" [quote lambda let-syntax #function("vi31g1e0e1c2qg032i213242;" [nconc map #function("r1g0Mi41e0g031i3132i31L3;" [cadr])]) cadr nconc copy-list cddr map #function("r1i31g0i2142;" [])]) macrocall?]) assq])])])
macrocall?
#function("r1g0MC16E02e0e1g0M^43;" [get *syntax-environment*])
lookup-sym
#function("r4g1A6;0c0;c1g1Mu42;" [(global) #function("vc0e1i00g0`33u42;" [#function("vg06M0i136C0c0g0L2;c1i12g0L3;e2i10i11Ni1317b02i00A6k0i125p0i12ay^44;" [arg closed lookup-sym]) index-of])])
load-process
#function("r1e0g041;" [eval])
load
#function("r1c0e1g0e232u42;" [#function("vc0qc1qt;" [#function("r0c0^u32^^^43;" [#function("vc0qm0;" [#function("r3e0i1031@6R0i00e1i1031g0e2g13143;e3i10312e2g141;" [io.eof? read load-process io.close])])]) #function("r1e0i00312e1c2i10g0L341;" [io.close raise load-error])]) file :read])
list?
#function("r1g0A17I02g0F16I02e0g0N41;" [list?])
list-tail
#function("r2e0g1`326?0g0;e1g0Ng1az42;" [<= list-tail])
list-ref
#function("r2e0g0g132M;" [list-tail])
list-partition
#function("r2c0^u42;" [#function("vc0qm02e1i01`326I0e2c341;e4g0i00i01`__3541;" [#function("r5g0?6O0e0g2`326L0e1g331g4K;g4;e2g2g1326o0i00g0g1`_e1g331g4K45;i00g0Ng1ag2yg0Mg3Kg445;" [> reverse! >=]) <= error "list-partition: invalid count" reverse!])])
list-head
#function("r2e0g1`326>0_;g0Me1g0Ng1az32K;" [<= list-head])
list->vector
#function("r1e0g0x2;" [vector])
length>
#function("r2g1`X6<0g0;g1`W6N0g0F16M02g0;g0?6Y0g1`X;e0g0Ng1az42;" [length>])
length=
#function("r2g1`X6;0^;g1`W6F0g0?;g0?6Q0g1`W;e0g0Ng1az42;" [length=])
lastcdr
#function("r1g0?6;0g0;e0g031N;" [last-pair])
last-pair
#function("r1g0N?6<0g0;e0g0N41;" [last-pair])
just-compile-args
#function("r3e0c1qg142;" [for-each #function("r1e0i00i02^g044;" [compile-in])])
iota
#function("r1e0e1g042;" [map-int identity])
io.readline
#function("r1e0g0c142;" [io.readuntil #\x000a])
index-of
#function("r3g1A6:0^;g0g1M<6F0g2;e0g0g1Ng2ay43;" [index-of])
in-env?
#function("r2e0c1qg142;" [any #function("r1e0i00g042;" [memq])])
identity
#function("r1g0;" [])
hex5
#function("r1e0e1g0b@32b5c243;" [string.lpad number->string #\0])
get-defined-vars
#function("r1e0i00g03141;" [delete-duplicates] #0=[#function("r1g0?6:0_;g0Mc0<16I02g0NF6\x8c0e1g031C16`02e1g031L117\x8b02e1g031F16\x8502e2g031C16\x8502e2g031L117\x8b02_;g0Mc3<6\xa40e4e5i00g0N32x2;_;" [define cadr caadr begin append map] #0#) ()])
for-each
#function("r2g1F6J0g0g1M312e0g0g1N42;];" [for-each])
foldr
#function("r3g2A6;0g1;g0g2Me0g0g1g2N3342;" [foldr])
foldl
#function("r3g2A6;0g1;e0g0g0g2Mg132g2N43;" [foldl])
fits-i8
#function("r1g0I16O02e0g0b\xb03216O02e1g0b\xaf42;" [>= <=])
filter
#function("r2i00g0g1_43;" [] #0=[#function("r3g1A6;0g2;g0g1M316V0i00g0g1Ng1Mg2K43;i00g0g1Ng243;" [] #0#) ()])
expand
#function("r1e0g041;" [macroexpand])
every
#function("r2g1?17O02g0g1M3116O02e0g0g1N42;" [every])
even?
#function("r1e0g0a32`W;" [logand])
eval
#function("r1e0e1g0313140;" [compile-thunk expand])
error
#function("s0e0c1g0K41;" [raise error])
encode-byte-code
#function("r1c0e1g031u42;" [#function("vc0e1g031u42;" [#function("vc0e1e2g031b3e3c4qi0032T2yc532u42;" [#function("vc0e1i0031`e230e230e330^u47;" [#function("v^g1g0X6\xbc02i10g1[m52g5e0<6k0e1g2i10g1ay[e2g431332g1b2ym15\xb90e3g4e4e5e6i0016\x8502e7g5c8326\x920c9g5u325\x940g53231322g1aym12g1g0X6\xb80c:i10g1[u325\xb90^5202e;c<qg3322e=g441;" [:label put! sizeof io.write byte get Instructions memq (:jmp :brt :brf) #function("vg0e0=6<0e1;g0e2=6G0e3;g0e4=6R0e5;^;" [:jmp :jmp.l :brt :brt.l :brf :brf.l]) #function("vc0i05u42;" [#function("ve0g0c1326T0e2i14e3i0031322i11ayo11;e0g0c4326w0e2i14e5i0031322i11ayo11;e0g0c6326\xb60e2i14e5i0031322i11ayo112e2i14e5i30i11[31322i11ayo11;e0g0c7326\xf50e2i14e3i0031322i11ayo112e2i14e3i30i11[31322i11ayo11;e0g0c832631e9i13e:i1431i00332e2i14i206\"0e35$0e;`31322i11ayo11;^;" [memv (:loadv.l :loadg.l :setg.l :loada.l :seta.l :largc :lvargc) io.write uint32 (:loada :seta :call :tcall :loadv :loadg :setg :list :+ :- :* :/ :vector :argc :vargc :loadi8 :apply :tapply) uint8 (:loadc :setc) (:loadc.l :setc.l) (:jmp :brf :brt) put! sizeof uint16])]) table.foreach #function("r2e0i04g0322e1i04i106L0e25N0e3e4i02g1323142;" [io.seek io.write uint32 uint16 get]) io.tostring!]) length table buffer]) >= length count #function("r1e0g0c142;" [memq (:loadv.l :loadg.l :setg.l :loada.l :seta.l :loadc.l :setc.l :jmp :brt :brf :largc :lvargc)]) 65536]) list->vector]) reverse!])
emit
#function("s2e0g1c1326M0e2g0g2M32L1m25N0^2c3e4g1c532u322c6e4g1c732u322g0`e8g1g2Kg0`[32\\2g0;" [memq (:loadv :loadg :setg) bcode:indexfor #function("vg016B02e0i02Mc1326O0e2g031o01;^;" [> 255 cadr]) assq ((:loadv :loadv.l) (:loadg :loadg.l) (:setg :setg.l) (:loada :loada.l) (:seta :seta.l)) #function("vg016T02e0i02Mc13217T02e0e2i0231c1326a0e2g031o01;^;" [> 255 cadr]) ((:loadc :loadc.l) (:setc :setc.l)) nreconc])
div
#function("r2g0g1Vg0`X16Q02g1`X16J02a17Q02b/17W02`y;" [])
display
#function("r1e0g0312];" [princ])
disassemble
#function("s1g1A6J0e0g0`322e1302];5K0^2c2g1Me3g031e4g031u44;" [disassemble newline #function("vc0^u42;" [#function("vc0qm02c1`e2i0131u43;" [#function("r1g0J16>02g0G@6T0e0c1312e2g0i10ay42;e3g041;" [princ "\n" disassemble print]) #function("v^g0g1X6M02c0e1c2q^e333u32520;" [#function("ve0i00`326C0e1305D0^2`i20azc2qw2e3e4i0031c5e6e7g031a32c8342i00ayo002c9g0u42;" [> newline #function("r1e0c141;" [princ "\t"]) princ hex5 ":  " string.tail string "\t" #function("ve0g0c1326Z0i20i32e2i31i1032[312i10b4yo10;e0g0c3326\x7f0i20i32i31i10[[312i10ayo10;e0g0c4326\xa30e5e6i31i10[31312i10ayo10;e0g0c7326\xcb0e5e6e2i31i103231312i10b4yo10;e0g0c8326\x0a0e5e6i31i10[31c9322i10ayo102e5e6i31i10[31312i10ayo10;e0g0c:326Q1e5e6e2i31i103231c9322i10b4yo102e5e6e2i31i103231312i10b4yo10;e0g0c;326{1e5c<e=e>i31i103231322i10b2yo10;e0g0c?326\xa51e5c<e=e2i31i103231322i10b4yo10;^;" [memv (:loadv.l :loadg.l :setg.l) ref-uint32-LE (:loadv :loadg :setg) (:loada :seta :call :tcall :list :+ :- :* :/ :vector :argc :vargc :loadi8 :apply :tapply) princ number->string (:loada.l :seta.l :largc :lvargc) (:loadc :setc) " " (:loadc.l :setc.l) (:jmp :brf :brt) "@" hex5 ref-uint16-LE (:jmp.l :brf.l :brt.l)])]) table.foldl #function("r3g217J02g1i21i00[<16J02g0;" []) Instructions]) length])]) function:code function:vals])
delete-duplicates
#function("r1g0?6;0g0;c0g0Mg0Nu43;" [#function("ve0g0g1326C0e1g141;g0e1g131K;" [member delete-duplicates])])
count
#function("r2c0^u42;" [#function("vc0qm02g0i00i01`43;" [#function("r3g1A6;0g2;i00g0g1Ng0g1M316T0g2ay5V0g243;" [])])])
copy-tree
#function("r1g0?6;0g0;e0g0M31e0g0N31K;" [copy-tree])
const-to-idx-vec
#function("r1c0e1e2g03131u42;" [#function("ve0c1qe2i0031322g0;" [table.foreach #function("r2i00g1g0\\;" []) bcode:ctable]) vector.alloc bcode:nconst])
compile-while
#function("r4c0e1g031e1g031u43;" [#function("ve0i00i01^^342e1i00g0322e0i00i01^i02342e2i00e3g1332e2i00e4322e0i00i01^i03342e2i00e5g0332e1i00g142;" [compile-in mark-label emit :brf :pop :jmp]) make-label])
compile-thunk
#function("r1e0c1_g0L341;" [compile lambda])
compile-sym
#function("r4c0e1g2g1`]34u42;" [#function("vc0g0Mu42;" [#function("vg0c0=6M0e1i10i13`[e2i003143;g0c3=6p0e1i10i13a[e2i0031e4i003144;e1i10i13b2[i1243;" [arg emit cadr closed caddr])]) lookup-sym])
compile-short-circuit
#function("r6g3?6E0e0g0g1g2g444;g3N?6Z0e0g0g1g2g3M44;c1e2g031u42;" [compile-in #function("ve0i00i01^i03M342e1i00e2322e1i00i05g0332e1i00e3322e4i00i01i02i03Ni04i05362e5i00g042;" [compile-in emit :dup :pop compile-short-circuit mark-label]) make-label])
compile-prog1
#function("r3e0g0g1^e1g231342e2g231F6e0e3g0g1^e2g231342e4g0e542;^;" [compile-in cadr cddr compile-begin emit :pop])
compile-or
#function("r4e0g0g1g2g3^e146;" [compile-short-circuit :brt])
compile-let
#function("r4c0g3Mg3Nu43;" [#function("ve0g1e1e2g03131326H0^5T0e3e4c5g032312e6i00e7e8i01g0]33332c9e:i00i01g133u42;" [length= length cadr error string "apply: incorrect number of arguments to " emit :loadv compile-f #function("ve0i10e1322e0i10i126K0e25M0e3ag0y43;" [emit :copyenv :tcall :call]) compile-arglist])])
compile-in
#function("r4g3C6E0e0g0g1g3c144;g3?6\xba0g3`<6[0e2g0e342;g3a<6k0e2g0e442;g3]<6{0e2g0e542;g3^<6\x8b0e2g0e642;g3_<6\x9b0e2g0e742;e8g3316\xaf0e2g0e9g343;e2g0e:g343;c;g3Mu42;" [compile-sym [:loada :loadc :loadg] emit :load0 :load1 :loadt :loadf :loadnil fits-i8 :loadi8 :loadv #function("vg0c0=6J0e1i00e2e3i033143;g0c4=6c0e5i00i01i02i0344;g0c6=6}0e7i00i01i02i03N44;g0c8=6\x930e9i00i01i0343;g0c:=6\xb90e1i00e2e;i01i0332332e1i00e<42;g0c==6\xd30e>i00i01i02i03N44;g0c?=6\xed0e@i00i01i02i03N44;g0cA=6\x110eBi00i01e3i0331c6eCi0331K44;g0cD=691eEi00i01e3i0331eFi0331eGi033145;g0cH=6^1eIi00i01]e3i0331342e1i00eJ42;g0cK=6\x8d1eIi00i01^eFi0331342eLi00i01e3i0331cM44;g0cN=6\xe31eIi00i01^c:_e3i0331L3342eOeFi0331316\xbf1^5\xc51ePcQ312eIi00i01^eFi0331342e1i00eR42;eSi00i01i02i0344;" [quote emit :loadv cadr if compile-if begin compile-begin prog1 compile-prog1 lambda compile-f :closure and compile-and or compile-or while compile-while cddr for compile-for caddr cadddr return compile-in :ret set! compile-sym [:seta :setc :setg] trycatch 1arg-lambda? error "trycatch: second form must be a 1-argument lambda" :trycatch compile-app])])
compile-if
#function("r4c0e1g031e1g031e2g331e3g331e4g331F6_0e5g3315`0^u46;" [#function("vg2]<6H0e0i00i01i02g344;g2^<6_0e0i00i01i02g444;e0i00i01^g2342e1i00e2g0332e0i00i01i02g3342i026\x9b0e1i00e3325\xa60e1i00e4g1332e5i00g0322e0i00i01i02g4342e5i00g142;" [compile-in emit :brf :ret :jmp mark-label]) make-label cadr caddr cdddr cadddr])
compile-for
#function("r5e0g4316h0e1g0g1^g2342e1g0g1^g3342e1g0g1^g4342e2g0e342;e4c541;" [1arg-lambda? compile-in emit :for error "for: third form must be a 1-argument lambda"])
compile-f
#function("s2c0e130e2g131u43;" [#function("vi02A@6D0e0g0e1325\xa20e2g1e3326o0e0g0e4g131A6b0e55d0e6e7g131335\xa20e4g131A6\x8a0e0g0e8e7g131335\xa20e0g0e9g1?6\x9a0`5\xa00e7g131332e:g0e;g131i00K]e<i0131342e0g0e=322e>e?e@g03131eAg03142;" [emit :let length> MAX_ARGS lastcdr :largc :lvargc length :argc :vargc compile-in to-proper caddr :ret function encode-byte-code bcode:code const-to-idx-vec]) make-code-emitter cadr])
compile-call
#function("r4c0g3Mu42;" [#function("vc0g0C16d02e1g0i0132@16d02g0E16d02e2g03116d02e3g031G6p0e3g0315r0g0u42;" [#function("vc0g0G16A02e1g031u42;" [#function("vg0@6H0e0i20i21^i00345I0^2c1e2i20i21i23N33u42;" [compile-in #function("vi006G0c0e1e2i00^33u42;e3i30i326W0e45Y0e5g043;" [#function("vg016C02e0i43Ng032@6R0e1i20g0325S0^2c2i10u42;" [length= argc-error #function("vg0e0=6Y0i10`W6K0e1i50e242;e1i50i20i1043;g0e3=6\x940i10`W6s0e1i50e442;i10b2W6\x860e1i50e542;e1i50i20i1043;g0e6=6\xe00i10`W6\xad0e7i30a42;i10aW6\xbf0e1i50e842;i10b2W6\xd20e1i50e942;e1i50i20i1043;g0e:=6\x080i10`W6\xfa0e1i50e;42;e1i50i20i1043;g0e<=6/0i10`W6!0e7i30a42;e1i50i20i1043;g0e==6Y1i10`W6K1e1i50e>c?43;e1i50i20i1043;g0e@=6\x8c1i10b2X6t1e7i30b242;e1i50i526\x841eA5\x861e@i1043;e1i50i2042;" [:list emit :loadnil :+ :load0 :add2 :- argc-error :neg :sub2 :* :load1 :/ :vector :loadv [] :apply :tapply])]) get arg-counts emit :tcall :call]) compile-arglist]) builtin->instruction]) in-env? constant? top-level-value])])
compile-begin
#function("r4g3?6D0e0g0g1g2^44;g3N?6Y0e0g0g1g2g3M44;e0g0g1^g3M342e1g0e2322e3g0g1g2g3N44;" [compile-in emit :pop compile-begin])
compile-arglist
#function("r3c0e1g2e232u42;" [#function("vg06g0e0i00e1i02e232i01332c3e4e5c6qe7g0e23232Ku322e2ay;e0i00i02i01332e8i0241;" [just-compile-args list-head MAX_ARGS #function("ve0i10i11^g044;" [compile-in]) nconc map #function("r1e0g0K;" [list]) list-partition length]) length> MAX_ARGS])
compile-app
#function("r4c0g3Mu42;" [#function("vg0F16`02g0Mc0<16`02e1e2g0313116`02e3e2g031e432@6t0e5i00i01i02i0344;e6i00i01i02i0344;" [lambda list? cadr length> MAX_ARGS compile-let compile-call])])
compile-and
#function("r4e0g0g1g2g3]e146;" [compile-short-circuit :brf])
compile
#function("r1e0_g042;" [compile-f])
char?
#function("r1e0g031c1<;" [typeof wchar])
cddr
#function("r1g0NN;" [])
cdddr
#function("r1g0NNN;" [])
cddar
#function("r1g0MNN;" [])
cdar
#function("r1g0MN;" [])
cdadr
#function("r1g0NMN;" [])
cdaar
#function("r1g0MMN;" [])
cadr
#function("r1g0NM;" [])
caddr
#function("r1g0NNM;" [])
cadddr
#function("r1g0NNNM;" [])
cadar
#function("r1g0MNM;" [])
caar
#function("r1g0MM;" [])
caadr
#function("r1g0NMM;" [])
caaar
#function("r1g0MMM;" [])
builtin->instruction
#function("r1c0e1e2c3g03231u42;" [#function("ve0e1g03216@02g0;" [has? Instructions]) intern string #\:])
bq-process
#function("r1c0^u42;" [#function("vc0qm02e1i00316]0i00H6Y0c2e3e4i003131u42;i00;i00?6l0c5i00L2;i00Mc6<6\x860e3e3e7i00313141;i00Mc8<6\x980e7i0041;e9g0i0032@6\xbb0c:e;i0031e<e=i0032u43;c>i00_u43;" [#function("r1g0F16K02g0Mc0<17K02g0Mc1<17U02g0c2<;" [*comma-at* *comma-dot* *comma*]) self-evaluating? #function("vg0Mc0<6A0e1g0NK;e2e1g0L3;" [list vector apply]) bq-process vector->list quote backquote cadr *comma* any #function("vg0A6=0c0g1K;e1c2g1Ke3g031L142;" [list nconc list* bq-process]) lastcdr map bq-bracket1 #function("v^g0F16A02g0Mc0<@6Z02e1g0M31g1Km12g0Nm05202c2g0F6t0e3g1e4g031L1325\x910g0A6\x830e5g1315\x910e3g1e6g031L132u42;" [*comma* bq-bracket #function("vg0NA6<0g0M;c0g0K;" [nconc]) nreconc cadr reverse! bq-process])])])
bq-bracket1
#function("r1g0F16@02g0Mc0<6J0e1g041;e2g041;" [*comma* cadr bq-process])
bq-bracket
#function("r1g0?6C0e0e1g031L2;g0Mc2<6W0e0e3g031L2;g0Mc4<6k0c5e3g031L2;g0Mc6<6{0e3g041;e0e1g031L2;" [list bq-process *comma* cadr *comma-at* copy-list *comma-dot*])
bcode:nconst
#function("r1g0b2[;" [])
bcode:indexfor
#function("r2c0e1g031e2g031u43;" [#function("ve0g0i01326G0e1g0i0142;e2g0i01g1332g1i00b2g1ay\\2;" [has? get put!]) bcode:ctable bcode:nconst])
bcode:ctable
#function("r1g0a[;" [])
bcode:code
#function("r1g0`[;" [])
assv
#function("r2g1?6:0^;e0g131g0=6J0g1M;e1g0g1N42;" [caar assv])
assoc
#function("r2g1?6:0^;e0g131g0>6J0g1M;e1g0g1N42;" [caar assoc])
array?
#function("r1g0H17E02c0e1g031u42;" [#function("vg0F16?02g0Mc0<;" [array]) typeof])
argc-error
#function("r2e0e1c2g0c3g1g1aW6J0c45L0c53541;" [error string "compile error: " " expects " " argument." " arguments."])
arg-counts
#table(:not 1  :set-cdr! 2  :cons 2  :number? 1  :equal? 2  :cdr 1  :vector? 1  :eqv? 2  := 2  :div0 2  :atom? 1  :aref 2  :compare 2  :< 2  :null? 1  :eq? 2  :car 1  :set-car! 2  :builtin? 1  :aset! 3  :bound? 1  :boolean? 1  :pair? 1  :symbol? 1  :fixnum? 1)
append
#function("s0g0A6:0_;g0NA6E0g0M;e0g0Me1g0NQ242;" [copy-list append])
any
#function("r2g1F16O02g0g1M3117O02e0g0g1N42;" [any])
abs
#function("r1g0`X6=0g0{;g0;" [])
__start
#function("r1e0302g0NF6Q0g0Nk12e2e3g031315a0g0k12e4e5312e6302e7`41;" [__init_globals *argv* __script cadr princ *banner* repl exit])
__script
#function("r1c0qc1qt;" [#function("r0e0i0041;" [load]) #function("r1e0g0312e1a41;" [print-exception exit])])
__init_globals
#function("r0e0c1<17K02e0c2<17K02e0c3<6Z0c4k52c6k75c0c8k52c9k72e:k;2e<k=2e>k?;" [*os-name* win32 win64 windows "\\" *directory-separator* "\r\n" *linefeed* "/" "\n" *stdout* *output-stream* *stdin* *input-stream* *stderr* *error-stream*])
MAX_ARGS
127
Instructions
#table(:sub2 74  :nop 0  :set-cdr! 32  :/ 37  :setc 63  :tapply 72  :lvargc 77  :cons 27  dummy_nil 80  :equal? 14  :cdr 30  :call 3  :eqv? 13  := 39  :setg.l 60  :list 28  :atom? 15  :aref 43  :load0 48  :let 70  dummy_t 78  :argc 66  :< 40  :null? 17  :loadg 53  :load1 49  :car 29  :brt.l 10  :vargc 67  :loada 55  :set-car! 31  :setg 59  :aset! 44  :bound? 21  :pair? 22  :symbol? 19  :fixnum? 25  :loadi8 50  :not 16  :* 36  :neg 75  :pop 2  :largc 76  :loadnil 47  :brf 6  :vector 42  :- 35  :loadv 51  :loada.l 56  :seta.l 62  :closure 65  dummy_f 79  :number? 20  :trycatch 68  :add2 73  :loadv.l 52  :vector? 24  :brf.l 9  :seta 61  :apply 33  :dup 1  :div0 38  :setc.l 64  :copyenv 69  :for 71  :loadc 57  :loadc.l 58  :compare 41  :eq? 12  :function? 26  :+ 34  :jmp 5  :loadt 45  :brt 7  :builtin? 23  :loadg.l 54  :tcall 4  :ret 11  :boolean? 18  :loadf 46  :jmp.l 8)
>=
#function("r2g1g0X17A02g0g1W;" [])
>
#function("r2g1g0X;" [])
<=
#function("r2g0g1X17A02g0g1W;" [])
1arg-lambda?
#function("r1g0F16e02g0Mc0<16e02g0NF16e02e1g031F16e02e2e1g031a42;" [lambda cadr length=])
1-
#function("r1g0az;" [])
1+
#function("r1g0ay;" [])
/=
#function("r2g0g1W@;" [])
*whitespace*
"\t\n\v\f\r \u0085  ᠎           \u2028\u2029  　"
*syntax-environment*
#table(define #function("s1g0C6B0c0g0g1ML3;c0g0Mc1g0Ng1KKL3;" [set! lambda])  letrec #function("s1c0e1e2g032e3e1c4qg032g132KKe1c5qg032K;" [lambda map car nconc #function("r1c0g0K;" [set!]) #function("r1^;" [])])  backquote #function("r1e0g041;" [bq-process])  assert #function("r1c0g0]c1c2c3g0L2L2L2L4;" [if raise quote assert-failed])  label #function("r2c0g0L1c1g0g1L3L3^L2;" [lambda set!])  do #function("s2c0e130g1Me2e3g032e2e4g032e2c5qg032u46;" [#function("vc0g0c1g2c2g1e3c4L1e5i01N3132e3c4L1e5i0231e3g0L1e5g43132L133L4L3L2L1e3g0L1e5g33132L3;" [letrec lambda if nconc begin copy-list]) gensym map car cadr #function("r1e0g031F6C0e1g041;g0M;" [cddr caddr])])  when #function("s1c0g0c1g1K^L4;" [if begin])  unwind-protect #function("r2c0e130e130u43;" [#function("vc0g1c1_i01L3L2L1c2c3i00c1g0L1c4g1L1c5g0L2L3L3L3g1L1L3L3;" [let lambda prog1 trycatch begin raise]) gensym])  dotimes #function("s1c0g0Me1g031u43;" [#function("vc0`c1g1aL3e2c3L1g0L1L1e4i013133L4;" [for - nconc lambda copy-list]) cadr])  define-macro #function("s1c0c1g0ML2c2g0Ng1KKL3;" [set-syntax! quote lambda])  unless #function("s1c0g0^c1g1KL4;" [if begin])  let #function("s1c0^u42;" [#function("vi00C6P0i00m02i01Mo002i01No015Q0^2c0c1e2c3qi0032i01KKe2c4qi0032u43;" [#function("vi006C0c0i00g0L35E0g0g1K;" [label]) lambda map #function("r1g0F6<0g0M;g0;" []) #function("r1g0F6?0e0g041;^;" [cadr])])])  cond #function("s0c0^u42;" [#function("vc0qm02g0i0041;" [#function("r1g0?6:0^;c0g0Mu42;" [#function("vg0Mc0<17A02g0M]<6V0g0NA6O0g0M;c1g0NK;g0NA6n0c2g0Mi10i00N31L3;c3g0Mc1g0NKi10i00N31L4;" [else begin or if])])])])  throw #function("r2c0c1c2c3L2g0g1L4L2;" [raise list quote thrown-value])  time #function("r1c0e130u42;" [#function("vc0g0c1L1L2L1c2i00c3c4c5c1L1g0L3c6L4L3L3;" [let time.now prog1 princ "Elapsed time: " - " seconds\n"]) gensym])  let* #function("s1g0?6L0e0c1L1_L1e2g13133L1;e0c1L1e3g031L1L1e2g0NF6}0e0c4L1g0NL1e2g13133L15\x7F0g13133e5g031L2;" [nconc lambda copy-list caar let* cadar])  case #function("s1c0^u42;" [#function("vc0qm02c1e230u42;" [#function("r2g1c0<6=0c0;g1A6E0^;g1?6X0c1g0e2g131L3;g1NA6m0c1g0e2g1M31L3;c3g0c4g1L2L3;" [else eqv? quote-value memv quote]) #function("vc0g0i10L2L1e1c2L1e3e4c5qi11323132L3;" [let nconc cond copy-list map #function("r1i10i00g0M32g0NK;" [])]) gensym])])  catch #function("r2c0e130u42;" [#function("vc0i01c1g0L1c2c3c4g0L2c5c6g0L2c7c8L2L3c5c9g0L2i00L3L4c:g0L2c;g0L2L4L3L3;" [trycatch lambda if and pair? eq car quote thrown-value cadr caddr raise]) gensym]))
*banner*
";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
