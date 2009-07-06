zero?
#function("7000r1~`W;" [])
vector.map
#function("8000r2c0e1\x7f31u42;" [#function("8000vc0e1~31u42;" [#function(":000v`\x80azc0qw2~;" [#function(":000r1\x80~i20i21~[31\\;" [])]) vector.alloc]) length])
vector->list
#function("9000r1c0e1~31_u43;" [#function(":000va~c0qw2\x7f;" [#function("8000r1i10\x80~z[\x81Ko01;" [])]) length])
untrace
#function("8000r1c0e1~31u42;" [#function("9000ve0~316@0e1\x80e2~31b2[42;^;" [traced? set-top-level-value! function:vals]) top-level-value])
traced?
#function("8000r1e0~31e0\x8031>;" [function:code] #0=[#function("\xb9000s0e0c1~K312c2~x2;" [println x #.apply] #0#) ()])
trace
#function("8000r1c0e1~31u322c2;" [#function("8000vc0e130u42;" [#function("?000ve0\x8031@6a0e1i10e2c3~c4c5c6c7i10L2~L3L2c8c7\x80L2~L3L3L33142;^;" [traced? set-top-level-value! eval lambda begin println cons quote apply]) gensym]) top-level-value ok])
to-proper
#function("8000r1~A640~;~?660~L1;~Me0~N31K;" [to-proper])
table.values
#function("9000r1e0c1q_~43;" [table.foldl #function("7000r3\x7fg2K;" [])])
table.pairs
#function("9000r1e0c1q_~43;" [table.foldl #function("7000r3~\x7fKg2K;" [])])
table.keys
#function("9000r1e0c1q_~43;" [table.foldl #function("7000r3~g2K;" [])])
table.invert
#function("8000r1c0e130u42;" [#function("9000ve0c1q_\x80332~;" [table.foldl #function("9000r3e0\x80\x7f~43;" [put!])]) table])
table.foreach
#function("9000r2e0c1q_\x7f43;" [table.foldl #function("8000r3\x80~\x7f322];" [])])
table.clone
#function("8000r1c0e130u42;" [#function("9000ve0c1q_\x80332~;" [table.foldl #function("9000r3e0\x80~\x7f43;" [put!])]) table])
symbol-syntax
#function("9000r1e0e1~^43;" [get *syntax-environment*])
string.trim
#function("9000r3c0^^u43;" [#function("8000vc0qm02c1qm12c2e3\x8031u42;" [#function(";000r4g2g3X16?02e0\x7fe1~g232326A0\x80~\x7fe2~g232g344;g2;" [string.find string.char string.inc]) #function("<000r3e0g2`3216D02e1\x7fe2~e3~g23232326?0\x81~\x7fe3~g23243;g2;" [> string.find string.char string.dec]) #function("<000ve0i10\x80i10i11`~34\x81i10i12~3343;" [string.sub]) length])])
string.tail
#function(";000r2e0~e1~`\x7f3342;" [string.sub string.inc])
string.rpad
#function("<000r3e0~e1g2\x7fe2~31z3242;" [string string.rep string.count])
string.rep
#function(";000r2\x7fb4X6`0e0\x7f`32650c1;\x7faW680e2~41;\x7fb2W690e2~~42;e2~~~43;e3\x7f316@0e2~e4~\x7faz3242;e4e2~~32\x7fb2U242;" [<= "" string odd? string.rep])
string.map
#function("9000r2c0e130e2\x7f31u43;" [#function("8000vc0`u322e1~41;" [#function(";000v^~\x81X6S02e0\x80i10e1i11~3231322e2i11~32m05\x0b/;" [io.putc string.char string.inc]) io.tostring!]) buffer length])
string.lpad
#function(";000r3e0e1g2\x7fe2~31z32~42;" [string string.rep string.count])
string.join
#function("8000r2~A650c0;c1e230u42;" ["" #function("8000ve0~\x80M322e1c2q\x80N322e3~41;" [io.write for-each #function("8000r1e0\x80i11322e0\x80~42;" [io.write]) io.tostring!]) buffer])
simple-sort
#function("8000r1~A17602~NA640~;c0~Mu42;" [#function("9000vc0e1c2q\x80N32u42;" [#function(":000ve0e1~M31\x80L1e1~N3143;" [nconc simple-sort]) separate #function("7000r1~\x80X;" [])])])
set-syntax!
#function("9000r2e0e1~\x7f43;" [put! *syntax-environment*])
separate
#function(":000r2\x80~\x7f__44;" [] #0=[#function(";000r4\x7fA680g2g3K;~\x7fM316@0\x80~\x7fN\x7fMg2Kg344;\x80~\x7fNg2\x7fMg3K44;" [] #0#) ()])
self-evaluating?
#function("8000r1~?16602~C@17K02e0~3116A02~C16:02~e1~31<;" [constant? top-level-value])
reverse!
#function("8000r1c0_u42;" [#function("9000v^\x80F6C02\x80N\x80~\x80m02P2o005\x1c/2~;" [])])
reverse
#function("9000r1e0e1_~43;" [foldl cons])
revappend
#function("8000r2e0e1~31\x7f42;" [nconc reverse])
repl
#function("9000r0c0^^u43;" [#function("6000vc0qm02c1qm12\x7f302e240;" [#function("8000r0e0c1312e2e3312c4c5qc6qtu42;" [princ "> " io.flush *output-stream* #function("8000ve0e131@16=02c2e3~31u42;" [io.eof? *input-stream* #function("7000ve0~312~k12];" [print that]) load-process]) #function("6000r0e040;" [read]) #function("7000r1e0e1312e2~41;" [io.discardbuffer *input-stream* raise])]) #function("7000r0c0qc1qt6;0e2302\x8140;^;" [#function("7000r0\x803016702e040;" [newline]) #function("7000r1e0~312e1e230312];" [print-exception print-stack-trace stacktrace]) newline]) newline])])
remainder
#function("8000r2~~\x7fV\x7fT2z;" [])
ref-int32-LE
#function("=000r2e0e1~\x7f`y[`32e1~\x7fay[b832e1~\x7fb2y[b@32e1~\x7fb3y[bH32R441;" [int32 ash])
ref-int16-LE
#function(";000r2e0e1~\x7f`y[`32e1~\x7fay[b832y41;" [int16 ash])
random
#function("8000r1e0~316<0e1e230~42;e330~T2;" [integer? mod rand rand.double])
quote-value
#function("7000r1e0~31640~;c1~L2;" [self-evaluating? quote])
println
#function("\xb9000s0e0~Q2e1302;" [print newline])
print-to-string
#function("8000r1c0e130u42;" [#function("8000ve0~\x80322e1~41;" [io.print io.tostring!]) buffer])
print-stack-trace
#function("8000r1c0^u42;" [#function(":000vc0qm02c1e2e3\x80b53231e430`u44;" [#function("9000r2c0e1c2q\x7f32u42;" [#function("6000v~A650c0;~M;" [?]) filter #function("7000r1~E16:02e0~31\x80<;" [top-level-value])]) #function("8000ve0c1q~42;" [for-each #function("9000r1e0c1i02c2332e3i10~`[\x8132e4~31NK312e5302i02ayo02;" [princ "#" " " print vector->list newline])]) reverse! list-tail environment])])
print-exception
#function("9000r1c0^^u43;" [#function("\xb9000vc0qm02c1qm12\x80F16D02\x80Mc2<16:02e3\x80b4326Q0~c4e5\x8031c6e7\x8031c8352\x7fe9\x8031315\xd20\x80F16@02\x80Mc:<16602\x80NF6A0~c;e5\x8031c<335\xac0\x80F16802\x80Mc=<6@0~c>312~\x80NQ25\x8f0\x80F16802\x80Mc?<6I0e@e7\x8031312~cAe5\x8031325i0eB\x803116:02e3\x80b2326K0\x7f\x80M312~cC312cDe5\x8031u325<0~cE312\x7f\x80312~eF41;" [#function("\xba000s0e0e1~x3;" [io.princ *error-stream*]) #function("\xba000s0e0e1~x3;" [io.print *error-stream*]) type-error length= "type-error: " cadr ": expected " caddr ", got " cadddr unbound-error "unbound-error: eval: variable " " has no value" error "error: " load-error print-exception "in file " list? ": " #function("8000ve0~3117502~C660\x80530\x81~41;" [string?]) "*** Unhandled exception: " *linefeed*])])
print
#function("\xba000s0e0e1~x3;" [io.print *output-stream*])
princ
#function("\xba000s0e0e1~x3;" [io.princ *output-stream*])
positive?
#function("8000r1e0~`42;" [>])
odd?
#function("7000r1e0~31@;" [even?])
nreconc
#function("8000r2e0e1~31\x7f42;" [nconc reverse!])
newline
#function("7000r0e0e1312];" [princ *linefeed*])
nestlist
#function(";000r3e0g2`32640_;\x7fe1~~\x7f31g2az33K;" [<= nestlist])
negative?
#function("7000r1~`X;" [])
mod0
#function("8000r2~~\x7fV\x7fT2z;" [])
mod
#function("9000r2~e0~\x7f32\x7fT2z;" [div])
memv
#function("8000r2\x7f?640^;\x7fM~=640\x7f;e0~\x7fN42;" [memv])
member
#function("8000r2\x7f?640^;\x7fM~>640\x7f;e0~\x7fN42;" [member])
mark-label
#function("9000r2e0~e1\x7f43;" [emit :label])
mapcar
#function(";000s1\x80~\x7f42;" [] #0=[#function("\xb7000r2\x7fA660~40;\x7fM?650\x7fM;~e0e1\x7f32Q2\x80~e0e2\x7f3232K;" [map car cdr] #0#) ()])
map-int
#function("9000r2e0\x7f`32640_;c1~`31_K_u43;" [<= #function(":000v~m12a\x81azc0qw2~;" [#function("8000r1\x81i10~31_KP2\x81No01;" [])])])
map!
#function("9000r2\x7f^\x7fF6B02\x7f~\x7fM31O2\x7fNm15\x1d/2;" [])
map
#function("8000r2c0_L1u42;" [#function("9000v~^\x81F6H02~\x80\x81M31_KPNm02\x81No015\x17/2N;" [])])
make-system-image
#function(";000r1c0e1~e2e3e434c5e6u44;" [#function("8000v^k02c1c2qu42;" [*print-pretty* #function("7000vc0qc1qt~302;" [#function(":000r0e0c1qe2e3e430313142;" [for-each #function("9000r1~E16b02e0~31@16W02e1~31G@16K02e2~i1132@16=02e3e1~3131@6\\0e4i10~322e5i10c6322e4i10e1~31322e5i10c642;^;" [constant? top-level-value memq iostream? io.print io.write "\n"]) reverse! simple-sort environment]) #function("7000r1\x80302e0~41;" [raise])]) #function("7000r0e0\x80312i02k1;" [io.close *print-pretty*])]) file :write :create :truncate (*linefeed* *directory-separator* *argv* that *print-pretty* *print-width* *print-readably*) *print-pretty*])
make-label
#function("6000r1e040;" [gensym])
make-code-emitter
#function("8000r0_e030`Z3;" [table])
macroexpand-1
#function("8000r1~?640~;c0e1~31u42;" [#function("\xb7000v~680~\x80Nx2;\x80;" []) macrocall?])
macroexpand
#function("9000r1c0^^u43;" [#function("8000vc0qm02c1qm12\x7f\x80_42;" [#function(":000r2c0e1~31F6N0e2~31F6=0c3e1~31K570e4~31530^u42;" [#function(":000vc0e1~31i11~\x8132u43;" [#function("=000ve0c1e2i1031~A660\x7f5A0c1~\x7fL3e3c4q~32Ke5i103144;" [list* lambda cadr map #function("6000r1^;" []) lastcdr]) get-defined-vars]) cddr cdddr begin caddr]) #function("9000r2~?640~;c0e1~M\x7f32u42;" [#function("\xb8000v~6F0i11e0~31\x80NQ2e1~3142;c2e3\x8031u42;" [cadr caddr #function("\xb8000v~6B0i21~i10NQ2i1142;i10Mc0<660i10;i10Mc1<6>0i20i10i1142;i10Mc2<6W0c3e4i1031e5c1L1_L1e6e7i10313133L1u43;e8c9qi1042;" [quote lambda let-syntax #function(";000vi31\x7fe0e1c2q~32i213242;" [nconc map #function("9000r1~Mi41e0~31i3132i31L3;" [cadr])]) cadr nconc copy-list cddr map #function("8000r1i31~i2142;" [])]) macrocall?]) assq])])])
macrocall?
#function("9000r1~MC16<02e0e1~M^43;" [get *syntax-environment*])
lookup-sym
#function("8000r4\x7fA650c0;c1\x7fMu42;" [(global) #function(":000vc0e1\x80~`33u42;" [#function(";000v~6G0i13680c0~L2;c1i12~L3;e2i10i11Ni1317502\x80A680i12570i12ay^44;" [arg closed lookup-sym]) index-of])])
load-process
#function("7000r1e0~41;" [eval])
load
#function("9000r1c0e1~e232u42;" [#function("7000vc0qc1qt;" [#function("9000r0c0^u32^^^43;" [#function("6000vc0qm0;" [#function(":000r3e0i1031@6C0\x80e1i1031~e2\x7f3143;e3i10312e2\x7f41;" [io.eof? read load-process io.close])])]) #function("9000r1e0\x80312e1c2i10~L341;" [io.close raise load-error])]) file :read])
list?
#function("7000r1~A17@02~F16902e0~N41;" [list?])
list-tail
#function("9000r2e0\x7f`32640~;e1~N\x7faz42;" [<= list-tail])
list-ref
#function("8000r2e0~\x7f32M;" [list-tail])
list-partition
#function("8000r2c0^u42;" [#function("<000vc0qm02e1\x81`32690e2c341;e4~\x80\x81`__3541;" [#function("<000r5~?6I0e0g2`326<0e1g331g4K;g4;e2g2\x7f326C0\x80~\x7f`_e1g331g4K45;\x80~N\x7fag2y~Mg3Kg445;" [> reverse! >=]) <= error "list-partition: invalid count" reverse!])])
list-head
#function(":000r2e0\x7f`32640_;~Me1~N\x7faz32K;" [<= list-head])
list->vector
#function("\xb7000r1e0~x2;" [vector])
length>
#function("9000r2\x7f`X640~;\x7f`W6;0~F16402~;~?660\x7f`X;e0~N\x7faz42;" [length>])
length=
#function("9000r2\x7f`X640^;\x7f`W650~?;~?660\x7f`W;e0~N\x7faz42;" [length=])
lastcdr
#function("7000r1~?640~;e0~31N;" [last-pair])
last-pair
#function("7000r1~N?640~;e0~N41;" [last-pair])
just-compile-args
#function("8000r3e0c1q\x7f42;" [for-each #function(":000r1e0\x80i02^~44;" [compile-in])])
iota
#function("8000r1e0e1~42;" [map-int identity])
io.readline
#function("8000r1e0~c142;" [io.readuntil #\x000a])
index-of
#function(":000r3\x7fA640^;~\x7fM<650g2;e0~\x7fNg2ay43;" [index-of])
in-env?
#function("8000r2e0c1q\x7f42;" [any #function("8000r1e0\x80~42;" [memq])])
identity
#function("6000r1~;" [])
hex5
#function("9000r1e0e1~b@32b5c243;" [string.lpad number->string #\0])
get-defined-vars
#function("8000r1e0\x80~3141;" [delete-duplicates] #0=[#function("\xb7000r1~?640_;~Mc0<16602~NF6m0e1~31C16:02e1~31L117V02e1~31F16E02e2~31C16:02e2~31L117402_;~Mc3<6>0e4e5\x80~N32x2;_;" [define cadr caadr begin append map] #0#) ()])
for-each
#function("8000r2\x7fF6@0~\x7fM312e0~\x7fN42;];" [for-each])
foldr
#function(";000r3g2A640\x7f;~g2Me0~\x7fg2N3342;" [foldr])
foldl
#function(":000r3g2A640\x7f;e0~~g2M\x7f32g2N43;" [foldl])
fits-i8
#function("8000r1~I16F02e0~b\xb03216:02e1~b\xaf42;" [>= <=])
filter
#function("9000r2\x80~\x7f_43;" [] #0=[#function(":000r3\x7fA650g2;~\x7fM316>0\x80~\x7fN\x7fMg2K43;\x80~\x7fNg243;" [] #0#) ()])
expand
#function("7000r1e0~41;" [macroexpand])
every
#function("8000r2\x7f?17D02~\x7fM3116:02e0~\x7fN42;" [every])
even?
#function("8000r1e0~a32`W;" [logand])
eval
#function("8000r1e0e1~313140;" [compile-thunk expand])
error
#function(":000s0e0c1~K41;" [raise error])
encode-byte-code
#function("8000r1c0e1~31u42;" [#function("8000vc0e1~31u42;" [#function(";000vc0e1e2~31b3e2~31b2VT2yc332u42;" [#function(">000vc0e1\x8031`e230e230e330^^u48;" [#function(">000ve0g4c1322^\x7f~X6\xe402i10\x7f[m52g5e2<6O0e3g2i10\x7fay[e4g431332\x7fb2ym15\xb30e0g4e5e6e7\x806<0c8g5u32540g53231322\x7faym12\x7f~X6:0i10\x7f[530^m62e9g5c:326^0e3g3e4g431g6332e0g4\x80670e;540e<`31322\x7faym15C0g6D6<0c=g5u32530^5z/2e>c?qg3322e@g441;" [io.write #int32(0) :label put! sizeof byte get Instructions #function("7000v~e0<650e1;~e2<650e3;~e4<650e5;i05;" [:jmp :jmp.l :brt :brt.l :brf :brf.l]) memq (:jmp :brf :brt) int32 int16 #function(":000ve0~c1326H0e2i04e3i0631322\x81ayo01;e0~c4326`0e2i04e5i0631322\x81ayo012e2i04e5i20\x81[31322\x81ayo01;e0~c6326`0e2i04e3i0631322\x81ayo012e2i04e3i20\x81[31322\x81ayo01;e2i04e5i0631322\x81ayo01;" [memq (:loadv.l :loadg.l :setg.l :loada.l :seta.l :largc :lvargc) io.write int32 (:loadc :setc) uint8 (:loadc.l :setc.l)]) table.foreach #function("<000r2e0i04~322e1i04i10670e2540e3e4i02\x7f32~z3142;" [io.seek io.write int32 int16 get]) io.tostring!]) length table buffer]) >= length 65536]) list->vector]) reverse!])
emit
#function("G000s2g2A6=0~`\x7f~`[K\\5\xdb0e0\x7fc1326A0e2~g2M32L1m2530^2c3e4\x7fc532u322c6e4\x7fc732u322\x7fe8<6\\0g2c9>6=0e:m12_m25F0g2c;>6=0e<m12_m2530^530^2\x7fe=<6\\0g2c>>6=0e?m12_m25F0g2c@>6=0eAm12_m2530^530^2~`eB\x7fg2K~`[32\\2~;" [memq (:loadv :loadg :setg) bcode:indexfor #function("8000v~16=02e0i02Mc1326;0e2~31o01;^;" [> 255 cadr]) assq ((:loadv :loadv.l) (:loadg :loadg.l) (:setg :setg.l) (:loada :loada.l) (:seta :seta.l)) #function("8000v~16O02e0i02Mc13217@02e0e2i0231c1326;0e2~31o01;^;" [> 255 cadr]) ((:loadc :loadc.l) (:setc :setc.l)) :loada (0) :loada0 (1) :loada1 :loadc (0 0) :loadc00 (0 1) :loadc01 nreconc])
div
#function("8000r2~\x7fV~`X16C02\x7f`X16402a17502b/17402`y;" [])
display
#function("7000r1e0~312];" [princ])
disassemble
#function("=000s1\x7fA6C0e0~`322e1302];530^2c2\x7fMe3~31e4~31u44;" [disassemble newline #function("8000vc0^u42;" [#function(":000vc0qm02`\x80azc1qw2e2c3e4\x81`32c5332c6b4e7\x8131u43;" [#function("9000r1~J16602~G@6D0e0c1312e2~i10ay42;e3~41;" [princ "\n" disassemble print]) #function("7000r1e0c141;" [princ "\t"]) princ "maxstack " ref-int32-LE "\n" #function(":000v^~\x7fX6E02c0e1c2q^e333u325\x19/;" [#function("<000ve0\x80b432690e130530^2`i20azc2qw2e3e4\x80b4z31c5e6e7~31a32c8342\x80ayo002c9~u42;" [> newline #function("7000r1e0c141;" [princ "\t"]) princ hex5 ":  " string.tail string "\t" #function("<000ve0~c1326P0i20i32e2i31i1032[312i10b4yo10;e0~c3326L0i20i32i31i10[[312i10ayo10;e0~c4326K0e5e6i31i10[31312i10ayo10;e0~c7326O0e5e6e2i31i103231312i10b4yo10;e0~c8326f0e5e6i31i10[31c9322i10ayo102e5e6i31i10[31312i10ayo10;e0~c:326n0e5e6e2i31i103231c9322i10b4yo102e5e6e2i31i103231312i10b4yo10;e0~c;326U0e5c<e=i10e>i31i1032y31322i10b2yo10;e0~c?326U0e5c<e=i10e2i31i1032y31322i10b4yo10;^;" [memq (:loadv.l :loadg.l :setg.l) ref-int32-LE (:loadv :loadg :setg) (:loada :seta :call :tcall :list :+ :- :* :/ :vector :argc :vargc :loadi8 :apply :tapply) princ number->string (:loada.l :seta.l :largc :lvargc) (:loadc :setc) " " (:loadc.l :setc.l) (:jmp :brf :brt) "@" hex5 ref-int16-LE (:jmp.l :brf.l :brt.l)])]) table.foldl #function("8000r3g217@02\x7fi21\x80[<16402~;" []) Instructions]) length])]) function:code function:vals])
delete-duplicates
#function("9000r1~?640~;c0~M~Nu43;" [#function("8000ve0~\x7f32680e1\x7f41;~e1\x7f31K;" [member delete-duplicates])])
count
#function("8000r2c0^u42;" [#function("9000vc0qm02~\x80\x81`43;" [#function(":000r3\x7fA650g2;\x80~\x7fN~\x7fM31690g2ay540g243;" [])])])
copy-tree
#function("8000r1~?640~;e0~M31e0~N31K;" [copy-tree])
const-to-idx-vec
#function("9000r1c0e1e2~3131u42;" [#function("9000ve0c1qe2\x8031322~;" [table.foreach #function("8000r2\x80\x7f~\\;" []) bcode:ctable]) vector.alloc bcode:nconst])
compile-while
#function("9000r4c0e1~31e1~31u43;" [#function(":000ve0\x80\x81^^342e1\x80~322e0\x80\x81^i02342e2\x80e3\x7f332e2\x80e4322e0\x80\x81^i03342e2\x80e5~332e1\x80\x7f42;" [compile-in mark-label emit :brf :pop :jmp]) make-label])
compile-thunk
#function("9000r1e0c1_~L341;" [compile lambda])
compile-sym
#function(";000r4c0e1g2\x7f`]34u42;" [#function("8000vc0~Mu42;" [#function(";000v~c0<6D0e1i10i13`[e2\x803143;~c3<6I0e1i10i13a[e2\x8031e4\x803144;e1i10i13b2[i1243;" [arg emit cadr closed caddr])]) lookup-sym])
compile-short-circuit
#function(":000r6g3?6=0e0~\x7fg2g444;g3N?6>0e0~\x7fg2g3M44;c1e2~31u42;" [compile-in #function("<000ve0\x80\x81^i03M342e1\x80e2322e1\x80i05~332e1\x80e3322e4\x80\x81i02i03Ni04i05362e5\x80~42;" [compile-in emit :dup :pop compile-short-circuit mark-label]) make-label])
compile-prog1
#function(";000r3e0~\x7f^e1g231342e2g231F6H0e3~\x7f^e2g231342e4~e542;^;" [compile-in cadr cddr compile-begin emit :pop])
compile-or
#function("<000r4e0~\x7fg2g3^e146;" [compile-short-circuit :brt])
compile-let
#function("9000r4c0g3Mg3Nu43;" [#function("=000ve0\x7fe1e2~313132660^5=0e3e4c5~32312e6\x80e7e8\x81~]33332c9e:\x80\x81\x7f33u42;" [length= length cadr error string "apply: incorrect number of arguments to " emit :loadv compile-f #function(";000ve0i10e1322e0i10i12670e2540e3a~y43;" [emit :copyenv :tcall :call]) compile-arglist])])
compile-in
#function(":000r4g3C6=0e0~\x7fg3c144;g3?6\x9a0g3`<6:0e2~e342;g3a<6:0e2~e442;g3]<6:0e2~e542;g3^<6:0e2~e642;g3_<6:0e2~e742;e8g3316<0e2~e9g343;e2~e:g343;c;g3Mu42;" [compile-sym [:loada :loadc :loadg] emit :load0 :load1 :loadt :loadf :loadnil fits-i8 :loadi8 :loadv #function("=000v~c0<6A0e1\x80e2e3i033143;~c4<6?0e5\x80\x81i02i0344;~c6<6@0e7\x80\x81i02i03N44;~c8<6<0e9\x80\x81i0343;~c:<6J0e1\x80e2e;\x81i0332332e1\x80e<42;~c=<6@0e>\x80\x81i02i03N44;~c?<6@0e@\x80\x81i02i03N44;~cA<6J0eB\x80\x81e3i0331c6eCi0331K44;~cD<6N0eE\x80\x81e3i0331eFi0331eGi033145;~cH<6I0eI\x80\x81]e3i0331342e1\x80eJ42;~cK<6Q0eI\x80\x81^eFi0331342eL\x80\x81e3i0331cM44;~cN<6v0eI\x80\x81^c:_e3i0331L3342eOeFi033131660^580ePcQ312eI\x80\x81^eFi0331342e1\x80eR42;eS\x80\x81i02i0344;" [quote emit :loadv cadr if compile-if begin compile-begin prog1 compile-prog1 lambda compile-f :closure and compile-and or compile-or while compile-while cddr for compile-for caddr cadddr return compile-in :ret set! compile-sym [:seta :setc :setg] trycatch 1arg-lambda? error "trycatch: second form must be a 1-argument lambda" :trycatch compile-app])])
compile-if
#function("=000r4c0e1~31e1~31e2g331e3g331e4g331F6;0e5g331530^u46;" [#function(";000vg2]<6>0e0\x80\x81i02g344;g2^<6>0e0\x80\x81i02g444;e0\x80\x81^g2342e1\x80e2~332e0\x80\x81i02g3342i026<0e1\x80e3325:0e1\x80e4\x7f332e5\x80~322e0\x80\x81i02g4342e5\x80\x7f42;" [compile-in emit :brf :ret :jmp mark-label]) make-label cadr caddr cdddr cadddr])
compile-for
#function(":000r5e0g4316X0e1~\x7f^g2342e1~\x7f^g3342e1~\x7f^g4342e2~e342;e4c541;" [1arg-lambda? compile-in emit :for error "for: third form must be a 1-argument lambda"])
compile-f
#function("=000s2c0e130e2\x7f31u43;" [#function("@000vi02A@6<0e0~e1325\x860e2\x7fe3326O0e0~e4\x7f31A670e5540e6e7\x7f31335_0e4\x7f31A6A0e0~e8e7\x7f31335G0e0~e9\x7f?660`570e7\x7f31332e:~e;\x7f31\x80K]e<\x8131342e0~e=322e>e?e@~3131eA~3142;" [emit :let length> MAX_ARGS lastcdr :largc :lvargc length :argc :vargc compile-in to-proper caddr :ret function encode-byte-code bcode:code const-to-idx-vec]) make-code-emitter cadr])
compile-call
#function("8000r4c0g3Mu42;" [#function("9000vc0~C16V02e1~\x8132@16J02~E16C02e2~3116902e3~31G6:0e3~31530~u42;" [#function("8000vc0~G16802e1~31u42;" [#function(";000v~@6A0e0i20i21^\x8034530^2c1e2i20i21i23N33u42;" [compile-in #function(":000v\x806@0c0e1e2\x80^33u42;e3i30i32670e4540e5~43;" [#function("9000v~16=02e0i43N~32@6=0e1i20~32530^2c2i10u42;" [length= argc-error #function(":000v~e0<6R0i10`W6<0e1i50e242;e1i50i20i1043;~e3<6e0i10`W6<0e1i50e442;i10b2W6<0e1i50e542;e1i50i20i1043;~e6<6v0i10`W6;0e7i30a42;i10aW6<0e1i50e842;i10b2W6<0e1i50e942;e1i50i20i1043;~e:<6R0i10`W6<0e1i50e;42;e1i50i20i1043;~e<<6Q0i10`W6;0e7i30a42;e1i50i20i1043;~e=<6T0i10`W6>0e1i50e>c?43;e1i50i20i1043;~e@<6]0i10b2X6<0e7i30b242;e1i50i52670eA540e@i1043;e1i50i2042;" [:list emit :loadnil :+ :load0 :add2 :- argc-error :neg :sub2 :* :load1 :/ :vector :loadv [] :apply :tapply])]) get arg-counts emit :tcall :call]) compile-arglist]) builtin->instruction]) in-env? constant? top-level-value])])
compile-begin
#function(":000r4g3?6<0e0~\x7fg2^44;g3N?6>0e0~\x7fg2g3M44;e0~\x7f^g3M342e1~e2322e3~\x7fg2g3N44;" [compile-in emit :pop compile-begin])
compile-arglist
#function("9000r3c0e1g2e232u42;" [#function("<000v~6^0e0\x80e1i02e232\x81332c3e4e5c6qe7~e23232Ku322e2ay;e0\x80i02\x81332e8i0241;" [just-compile-args list-head MAX_ARGS #function(":000ve0i10i11^~44;" [compile-in]) nconc map #function("7000r1e0~K;" [list]) list-partition length]) length> MAX_ARGS])
compile-app
#function("8000r4c0g3Mu42;" [#function(":000v~F16W02~Mc0<16M02e1e2~313116?02e3e2~31e432@6?0e5\x80\x81i02i0344;e6\x80\x81i02i0344;" [lambda list? cadr length> MAX_ARGS compile-let compile-call])])
compile-and
#function("<000r4e0~\x7fg2g3]e146;" [compile-short-circuit :brf])
compile
#function("8000r1e0_~42;" [compile-f])
char?
#function("7000r1e0~31c1<;" [typeof wchar])
cddr
#function("6000r1~NN;" [])
cdddr
#function("6000r1~NNN;" [])
cddar
#function("6000r1~MNN;" [])
cdar
#function("6000r1~MN;" [])
cdadr
#function("6000r1~NMN;" [])
cdaar
#function("6000r1~MMN;" [])
cadr
#function("6000r1~NM;" [])
caddr
#function("6000r1~NNM;" [])
cadddr
#function("6000r1~NNNM;" [])
cadar
#function("6000r1~MNM;" [])
caar
#function("6000r1~MM;" [])
caadr
#function("6000r1~NMM;" [])
caaar
#function("6000r1~MMM;" [])
builtin->instruction
#function("9000r1e0\x80~^43;" [get] [#table(#.number? :number?  #.cons :cons  #.fixnum? :fixnum?  #.equal? :equal?  #.eq? :eq?  #.symbol? :symbol?  #.div0 :div0  #.builtin? :builtin?  #.aset! :aset!  #.- :-  #.boolean? :boolean?  #.not :not  #.apply :apply  #.atom? :atom?  #.set-cdr! :set-cdr!  #./ :/  #.function? :function?  #.vector :vector  #.list :list  #.bound? :bound?  #.< :<  #.* :*  #.cdr :cdr  #.null? :null?  #.+ :+  #.eqv? :eqv?  #.compare :compare  #.aref :aref  #.set-car! :set-car!  #.car :car  #.pair? :pair?  #.= :=  #.vector? :vector?) ()])
bq-process
#function("8000r1c0^u42;" [#function(":000vc0qm02e1\x80316H0\x80H6A0c2e3e4\x803131u42;\x80;\x80?680c5\x80L2;\x80Mc6<6@0e3e3e7\x80313141;\x80Mc8<680e7\x8041;e9~\x8032@6D0c:e;\x8031e<e=\x8032u43;c>\x80_u43;" [#function("7000r1~F16B02~Mc0<17802~Mc1<17702~c2<;" [*comma-at* *comma-dot* *comma*]) self-evaluating? #function("8000v~Mc0<680e1~NK;e2e1~L3;" [list vector apply]) bq-process vector->list quote backquote cadr *comma* any #function("9000v~A670c0\x7fK;e1c2\x7fKe3~31L142;" [list nconc list* bq-process]) lastcdr map bq-bracket1 #function("<000v^~F16902~Mc0<@6E02e1~M31\x7fKm12~Nm05\x0f/2c2~F6A0e3\x7fe4~31L1325K0~A6:0e5\x7f315>0e3\x7fe6~31L132u42;" [*comma* bq-bracket #function("7000v~NA650~M;c0~K;" [nconc]) nreconc cadr reverse! bq-process])])])
bq-bracket1
#function("7000r1~F16802~Mc0<680e1~41;e2~41;" [*comma* cadr bq-process])
bq-bracket
#function("8000r1~?6<0e0e1~31L2;~Mc2<6<0e0e3~31L2;~Mc4<6<0c5e3~31L2;~Mc6<680e3~41;e0e1~31L2;" [list bq-process *comma* cadr *comma-at* copy-list *comma-dot*])
bcode:nconst
#function("7000r1~b2[;" [])
bcode:indexfor
#function("9000r2c0e1~31e2~31u43;" [#function(":000ve0~\x8132690e1~\x8142;e2~\x81\x7f332\x7f\x80b2\x7fay\\2;" [has? get put!]) bcode:ctable bcode:nconst])
bcode:ctable
#function("7000r1~a[;" [])
bcode:code
#function("7000r1~`[;" [])
assv
#function("8000r2\x7f?640^;e0\x7f31~=650\x7fM;e1~\x7fN42;" [caar assv])
assoc
#function("8000r2\x7f?640^;e0\x7f31~>650\x7fM;e1~\x7fN42;" [caar assoc])
array?
#function("8000r1~H17=02c0e1~31u42;" [#function("7000v~F16802~Mc0<;" [array]) typeof])
argc-error
#function("=000r2e0e1c2~c3\x7f\x7faW670c4540c53541;" [error string "compile error: " " expects " " argument." " arguments."])
arg-counts
#table(:not 1  :set-cdr! 2  :cons 2  :number? 1  :equal? 2  :cdr 1  :vector? 1  :eqv? 2  := 2  :div0 2  :atom? 1  :aref 2  :compare 2  :< 2  :null? 1  :eq? 2  :car 1  :set-car! 2  :builtin? 1  :aset! 3  :bound? 1  :boolean? 1  :pair? 1  :symbol? 1  :fixnum? 1)
any
#function("8000r2\x7fF16D02~\x7fM3117:02e0~\x7fN42;" [any])
abs
#function("7000r1~`X650~{;~;" [])
__start
#function("8000r1e0302~NF6C0~Nk12e2e3~31315A0~k12e4e5312e6302e7`41;" [__init_globals *argv* __script cadr princ *banner* repl exit])
__script
#function("7000r1c0qc1qt;" [#function("7000r0e0\x8041;" [load]) #function("7000r1e0~312e1a41;" [print-exception exit])])
__init_globals
#function("7000r0e0c1<17B02e0c2<17802e0c3<6>0c4k52c6k75;0c8k52c9k72e:k;2e<k=2e>k?;" [*os-name* win32 win64 windows "\\" *directory-separator* "\r\n" *linefeed* "/" "\n" *stdout* *output-stream* *stdin* *input-stream* *stderr* *error-stream*])
MAX_ARGS
127
Instructions
#table(:sub2 74  :nop 0  :set-cdr! 32  :/ 37  :setc 63  :tapply 72  :lvargc 77  :cons 27  :loada1 79  dummy_nil 84  :equal? 14  :cdr 30  :call 3  :eqv? 13  := 39  :setg.l 60  :list 28  :atom? 15  :aref 43  :load0 48  :let 70  dummy_t 82  :argc 66  :< 40  :null? 17  :loadg 53  :load1 49  :car 29  :brt.l 10  :vargc 67  :loada 55  :set-car! 31  :setg 59  :aset! 44  :loadc01 81  :bound? 21  :pair? 22  :symbol? 19  :fixnum? 25  :loadi8 50  :not 16  :* 36  :neg 75  :pop 2  :largc 76  :loadnil 47  :brf 6  :vector 42  :- 35  :loadv 51  :loada.l 56  :seta.l 62  :closure 65  :loadc00 80  :number? 20  dummy_f 83  :trycatch 68  :add2 73  :loadv.l 52  :vector? 24  :brf.l 9  :seta 61  :apply 33  :dup 1  :div0 38  :setc.l 64  :copyenv 69  :for 71  :loada0 78  :loadc 57  :loadc.l 58  :compare 41  :eq? 12  :function? 26  :+ 34  :jmp 5  :loadt 45  :brt 7  :builtin? 23  :loadg.l 54  :tcall 4  :ret 11  :boolean? 18  :loadf 46  :jmp.l 8)
>=
#function("7000r2\x7f~X17602~\x7fW;" [])
>
#function("7000r2\x7f~X;" [])
<=
#function("7000r2~\x7fX17602~\x7fW;" [])
1arg-lambda?
#function("8000r1~F16Z02~Mc0<16P02~NF16H02e1~31F16=02e2e1~31a42;" [lambda cadr length=])
1-
#function("7000r1~az;" [])
1+
#function("7000r1~ay;" [])
/=
#function("7000r2~\x7fW@;" [])
*whitespace*
"\t\n\v\f\r \u0085  ᠎           \u2028\u2029  　"
*syntax-environment*
#table(define #function("=000s1~C6:0c0~\x7fML3;c0~Mc1~N\x7fKKL3;" [set! lambda])  letrec #function(">000s1c0e1e2~32e3e1c4q~32\x7f32KKe1c5q~32K;" [lambda map car nconc #function("7000r1c0~K;" [set!]) #function("6000r1^;" [])])  backquote #function("7000r1e0~41;" [bq-process])  assert #function("<000r1c0~]c1c2c3~L2L2L2L4;" [if raise quote assert-failed])  label #function(":000r2c0~L1c1~\x7fL3L3^L2;" [lambda set!])  do #function("A000s2c0e130\x7fMe2e3~32e2e4~32e2c5q~32u46;" [#function("C000vc0~c1g2c2\x7fe3c4L1e5\x81N3132e3c4L1e5i0231e3~L1e5g43132L133L4L3L2L1e3~L1e5g33132L3;" [letrec lambda if nconc begin copy-list]) gensym map car cadr #function("7000r1e0~31F680e1~41;~M;" [cddr caddr])])  when #function("<000s1c0~c1\x7fK^L4;" [if begin])  unwind-protect #function("9000r2c0e130e130u43;" [#function("@000vc0\x7fc1_\x81L3L2L1c2c3\x80c1~L1c4\x7fL1c5~L2L3L3L3\x7fL1L3L3;" [let lambda prog1 trycatch begin raise]) gensym])  dotimes #function("<000s1c0~Me1~31u43;" [#function("=000vc0`c1\x7faL3e2c3L1~L1L1e4\x813133L4;" [for - nconc lambda copy-list]) cadr])  define-macro #function("=000s1c0c1~ML2c2~N\x7fKKL3;" [set-syntax! quote lambda])  unless #function("=000s1c0~^c1\x7fKL4;" [if begin])  let #function(";000s1c0^u42;" [#function(";000v\x80C6D0\x80m02\x81Mo002\x81No01530^2c0c1e2c3q\x8032\x81KKe2c4q\x8032u43;" [#function("8000v\x806;0c0\x80~L3530~\x7fK;" [label]) lambda map #function("6000r1~F650~M;~;" []) #function("7000r1~F680e0~41;^;" [cadr])])])  cond #function(":000s0c0^u42;" [#function("7000vc0qm02~\x8041;" [#function("8000r1~?640^;c0~Mu42;" [#function(":000v~Mc0<17702~M]<6A0~NA650~M;c1~NK;~NA6@0c2~Mi10\x80N31L3;c3~Mc1~NKi10\x80N31L4;" [else begin or if])])])])  throw #function(":000r2c0c1c2c3L2~\x7fL4L2;" [raise list quote thrown-value])  time #function("8000r1c0e130u42;" [#function(">000vc0~c1L1L2L1c2\x80c3c4c5c1L1~L3c6L4L3L3;" [let time.now prog1 princ "Elapsed time: " - " seconds\n"]) gensym])  let* #function("A000s1~?6E0e0c1L1_L1e2\x7f3133L1;e0c1L1e3~31L1L1e2~NF6H0e0c4L1~NL1e2\x7f3133L1530\x7f3133e5~31L2;" [nconc lambda copy-list caar let* cadar])  case #function(";000s1c0^u42;" [#function("8000vc0qm02c1e230u42;" [#function("9000r2\x7fc0<650c0;\x7fA640^;\x7fC6=0c1~e2\x7f31L3;\x7f?6=0c3~e2\x7f31L3;\x7fNA6>0c3~e2\x7fM31L3;e4e5\x7f326=0c6~c7\x7fL2L3;c8~c7\x7fL2L3;" [else eq? quote-value eqv? every symbol? memq quote memv]) #function("=000vc0~i10L2L1e1c2L1e3e4c5qi11323132L3;" [let nconc cond copy-list map #function("8000r1i10\x80~M32~NK;" [])]) gensym])])  catch #function("8000r2c0e130u42;" [#function("@000vc0\x81c1~L1c2c3c4~L2c5c6~L2c7c8L2L3c5c9~L2\x80L3L4c:~L2c;~L2L4L3L3;" [trycatch lambda if and pair? eq car quote thrown-value cadr caddr raise]) gensym]))
*banner*
";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
