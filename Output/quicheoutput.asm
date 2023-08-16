;1: var x%:=1
test1:
  ld (ix-$02),$01
  ld (ix-$01),$00
;2: var b:=true
  ld (ix-$03),$01
;3: if b then
  ld a,(ix-$03)
  and a
  jp z,test3
;4: x:=2
test2:
  ld (ix-$02),$02
  ld (ix-$01),$00
;5: else
  jp test4
;6: x:=3
test3:
  ld (ix-$02),$03
  ld (ix-$01),$00
test4:
test5:
  ret
;--CodeGen Finished
