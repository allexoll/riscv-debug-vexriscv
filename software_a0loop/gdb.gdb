target extended-remote localhost:3333
set remotetimeout 6000
set arch riscv:rv32
set print asm-demangle on
monitor reset halt
set confirm off
set backtrace limit 32

load

break main

display $pc
display $a0
display $a1
display $a2
display $a3
display $a4
display $a5
display $a6
display $a7
