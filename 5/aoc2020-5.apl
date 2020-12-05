#!/usr/bin/env -S apl --script
handle ← ⎕FIO[3] "input.txt"
input ← 10000 ⎕FIO[6] handle
⊣ ⎕FIO[4] handle
count ← (≢input) ÷ 11
input ← 0 ¯1 ↓ count 11 ⍴ input
row ← 2 ⊥ ⍉ ¯1 + 70 66 ⍳ 0 ¯3 ↓ input
col ← 2 ⊥ ⍉ ¯1 + 76 82 ⍳ 0 7 ↓ input
id ← col + 8 × row
id ← id[⍋id]
¯1 ↑ id
1 + id[((1↓id) - (¯1↓id)) ⍳ 2]
)OFF
