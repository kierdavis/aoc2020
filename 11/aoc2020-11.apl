#!/usr/bin/env -S apl

handle ← ⎕FIO[3] "test.txt"
inputString ← 10000 ⎕FIO[6] handle
⊣ ⎕FIO[4] handle

floorChar ← 46
seatChar ← 76
newlineChar ← 10

width ← inputString ⍳ newlineChar
height ← (≢inputString) ÷ width
inputGrid ← 0 ¯1 ↓ height width ⍴ inputString
seats ← inputGrid = seatChar

shift ← {((⍴⍺) × (0=⍵) + ×-⍵) ↑ (-⍵) ↓ ⍺}
neighbours ← {⊃ +/ (⊂⍵) shift¨ (¯1 ¯1) (¯1 0) (¯1 1) (0 ¯1) (0 1) (1 ¯1) (1 0) (1 1)}
next ← {seats ∧ (~ ⍵ ∧ 4 ≤ neighbours ⍵) ∧ ((~⍵) ∧ 0 = neighbours ⍵) ∨ ⍵}
initial ← (⍴ seats) ↑ 0
fixpoint ← next⍣≡ initial

⍝ Answer to part A = number of occupied seats at the fixed point:
+/ ∊ fixpoint
