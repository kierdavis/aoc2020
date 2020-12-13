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

directions ← (¯1 ¯1) (¯1 0) (¯1 1) (0 ¯1) (0 1) (1 ¯1) (1 0) (1 1)

shift ← {((⍴⍺) × (0=⍵) + ×-⍵) ↑ (-⍵) ↓ ⍺}
neighbours ← {⊃ +/ (⊂⍵) shift¨ directions}
next ← {seats ∧ (~ ⍵ ∧ 4 ≤ neighbours ⍵) ∧ ((~⍵) ∧ 0 = neighbours ⍵) ∨ ⍵}
initial ← (⍴ seats) ↑ 0
fixpoint ← next⍣≡ initial

⍝ Answer to part A = number of occupied seats at the fixed point:
+/ ∊ fixpoint

shape ← ⍴ seats
coordsInDir ← {(~ ∨/¨ (0≥r) ∨ (⊂shape)<r) / r ← (⊂⍺) + (⊂⍵) × ⍳⌈/shape}
seatCoordsInDir ← {(r ⌷¨ ⊂seats) / r ← ⍺ coordsInDir ⍵}
