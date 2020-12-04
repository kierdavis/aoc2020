#!/usr/bin/env -S sed -Ef

# Step 1: coalesce lines into passport records.
# sed receives input a line at a time, but we want to process input in
# chunks delimited by empty lines. We use the N command in a loop to
# achieve this.
:coalesce
# N causes an immediate exit if there is no more input, so we first check
# if we are currently on the last input line (address $). If so, jump to
# straight to `coalesced`.
$bcoalesced
# Append the next line of input to the pattern space, separated by a newline.
N
# If the pattern space doesn't end with a newline (i.e. if the line we
# just appended wasn't empty), jump back to `coalesce`.
/[^\n]$/bcoalesce
:coalesced
# Now, the pattern space contains the entire passport record (with embedded
# newlines).

# Step 2: validate that all required fields are present.
# If the pattern space does not contain "byr:", jump to `next`.
# Likewise for the other six required fields.
/byr:/!bnext
/iyr:/!bnext
/eyr:/!bnext
/hgt:/!bnext
/hcl:/!bnext
/ecl:/!bnext
/pid:/!bnext

# Step 3: increment the count of valid passports (this is where shit gets fun).
# The count is stored in the hold space between iterations of the script. It is
# initially the empty string, which is implicitly treated as "0".
# Begin by copying the hold space to the pattern space. There's nothing interesting
# left in the pattern space anyway.
g
# Append a '+' marker to the pattern space. This marker indicates that the digit to
# the left needs to be incremented.
s/$/+/
# If a 9 needs to be incremented, change it to a 0 and shift the '+' marker along.
tcarry
:carry
s/9\+/\+0/;tcarry
# If any other digit needs to be incremented, just increment it.
s/(^|0)\+/1/
s/1\+/2/
s/2\+/3/
s/3\+/4/
s/4\+/5/
s/5\+/6/
s/6\+/7/
s/7\+/8/
s/8\+/9/
# Put the result back into the hold space.
h

# Step 4: prepare for the next passport record.
:next
# If we haven't reached the last line of input yet, delete the pattern space and
# restart the script without printing anything.
$!d
# Otherwise, we've consumed all the input. Fetch the valid passport count from the
# hold space into the pattern space so that sed will implicitly print it.
g
