#lang brag
; changing the order of those lines can cause errors. I tinkered with it until it was not erroring out.
empty-structure: 'LEFT-BRACKET 'RIGHT-BRACKET
word: 'NUMBER | 'STRING | 'DATE
key-value: word 'EQUAL word-or-structure
mapping: 'LEFT-BRACKET key-value+ 'RIGHT-BRACKET
list: 'LEFT-BRACKET word-or-structure+ 'RIGHT-BRACKET
word-or-structure: word | structure
structure: mapping | list | empty-structure
; uncommenting this gives: key-value: Rule key-value has no definition in: key-value
; top-level-mapping: key-value+