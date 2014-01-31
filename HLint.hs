import "hint" HLint.HLint

-- Used hlint --find src/ in the lens repo to generate these
infixl 9 :>

infixr 9 <.>, <., .>, ...
infixr 9 #.

infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!
infixl 8 ^., ^@.
infixl 8 ^#
infixl 8 .#

infixr 8 ^!, ^@!

infixr 4 </>~, <</>~, <.>~, <<.>~
infixr 4 <#~, #~, #%~, <#%~, #%%~
infixr 4 .|.~, .&.~, <.|.~, <.&.~
infixr 4 %@~, .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, <>~, ||~, %~
infixr 4 %%@~, <%@~, %%~, <+~, <*~, <-~, <//~, <^~, <^^~, <**~

infix 4 </>=, <</>=, <.>=, <<.>=
infix 4 <#=, #=, #%=, <#%=, #%%=
infix 4 .|.=, .&.=, <.|.=, <.&.=
infix 4 %@=, .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, <>=, ||=, %=
infix 4 %%@=, <%@=, %%=, <+=, <*=, <-=, <//=, <^=, <^^=, <**=

infixr 2 `zoom`, `magnify`
infixr 2 <~
infixr 2 <<~

infixl 1 &, <&>, ??
