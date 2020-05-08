{
module Lexer (scan,Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = a-z
$hatch = '

tokens :-
    $white+                         ;
    "\"                             { \_ -> TEscape         }
    "."                             { \_ -> TDot            }
    "("                             { \_ -> TLeftBracket    }
    ")"                             { \_ -> TRightBracket   }
    [$alpha][$alpha$digit$hatch]*   { \s -> TVariable s     }

{
data Token = TEscape
           | TDot
           | TLeftBracket
           | TRightBracket
           | TVariable String
           deriving (Eq,Show)

scan :: String -> [Token]
scan = alexScanTokens
}
