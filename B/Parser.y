{
module Parser (parse) where

import Lexer (scan,Token(..))
import Expression (Expr(..))
}

%name               parseTokens
%tokentype          { Token         }
%error              { parseError    }

%token
    esc             { TEscape       }
    dot             { TDot          }
    lbr             { TLeftBracket  }
    rbr             { TRightBracket }
    var             { TVariable $$  }
%%

Expression  : Application esc var dot Expression    { Appl $1 (Abstr $3 $5)       }
            | esc var dot Expression                { Abstr $2 $4                 }
            | Application                           { $1                          }

Application : Application Atom                      { Appl $1 $2                  }
            | Atom                                  { $1                          }

Atom        : lbr Expression rbr                    { $2                          }
            | var                                   { Var $1                      }


{
parse :: String -> Expr
parse = parseTokens . scan

parseError :: [Token] -> a
parseError = error "Parse error."
}
