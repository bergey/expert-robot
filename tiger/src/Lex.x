{
module Main where

import           Data.Text (Text)
import           Prelude hiding (LT, GT)

import qualified Data.Text as T

}

%wrapper "basic"

$unicodeIds = $printable # [$white \;\'\(\)]

tokens :-

    $white+            ;
    \/\* (\*[^\/] | [^\*] )* \*\/     ; -- comments TODO nested
    "type"                 { \_ -> Type }
     "array"               { \_ -> Array }
     "of"                  { \_ -> Of }
     "var"                 { \_ -> Var }
     "function"            { \_ -> Function }
     "let"                 { \_ -> Let }
     "in"                  { \_ -> In }
     "end"                 { \_ -> End }
     "nil"                 { \_ -> Nil }
     "if"                  { \_ -> If }
     "then"                { \_ -> Then }
     "else"                { \_ -> Else }
     "while"               { \_ -> While }
     "do"                  { \_ -> Do }
     "for"                 { \_ -> For }
     "to"                  { \_ -> To }
     "break"               { \_ -> Break }
    "="                    { \_ -> Eq }
    ":="                   { \_ -> DefEq }
    ":"                    { \_ -> Colon }
    ","                    { \_ -> Comma }
    "."                    { \_ -> Dot }
    ";"                    { \_ -> SemiColon }
    "&"                    { \_ -> And }
    "|"                    { \_ -> Or }
    "+"                    { \_ -> Plus }
    "-"                    { \_ -> Minus }
    "*"                    { \_ -> Times }
    "/"                    { \_ -> Slash }
    "<>"                   { \_ -> NotEq }
    ">"                    { \_ -> GT }
    "<"                    { \_ -> LT }
    ">="                   { \_ -> GTE }
    "<="                   { \_ -> LTE }
    "("                    { \_ -> LParen }
    ")"                    { \_ -> RParen }
    "{"                    { \_ -> LBrace }
    "}"                    { \_ -> RBrace }
    "["                    { \_ -> LBracket }
    "]"                    { \_ -> RBracket }
    [0-9]+                 {Int . read}
    \" ( \\\" | [^\"] )* \" { String . T.pack . tail . init }
    $unicodeIds+           {Name . T.pack}

{

data Token = Type | Array | Of | Var | Function | Let | In | End | Nil
    | If | Then | Else | While | Do | For | To | Break
    | Eq | DefEq | Colon | Comma | Dot | SemiColon
    | And | Or | Plus | Minus | Times | Slash | NotEq | GT | LT | GTE | LTE
    | LParen | RParen | LBrace | RBrace | LBracket | RBracket
    | StartComment | EndComment
    | Name Text | String Text | Int Int
    deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
