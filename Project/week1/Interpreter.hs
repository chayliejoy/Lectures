module Interpreter (interprete) where

import Language

import Graphics.Rendering.Cairo hiding (x)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List

-- The state is the current object list, the stack, and the directory
data State = State [PSExpr] [PSExpr]

instance Show State where
  show (State c s) = cstr ++ "\n" ++ sstr
    where
      cstr = case c of
        [] -> " Execution stack empty."
        _  -> " Execution Stack:\n  " ++ concat (intersperse "\n  " (map show c))
      sstr = case s of
        [] -> " Data stack empty."
        _  -> " Data Stack:\n  " ++ concat (intersperse "\n  " (map show s))

emptyState :: State
emptyState = State [] []

interprete :: PSExpr -> Render (Result State)
interprete (PSProcedure cmds) = bigStepPs $ State cmds []
interprete _ = return $ Left ("Unexpected toplevel symbol", emptyState)

-- Helper for binary arithmetic operators
binNumOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> State -> Result State
binNumOp iop _   (State c ((PSInt i):(PSInt j):s)  ) = Right (State c ((PSInt  $ iop j i):s))
binNumOp _   dop (State c ((PSReal i):(PSReal j):s)) = Right (State c ((PSReal $ dop j i):s))
binNumOp _   dop (State c ((PSInt i):(PSReal j):s) ) = Right (State c ((PSReal $ dop j (fromIntegral i)):s))
binNumOp _   dop (State c ((PSReal i):(PSInt j):s) ) = Right (State c ((PSReal $ dop (fromIntegral j) i):s))
binNumOp _ _ s = Left ("Illegal arguments to binary operator.", s)

-- Helper for unary arithmetic operators
unaryNumOp :: (Int -> Int) -> (Double -> Double) -> State -> Result State
unaryNumOp iop _   (State c ((PSInt i):s) ) = Right (State c ((PSInt  $ iop i):s))
unaryNumOp _   dop (State c ((PSReal i):s)) = Right (State c ((PSReal $ dop i):s))
unaryNumOp _ _ s = Left ("Illegal arguments to unary operator.", s)

-- Helper to handle the case where a recursion can fail
tryRecurse :: Result State -> Render (Result State)
tryRecurse r@(Left _) = return r
tryRecurse (Right s) = bigStepPs s

-- Main interpreter
bigStepPs :: State -> Render (Result State)
bigStepPs s@(State [] _) = return $ Right s
bigStepPs (State (i@(PSInt _):c)         s) = bigStepPs (State c (i:s))
bigStepPs (State (r@(PSReal _):c)        s) = bigStepPs (State c (r:s))
bigStepPs (State (b@(PSBoolean _):c)     s) = bigStepPs (State c (b:s))
bigStepPs (State (a@(PSArray _):c)       s) = bigStepPs (State c (a:s))
bigStepPs (State (p@(PSProcedure _):c)   s) = bigStepPs (State c (p:s))

bigStepPs (State (b@(PSLiteralName _):c) s) = bigStepPs (State c (b:s))

bigStepPs s@(State ((PSExecutableName n):cmds) stack) =
  case lookup n sysdict of
    Just e -> bigStepPs (State (e:cmds) stack)
    Nothing ->  return $ Left ("name not found: " ++ n, s)

bigStepPs (State (PSOp PSdup:c)  (a:s))   = bigStepPs $ State c (a:a:s)
bigStepPs (State (PSOp PSpop:c)  (_:s))   = bigStepPs $ State c      s 
bigStepPs (State (PSOp PSexch:c) (a:b:s)) = bigStepPs $ State c (b:a:s)
bigStepPs (State (PSOp PSadd:c) s) = tryRecurse $ binNumOp (+) (+) (State c s)
bigStepPs (State (PSOp PSsub:c) s) = tryRecurse $ binNumOp (-) (-) (State c s)
bigStepPs (State (PSOp PSmul:c) s) = tryRecurse $ binNumOp (*) (*) (State c s)

bigStepPs (State (PSOp PSdiv:c) ((PSInt  j):(PSInt  i):s)) = bigStepPs (State c (PSReal ((fromIntegral i) / (fromIntegral j)):s))
bigStepPs (State (PSOp PSdiv:c) ((PSReal j):(PSInt  i):s)) = bigStepPs (State c (PSReal ((fromIntegral i) / j):s))
bigStepPs (State (PSOp PSdiv:c) ((PSInt  j):(PSReal i):s)) = bigStepPs (State c (PSReal (i / (fromIntegral j)):s))
bigStepPs (State (PSOp PSdiv:c) ((PSReal j):(PSReal i):s)) = bigStepPs (State c (PSReal (i / j):s))

bigStepPs (State (PSOp PSneg:c) s) = tryRecurse $ unaryNumOp (0 -) (0.0 -) (State c s)

bigStepPs (State (PSOp PSeq:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a == b):s))
bigStepPs (State (PSOp PSne:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a /= b):s))
bigStepPs (State (PSOp PSgt:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a > b):s) )
bigStepPs (State (PSOp PSge:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a >= b):s))
bigStepPs (State (PSOp PSlt:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a < b):s) )
bigStepPs (State (PSOp PSle:c) (b:a:s)) = bigStepPs (State c (PSBoolean (a <= b):s))

bigStepPs (State (PSOp PSifelse:c) (PSProcedure no:PSProcedure yes:PSBoolean cond:s))
  | cond      = bigStepPs $ State (yes ++ c) s
  | otherwise = bigStepPs $ State (no ++ c)  s

-- Thise implementation of repeate is very naive: it simply copies to code n times
bigStepPs (State (PSOp PSrepeat:c) (PSProcedure code:PSInt n:s)) = bigStepPs $ State c' s
  where c' = (concat $ take n (repeat code)) ++ c

bigStepPs (State (PSOp PSnewpath:c) s) = newPath >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSmoveto:c) ((PSInt y):(PSInt x):s)) =
  moveTo (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s)
bigStepPs (State (PSOp PSmoveto:c) ((PSReal y):(PSReal x):s)) =
  moveTo x y                               >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSclosepath:c) s) = closePath >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSlineto:c) ((PSInt y):(PSInt x):s)) =
  lineTo (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s)
bigStepPs (State (PSOp PSlineto:c) ((PSReal y):(PSReal x):s)) =
  lineTo x y                               >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSsetlinewidth:c) ((PSInt w):s)) =
  setLineWidth (fromIntegral w) >> (bigStepPs $ State c s)
bigStepPs (State (PSOp PSsetlinewidth:c) ((PSReal w):s)) =
  setLineWidth w                >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSstroke:c) s) = stroke >> (bigStepPs $ State c s)

bigStepPs (State (PSOp PSsetrgbcolor:c) ((PSReal b):(PSReal g):(PSReal r):s)) =
  setSourceRGB r g b >> (bigStepPs $ State c s)

bigStepPs s = return $ Left ("No case for current state.", s)
