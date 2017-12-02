module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, myBacksp)
import Graphics.UI.Gtk.Layout.Grid
import Control.Concurrent
import Graphics.UI.Gtk.Buttons.Button


main :: IO ()
main = do
  st <- newIORef (Value "" Nothing)
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator ©️(Emrys & Lydia)"
             , windowResizable     := True
             , windowDefaultWidth  := 260
             , windowDefaultHeight := 380 ]

  display <- entryNew
  set display [ entryEditable := False
              , entryXalign   := 1 -- makes contents right-aligned
              , entryText     := "0" ]

  grid <- gridNew
  gridSetRowHomogeneous grid True
  let position a b w h item = gridAttach grid item a b w h
      defButton = myNewBtn st display
  position 0 0 5 1 display

  defButton "←"   myBacksp  >>= position 0 2 2 1
  defButton "AC"  myClearCurr >>= position 2 2 2 1
--  defButton "C"   myClear >>= a 2 2 1 1

  defButton "1"   (myCharEntered '1') >>= position 0 5 1 1
  defButton "2"   (myCharEntered '2') >>= position 1 5 1 1
  defButton "3"   (myCharEntered '3') >>= position 2 5 1 1

  defButton "4"   (myCharEntered '4') >>= position 0 4 1 1
  defButton "5"   (myCharEntered '5') >>= position 1 4 1 1
  defButton "6"   (myCharEntered '6') >>= position 2 4 1 1

  defButton "7"   (myCharEntered '7') >>= position 0 3 1 1
  defButton "8"   (myCharEntered '8') >>= position 1 3 1 1
  defButton "9"   (myCharEntered '9') >>= position 2 3 1 1

  defButton "+"   (myOp Addition) >>= position 3 3 1 1
  defButton "–"   (myOp Subtraction) >>= position 3 4 1 1
  defButton "*"   (myOp Multiplication) >>= position 3 5 1 1
  defButton "/"   (myOp Division) >>= position 3 6 1 1

  defButton "%"   (myOp Percent) >>= position 4 2 1 1
  defButton "mod"   (myOp Module) >>= position 4 4 1 1
  defButton "^"   (myOp Power) >>= position 4 3 1 1
  defButton "="   myFuncBody >>= position 4 5 1 2
  defButton "0"   (myCharEntered '0') >>= position 0 6 2 1
  defButton "."   myNumDot >>= position 2 6 1 1

  containerAdd window grid
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  widgetShowAll window
  mainGUI


data Value = Value String (Maybe Action)

data Action
  = Addition       String
  | Subtraction    String
  | Multiplication String
  | Division       String
  | Power          String
  | Module         String
  | Percent        String

mybuttonAct :: (String -> String) -> Action -> Action
mybuttonAct f (Addition       a) = Addition       (f a)
mybuttonAct f (Subtraction    a) = Subtraction    (f a)
mybuttonAct f (Multiplication a) = Multiplication (f a)
mybuttonAct f (Division       a) = Division       (f a)
mybuttonAct f (Power          a) = Power          (f a)
mybuttonAct f (Module         a) = Module         (f a)
mybuttonAct f (Percent        a) = Percent        (f a)

-- Get second argument from 'Action'.
myArgu2 :: Action -> String
myArgu2 (Addition       a) = a
myArgu2 (Subtraction    a) = a
myArgu2 (Multiplication a) = a
myArgu2 (Division       a) = a
myArgu2 (Power          a) = a
myArgu2 (Module         a) = a
myArgu2 (Percent        a) = "10.0"

-- Give out and calculate final value.
myValueGiven :: Value -> String
myValueGiven (Value x action) =
  g x ++ f a ++ (if null y then "" else g y)
  where
    (a, y) =
      case action of
        Nothing                   -> ("", "")
        Just (Addition       arg) -> ("+", arg)
        Just (Subtraction    arg) -> ("–", arg)
        Just (Multiplication arg) -> ("*", arg)
        Just (Division       arg) -> ("÷", arg)
        Just (Power          arg) -> ("^", arg)
        Just (Module         arg) -> ("mod", arg)
        Just (Percent        arg) -> ("%", arg)
    f "" = ""
    f l  = " " ++ l ++ " "
    g "" = "0"
    g xs = reverse xs

-- Operate the operator, and change the state during operating
myOp :: (String -> Action) -> Value -> Value
myOp o value =
  let (Value x action) = myFuncBody value
  in Value x $ Just $
    case action of
      Nothing -> o ""
      Just  a -> o (myArgu2 a)

-- Delete the element (and change the state)
myBacksp :: Value -> Value
myBacksp (Value x action) =
  case action of
    Nothing -> Value (drop 1 x) Nothing
    Just  a -> Value x (Just $ mybuttonAct (drop 1) a)

-- Add dot between numbers.
myNumDot :: Value -> Value
myNumDot (Value x action) =
  let f xs = if '.' `elem` xs then xs else '.' : xs
  in case action of
       Nothing -> Value (f x) Nothing
       Just  a -> Value x (Just $ mybuttonAct f a)

-- When a special char is entered, change its state.
myCharEntered :: Char -> Value -> Value
myCharEntered ch (Value x action) =
  case action of
    Nothing -> Value (ch:x) Nothing
    Just  a -> Value x (Just $ mybuttonAct (ch:) a)


-- Remove current element and change its state
myClearCurr :: Value -> Value
myClearCurr (Value x action) =
  case action of
    Nothing -> Value "" Nothing
    Just  a ->
      if null (myArgu2 a)
        then Value "" Nothing
        else Value x (Just $ mybuttonAct (const "") a)

-- Clear elements on calculator..basically set it to default value
myClear :: Value -> Value
myClear = const (Value "" Nothing)

-- Check current state of calculator and implement functions
myFuncBody :: Value -> Value
myFuncBody (Value x action) =
  case action of
    Nothing -> Value x Nothing
    Just  a ->
      if null (myArgu2 a)
        then Value x action
        else Value result Nothing
          where
            g  :: String -> Double
            g ""       = 0
            g ('.':xs) = g ('0':'.':xs)
            g xs       = read (reverse xs)
            x' = g x
            y' = g (myArgu2 a)
            result = reverse . show $
              case a of
                Addition       _ -> x' + y'
                Subtraction    _ -> x' - y'
                Multiplication _ -> x' * y'
                Division       _ -> x' / y'
                Power          _ -> x' ^ (floor y')
                Module         _ -> fromIntegral ((floor x') `mod` (floor y'))
                Percent        _ -> x' * y'

-- Make my new button and put it into correct position on mycalculator.
myNewBtn
  :: IORef Value       -- IORef :: calculator state
  -> Entry             -- show updated info (on screen)
  -> String            -- label(s)
  -> (Value -> Value)  -- change state after pressing buttons
  -> IO Button         -- produce (a) button object(s) 

myNewBtn st display label mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    myUpdateScreen display value
  return btn


-- Update the calculator’s screen and shows values.
myUpdateScreen :: Entry -> Value -> IO ()
myUpdateScreen display value =
  set display [ entryText := myValueGiven value ]
