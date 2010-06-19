module Data.UI.Input.Keyboard.Vty(fromVty, toVty, keyFromVty, keyToVty) where

import Prelude hiding (Left, Right)
import Control.Arrow((***))
import Control.Applicative(liftA2)
import Data.UI.Input.Keyboard.ModKey(ModKey(..), unModKey)
import Data.UI.Input.Keyboard.Mods(ModState(..), ModsState(..))
import Data.UI.Input.Keyboard.Keys(Key(..))
import qualified Graphics.Vty as Vty
import Data.Char(intToDigit, digitToInt)

modsFromVty :: [Vty.Modifier] -> ModsState
modsFromVty ms = ModsState
            (modState (Vty.MShift `elem` ms))
            (modState (Vty.MCtrl `elem` ms))
            (modState (Vty.MAlt `elem` ms))
            (modState (Vty.MMeta `elem` ms))
            False False
  where
    modState p = ModState p False False

modsToVty :: ModsState -> Maybe [Vty.Modifier]
modsToVty (ModsState shiftState ctrlState altState metaState False False) =
  Just . concat $ [
    [Vty.MShift | anyPressed shiftState],
    [Vty.MCtrl | anyPressed ctrlState],
    [Vty.MAlt | anyPressed altState],
    [Vty.MMeta | anyPressed metaState]
    ]
modsToVty _ = Nothing

fromVty :: ([Vty.Modifier], Vty.Key) -> ModKey
fromVty = uncurry ModKey . (modsFromVty *** keyFromVty)

toVty :: ModKey -> Maybe ([Vty.Modifier], Vty.Key)
toVty = uncurry (liftA2 (,)) . (modsToVty *** keyToVty) . unModKey

keyFromVty :: Vty.Key -> Key
keyFromVty key =
  case key of
    Vty.KBS         -> Backspace
    Vty.KASCII '\t' -> Tab
    Vty.KBackTab    -> BackTab
    Vty.KEnter      -> Return
    -- No Clear
    Vty.KPause      -> Pause
    Vty.KEsc        -> Escape
    Vty.KASCII c ->
      case c of
        ' '  -> Space
        '!'  -> Exclaim
        '"'  -> DoubleQuote
        '#'  -> Hash
        '$'  -> Dollar
        '&'  -> Ampersand
        '\'' -> Quote
        '('  -> LeftParen
        ')'  -> RightParen
        '*'  -> Asterisk
        '+'  -> Plus
        ','  -> Comma
        '-'  -> Minus
        '.'  -> Period
        '/'  -> Slash
        ':'  -> Colon
        ';'  -> Semicolon
        '<'  -> Less
        '='  -> Equals
        '>'  -> Greater
        '?'  -> QuestionMark
        '@'  -> At
        '['  -> LeftBracket
        '\\' -> Backslash
        ']'  -> RightBracket
        '^'  -> Caret
        '_'  -> Underscore
        '`'  -> Backquote
        x
          | x `elem` ['0'..'9'] -> Digit (digitToInt x)
          | otherwise -> Letter x

    Vty.KDel        -> Delete
    Vty.KIns        -> Insert
    -- No "World" keys
    -- No KeypadDigit
    -- No KeypadPeriod
    -- No KeypadDivide
    -- No KeypadMultiply
    -- No KeypadMinus
    -- No KeypadPlus
    -- No KeypadEnter
    -- No KeypadEquals
    Vty.KUp         -> Up
    Vty.KDown       -> Down
    Vty.KRight      -> Right
    Vty.KLeft       -> Left
    Vty.KHome       -> Home
    Vty.KEnd        -> End
    Vty.KPageUp     -> PageUp
    Vty.KPageDown   -> PageDown
    Vty.KFun n      -> Function n
    -- No NumLock
    -- No CapsLock
    -- No ScrolLock
    -- No RShift
    -- No LShift
    -- No RCtrl
    -- No LCtrl
    -- No RAlt
    -- No LAlt
    -- No RCmdKey
    -- No LCmdKey
    -- No LSuper
    -- No RSuper
    -- No Mode
    -- No Compose
    -- No Help
    Vty.KPrtScr     -> PrintScreen
    Vty.KNP5        -> KeypadDigit 5
    Vty.KMenu       -> Menu
    -- No Sysreq
    -- No Break
    -- No Power
    -- No Euro
    -- No Undo

keyToVty :: Key -> Maybe Vty.Key
keyToVty key =
  case key of
    Backspace -> Just $ Vty.KBS
    Tab -> Just $ Vty.KASCII '\t'
    BackTab -> Just $ Vty.KBackTab
    Return -> Just $ Vty.KEnter
    Pause -> Just $ Vty.KPause
    Escape -> Just $ Vty.KEsc
    Space -> Just $ Vty.KASCII ' '
    Exclaim -> Just $ Vty.KASCII '!'
    DoubleQuote -> Just $ Vty.KASCII '"'
    Hash -> Just $ Vty.KASCII '#'
    Dollar -> Just $ Vty.KASCII '$'
    Ampersand -> Just $ Vty.KASCII '&'
    Quote -> Just $ Vty.KASCII '\''
    LeftParen -> Just $ Vty.KASCII '('
    RightParen -> Just $ Vty.KASCII ')'
    Asterisk -> Just $ Vty.KASCII '*'
    Plus -> Just $ Vty.KASCII '+'
    Comma -> Just $ Vty.KASCII ','
    Minus -> Just $ Vty.KASCII '-'
    Period -> Just $ Vty.KASCII '.'
    Slash -> Just $ Vty.KASCII '/'
    Colon -> Just $ Vty.KASCII ':'
    Semicolon -> Just $ Vty.KASCII ';'
    Less -> Just $ Vty.KASCII '<'
    Equals -> Just $ Vty.KASCII '='
    Greater -> Just $ Vty.KASCII '>'
    QuestionMark -> Just $ Vty.KASCII '?'
    At -> Just $ Vty.KASCII '@'
    LeftBracket -> Just $ Vty.KASCII '['
    Backslash -> Just $ Vty.KASCII '\\'
    RightBracket -> Just $ Vty.KASCII ']'
    Caret -> Just $ Vty.KASCII '^'
    Underscore -> Just $ Vty.KASCII '_'
    Backquote -> Just $ Vty.KASCII '`'
    Digit x -> Just $ Vty.KASCII (intToDigit x)
    Letter x -> Just $ Vty.KASCII x
    Delete -> Just $ Vty.KDel
    Insert -> Just $ Vty.KIns
    Up -> Just $ Vty.KUp
    Down -> Just $ Vty.KDown
    Right -> Just $ Vty.KRight
    Left -> Just $ Vty.KLeft
    Home -> Just $ Vty.KHome
    End -> Just $ Vty.KEnd
    PageUp -> Just $ Vty.KPageUp
    PageDown -> Just $ Vty.KPageDown
    Function n -> Just $ Vty.KFun n
    PrintScreen -> Just $ Vty.KPrtScr
    KeypadDigit 5 -> Just $ Vty.KNP5
    Menu -> Just Vty.KMenu
    Clear -> Nothing
    World _ -> Nothing
    KeypadDigit _ -> Nothing
    KeypadPeriod -> Nothing
    KeypadDivide -> Nothing
    KeypadMultiply -> Nothing
    KeypadMinus -> Nothing
    KeypadPlus -> Nothing
    KeypadEnter -> Nothing
    KeypadEquals -> Nothing
    NumLock -> Nothing
    CapsLock -> Nothing
    ScrolLock -> Nothing
    RShift -> Nothing
    LShift -> Nothing
    RCtrl -> Nothing
    LCtrl -> Nothing
    RAlt -> Nothing
    LAlt -> Nothing
    RCmdKey -> Nothing
    LCmdKey -> Nothing
    LSuper -> Nothing
    RSuper -> Nothing
    Mode -> Nothing
    Compose -> Nothing
    Help -> Nothing
    Sysreq -> Nothing
    Break -> Nothing
    Power -> Nothing
    Euro -> Nothing
    Undo -> Nothing
