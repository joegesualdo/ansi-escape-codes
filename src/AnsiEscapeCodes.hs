module AnsiEscapeCodes where

import Data.List (intercalate)

-- RESOURCES
--   https://en.wikipedia.org/wiki/ANSI_escape_code
--   http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html

-- Types ----------------------------------------------------

-- Reference: http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
data ASCIIControlCode =
  NullCode
  | StartOfHeadingCode
  | StartOfTextCode
  | EndofTextCode
  | EndOfTransmissionCode
  | EnquiryCode
  | AcknowledgeCode
  | BellCode
  | BackspaceCode
  | HorizontalTabCode
  | LineFeedCode
  | VerticalTabCode
  | FormFeedCode
  | CarriageReturnCode
  | ShiftOutCode
  | ShiftInCode
  | DataLinkEscapeCode
  | DeviceControlOneCode
  | DeviceControlTwoCode
  | DeviceControlThreeCode
  | DeviceControlFourCode
  | NegativeAcknowledgeCode
  | SynchronousIdleCode
  | EndOfTransmissionBlockCode
  | CancelCode
  | EndOfMediumCode
  | SubsituteCode
  | EscapeCode
  | FileSeperatorCode
  | GroupSeperatorCode
  | RecordSeperatorCode
  | UnitSeperatorCode
  | SpaceCode
  | DeleteCode

-- TODO: Implement for the rest of the CSIParameters
data CSIParameter =
  CursorUpParameter Integer
  | CursorDownParameter Integer
  | CursorForwardParameter Integer
  | CursorBackParameter Integer
  | CursorNextLineParameter Integer
  | CursorPreviousLineParameter Integer
  | CursorHorizontalAbsoluteParameter Integer
  | CursorPositionParameter Integer Integer
  | EraseScreenAfterCursorParameter
  | EraseScreenBeforeCursorParameter
  | EraseScreenParameter
  | EraseScreenAndScrollbackParameter
  | EraseLineAfterCursorParameter
  | EraseLineBeforeCursorParameter
  | EraseLineParameter
  | ScrollUpParameter Integer
  | ScrollDownParameter Integer
  | HorizontalVerticalPositionParameter Integer Integer
  | SelectGraphicRenditionSequenceParameter [SelectGraphicRenditionParameter]
  | AUXPortOnParameter
  | AUXPortOffParameter
  | DeviceStatusReportParameter
  | SaveCursorPositionParameter
  | RestoreCursorPositionParameter

data SelectGraphicRenditionParameter =
  ResetNormalParameter
  | BoldParameter
  | IncreasedSensitivityParameter
  | FaintParameter
  | DecreasedSensitivityParameter
  | ItalicParameter
  | UnderlineParameter
  | SlowBlinkParameter
  | RapidBlinkParameter
  | ReverseVideoParameter
  | ConcealParameter
  | CrossedOutParameter
  | PrimaryFontParameter
  | DefaultFontParameter
  | AlternativeFontParameter Int
  | FrakturParameter
  | BoldOffParameter
  | DoubleUnderlineParameter
  | NormalColorParameter
  | NormalIntensityParameter
  | NotItalicParameter
  | NotFrakturParameter
  | UnderlineOffParameter
  | BlinkOffParameter
  | InverseOffParameter
  | RevealParameter
  | NotCrossedOutParameter
  | SetForegroundColorParameter ForegroundColor
  | DefaultForegroundColorParameter
  | SetBackgroundColorParameter BackgroundColor
  | DefaultBackgroundColorParameter
  | FramedParameter
  | EncircledParameter
  | OverlinedParameter
  | NotFramedParameter
  | NotEncircledParameter
  | NotOverlinedParameter
  | IdeogramUnderlineParameter
  | RightSideLineParameter
  | IdeogramDoubleUnderlineParameter
  | DoubleLIneOnRightSideParameter
  | IdeogramStressMarkingParameter
  | IdeogramAttributesOffParameter
  | SetBrightForegroundColorParameter BrightForegroundColor
  | SetBrightBackgroundColorParameter BrightBackgroundColor

data ForegroundColor =
  BlackForeground
  | RedForeground
  | GreenForeground
  | YellowForeground
  | BlueForeground
  | MagentaForeground
  | CyanForeground
  | WhiteForeground
  | ForegroundColor Color

data BrightForegroundColor =
  BrightBlackForeground
  | BrightRedForeground
  | BrightGreenForeground
  | BrightYellowForeground
  | BrightBlueForeground
  | BrightMagentaForeground
  | BrightCyanForeground
  | BrightWhiteForeground

data BackgroundColor =
  BlackBackground
  | RedBackground
  | GreenBackground
  | YellowBackground
  | BlueBackground
  | MagentaBackground
  | CyanBackground
  | WhiteBackground
  | BackgroundColor Color

data BrightBackgroundColor =
  BrightBlackBackground
  | BrightRedBackground
  | BrightGreenBackground
  | BrightYellowBackground
  | BrightBlueBackground
  | BrightMagentaBackground
  | BrightCyanBackground
  | BrightWhiteBackground

data Color =
  Color256 Integer-- TODO: Validate only integers 1-256
  | RGB Integer Integer Integer -- TODO: Validate only integers 1-256

data EscapeSequence =
  CursorUpSequence Integer
  | CursorDownSequence Integer
  | CursorForwardSequence Integer
  | CursorBackSequence Integer
  | CursorNextLineSequence Integer
  | CursorPreviousLineSequence Integer
  | CursorHorizontalAbsoluteSequence Integer
  | CursorPositionSequence Integer Integer
  | EraseScreenAfterCursorSequence
  | EraseScreenBeforeCursorSequence
  | EraseScreenSequence
  | EraseScreenAndScrollbackSequence
  | EraseLineAfterCursorSequence
  | EraseLineBeforeCursorSequence
  | EraseLineSequence
  | ScrollUpSequence Integer
  | ScrollDownSequence Integer
  | HorizontalVerticalPositionSequence Integer Integer
  | SelectGraphicRenditionSequence [SelectGraphicRenditionParameter]
  | AUXPortOnSequence
  | AUXPortOffSequence
  | DeviceStatusReportSequence
  | SaveCursorPositionSequence
  | RestoreCursorPositionSequence
  | ResetColorsSequence
  | ResetAttributesSequence

data EscapeSequenceEntry = EscapeSequenceEntry

-- Instances ----------------------------------------------------

-- TODO: Implement for the rest of the ASCIIControlCodes
instance Show ASCIIControlCode where
  show EscapeCode = "\x001b"
  show _ = error "That Ascii Control Code doesn't exist"

instance Show EscapeSequenceEntry where
  show _ = (show EscapeCode) ++ "["

instance Show CSIParameter where
  show (CursorUpParameter moveCount) = (show moveCount) ++ "A"
  show (CursorDownParameter moveCount) = (show moveCount) ++ "B"
  show (CursorForwardParameter moveCount) = (show moveCount) ++ "C"
  show (CursorBackParameter moveCount) = (show moveCount) ++ "D"
  show (CursorNextLineParameter moveCount) = (show moveCount) ++ "E"
  show (CursorPreviousLineParameter moveCount) = (show moveCount) ++ "F"
  show (CursorHorizontalAbsoluteParameter moveCount) = (show moveCount) ++ "F"
  show (CursorPositionParameter rowPosition columnPosition) = (show rowPosition) ++ ";" ++ (show columnPosition) ++ "H"
  show EraseScreenAfterCursorParameter = "0J"
  show EraseScreenBeforeCursorParameter = "1J"
  show EraseScreenParameter = "2J"
  show EraseScreenAndScrollbackParameter= "3J"
  show EraseLineAfterCursorParameter = "0K"
  show EraseLineBeforeCursorParameter = "1K"
  show EraseLineParameter = "2K"
  show (ScrollUpParameter moveCount) = (show moveCount) ++ "S"
  show (ScrollDownParameter moveCount) = (show moveCount) ++ "T"
  show (HorizontalVerticalPositionParameter rowPosition columnPosition) = (show rowPosition) ++ ";" ++ (show columnPosition) ++ "f"
  show (SelectGraphicRenditionSequenceParameter sgrParameters) = (intercalate ";" $ map show sgrParameters) ++ "m"
  show (AUXPortOnParameter) = "5i"
  show (AUXPortOffParameter) = "4i"
  show (DeviceStatusReportParameter) = "6n" -- This is probably wrong
  show (SaveCursorPositionParameter) = "s"
  show (RestoreCursorPositionParameter) = "u"

instance Show SelectGraphicRenditionParameter where
  show ResetNormalParameter = "0"
  show BoldParameter = "1"
  show IncreasedSensitivityParameter = "1"
  show FaintParameter = "2"
  show DecreasedSensitivityParameter = "2"
  show ItalicParameter = "3"
  show UnderlineParameter = "4"
  show SlowBlinkParameter = "5"
  show RapidBlinkParameter = "6"
  show ReverseVideoParameter = "7"
  show ConcealParameter = "8"
  show CrossedOutParameter = "9"
  show PrimaryFontParameter = "10"
  show DefaultFontParameter = "10"
  show (AlternativeFontParameter x) = (show x) -- TODO: x should only be 11-19
  show FrakturParameter = "20"
  show BoldOffParameter = "21"
  show DoubleUnderlineParameter = "21"
  show NormalColorParameter = "22"
  show NormalIntensityParameter = "22"
  show NotItalicParameter = "23"
  show NotFrakturParameter = "23"
  show UnderlineOffParameter = "24"
  show BlinkOffParameter = "25"
  show InverseOffParameter = "27"
  show RevealParameter = "28"
  show NotCrossedOutParameter = "29"
  show (SetForegroundColorParameter (ForegroundColor (RGB redColorCode  greenColorCode blueColorCode))) = "38;" ++ (show (ForegroundColor (RGB redColorCode greenColorCode blueColorCode)))
  show (SetForegroundColorParameter (ForegroundColor (Color256 colorCode256))) = "38;" ++ (show (ForegroundColor (Color256 colorCode256)))
  show (SetForegroundColorParameter color) = (show color)
  show DefaultForegroundColorParameter = "39"

instance Show ForegroundColor where
  show BlackForeground = "30"
  show RedForeground = "31"
  show GreenForeground = "32"
  show YellowForeground = "33"
  show BlueForeground = "34"
  show MagentaForeground = "35"
  show CyanForeground = "36"
  show WhiteForeground = "37"
  show (ForegroundColor (Color256 colorCode)) = "5;" ++ (show colorCode)
  show (ForegroundColor (RGB redColorCode greenColorCode blueColorCode)) = "2;" ++ (show redColorCode) ++ ";" ++ (show greenColorCode) ++ ";" ++ (show blueColorCode)

instance Show BackgroundColor where
  show BlackBackground = "40"
  show RedBackground = "41"
  show GreenBackground = "42"
  show YellowBackground = "43"
  show BlueBackground = "44"
  show MagentaBackground = "45"
  show CyanBackground = "46"
  show WhiteBackground = "47"
  show (BackgroundColor (Color256 colorCode)) = "5;" ++ (show colorCode)
  show (BackgroundColor (RGB redColorCode greenColorCode blueColorCode)) = "2;" ++ (show redColorCode) ++ ";" ++ (show greenColorCode) ++ ";" ++ (show blueColorCode)

instance Show BrightForegroundColor where
  show BrightBlackForeground = "1;30"
  show BrightRedForeground = "1;31"
  show BrightGreenForeground = "1;32"
  show BrightYellowForeground = "1;33"
  show BrightBlueForeground = "1;34"
  show BrightMagentaForeground = "1;35"
  show BrightCyanForeground = "1;36"
  show BrightWhiteForeground = "1;37"

instance Show BrightBackgroundColor where
  show BrightBlackBackground = "100"
  show BrightRedBackground = "101"
  show BrightGreenBackground = "102"
  show BrightYellowBackground = "103"
  show BrightBlueBackground = "104"
  show BrightMagentaBackground = "105"
  show BrightCyanBackground = "106"
  show BrightWhiteBackground = "107"

instance Show EscapeSequence where
  show (CursorUpSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorUpParameter moveCount))
  show (CursorDownSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorDownParameter moveCount))
  show (CursorForwardSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorForwardParameter moveCount))
  show (CursorBackSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorBackParameter moveCount))
  show (CursorNextLineSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorNextLineParameter moveCount))
  show (CursorPreviousLineSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorPreviousLineParameter moveCount))
  show (CursorHorizontalAbsoluteSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (CursorHorizontalAbsoluteParameter moveCount))
  show (CursorPositionSequence rowPosition columnPosition) =
    (show EscapeSequenceEntry)
    ++ (show (CursorPositionParameter rowPosition columnPosition))
  show (EraseScreenAfterCursorSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseScreenAfterCursorParameter))
  show (EraseScreenBeforeCursorSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseScreenBeforeCursorParameter))
  show (EraseScreenSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseScreenParameter))
  show (EraseScreenAndScrollbackSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseScreenAndScrollbackParameter))
  show (EraseLineAfterCursorSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseLineAfterCursorParameter))
  show (EraseLineBeforeCursorSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseLineBeforeCursorParameter))
  show (EraseLineSequence) =
    (show EscapeSequenceEntry)
    ++ (show (EraseLineParameter))
  show (ScrollUpSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (ScrollUpParameter moveCount))
  show (ScrollDownSequence moveCount) =
    (show EscapeSequenceEntry)
    ++ (show (ScrollDownParameter moveCount))
  show (HorizontalVerticalPositionSequence rowPosition columnPosition) =
    (show EscapeSequenceEntry)
    ++ (show (HorizontalVerticalPositionParameter rowPosition columnPosition))
  show (SelectGraphicRenditionSequence srgParameters) =
    (show EscapeSequenceEntry)
    ++ (show (SelectGraphicRenditionSequenceParameter srgParameters))
  show (AUXPortOnSequence) =
    (show EscapeSequenceEntry)
    ++ (show (AUXPortOnParameter))
  show (AUXPortOffSequence) =
    (show EscapeSequenceEntry)
    ++ (show (AUXPortOffParameter))
  show (DeviceStatusReportSequence) =
    (show EscapeSequenceEntry)
    ++ (show (DeviceStatusReportParameter))
  show (SaveCursorPositionSequence) =
    (show EscapeSequenceEntry)
    ++ (show (SaveCursorPositionParameter))
  show (RestoreCursorPositionSequence) =
    (show EscapeSequenceEntry)
    ++ (show (RestoreCursorPositionParameter))
  show (ResetAttributesSequence) =
    (show EscapeCode)
    ++ "[0m"
  show (ResetColorsSequence) =
    (show EscapeCode)
    ++ "[39;49m"

-- -- Main -------------------------------------------------
-- main :: IO ()
-- main = do
--   putStr $
--     "Hi"
--     ++ (show (CursorForwardSequence 1))
--     ++ (show (CursorForwardSequence 1))
--     ++ "B"
--     ++ (show (CursorForwardSequence 1))
--     ++ (show (CursorForwardSequence 1))
--     ++ "O"
--     ++ (show (CursorForwardSequence 1))
--     ++ (show (CursorForwardSequence 1))
--     ++ "0"

    -- (show (SelectGraphicRenditionSequence [UnderlineParameter, (SetForegroundColorParameter (ForegroundColor (RGB 177 209 174)))]))
  -- putStr $
  --   (show (CursorDownSequence 3)) ++ "Bro" ++
  --     (show (CursorNextLineSequence 1)) ++ "meow\n"

