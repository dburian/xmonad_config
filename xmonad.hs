import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Util.Loggers
-- import XMonad.Util.Brightness (increase, decrease)
import XMonad.Actions.Promote
-- import XMonad.Actions.Volume
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Ungrab
import XMonad.Layout.NoBorders (smartBorders)

-- Variable definitions
myModMask = mod4Mask
myTerminal = "alacritty"

myDmenu :: String
myDmenu = "rofi -config ~/.config/rofi/config.rasi -show"

myXmobar :: String
myXmobar = "xmobar ~/.config/xmobar/xmobarrc"

myScreenShotArgs :: String
myScreenShotArgs = "-e 'mv $f ~/downloads/'"

myKeys :: [(String, X ())]
myKeys =
    [ ("M-<Space>", spawn myDmenu)
    , ("M-<Return>", spawn myTerminal)
    , ("M-S-<Return>", promote)
    , ("M-k", sendMessage NextLayout)
    , ("M-S-s", unGrab *> spawn ("scrot -s " ++ myScreenShotArgs))
    , ("M-s", unGrab *> spawn ("scrot " ++ myScreenShotArgs))
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
    -- , ("<XF86MonBrightnessUp>", increase)
    -- , ("<XF86MonBrightnessDown>", decrease)
    ]

myLayout = smartBorders $ myTall ||| Mirror myTall ||| Full
  where
    myTall = Tall 1 (10/100) (1/2)


myWhite = "#ffffff"
myBlack = "#000000"
myGray = "#aaaaaa"

myConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , focusFollowsMouse = False
  , layoutHook = myLayout
  , normalBorderColor = myBlack
  , focusedBorderColor = myWhite
  }
  `additionalKeysP` myKeys


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = " "
    , ppWsSep = ""
    , ppTitleSanitize   = xmobarStrip
    , ppVisible         = xmobarColor myBlack myGray . wrap " " " "
    , ppCurrent         = xmobarColor myBlack myWhite . wrap " " " "
    , ppHidden          = xmobarColor myWhite myBlack . wrap " " " "
    , ppHiddenNoWindows = xmobarColor myGray myBlack . wrap " " " "
    , ppUrgent          = wrap "!" "!"
    , ppOrder           = myPPOrder
    , ppExtras          = [logTitles ppWindow ppWindow]
    }
  where
    myPPOrder [ws, l, _, _] = [ws, l]
    myPPOrder a = a

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30


myXmobarProp = withEasySB (statusBarProp myXmobar (pure myXmobarPP)) defToggleStrutsKey


main :: IO()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . myXmobarProp
  $ myConfig
