import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Util.Loggers
-- import XMonad.Util.Brightness (increase, decrease)
import XMonad.Actions.Promote
-- import XMonad.Actions.Volume
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftPrevScreen, shiftNextScreen)
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
    , ("M-w", prevScreen)
    , ("M-S-w", shiftPrevScreen >> prevScreen)
    , ("M-e", nextScreen)
    , ("M-S-e", shiftNextScreen >> nextScreen)
    , ("M-k", sendMessage NextLayout)
    , ("M-S-s", unGrab *> spawn ("scrot -s " ++ myScreenShotArgs))
    , ("M-s", unGrab *> spawn ("scrot " ++ myScreenShotArgs))
    , ("<XF86AudioMute>", spawn "pamixer -t")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
    -- , ("<XF86MonBrightnessUp>", increase)
    -- , ("<XF86MonBrightnessDown>", decrease)
    ]

myLayout = smartBorders $ myTall ||| Mirror myTall ||| Full
  where
    myTall = Tall 1 (10/100) (1/2)

myConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , focusFollowsMouse = False
  , layoutHook = myLayout
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#ffffff"
  }
  `additionalKeysP` myKeys

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = lowWhite "  "
    , ppTitleSanitize   = xmobarStrip
    , ppVisible         = wrap " " "" . xmobarBorder "Top" "#ffff00" 2
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = myPPOrder
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    myPPOrder [ws, l, _, _] = [ws, l]
    myPPOrder a = a

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myXmobarProp = withEasySB (statusBarProp myXmobar (pure myXmobarPP)) defToggleStrutsKey


main :: IO()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . myXmobarProp
  $ myConfig
