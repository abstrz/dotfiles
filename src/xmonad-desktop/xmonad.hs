-- <*> :: Monad m => m -> m -> m
import           XMonad
import qualified XMonad.StackSet               as W

--System
import           System.Exit

--Hooks
import           XMonad.Hooks.DynamicLog       (PP (..), dynamicLogWithPP,
                                                shorten, wrap, xmobarColor,
                                                xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks      (ToggleStruts (..), avoidStruts,
                                                docks, docksEventHook)
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.WorkspaceHistory

--Prompt
import           Control.Arrow                 (first)
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Shell
--Util
import           XMonad.Util.Run               (hPutStrLn, spawnPipe)
import           XMonad.Util.SpawnOnce

--Data
import qualified Data.Map                      as M
import           Data.Monoid

--Layout
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing

myFont :: String
myFont = "xft:Iosevka:bold:size=10:antialias=true:hinting=true"


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset --Get windows in list of screens, get current screen, get the workspace info, get

myTerminal :: String
myTerminal = "tabbed alacritty --embed"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth   = 1

myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#000055"

myModMask :: KeyMask
myModMask  = mod4Mask

altMask :: KeyMask
altMask = mod1Mask 

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
               where
                 doubleLts '<' = "<<"
                 doubleLts x = [x]
ws :: [String]                               
ws = ["system", "system", "system", "web", "web", "web", "misc"]
myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ws
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ "> " ++ ws ++ " </action>" |
                      (i, ws) <- zip [1..(length ws)] l,
                      let n = i ]


myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "black"
      , fgColor             = "AAAAAA"
      , bgHLight            = "#AAAAAA"
      , fgHLight            = "#000000"
      , borderColor         = "#AAAAAA"
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      , position            = Top
--      , position            = CenteredAt { xpCenterY = 0.0, xpWidth = 0.0 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = ""
      , autoComplete        = Nothing 
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Just 10
      }

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

      
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())                    
myKeys conf @ XConfig {XMonad.modMask = modm} = M.fromList $  
                                                              
    -- launch a terminal
    [ ((modm , xK_Return), spawn $ XMonad.terminal conf)

    -- launch ide
    , ((modm,               xK_apostrophe ), spawn "emacs")

    -- launch file-explorer
    , ((modm,               xK_d ), spawn "thunar")

    -- launch web-browser
    , ((modm,               xK_backslash),   spawn "firefox")

    -- launch reader
    , ((modm,               xK_z),           spawn "okular")

    -- launch program explorer 
    , ((modm,               xK_p),           shellPrompt myXPConfig)

    -- launch appearances
    , ((modm, xK_o),                         spawn "lxappearance")
      
    -- close focused window
    , ((modm, xK_c),                    kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ),      sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ),      setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ),      refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ),      windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ),      windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ),      windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ),      windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return),      windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ),      windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ),      windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ),      sendMessage Shrink)

    --Toggle struts
    , ((modm,               xK_b     ),      sendMessage ToggleStruts)

    -- Expand the master area
    , ((modm,               xK_l     ),      sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ),      withFocused $ windows . W.sink)

    -- Increment spacing
    , ((modm,               xK_equal ),      incWindowSpacing 1)
    -- Decrement spacing
    , ((modm,               xK_minus ),      decWindowSpacing 1)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ),      sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period),      sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ),      io exitSuccess )

    -- Restart xmonad
    , ((modm              , xK_q     ),      spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [1,0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList 

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


myLayout = avoidStruts (tiled ||| Mirror tiled ||| noBorders Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True $
              Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
               [ className =? "Gimp"           --> doFloat
               , className =? "Steam"          --> doFloat
               , className =? "Tor Browser"    --> doFloat
               , className =? "Foxit Reader"   --> doFloat
               , className =? "Lxappearance"   --> doFloat
               , className =? "Thunar"         --> doFloat ] 

myEventHook :: Event -> X All
myEventHook = serverModeEventHookCmd
              <+> serverModeEventHook
              <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
              <+> docksEventHook 

myLogSettings :: X ()
myLogSettings = fadeInactiveLogHook fadeAmount
              where fadeAmount = 1.0                  
myLogHook b = workspaceHistoryHook <+>  myLogSettings <+> dynamicLogWithPP xmobarPP
                             { ppOutput = (hPutStrLn b)
                               --ppOutput = \x -> hPutStrLn b1 x  >> hPutStrLn b2 x 
                             , ppCurrent = xmobarColor "#c3e88d" "" . wrap "(" ")" -- Current workspace in xmobar
                             , ppVisible = xmobarColor "#c18a41" ""                -- Visible but not current workspace
                             , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                             , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                             , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                             , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
                             , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                             , ppExtras  = [windowCount]                           -- # of windows current workspace
                             , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                             }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/Scripts/screens.sh"
  spawnOnce "picom -b &"
  spawnOnce "xset r rate 150 50 &"
  spawnOnce "xinput --set-prop 10 'libinput Accel Profile Enabled' 0, 1"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "nitrogen --restore"
  spawnOnce "bluetoothctl power on"
  spawnOnce "steam-tweaks"
  spawnOnce "xinput disable \"Sony Computer Entertainment Wireless Controller Touchpad\""
  spawnOnce "doas mount /dev/sdc ~/Drives/usb"
main :: IO () 
main = do
             
  xmproc1  <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobar/.xmobarrc0"
--  xmproc2 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobar/.xmobarrc1"
  xmonad $ ewmh def
       {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook xmproc1
       }
